#' @title saveGiotto
#' @name saveGiotto
#' @description Saves a Giotto object to a specific folder structure.
#'   Save location and behavior changes when the managed by a `gsource`
#'   inheriting project manager (see [GiottoDisk::gsource] and
#'   [GiottoDisk::gDirSource-class] for examples).
#' @param gobject Giotto object
#' @param foldername Folder name (ignored when `gsource` is managed)
#' @param dir Directory where to create the folder
#'   (ignored when `gsource` is managed)
#' @param method method to save main object
#' @param method_params additional method parameters for RDS or qs
#' @param overwrite Overwrite existing folders
#' @param export_image logical. Write out an image of the format specified by
#' `image_filetype` when saving a `giottoLargeImage`.
#' Future image loads and reconnects will point to this new file.
#' @param image_filetype the image filetype to use, see
#' \code{\link[terra]{writeRaster}}. Default is "PNG". For TIFF outputs, try
#' "COG"
#' @param include_feat_coord logical. Whether to keep the feature coordinates
#' when saving. Dropping them can improve performance for large datasets.
#' @param verbose be verbose
#' @param ... additional parameters for \code{\link[terra]{writeRaster}}
#' @returns Creates a directory with Giotto object information
#' @details Works together with \code{\link{loadGiotto}} to save and re-load
#' Giotto objects. Additional method_params need to be provided as a list
#' and will go to \code{\link[base]{saveRDS}} or \code{\link[qs]{qsave}}
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' saveGiotto(gobject = g, dir = tempdir(), overwrite = TRUE)
#' @export
saveGiotto <- function(
        gobject,
        foldername = "saveGiottoDir",
        dir = getwd(),
        method = c("RDS", "qs"),
        method_params = list(),
        overwrite = FALSE,
        export_image = TRUE,
        image_filetype = "PNG",
        include_feat_coord = TRUE,
        verbose = TRUE,
        ...) {
  
    if (!is.null(gobject@source)) {
        return(GiottoDisk::snapshotSave(gobject@source, gobject,
            method = method,
            method_params = method_params,
            overwrite = overwrite,
            export_image = export_image,
            verbose = verbose,
            ...
        ))
    }
  
    # check params
    checkmate::assert_character(foldername)
    checkmate::assert_character(dir)
    checkmate::assert_list(method_params)
    checkmate::assert_character(image_filetype)
    overwrite <- as.logical(overwrite)
    method <- match.arg(arg = method, choices = c("RDS", "qs"))

    ## set directory path and folder
    dir <- normalizePath(dir)
    final_dir <- file.path(dir, foldername)

    if (isFALSE(include_feat_coord)) {
        gobject@feat_info <- NULL
    }

    # change name of `overwrite` var to avoid confusion with underlying
    # function call params
    do_overwrite <- FALSE
    if (dir.exists(final_dir)) {
        if (!overwrite) {
            stop(wrap_txt(
                "Folder already exists and overwrite = FALSE abort saving"
            ))
        } else {
            wrap_msg("Folder already exist and overwrite = TRUE,
                    overwrite folder")
            do_overwrite <- TRUE
            use_dir <- file.path(dir, ".giotto_scratch")
            dir.create(use_dir, recursive = TRUE, showWarnings = FALSE)
        }
    } else {
        dir.create(final_dir, recursive = TRUE)
        use_dir <- final_dir
    }

    ## save spatVector objects related to feature information
    vmsg(.v = verbose, "1. Start writing feature information")
    feat_info_names <- list_feature_info_names(gobject)

    if (!is.null(feat_info_names)) {
        for (feat in feat_info_names) {
            .save_external(gobject@feat_info[[feat]],
                dir = use_dir,
                verbose = verbose
            )
        }
    }

    ## save spatVector objects related to spatial information
    if (verbose) wrap_msg("2. Start writing spatial information \n")
    spat_info_names <- list_spatial_info_names(gobject)

    if (!is.null(spat_info_names)) {
        for (spatinfo in spat_info_names) {
            .save_external(gobject@spatial_info[[spatinfo]],
                dir = use_dir,
                verbose = verbose
            )
        }
    }

    ## save images
    vmsg(.v = verbose, "3. Start writing image information")
    # only `giottoLargeImages` need to be saved separately
    image_names <- list_images_names(gobject, img_type = "largeImage")

    if (!is.null(image_names) && export_image) {
        image_dir <- paste0(use_dir, "/", "Images")
        dir.create(image_dir)
        for (image in image_names) {
            vmsg(.v = verbose, "For image information: ", image)

            img <- gobject[["images", image]][[1L]] # extract image
            r <- img@raster_object

            if (!is.null(r)) {
                # save extent info (needed for non-COG outputs)
                img@extent <- terra::ext(r)[]
                # update filepath
                filename <- file.path(image_dir, paste0(image, "_spatRaster"))
                img@file_path <- filename

                # save raster
                terra::writeRaster(
                    x = r,
                    filename = filename,
                    filetype = image_filetype,
                    NAflag = NA,
                    overwrite = TRUE
                )

                # update image in gobject
                gobject <- setGiotto(
                    gobject, img,
                    verbose = FALSE
                )
            }
        }
    }


    ## save whole Giotto object

    switch(method,
        "RDS" = do.call(
            "saveRDS",
            args = c(
                object = gobject,
                file = paste0(use_dir, "/", "gobject.RDS"),
                method_params
            )
        ),
        "qs" = {
            package_check(pkg_name = "qs", repository = "CRAN")
            qsave_fun <- get("qsave", asNamespace("qs"))
            do.call(
                qsave_fun,
                args = c(
                    x = gobject,
                    file = paste0(use_dir, "/", "gobject.qs"),
                    method_params
                )
            )
        }
    )

    ## write provenance sidecars
    # Lightroom-style: the payload above is untouched, the description of it
    # sits beside it as greppable text. Never allowed to fail a save.
    .save_sidecars(gobject, dir = use_dir, verbose = verbose)

    # effect overwrite
    if (do_overwrite) {
        unlink(x = final_dir, recursive = TRUE)
        file.rename(from = use_dir, to = final_dir)
    }
}


# Write `manifest.json` (what the object is) and `history.ndjson` (why it
# looks that way) next to a saved object. Both are derived, so a failure to
# write one is a lost convenience, never lost data - hence the warning rather
# than an error.
.save_sidecars <- function(gobject, dir, verbose = TRUE) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
        vmsg(
            .v = verbose,
            "{jsonlite} not installed: manifest sidecars not written"
        )
        return(invisible(FALSE))
    }
    tryCatch(
        {
            objManifest_json(
                gobject,
                file = file.path(dir, "manifest.json"), level = "full"
            )
            objHistory_ndjson(
                gobject,
                file = file.path(dir, "history.ndjson")
            )
            invisible(TRUE)
        },
        error = function(e) {
            warning("manifest sidecars not written: ",
                conditionMessage(e), call. = FALSE)
            invisible(FALSE)
        }
    )
}



#' @title loadGiotto
#' @name loadGiotto
#' @description Saves a Giotto object to a specific folder structure
#' @param path_to_folder path to folder where Giotto object was stored
#' with \code{\link{saveGiotto}}
#' @param load_params additional parameters for loading or reading giotto object
#' @param reconnect_giottoImage (default = TRUE) whether to attempt
#' reconnection of magick based image objects
#' @param python_path (optional) manually set your python path
#' @param init_gobject logical. Whether to initialize the `giotto` object after
#' loading. (default = TRUE)
#' @param verbose be verbose
#' @details Works together with \code{\link{saveGiotto}} to save and re-load
#' Giotto objects.
#' Additional load_params need to be provided as a list and will
#' go to \code{\link[base]{readRDS}} or \code{\link[qs]{qread}}
#' You can set the python path, alternatively it will look for an existing
#' Giotto python environment.
#' @returns Giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' td <- tempdir()
#' saveGiotto(gobject = g, dir = td)
#'
#' loadGiotto(path_to_folder = paste0(td, "/saveGiottoDir"))
#' @export
loadGiotto <- function(path_to_folder,
    load_params = list(),
    reconnect_giottoImage = TRUE,
    python_path = NULL,
    init_gobject = TRUE,
    verbose = TRUE,
    ...) {
    # data.table vars
    img_type <- NULL

    path_to_folder <- path.expand(path_to_folder)
    vmsg(.v = verbose, .is_debug = TRUE, "load from:", path_to_folder)

    if (!file.exists(path_to_folder)) {
        stop("path_to_folder does not exist \n")
    }
  
    ## 1. load giotto object
    src <- FALSE
    if (requireNamespace("GiottoDisk", quietly = TRUE)) {
        # detect if managed source. Return FALSE if not, src object if TRUE
        src <- GiottoDisk::resolveSource(path_to_folder)
    }
    if (isFALSE(src)) {
        gobject <- .load_gobject_core(
            path_to_folder = path_to_folder,
            load_params = load_params,
            verbose = verbose
        )
      
          ### ### spatial information loading ### ###
        # terra vector objects are serialized as .shp files.
        # These .shp files have to be read back in and then the relevant objects
        # in the giotto object need to be regenerated.

        ## 2. read in spatial features
        gobject <- .load_giotto_feature_info(
            gobject = gobject,
            path_to_folder = path_to_folder,
            verbose = verbose
        )


        ## 3. read in spatial polygons
        gobject <- .load_giotto_spatial_info(
            gobject = gobject,
            path_to_folder = path_to_folder,
            verbose = verbose
        )
    } else {
        gobject <- GiottoDisk::snapshotLoad(src, 
            load_params = load_params,
            verbose = verbose,
            ...
        )
    }


    ## 4. images
    # compatibility for pre-v0.3.0
    gobject <- .update_image_slot(gobject) # merge largeImages slot to images
    gobject <- .load_giotto_images(
        gobject = gobject,
        path_to_folder = path_to_folder,
        verbose = verbose
    )

    if (isTRUE(reconnect_giottoImage) && length(gobject[["images"]]) > 0) {
        imglist <- lapply(gobject[["images"]], reconnect)
        gobject <- setGiotto(gobject, imglist,
            verbose = FALSE, initialize = FALSE
        )
    }


    ## 5. Update python path (do not initialize yet)
    # ***if python should be used...***
    if (isTRUE(getOption("giotto.use_conda", TRUE))) {
        identified_python_path <- set_giotto_python_path(
            python_path = python_path,
            verbose = verbose,
            initialize = TRUE
        )
        vmsg(.v = verbose, .is_debug = TRUE, identified_python_path)
        instructions(gobject, "python_path", initialize = FALSE) <-
            identified_python_path
    } else {
        # ***if python is not needed...***
        instr <- instructions(gobject)
        instr["python_path"] <- list(NULL)
        instructions(gobject, initialize = FALSE) <- instr
    }

    ## 6. overallocate for data.tables
    # (data.tables when read from disk have a truelength of 0)
    gobject <- .giotto_alloc_dt(gobject)

    ## 7. initialize
    if (isTRUE(init_gobject)) {
        gobject <- initialize(gobject)
    }

    return(gobject)
}

# internals ####

# * internal generics ####

setGeneric(".save_external", function(x, ...) standardGeneric(".save_external"))
setGeneric(".load_external", function(x, ...) standardGeneric(".load_external"))

## * save methods ####
setMethod(".save_external", signature("ANY"), function(x, ...) return())
# dir - save directory
setMethod(".save_external", signature("giottoPolygon"), function(x, dir,
    verbose = NULL, ...) {
    dir <- file.path(dir, "SpatialInfo")
    if (!dir.exists(dir)) dir.create(dir)
    vmsg(.v = verbose, "For spatial information: ", objName(x))
    if (!is.null(x@spatVector)) {
        name_fmt1 <- paste0(objName(x), "_spatInfo_%s")
        .save_external(x@spatVector,
            name_fmt = name_fmt1,
            dir = dir,
            ...
        )
    }
    if (!is.null(x@spatVectorCentroids)) {
        name_fmt2 <- paste0(objName(x), "_spatInfo_%sCentroids")
        .save_external(x@spatVectorCentroids,
            name_fmt = name_fmt2,
            dir = dir,
            ...
        )
    }
})
setMethod(".save_external", signature("giottoPoints"), function(x, dir,
    verbose = NULL, ...) {
    dir <- file.path(dir, "Features")
    if (!dir.exists(dir)) dir.create(dir)
    vmsg(.v = verbose, "For feature: ", objName(x))
    if (is.null(x@spatVector)) return()
    name_fmt <- paste0(objName(x), "_feature_%s")
    .save_external(x@spatVector, name_fmt = name_fmt, dir = dir, ...)
})
setMethod(".save_external", signature("giottoBinPoints"), function(x, dir,
    verbose = NULL, ...) {
    dir <- file.path(dir, "Features")
    if (!dir.exists(dir)) dir.create(dir)
    vmsg(.v = verbose, "For feature: ", objName(x))
    name_fmt <- paste0(objName(x), "_feature_%s")
    .save_external(x@spatial, name_fmt = name_fmt, dir = dir,
        write_colnames = FALSE, ...
    )
})
# name - character. Used in naming for files
setMethod(".save_external", signature("SpatVector"), function(x, name_fmt, dir,
    write_colnames = TRUE, ...) {
    name <- sprintf(name_fmt, "spatVector")
    fname_txt <- file.path(dir, paste0(name, "_names.txt"))
    fname_shp <- file.path(dir, paste0(name, ".shp"))
    if (write_colnames) {
        sv_names <- names(x)
        write.table(sv_names,
            file = fname_txt,
            col.names = FALSE,
            row.names = FALSE
        )
    }
    terra::writeVector(x,
        filename = fname_shp,
        overwrite = TRUE
    )
})

## * load methods ####

setMethod(".load_external", signature("ANY"), function(x, ...) return())
setMethod(".load_external", signature("giottoPolygon"), function(x, dir, version, ...) {
    dir <- file.path(dir, "SpatialInfo")
    if (!dir.exists(dir)) return(x)
    name_fmt1 <- paste0(objName(x), "_spatInfo_%s")
    x@spatVector <- .load_external(x@spatVector,
        name_fmt = name_fmt1, dir = dir, read_colnames = TRUE, oname = objName(x), ...
    )
    name_fmt2 <- paste0(objName(x), "_spatInfo_%sCentroids")
    x@spatVectorCentroids <- .load_external(x@spatVectorCentroids,
        name_fmt = name_fmt2, dir = dir, read_colnames = TRUE, oname = objName(x), ...
    )
    if (version >= "0.5.0" || is.null(x@overlaps)) return(x)

    # catch legacy overlap outputs
    name_fmt3 <- paste0(objName(x), "_spatInfo_%sOverlaps")
    overlap_names <- names(x@overlaps)
    names(overlap_names) <- overlap_names # ensure lapply outputs are named
    x@overlaps <- lapply(overlap_names, function(overlap_feat) {
        .load_external(x@overlaps[[overlap_feat]],
            name_fmt = name_fmt3,
            dir = dir,
            read_colnames = TRUE,
            oname = objName(x),
            prefix = overlap_feat,
            ...
        )
    })
    x
})
setMethod(".load_external", signature("giottoPoints"), function(x, dir, ...) {
    dir <- file.path(dir, "Features")
    if (!dir.exists(dir)) return(x)
    name_fmt <- paste0(objName(x), "_feature_%s")
    x@spatVector <- .load_external(x@spatVector,
        name_fmt = name_fmt, dir = dir, read_colnames = TRUE, oname = objName(x), ...
    )
    x
})
setMethod(".load_external", signature("giottoBinPoints"), function(x, dir, ...) {
    dir <- file.path(dir, "Features")
    if (!dir.exists(dir)) return(x)
    name_fmt <- paste0(objName(x), "_feature_%s")
    x@spatial <- .load_external(x@spatial,
        name_fmt = name_fmt, dir = dir, read_colnames = FALSE, oname = objName(x), ...
    )
    x
})
setMethod(".load_external", signature("SpatVector"), function(x, name_fmt, dir,
    read_colnames = TRUE, verbose = NULL, oname, prefix = NULL, ...) {
    if (!is.null(prefix)) name_fmt <- paste(prefix, name_fmt, sep = "_")
    name <- sprintf(name_fmt, "spatVector")
    fname_txt <- file.path(dir, paste0(name, "_names.txt"))
    fname_shp <- file.path(dir, paste0(name, ".shp"))

    if (!file.exists(fname_shp)) return(x)
    vmsg(.v = verbose, .is_debug = TRUE, .initial = "  ",
        sprintf("[%s] %s", oname, basename(fname_shp))
    )
    sv <- terra::vect(fname_shp)
    if (read_colnames && file.exists(fname_txt)) {
        # an empty names file records a SpatVector that had no attributes.
        # `fread()` cannot read a zero byte file.
        sv_names <- character()
        if (file.size(fname_txt) > 0L) {
            sv_names <- data.table::fread(
                input = fname_txt, header = FALSE
            )[["V1"]]
        }
        if (length(sv_names) == 0L) {
            # GDAL adds a placeholder "FID" field when a shapefile is written
            # with an empty attribute table. Drop it again.
            terra::values(sv) <- NULL
        } else if (length(sv_names) != ncol(sv)) {
            stop(sprintf(
                "[%s] %s: %d saved column names for %d attributes",
                oname, basename(fname_shp), length(sv_names), ncol(sv)
            ), call. = FALSE)
        } else {
            names(sv) <- sv_names
        }
    }
    sv
})

# * helpers ####

# load in the gobject S4 object.
# the contained point-based information will need to be regenerated/reconnected
# returns either a gobject or nothing if the file is missing or errors
.load_gobject_core <- function(path_to_folder, load_params, verbose = NULL) {
    vmsg(.v = verbose, "1. read Giotto object")

    # gobject is expected to be saved with a filename like gobject.[ext]
    # This item is the main S4 structure.
    gobject_file <- list.files(path_to_folder, pattern = "gobject")

    if (identical(gobject_file, character(0))) { # no matches
        vmsg(.v = verbose, "giotto object was not found
        skip loading giotto object")
    } else if (length(gobject_file) > 1) { # more than one match
        vmsg(.v = verbose, "more than 1 giotto object was found
        skip loading giotto object")
    } else {
        # pick a reading function
        read_fun <- NULL
        if (grepl(".RDS", x = gobject_file)) { # .RDS file
            read_fun <- "readRDS"
            full_path <- file.path(path_to_folder, "gobject.RDS")
        }
        if (grepl(".qs", x = gobject_file)) { # .qs file
            package_check(pkg_name = "qs", repository = "CRAN")
            read_fun <- get("qread", asNamespace("qs"))
            full_path <- file.path(path_to_folder, "gobject.qs")
        }

        if (is.null(read_fun)) { # unrecognized file
            stop(
                "object is not a recognized save format.\n ",
                ".RDS, .qs are supported\n"
            )
        }

        # read in the object
        gobject <- do.call(
            read_fun,
            args = c(file = full_path, load_params)
        )
        return(gobject)
    }
}

# load and append spatial feature information
.load_giotto_feature_info <- function(gobject, path_to_folder, verbose = NULL) {
    vmsg(.v = verbose, "2. read Giotto feature information")
    if (is.null(gobject@feat_info)) return(gobject)

    gobject@feat_info <- lapply(gobject@feat_info, function(data) {
        .load_external(data,
            dir = path_to_folder,
            verbose = verbose
        )
    })

    return(gobject)
}

# load and append to gobject the spatial polygon information
.load_giotto_spatial_info <- function(gobject, path_to_folder, verbose = NULL) {
    vmsg(.v = verbose, "3. read Giotto spatial information")
    if (is.null(gobject@spatial_info)) return(gobject)

    gversion <- .gversion(gobject)
    gobject@spatial_info <- lapply(gobject@spatial_info, function(data) {
        .load_external(data,
            dir = path_to_folder,
            version = gversion,
            verbose = verbose
        )
    })

    return(gobject)
}


# the actual reconnection step is done through reconnect() after this step now
# .update_giotto_image() <- this is NEEDED for legacy structure support
# raster reload <- not needed
# filepath update <- now done after this, but useful for legacy saves
.load_giotto_images <- function(gobject, path_to_folder, verbose = NULL) {
    vmsg(.v = verbose, "4. read Giotto image information")
    vmsg(
        .v = verbose, .is_debug = TRUE, .initial = " ",
        box_chars()$l, "subdir: /Images/", sep = ""
    )

    imgs_dir <- file.path(path_to_folder, "Images")
    manifest <- dir_manifest(imgs_dir)
    basenames <- names(manifest)

    # basenames of imgs to load
    img_files <- basenames[grepl("_spatRaster$", basenames)]

    # return early if none, also catches when dir does not exist
    if (length(img_files) == 0) {
        return(gobject)
    }

    # parse the image name to load
    imgs <- gsub(img_files, pattern = "_spatRaster", replacement = "")

    names(img_files) <- imgs

    for (img in imgs) {
        load_img <- manifest[[img_files[[img]]]]

        vmsg(
            .v = verbose, .is_debug = TRUE, .initial = "  ",
            sprintf("[%s] %s", img, basename(load_img))
        )
        spatRaster <- terra::rast(load_img)

        gobject@images[[img]]@raster_object <- spatRaster
        # file path updating now happens during export, but keep this in for
        # legacy saved object support
        gobject@images[[img]]@file_path <- load_img
        gobject@images[[img]] <- .update_giotto_image(gobject@images[[img]])
        gobject@images[[img]] <- initialize(gobject@images[[img]])
    }

    return(gobject)
}
