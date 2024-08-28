## I/O helpers ####

#' @title saveGiotto
#' @name saveGiotto
#' @description Saves a Giotto object to a specific folder structure
#' @param gobject Giotto object
#' @param foldername Folder name
#' @param dir Directory where to create the folder
#' @param method method to save main object
#' @param method_params additional method parameters for RDS or qs
#' @param overwrite Overwrite existing folders
#' @param image_filetype the image filetype to use, see
#' \code{\link[terra]{writeRaster}}. Default is "PNG". For TIFF outputs, try
#' "COG"
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
        image_filetype = "PNG",
        verbose = TRUE,
        ...) {
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

    overwriting <- FALSE
    if (dir.exists(final_dir)) {
        if (!overwrite) {
            stop(wrap_txt(
                "Folder already exist and overwrite = FALSE abort saving"
            ))
        } else {
            wrap_msg("Folder already exist and overwrite = TRUE,
                    overwrite folder")
            overwriting <- TRUE
            use_dir <- file.path(dir, ".giotto_scratch")
            dir.create(use_dir, recursive = TRUE)
        }
    } else {
        dir.create(final_dir, recursive = TRUE)
        use_dir <- final_dir
    }

    ## save spatVector objects related to feature information
    vmsg(.v = verbose, "1. Start writing feature information")
    feat_info_names <- list_feature_info_names(gobject)

    if (!is.null(feat_info_names)) {
        feat_dir <- paste0(use_dir, "/", "Features")
        dir.create(feat_dir)
        for (feat in feat_info_names) {
            if (verbose) wrap_msg("For feature: ", feat, "\n")

            # original spatvector
            if (!is.null(gobject@feat_info[[feat]]@spatVector)) {
                # write names of spatvector
                spatvecnames <- names(gobject@feat_info[[feat]]@spatVector)
                filename_names <- paste0(
                    feat_dir, "/", feat, "_feature_spatVector_names.txt"
                )
                write.table(
                    x = spatvecnames,
                    file = filename_names,
                    col.names = FALSE,
                    row.names = FALSE
                )

                # write spatvector
                filename <- paste0(
                    feat_dir, "/", feat, "_feature_spatVector.shp"
                )
                terra::writeVector(
                    x = gobject@feat_info[[feat]]@spatVector,
                    filename = filename
                )
            }

            # network
            # ? data.table object
        }
    }


    ## save spatVector objects related to spatial information
    if (verbose) wrap_msg("2. Start writing spatial information \n")
    spat_info_names <- list_spatial_info_names(gobject)

    if (!is.null(spat_info_names)) {
        spatinfo_dir <- paste0(use_dir, "/", "SpatialInfo")
        dir.create(spatinfo_dir)
        for (spatinfo in spat_info_names) {
            vmsg(.v = verbose, "For spatial information: ", spatinfo)

            # original spatVectors
            if (!is.null(gobject@spatial_info[[spatinfo]]@spatVector)) {
                # write names of spatvector
                spatvecnames <- names(
                    gobject@spatial_info[[spatinfo]]@spatVector
                )
                filename_names <- paste0(
                    spatinfo_dir, "/", spatinfo,
                    "_spatInfo_spatVector_names.txt"
                )
                write.table(
                    x = spatvecnames, file = filename_names,
                    col.names = FALSE, row.names = FALSE
                )

                # write spatvector
                filename <- paste0(
                    spatinfo_dir, "/", spatinfo,
                    "_spatInfo_spatVector.shp"
                )
                terra::writeVector(
                    gobject@spatial_info[[spatinfo]]@spatVector,
                    filename = filename
                )
            }

            # spatVectorCentroids
            if (!is.null(
                gobject@spatial_info[[spatinfo]]@spatVectorCentroids
            )) {
                # write names of spatvector
                spatvecnames <- names(
                    gobject@spatial_info[[spatinfo]]@spatVectorCentroids
                )
                filename_names <- paste0(
                    spatinfo_dir, "/", spatinfo,
                    "_spatInfo_spatVectorCentroids_names.txt"
                )
                write.table(
                    x = spatvecnames, file = filename_names,
                    col.names = FALSE, row.names = FALSE
                )

                # write spatvector
                filename <- paste0(
                    spatinfo_dir, "/", spatinfo,
                    "_spatInfo_spatVectorCentroids.shp"
                )
                terra::writeVector(
                    gobject@spatial_info[[spatinfo]]@spatVectorCentroids,
                    filename = filename
                )
            }

            # overlap information
            if (!is.null(gobject@spatial_info[[spatinfo]]@overlaps)) {
                for (feature in names(
                    gobject@spatial_info[[spatinfo]]@overlaps
                )) {
                    if (feature == "intensity") next
                    # intensities are stored as data.table
                    # They are already saveable with the rest of the gobject.
                    # Skip.

                    # write names of spatvector
                    spatvecnames <- names(
                        gobject@spatial_info[[spatinfo]]@overlaps[[feature]]
                    )
                    filename_names <- paste0(
                        spatinfo_dir, "/", feature, "_",
                        spatinfo, "_spatInfo_spatVectorOverlaps_names.txt"
                    )
                    write.table(
                        x = spatvecnames, file = filename_names,
                        col.names = FALSE, row.names = FALSE
                    )

                    # write spatvector
                    filename <- paste0(
                        spatinfo_dir, "/", feature, "_",
                        spatinfo,
                        "_spatInfo_spatVectorOverlaps.shp"
                    )
                    terra::writeVector(
                        gobject@spatial_info[[spatinfo]]@overlaps[[feature]],
                        filename = filename
                    )
                }
            }
        }
    }



    ## save images
    vmsg(.v = verbose, "3. Start writing image information")
    # only `giottoLargeImages` need to be saved separately
    image_names <- list_images_names(gobject, img_type = "largeImage")

    if (!is.null(image_names)) {
        image_dir <- paste0(use_dir, "/", "Images")
        dir.create(image_dir)
        for (image in image_names) {
            vmsg(.v = verbose, "For image information: ", image)

            r <- gobject@images[[image]]@raster_object

            if (!is.null(r)) {
                # save extent info just in case
                gobject@images[[image]]@extent <- terra::ext(r)[]

                # save raster
                filename <- paste0(image_dir, "/", image, "_spatRaster")
                terra::writeRaster(
                    x = r,
                    filename = filename,
                    filetype = image_filetype,
                    NAflag = NA,
                    overwrite = TRUE
                ) # test
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

    # effect overwrite
    if (overwrite && overwriting) {
        unlink(x = final_dir, recursive = TRUE)
        file.rename(from = use_dir, to = final_dir)
    }
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
    verbose = TRUE) {
    # data.table vars
    img_type <- NULL

    path_to_folder <- path.expand(path_to_folder)
    vmsg(.v = verbose, .is_debug = TRUE, "load from:", path_to_folder)

    if (!file.exists(path_to_folder)) {
        stop("path_to_folder does not exist \n")
    }

    ## 1. load giotto object
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


    ## 4. images
    # compatibility for pre-v0.3.0
    gobject <- .update_image_slot(gobject)
    gobject <- .load_giotto_images(
        gobject = gobject,
        path_to_folder = path_to_folder,
        verbose = verbose
    )
    
    if (isTRUE(reconnect_giottoImage)) {
        if (!is.null(list_images(gobject))) {
            if (list_images(gobject)[img_type == "image", .N] > 0) {
                gobject <- reconnectGiottoImage(gobject,
                    reconnect_type = "image"
                )
            }
        }
    }


    ## 5. Update python path (do not initialize yet)
    # ***if python should be used...***
    if (isTRUE(getOption("giotto.use_conda", TRUE))) {
        identified_python_path <- set_giotto_python_path(
            python_path = python_path,
            verbose = verbose
        )
        gobject <- changeGiottoInstructions(
            gobject = gobject,
            params = c("python_path"),
            new_values = c(identified_python_path),
            init_gobject = FALSE
        )
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


# load in the gobject S4 object.
# the contained point-based information will need to be regenerated/reconnected
# returns either a gobject or nothing if the file is missing or errors
.load_gobject_core <- function(
        path_to_folder, load_params, verbose = NULL
) {
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
            stop("object is not a recognized save format.\n ",
                 ".RDS, .qs are supported\n")
        }
        
        # read in the object
        gobject <- do.call(
            read_fun, args = c(file = full_path, load_params)
        )
        return(gobject)
    }
}

# load and append spatial feature information
.load_giotto_feature_info <- function(
        gobject, path_to_folder, verbose = NULL
) {
    vmsg(.v = verbose, "2. read Giotto feature information")
    vmsg(.v = verbose, .is_debug = TRUE, .initial = " ", 
         box_chars()$l, "subdir: /Features/", sep = "")
    
    feats_dir <- file.path(path_to_folder, "Features")
    manifest <- dir_manifest(feats_dir)
    basenames <- names(manifest)
    
    # basenames of .shp files to load
    shp_files <- basenames[grepl(".shp", basenames)]
    
    # return early if none, also catches when dir does not exist
    if (length(shp_files) == 0) return(gobject)
    
    # parse the feature type(s) to load from the .shp basenames
    feats <- gsub(shp_files, 
        pattern = "_feature_spatVector.shp", replacement = ""
    )
    
    # basenames of .txt files to load
    # These have attribute info names (e.g. feat_ID, feat_ID_uniq)
    # this is done since serialized SpatVectors may have clipped names.
    txt_files <- paste0(feats, "_feature_spatVector_names.txt")
    
    # ordering of files follow feats.
    # Apply name to make indexing simple and unique
    names(shp_files) <- names(txt_files) <- feats
    
    # iterate through features discovered and load/regenerate each
    # then append the information to the gobject
    for (feat in feats) {
        load_shp <- manifest[[shp_files[[feat]]]]
        load_txt <- manifest[[txt_files[[feat]]]]

        vmsg(.v = verbose, .is_debug = TRUE, .initial = "  ",
             sprintf("[%s] %s", feat, basename(load_shp)))
        spatVector <- terra::vect(x = load_shp)
        
        # read in original column names and assign to SpatVector
        spatVector_names <- data.table::fread(
            input = load_txt, header = FALSE
        )[["V1"]]
        names(spatVector) <- spatVector_names
        
        gobject@feat_info[[feat]]@spatVector <- spatVector
    }

    return(gobject)
}

# load and append to gobject the spatial polygon information
.load_giotto_spatial_info <- function(
        gobject, path_to_folder, verbose = NULL
) {
    vmsg(.v = verbose, "3. read Giotto spatial information")
    vmsg(.v = verbose, .is_debug = TRUE, .initial = " ",
         box_chars()$l, "subdir: /SpatialInfo/", sep = "")
    
    spat_dir <- file.path(path_to_folder, "SpatialInfo")
    manifest <- dir_manifest(spat_dir)
    basenames <- names(manifest)
    
    # basenames of .shp files to load
    # there are other .shp files for centroids and overlaps in this dir
    # so the search term is more specific
    shp_files <- basenames[grepl("spatVector.shp", basenames)]
    
    # return early if none, also catches when dir does not exist
    if (length(shp_files) == 0) return(gobject) 
    
    ## 3.1. shapes
    vmsg(.v = verbose, "3.1 read Giotto spatial shape information")
    
    # parse the spatial unit(s) to load from the .shp basenames
    spats <- gsub(shp_files,
        pattern = "_spatInfo_spatVector.shp", replacement = "" 
    )
    
    # basenames of .txt files to load
    # .shp files may clip these normally, so we load them separately
    txt_files <- paste0(spats, "_spatInfo_spatVector_names.txt")
    
    # ordering of files follow spats.
    # Apply name to make indexing simple and unique
    names(shp_files) <- names(txt_files) <- spats

    # iterate through spat units discovered and load/regen each
    # then append the info to the gobject
    for (spat in spats) {
        load_shp <- manifest[[shp_files[[spat]]]]
        load_txt <- manifest[[txt_files[[spat]]]]
        
        vmsg(.v = verbose, .is_debug = TRUE, .initial = "  ",
             sprintf("[%s] %s", spat, basename(load_shp)))
        spatVector <- terra::vect(x = load_shp)
        
        # read in original column names and assign to spatVector
        spatVector_names <- data.table::fread(
            input = load_txt, header = FALSE
        )[["V1"]]
        names(spatVector) <- spatVector_names
        
        gobject@spatial_info[[spat]]@spatVector <- spatVector
    }
    

    # load centroids of gpoly
    gobject <- .load_giotto_spatial_info_centroids(
        gobject = gobject,
        manifest = manifest,
        basenames = basenames,
        spats = spats,
        verbose = verbose
    )
    
    # load overlaps of gpoly
    gobject <- .load_giotto_spatial_info_overlaps(
        gobject = gobject,
        manifest = manifest,
        verbose = verbose
    )
    
    return(gobject)
}

# load and append to gobject the polygons centroids information
.load_giotto_spatial_info_centroids <- function(
        gobject, manifest, basenames, spats, verbose = NULL
) {
    ## 3.2. centroids
    vmsg(.v = verbose, "3.2 read Giotto spatial centroid information \n")
    
    # these files are optional, depending on if they have been calculated.
    # They may not exist
    
    shp_search <- paste0(spats, "_spatInfo_spatVectorCentroids.shp")
    shp_files <- basenames[basenames %in% shp_search]
    
    # return early if none exist
    if (length(shp_files) == 0) return(gobject)
    
    txt_files <- paste0(spats, "_spatInfo_spatVectorCentroids_names.txt")
    
    # ordering of files follow spats
    # apply name for simple and unique indexing
    names(shp_files) <- names(txt_files) <- spats
    
    # iterate through spat_units and load/regen then append the data
    # to the gobject
    for (spat in spats) {
        load_shp <- manifest[[shp_files[[spat]]]]
        load_txt <- manifest[[txt_files[[spat]]]]
        
        if (is.null(load_shp)) next # skip to next spat_unit if none
        vmsg(.v = verbose, .is_debug = TRUE, .initial = "  ",
             sprintf("[%s] %s", spat, basename(load_shp)))
        spatVector <- terra::vect(load_shp)
        
        # read in original column names and assign to spatVector
        spatVector_names <- data.table::fread(
            input = load_txt, header = FALSE
        )[["V1"]]
        names(spatVector) <- spatVector_names
        
        gobject@spatial_info[[spat]]@spatVectorCentroids <- spatVector
    }
    return(gobject)
}

# load and append to gobject the polygons overlaps information
.load_giotto_spatial_info_overlaps <- function(
        gobject, manifest, verbose = NULL
) {
    ## 3.3. overlaps
    vmsg(.v = verbose, "3.3 read Giotto spatial overlap information \n")
    
    si <- get_polygon_info_list(gobject) # none case taken care of in 3.1
    spats <- names(si)
    
    # These files are optional, depending on if they have been calculated.
    # They may not exist
    # They are named in "feattype_spatunit_postfix.extension" convention
    
    for (spat in spats) {
        feats <- .gpoly_overlap_names(si[[spat]], type = "point")
        if (is.null(feats)) next # goto next spat_unit if no overlaps
        
        for(feat in feats) {
            
            # format: feattype_spatunit
            comb <- paste(feat, spat, sep = "_")
            
            # format: feattype_spatunit_postfix.extension
            shp_file <- paste0(comb, "_spatInfo_spatVectorOverlaps.shp")
            txt_file <- paste0(comb, "_spatInfo_spatVectorOverlaps_names.txt")
            load_shp <- manifest[[shp_file]]
            load_txt <- manifest[[txt_file]]
            
            vmsg(.v = verbose, .is_debug = TRUE, .initial = "  ",
                 sprintf("[%s and %s] %s", spat, feat, basename(load_shp)))
            spatVector <- terra::vect(load_shp)
            
            # read in original column names
            spatVector_names <- data.table::fread(
                input = load_txt, header = FALSE
            )[["V1"]]
            names(spatVector) <- spatVector_names
            
            # append
            gobject@spatial_info[[spat]]@overlaps[[feat]] <- spatVector
        }
    }
    
    return(gobject)
}


.load_giotto_images <- function(gobject, path_to_folder, verbose = NULL) {
    
    vmsg(.v = verbose, "4. read Giotto image information")
    vmsg(.v = verbose, .is_debug = TRUE, .initial = " ", 
         box_chars()$l, "subdir: /Images/", sep = "")

    imgs_dir <- file.path(path_to_folder, "Images")
    manifest <- dir_manifest(imgs_dir)
    basenames <- names(manifest)
    
    # basenames of imgs to load
    img_files <- basenames[grepl("_spatRaster$", basenames)]
    
    # return early if none, also catches when dir does not exist
    if (length(img_files) == 0) return(gobject)
    
    # parse the image name to load
    imgs <- gsub(img_files, pattern = "_spatRaster", replacement = "")
    
    names(img_files) <- imgs
    
    for (img in imgs) {
        load_img <- manifest[[img_files[[img]]]]
        
        vmsg(.v = verbose, .is_debug = TRUE, .initial = "  ",
             sprintf("[%s] %s", img, basename(load_img)))
        spatRaster <- terra::rast(load_img)
        
        gobject@images[[img]]@raster_object <- spatRaster
        gobject@images[[img]]@file_path <- load_img
    }
    
    return(gobject)
}

.gpoly_overlap_names <- function(x, type = c("point", "intensity")) {
    type <- match.arg(type, choices = c("point", "intensity"))
    ovlps <- overlaps(x)
    if (is.null(ovlps)) return(NULL)
    
    switch(type,
        "point" = {
            res <- names(ovlps)
            res <- res[res != "intensity"]
            if (length(res) == 0) res <- NULL
            return(res)
        },
        "intensity" = {
            res <- names(ovlps$intensity)
        }
    )
    return(res)
}

