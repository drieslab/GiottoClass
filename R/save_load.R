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

    if (!file.exists(path_to_folder)) {
        stop("path_to_folder does not exist \n")
    }

    ## 1. load giotto object
    vmsg(.v = verbose, "1. read Giotto object")

    gobject_file <- list.files(path_to_folder, pattern = "gobject")

    if (identical(gobject_file, character(0))) {
        vmsg(.v = verbose, "giotto object was not found
         skip loading giotto object")
    } else if (length(gobject_file) > 1) {
        vmsg(.v = verbose, "more than 1 giotto object was found
         skip loading giotto object")
    } else {
        if (grepl(".RDS", x = gobject_file)) {
            gobject <- do.call(
                "readRDS",
                c(
                    file = paste0(path_to_folder, "/", "gobject.RDS"),
                    load_params
                )
            )
        }

        if (grepl(".qs", x = gobject_file)) {
            package_check(pkg_name = "qs", repository = "CRAN")
            qread_fun <- get("qread", asNamespace("qs"))
            gobject <- do.call(
                qread_fun,
                c(
                    file = paste0(path_to_folder, "/", "gobject.qs"),
                    load_params
                )
            )
        }
    }






    ## 2. read in features
    vmsg(.v = verbose, "2. read Giotto feature information")
    feat_files <- list.files(
        path = paste0(path_to_folder, "/Features"),
        pattern = ".shp"
    )

    if (length(feat_files) != 0) {
        feat_names <- gsub(feat_files,
            pattern = "_feature_spatVector.shp",
            replacement = ""
        )
        feat_paths <- list.files(
            path = paste0(path_to_folder, "/Features"),
            pattern = ".shp", full.names = TRUE
        )

        vector_names_paths <- list.files(
            path = paste0(path_to_folder, "/Features"), pattern = ".txt",
            full.names = TRUE
        )

        for (feat_i in seq_len(length(feat_names))) {
            if (verbose) wrap_msg(feat_paths[feat_i])
            spatVector <- terra::vect(x = feat_paths[feat_i])

            # read in original column names and assign to spatVector
            spatVector_names <- fread(
                input = vector_names_paths[feat_i],
                header = FALSE
            )[["V1"]]
            names(spatVector) <- spatVector_names

            feat_name <- feat_names[feat_i]
            if (verbose) wrap_msg(feat_name)
            gobject@feat_info[[feat_name]]@spatVector <- spatVector
        }
    }


    ## 3. read in spatial polygons
    vmsg(.v = verbose, "3. read Giotto spatial information")

    spat_paths <- list.files(
        path = paste0(path_to_folder, "/SpatialInfo"),
        pattern = "spatVector.shp",
        full.names = TRUE
    )
    spat_files <- basename(spat_paths)

    vector_names_paths <- list.files(
        path = paste0(path_to_folder, "/SpatialInfo"),
        pattern = "spatVector_names.txt",
        full.names = TRUE
    )

    if (length(spat_files) != 0) {
        ## 3.1. shapes
        if (isTRUE(verbose)) {
            wrap_msg("3.1 read Giotto spatial shape information")
            wrap_msg(spat_files)
        }

        spat_names <- gsub(spat_files,
            pattern = "_spatInfo_spatVector.shp",
            replacement = ""
        )

        for (spat_i in seq_len(length(spat_names))) {
            spatVector <- terra::vect(x = spat_paths[spat_i])

            # read in original column names and assign to spatVector
            spatVector_names <- fread(
                input = vector_names_paths[spat_i],
                header = FALSE
            )[["V1"]]
            names(spatVector) <- spatVector_names

            spat_name <- spat_names[spat_i]
            if (isTRUE(verbose)) message(spat_name)
            gobject@spatial_info[[spat_name]]@spatVector <- spatVector
        }

        ## 3.2. centroids
        if (isTRUE(verbose)) {
            wrap_msg("\n 3.2 read Giotto spatial centroid information \n")
        }

        centroid_search_term <- gsub(spat_files,
            pattern = "_spatInfo_spatVector.shp",
            replacement = "_spatInfo_spatVectorCentroids.shp"
        )
        centroid_paths <- vapply(
            centroid_search_term,
            function(gp_centroid) {
                list.files(
                    path = paste0(path_to_folder, "/SpatialInfo"),
                    pattern = gp_centroid, full.names = TRUE
                )
            },
            FUN.VALUE = character(1L),
            USE.NAMES = FALSE)

        # check if centroid are provided for spatvector polygons
        test_missing <- unlist(lapply(centroid_paths,
            FUN = function(x) identical(x, character(0))
        ))
        centroid_paths <- centroid_paths[!test_missing]

        if (length(centroid_paths) == 0) {
            if (verbose) {
                wrap_msg("No centroids were found, centroid loading will be
                        skipped \n")
            }
        } else {
            centroid_files <- basename(centroid_paths)

            if (length(centroid_files != 0)) {
                spat_names <- gsub(centroid_files,
                    pattern = "_spatInfo_spatVectorCentroids.shp",
                    replacement = ""
                )

                vector_names_paths <- list.files(
                    path = paste0(path_to_folder, "/SpatialInfo"),
                    pattern = "spatVectorCentroids_names.txt",
                    full.names = TRUE
                )

                for (spat_i in seq_len(length(spat_names))) {
                    spatVector <- terra::vect(x = centroid_paths[spat_i])

                    # read in original column names and assign to spatVector
                    spatVector_names <- fread(
                        input = vector_names_paths[spat_i],
                        header = FALSE
                    )[["V1"]]
                    names(spatVector) <- spatVector_names

                    spat_name <- spat_names[spat_i]
                    if (isTRUE(verbose)) message(spat_name)
                    gobject@spatial_info[[spat_name]]@spatVectorCentroids <-
                        spatVector
                }
            }
        }


        ## 3.3. overlaps
        if (isTRUE(verbose)) {
            wrap_msg("\n3.3 read Giotto spatial overlap information \n")
        }

        overlap_search_term <- gsub(spat_files,
            pattern = "_spatInfo_spatVector.shp",
            replacement = "_spatInfo_spatVectorOverlaps.shp"
        )
        overlap_files <- list.files(
            path = paste0(path_to_folder, "/SpatialInfo"),
            pattern = "spatVectorOverlaps.shp"
        )

        # check if overlap information is available
        if (length(overlap_files) == 0) {
            if (verbose) {
                wrap_msg("No overlaps were found, overlap loading will be
                        skipped")
            }
        } else {
            wrap_msg(overlap_files)

            # find overlaps per spatVector
            for (sv_i in seq_along(overlap_search_term)) {
                overlap_paths <- list.files(
                    path = paste0(path_to_folder, "/SpatialInfo"),
                    pattern = overlap_search_term[sv_i], full.names = TRUE
                )
                overlap_filenames <- basename(overlap_paths)

                # get matching names files for the spatVector.shp files
                overlap_column_names <- gsub(overlap_filenames,
                    pattern = "spatVectorOverlaps.shp",
                    replacement = "spatVectorOverlaps_names.txt"
                )
                overlap_paths_colnames <- paste0(
                    dirname(overlap_paths), "/",
                    overlap_column_names
                )

                for (spat_i in seq_along(overlap_filenames)) {
                    spatVector <- terra::vect(x = overlap_paths[spat_i])

                    # read in original column names and assign to spatVector
                    spatVector_names <- fread(
                        input = overlap_paths_colnames[spat_i],
                        header = FALSE
                    )[["V1"]]
                    if (verbose) wrap_msg(spatVector_names)
                    names(spatVector) <- spatVector_names

                    feat_name <- gsub(overlap_filenames[spat_i],
                        pattern = paste0("_", overlap_search_term[sv_i]),
                        replacement = ""
                    )
                    spat_name <- gsub(overlap_filenames[spat_i],
                        pattern = paste0(feat_name, "_"),
                        replacement = ""
                    )
                    spat_name <- gsub(spat_name,
                        pattern = "_spatInfo_spatVectorOverlaps.shp",
                        replacement = ""
                    )

                    if (isTRUE(verbose)) wrap_msg(spat_name, " and ", feat_name)
                    gobject@spatial_info[[spat_name]]@overlaps[[feat_name]] <-
                        spatVector
                }
            }
        }
    }




    ## 4. images
    vmsg(.v = verbose, "\n4. read Giotto image information")

    # compatibility for pre-v0.3.0
    gobject <- .update_image_slot(gobject)

    image_files <- list.files(path = paste0(path_to_folder, "/Images"))
    if (length(image_files) != 0) {
        image_names <- unique(gsub(image_files,
            pattern = "_spatRaster.*",
            replacement = ""
        ))
        for (image_i in seq_len(length(image_names))) {
            image_name <- image_names[image_i]
            if (verbose) image_name
            new_path <- paste0(
                path_to_folder, "/Images", "/", image_name,
                "_spatRaster"
            )
            spatRaster <- terra::rast(x = new_path)

            gobject@images[[image_name]]@raster_object <- spatRaster
            gobject@images[[image_name]]@file_path <- new_path
        }
    }

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
