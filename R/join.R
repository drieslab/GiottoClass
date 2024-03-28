#### joining giotto object ####

#' @title .join_expression_matrices
#' @name .join_expression_matrices
#' @keywords internal
#' @noRd
.join_expression_matrices <- function(matrix_list) {
    # find all features
    final_feats <- list()
    for (matr_i in seq_len(length(matrix_list))) {
        rowfeats <- rownames(matrix_list[[matr_i]])
        final_feats[[matr_i]] <- rowfeats
    }

    final_feats <- unique(unlist(final_feats))
    final_feats <- mixedsort(final_feats)



    # extend matrices with missing ids
    final_mats <- list()
    for (matr_i in seq_len(length(matrix_list))) {
        matr <- matrix_list[[matr_i]]

        missing_feats <- final_feats[!final_feats %in% rownames(matr)]

        missing_mat <- Matrix::Matrix(
            data = 0,
            nrow = length(missing_feats), ncol = ncol(matr),
            dimnames = list(missing_feats, colnames(matr))
        )

        mat_ext <- rbind(matr, missing_mat)
        mat_ext <- mat_ext[match(final_feats, rownames(mat_ext)), ]

        final_mats[[matr_i]] <- mat_ext
    }

    combined_matrix <- do.call("cbind", final_mats)
    return(list(matrix = combined_matrix, sort_all_feats = final_feats))
}

#' @title .join_spatlocs
#' @name .join_spatlocs
#' @keywords internal
#' @noRd
.join_spatlocs <- function(dt_list) {
    final_list <- do.call("rbind", dt_list) # breaks DT reference
    return(final_list)
}

#' @title .join_cell_meta
#' @name .join_cell_meta
#' @keywords internal
#' @noRd
.join_cell_meta <- function(dt_list) {
    final_list <- do.call("rbind", dt_list)
    return(final_list)
}

#' @title .join_feat_meta
#' @name .join_feat_meta
#' @keywords internal
#' @noRd
.join_feat_meta <- function(dt_list) {
    feat_ID <- NULL

    comb_meta <- do.call("rbind", c(dt_list, fill = TRUE))
    comb_meta <- unique(comb_meta)

    # find feat_IDs with multiple metadata versions
    dup_feats <- unique(comb_meta[duplicated(feat_ID), feat_ID])

    # pick first entry of duplicates
    comb_meta <- comb_meta[!duplicated(feat_ID)]

    if (length(dup_feats) > 0) {
        warning(wrap_txt(
            "feature metadata: multiple versions of metadata for:\n",
            dup_feats,
            "\n First entry will be selected for joined object."
        ))
    }

    return(comb_meta)
}


#' @title Join giotto objects
#' @name joinGiottoObjects
#' @description Function to join multiple giotto objects together
#' @param gobject_list list of giotto objects
#' @param gobject_names unique giotto names for each giotto object
#' @param join_method method to join giotto objects, see details
#' @param z_vals distance(s) along z-axis if method is 
#' z-stack (default is step of 1000)
#' @param x_shift list of values to shift along x-axis if method is shift
#' @param y_shift list of values to shift along y-axis if method is shift
#' @param x_padding padding between datasets/images if method is shift
#' @param y_padding padding between datasets/images if method is shift
#' @param verbose be verbose
#' Preview where each gobject will be in space with bounding polygons
#' @details This function joins both the expression and spatial information of
#' multiple giotto objects into a single one. Giotto supports multiple ways of
#' joining spatial information as selected through param \code{join_method}:
#'
#' \itemize{
#'   \item{\strong{\code{"shift"}}} {
#'      (default) Spatial locations of different datasets are shifted
#'      by numeric vectors of values supplied through \code{x_shift},
#'      \code{y_shift}, \code{x_padding}, and \code{y_padding}. This is 
#'      particularly useful for data
#'      that is provided as tiles or ROIs or when analyzing multiple spatial 
#'      datasets together and keeping their spatial data separate.
#'
#'     \strong{If shift values are given then a value is needed for each giotto 
#'     object to be joined in \code{gobject_list}. Order matters.}
#'
#'     If a regular step value is desired instead of a specific list of values, 
#'     use \code{x_padding} and \code{y_padding}. Both shift and padding values 
#'     can be used at the same time.
#'
#'     Leaving \code{x_shift} and \code{y_shift} values as \code{NULL} will 
#'     have Giotto estimate an appropriate \code{x_shift} value based on the 
#'     x dimension of available image objects. If no image objects are 
#'     available, a default behavior of \code{x_padding = 1000} will be applied.
#'   }
#'   \item{\strong{\code{"z_stack"}}} {
#'     Datasets are spatially combined with no change to x and y
#'     spatial locations, but a z value is incorporated for each dataset based 
#'     on input supplied through param \code{z_vals}. To specify a z value for 
#'     each dataset to join, a numeric vector must be given with a value for 
#'     each element in \code{gobject_list}. Order matters.
#'
#'     Alternatively, a single numeric value can be supplied to \code{z_vals} 
#'     in which case this input will be treated as a z step value.
#'   }
#'   \item{\strong{\code{"no_change"}}} {
#'     No changes are applied to the spatial locations of the datasets when 
#'     joining.
#'   }
#' }
#'
#' @concept giotto
#' @returns giotto object
#' @export
joinGiottoObjects <- function(gobject_list,
    gobject_names = NULL,
    join_method = c("shift", "z_stack", "no_change"),
    z_vals = 1000,
    x_shift = NULL,
    y_shift = NULL,
    x_padding = NULL,
    y_padding = NULL,
    # dry_run = FALSE,
    verbose = FALSE) {
    # define for data.table
    sdimz <- cell_ID <- sdimx <- sdimy <- name <- NULL

    n_gobjects <- length(gobject_list)

    ## check general input params
    if (n_gobjects == 0L) 
        stop(wrap_txt("A list of Giotto objects to be joined must be provided.",
                    errWidth = TRUE))
    if (n_gobjects == 1L) 
        stop(wrap_txt("Only one gobject provided in gobject_list.", 
                    errWidth = TRUE))
    if (!is.vector(gobject_names) | !is.character(gobject_names)) 
        stop(wrap_txt("gobject_names need to be a vector with unique names for 
                    the giotto objects", errWidth = TRUE))
    if (n_gobjects != length(gobject_names)) 
        stop(wrap_txt("each giotto object in the list needs to have a unique 
                    (short) name", errWidth = TRUE))
    if (is.null(join_method)) 
        wrap_msg('No join_method given. Defaulting to "shift"')


    ## determine join method
    join_method <- match.arg(arg = join_method, 
                            choices = c("shift", "z_stack", "no_change"))
    if (isTRUE(verbose)) message("Join method:", join_method)


    # **** For shift workflow ****
    if (join_method == "shift") {
        # Make sure enough x_shift and y_shift values are given to cover 
        # all gobjects
        if (!is.null(x_shift)) if (length(x_shift) != n_gobjects) 
            stop(wrap_txt("A numeric vector with an x_shift value for each 
                        gobject in gobject_list must be given.\n", 
                        errWidth = TRUE))
        if (!is.null(y_shift)) if (length(y_shift) != n_gobjects) 
            stop(wrap_txt("A numeric vector with a y_shift value for each 
                        gobject in gobject_list must be given.\n", 
                        errWidth = TRUE))

        # Set defaults if no shift params are given
        if (is.null(x_shift) & is.null(y_shift) & is.null(x_padding) & 
            is.null(y_padding)) {
            wrap_msg("No xy shift or specific padding values given. 
                    Using defaults: x_padding = 1000")
            x_padding <- 1000
        }
        # Assign default padding values if NULL
        if (is.null(x_padding)) x_padding <- 0
        if (is.null(y_padding)) y_padding <- 0
    }



    # **** For no_change workflow ****
    if (join_method == "no_change") {
        join_method <- "shift"
        x_shift <- rep(0, n_gobjects)
        y_shift <- rep(0, n_gobjects)
        x_padding <- 0
        y_padding <- 0
    }



    # **** For z_stack workflow ****
    if (join_method == "z_stack") {
        if (!(is.atomic(z_vals) && is.numeric(z_vals))) {
            stop(wrap_txt("z_vals requires either a single numeric or an 
                        atomic vector of numerics with one value for each z 
                        slice (Giotto object).", errWidth = TRUE))
        }
        if ((length(z_vals) != n_gobjects) && (length(z_vals) != 1)) {
            stop(wrap_txt("If more than one z_value is given, there must be 
                        one for each Giotto object to be joined.", 
                        errWidth = TRUE))
        }

        # expand z_vals if given as a step value
        if (length(z_vals) == 1) {
            if (isTRUE(verbose)) 
                wrap_msg("Only one value given through z_vals param
                        Treating this value as a z step")
            z_vals <- (seq_len(n_gobjects) - 1) * z_vals # Find z vals stepwise
        }
    }


    # TODO # **** dry run ****
    # dry_run (experimental) Works best for join_method 'shift' or 'no_change'.
    # 
    # if(isTRUE(dry_run)) {
    #   if(isTRUE(verbose)) wrap_msg('dry_run = TRUE:
    #                                Spatial preview of join operation.')
    #   # Detect sources of bounds info
    #   avail_bound_info = list(
    #     avail_img = lapply(gobject_list, list_images),
    #     avail_spat_info = lapply(gobject_list, list_spatial_info),
    #     avail_feat_info = lapply(gobject_list, list_feature_info),
    #     avail_spatlocs = lapply(gobject_list, list_spatial_locations)
    #   )
    #   avail_bound = lapply(avail_bound_info, function(avail) {
    #     isTRUE(!is.null(unlist(avail)))
    #   })
    #   if(is.null(unlist(avail_bound))) 
    #   stop(wrap_txt('dry_run error: No shared sources of bounds info
    #   Previewing from heterogenous sources not yet implemented'))
    #
    #   bound_to_use = avail_bound_info[names(avail_bound_info)[1]]
    #
    #   # get bound info
    #   if(names(bound_to_use) == 'avail_img') {
    #
    #   }
    #   if(names(bound_to_use) == 'avail_spat_info') {
    #
    #   }
    #   if(names(bound_to_use) == 'avail_feat_info') {
    #
    #   }
    #   if(names(bound_to_use) == 'avail_spatlocs') {
    #
    #   }
    #
    # }


    # keep instructions from first giotto object
    first_instructions <- gobject_list[[1]]@instructions

    # keep features from all giotto objects
    existing_features <- vector()
    for (obj_i in seq_len(n_gobjects)) {
        obj_i_features <- gobject_list[[obj_i]]@expression_feat
        existing_features <- c(existing_features, obj_i_features)
    }
    first_features <- unique(existing_features)



    updated_object_list <- list()

    ## 0. re-scale spatial locations ##
    ## ----------------------------- ##






    ## 1. update giotto objects ##
    ## ------------------------ ##
    if (verbose == TRUE) wrap_msg("start updating objects")

    all_feat_ID_list <- list()
    all_cell_ID_list <- list()
    all_image_list <- list()
    all_largeImage_list <- list()

    xshift_list <- list()
    yshift_list <- list()

    all_spatinfo_list <- list()


    if (verbose) wrap_msg("A) Update giotto Objects \n")

    for (gobj_i in seq_len(n_gobjects)) {
        gobj <- gobject_list[[gobj_i]]
        gname <- gobject_names[[gobj_i]]


        ## 0. update cell ID and feat ID
        if (verbose) wrap_msg("0. Update cell and feature IDs \n")

        for (spat_unit in names(gobj@cell_ID)) {
            old_cell_ID <- get_cell_id(gobject = gobj, spat_unit = spat_unit)
            new_cell_ID <- paste0(gname, "-", old_cell_ID)
            all_cell_ID_list[[spat_unit]][[gobj_i]] <- new_cell_ID
            # TODO setting might not be necessary
            gobj <- set_cell_id(
                gobject = gobj,
                spat_unit = spat_unit,
                cell_IDs = new_cell_ID,
                set_defaults = FALSE
            )
        }


        # TODO this varies by active spat_unit and might no longer be needed....
        for (feat_type in names(gobj@feat_ID)) {
            all_feat_ID_list[[feat_type]][[gobj_i]] <- get_feat_id(
                gobject = gobj, feat_type = feat_type)
        }



        ## 1. update expression and all feature IDs
        if (verbose) wrap_msg("1. Update expression IDs \n")

        # provide unique cell ID name
        for (spat_unit in names(gobj@expression)) {
            for (feat_type in names(gobj@expression[[spat_unit]])) {
                for (matr in names(gobj@expression[[spat_unit]][[feat_type]])) {
                    colnames(gobj@expression[[spat_unit]][[feat_type]][[matr]][]
                            ) <- gobj@cell_ID[[spat_unit]]
                }

                # all_feat_ID_list[[feat_type]][[gobj_i
                # ]] = gobj@feat_ID[[feat_type]]
            }
        }




        ## 2. update images
        # change individual names
        if (verbose) wrap_msg("2. Update images \n")

        images_found <- !is.null(gobj@images)

        if (images_found) {
            names(gobj@images) <- paste0(gname, "-", names(gobj@images))
            for (imname in names(gobj@images)) {
                gobj@images[[imname]]@name <- paste0(gname, "-", 
                                                    gobj@images[[imname]]@name)


                if (join_method == "shift") {
                    ## shift in x-direction
                    if (is.null(x_shift)) {
                        # estimate x_shift step directly from giotto image
                        gimage <- gobj@images[[imname]]

                        my_xmax <- gimage@minmax[1]
                        my_xmin <- gimage@minmax[2]
                        xmax_b <- gimage@boundaries[1]
                        xmin_b <- gimage@boundaries[2]
                        xmin <- my_xmin - xmin_b
                        xmax <- my_xmax + xmax_b

                        add_to_x <- ((gobj_i - 1) * (xmax - xmin)) + (
                            (gobj_i - 1) * x_padding)
                    } else {
                        x_shift_i <- x_shift[[gobj_i]]
                        add_to_x <- x_shift_i + (x_padding * (gobj_i - 1))
                    }

                    if (verbose) 
                        cat("Image: for ", imname, " add_to_x = ", 
                            add_to_x, "\n")

                    gobj@images[[imname]]@minmax[c("xmax_sloc", "xmin_sloc")] <-
                        gobj@images[[imname]]@minmax[c("xmax_sloc", "xmin_sloc"
                                                    )] + add_to_x
                    xshift_list[[gobj_i]] <- add_to_x


                    ## shift in y-direction
                    if (!is.null(y_shift)) {
                        y_shift_i <- y_shift[[gobj_i]]
                        add_to_y <- y_shift_i + (y_padding * (gobj_i - 1))

                        if (verbose) 
                            cat("Image: for ", imname, " add_to_y = ", 
                                add_to_y, "\n")

                        gobj@images[[imname]]@minmax[c("ymax_sloc", "ymin_sloc"
                            )] <- gobj@images[[imname]]@minmax[c("ymax_sloc", 
                                                        "ymin_sloc")] + add_to_y
                        yshift_list[[gobj_i]] <- add_to_y
                    }
                }

                all_image_list[[imname]] <- gobj@images[[imname]]
            }
        }


        ## 2.2 update largeImages
        # change individual names

        images_found <- !is.null(gobj@largeImages)

        if (images_found) {
            names(gobj@largeImages) <- paste0(gname, "-", 
                                            names(gobj@largeImages))
            for (imname in names(gobj@largeImages)) {
                gobj@largeImages[[imname]]@name <- paste0(gname, "-", 
                                                gobj@largeImages[[imname]]@name)


                if (join_method == "shift") {
                    ## shift in x-direction (always happens if not already 
                    ## defined during giottoImage section)
                    if (!list_element_exists(xshift_list, gobj_i)) {
                        if (is.null(x_shift)) {
                            # estimate x_shift step directly from giotto image
                            extent <- terra::ext(
                                gobj@largeImages[[imname]]@raster_object)

                            xmax <- extent$xmax[[1]]
                            xmin <- extent$xmin[[1]]

                            add_to_x <- ((gobj_i - 1) * (xmax - xmin)) + 
                                ((gobj_i - 1) * x_padding)
                        } else {
                            x_shift_i <- x_shift[[gobj_i]]
                            add_to_x <- x_shift_i + (x_padding * (gobj_i - 1))
                        }

                        # record xshift (if not already done)
                        xshift_list[[gobj_i]] <- add_to_x
                    }


                    if (verbose) 
                        cat("largeImage: for ", imname, " add_to_x = ", 
                            add_to_x, "\n")



                    gobj@largeImages[[imname]]@raster_object <-
                        terra::shift(gobj@largeImages[[imname]]@raster_object, 
                                    dx = xshift_list[[gobj_i]])


                    ## shift in y-direction (only happens when y_shift 
                    ## is provided)
                    if (!is.null(y_shift)) {
                        if (!list_element_exists(yshift_list, gobj_i)) {
                            y_shift_i <- y_shift[[gobj_i]]
                            add_to_y <- y_shift_i + (y_padding * (gobj_i - 1))

                            yshift_list[[gobj_i]] <- add_to_y
                        }


                        if (verbose) 
                            cat("largeImage: for ", imname, " add_to_y = ", 
                                add_to_y, "\n")


                        gobj@largeImages[[imname]]@raster_object <-
                            terra::shift(
                                gobj@largeImages[[imname]]@raster_object, 
                                dy = yshift_list[[gobj_i]])
                    }

                    # save extent info
                    gobj@largeImages[[imname]]@extent <- terra::ext(
                        gobj@largeImages[[imname]]@raster_object)[]
                }

                all_largeImage_list[[imname]] <- gobj@largeImages[[imname]]
            }
        }




        ## 3. update spatial location
        if (verbose) wrap_msg("3. Update spatial locations \n")

        # add padding to x-axis
        # update cell ID

        # If no images were present
        if (length(xshift_list) == 0) xshift_list <- (
            (seq_along(gobject_list) - 1) * x_padding)

        available_locs <- list_spatial_locations(gobj)

        for (spat_unit_i in available_locs[["spat_unit"]]) {
            for (locs_i in available_locs[spat_unit == spat_unit_i, name]) {
                spat_obj <- get_spatial_locations(gobj,
                    spat_unit = spat_unit_i,
                    spat_loc_name = locs_i,
                    output = "spatLocsObj",
                    copy_obj = TRUE,
                    set_defaults = FALSE
                )
                myspatlocs <- slot(spat_obj, "coordinates")

                if (join_method == "z_stack") {
                    myspatlocs[, sdimz := z_vals[gobj_i]]
                    myspatlocs[, 
                        cell_ID := get_cell_id(gobj, spat_unit = spat_unit_i)]
                    myspatlocs <- myspatlocs[, .(sdimx, sdimy, sdimz, cell_ID)]
                } else if (join_method == "shift") {
                    # shift for x-axis
                    if (is.null(x_shift)) {
                        add_to_x <- xshift_list[[gobj_i]]
                    } else {
                        x_shift_i <- x_shift[[gobj_i]]
                        add_to_x <- x_shift_i + (x_padding * (gobj_i - 1))
                    }

                    if (verbose) 
                        cat("Spatial locations: for ", locs_i, 
                            " add_to_x = ", add_to_x, "\n")


                    myspatlocs[, sdimx := sdimx + add_to_x]
                    myspatlocs[, cell_ID := get_cell_id(gobj, 
                                                    spat_unit = spat_unit_i)]
                }

                # shift for y-axis
                if (!is.null(y_shift)) {
                    y_shift_i <- y_shift[[gobj_i]]
                    add_to_y <- y_shift_i + (y_padding * (gobj_i - 1))

                    if (verbose) 
                        cat("Spatial locations: for ", locs_i, 
                            " add_to_y = ", add_to_y, "\n")

                    myspatlocs[, sdimy := sdimy + add_to_y]
                }

                slot(spat_obj, "coordinates") <- myspatlocs

                ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                gobj <- set_spatial_locations(gobject = gobj, 
                                            spatlocs = spat_obj)
                ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            }
        }





        ## 4. cell metadata
        # * (feat metadata happens during joined object creation)
        # rbind metadata
        # create capture area specific names
        if (verbose) wrap_msg("4. rbind cell metadata")

        for (spat_unit in names(gobj@cell_metadata)) {
            for (feat_type in names(gobj@cell_metadata[[spat_unit]])) {
                gobj@cell_metadata[[spat_unit]][[feat_type]]@metaDT[["cell_ID"
                                                ]] <- gobj@cell_ID[[spat_unit]]
                gobj@cell_metadata[[spat_unit]][[feat_type]]@metaDT[["list_ID"
                                                            ]] <- gname
            }
        }



        ## 5. prepare spatial information
        if (verbose) wrap_msg("5. prepare spatial information")

        spatinfo_vector <- vector()
        for (spat_info in names(gobj@spatial_info)) {
            # spatVector
            if (verbose) wrap_msg("-- 5.1. spatVector")
            poly_ids <- gobj@spatial_info[[spat_info]]@spatVector$poly_ID
            gobj@spatial_info[[spat_info]]@spatVector$poly_ID <- paste0(
                gname, "-", poly_ids)

            # spatVectorCentroids
            if (verbose) wrap_msg("-- 5.2. spatVectorCentroids")
            if (!is.null(gobj@spatial_info[[spat_info]]@spatVectorCentroids)) {
                poly_ids <- gobj@spatial_info[[spat_info
                                            ]]@spatVectorCentroids$poly_ID
                gobj@spatial_info[[spat_info]]@spatVectorCentroids$poly_ID <- 
                    paste0(gname, "-", poly_ids)
            }


            # overlaps??
            if (verbose) wrap_msg("-- 5.3. overlaps")
            # TODO


            if (join_method == "shift") {
                if (verbose) wrap_msg("-- 5.4. shift")

                ## for x-axis
                if (is.null(x_shift)) {
                    add_to_x <- xshift_list[[gobj_i]]
                } else {
                    x_shift_i <- x_shift[[gobj_i]]
                    add_to_x <- x_shift_i + (x_padding * (gobj_i - 1))
                }

                ## for y-axis
                if (!is.null(y_shift)) {
                    y_shift_i <- y_shift[[gobj_i]]
                    add_to_y <- y_shift_i + (y_padding * (gobj_i - 1))
                } else {
                    add_to_y <- 0
                }

                if (verbose) 
                    cat("Spatial info: for ", spat_info, " add_to_x = ", 
                        add_to_x, "\n")
                if (verbose) 
                    cat("Spatial info: for ", spat_info, " add_to_y = ", 
                        add_to_y, "\n")

                gobj@spatial_info[[spat_info]]@spatVector <- terra::shift(
                    x = gobj@spatial_info[[spat_info]]@spatVector,
                    dx = add_to_x,
                    dy = add_to_y
                )
                if (!is.null(gobj@spatial_info[[spat_info]]@spatVectorCentroids
                            )) {
                    gobj@spatial_info[[spat_info]]@spatVectorCentroids <- 
                        terra::shift(x = gobj@spatial_info[[spat_info
                                                    ]]@spatVectorCentroids, 
                                    dx = add_to_x, dy = add_to_y)
                }
            }

            spatinfo_vector <- c(spatinfo_vector, spat_info)
            all_spatinfo_list[[gobj_i]] <- spatinfo_vector
        }


        ## 6. prepare feature information
        if (verbose) wrap_msg("6. prepare feature information \n")

        for (feat_info in names(gobj@feat_info)) {
            # spatVector
            feat_ids_uniq <- gobj@feat_info[[feat_info]]@spatVector$feat_ID_uniq
            gobj@feat_info[[feat_info]]@spatVector$feat_ID_uniq <- paste0(
                gname, "-", feat_ids_uniq)

            # networks??
            # TODO


            if (join_method == "shift") {
                ## for x-axis
                if (is.null(x_shift)) {
                    add_to_x <- xshift_list[[gobj_i]]
                } else {
                    x_shift_i <- x_shift[[gobj_i]]
                    add_to_x <- x_shift_i + (x_padding * (gobj_i - 1))
                }

                ## for y-axis
                if (!is.null(y_shift)) {
                    y_shift_i <- y_shift[[gobj_i]]
                    add_to_y <- y_shift_i + (y_padding * (gobj_i - 1))
                } else {
                    add_to_y <- 0
                }

                if (verbose) 
                    cat("Feature info: for ", feat_info, " add_to_x = ", 
                        add_to_x, "\n")
                if (verbose) 
                    cat("Feature info: for ", feat_info, " add_to_y = ", 
                        add_to_y, "\n")

                gobj@feat_info[[feat_info]]@spatVector <- terra::shift(
                    x = gobj@feat_info[[feat_info]]@spatVector, 
                    dx = add_to_x, dy = add_to_y)
            }
        }


        updated_object_list[[gobj_i]] <- gobj
    }

    # return(updated_object_list)




    ## 2. prepare for new giotto object ##
    ## -------------------------------- ##
    if (verbose) wrap_msg("B) Prepare to create new Giotto object \n")

    comb_gobject <- new("giotto",
        expression_feat = first_features,
        instructions = first_instructions,
        versions = .versions_info(),
        join_info = NULL
    )





    ## 3. merge updated data  ##
    ## ------------------------ ##
    if (verbose) wrap_msg("C) Merge updated data \n")

    first_obj <- updated_object_list[[1]]

    if (verbose) wrap_msg("1. cell and feature IDs \n")
    ## cell IDs
    for (spat_unit in names(all_cell_ID_list)) {
        combined_cell_ID <- unlist(all_cell_ID_list[[spat_unit]])
        comb_gobject@cell_ID[[spat_unit]] <- combined_cell_ID
    }

    ## feat IDs
    for (feat_type in names(all_feat_ID_list)) {
        combined_feat_ID <- unique(unlist(all_feat_ID_list[[feat_type]]))
        comb_gobject@feat_ID[[feat_type]] <- combined_feat_ID
    }



    ## expression and feat IDs
    ## if no expression matrices are provided, then just combine all feature IDs
    if (verbose) wrap_msg("2. expression data \n")

    avail_expr <- list_expression(gobject = first_obj)

    if (is.null(avail_expr)) {
        ## feat IDS
        for (feat in first_features) {
            combined_feat_ID <- unique(unlist(all_feat_ID_list[[feat]]))
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            comb_gobject <- set_feat_id(
                gobject = comb_gobject,
                feat_type = feat,
                feat_IDs = combined_feat_ID,
                set_defaults = FALSE
            )
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        }

        # Moved de novo feature metadata generation as a catch to the end of 
        # the fxn. Done through init_feat_meta()

        # S4_feat_metadata = create_feat_meta_obj(spat_unit = spat_unit,
        # feat_type = feat_type,
        # metaDT = data.table::data.table(feat_ID = combined_feat_ID))

        # comb_gobject = set_feature_metadata(gobject = comb_gobject,
        #                                     S4_feat_metadata,
        #                                     set_defaults = FALSE)
    } else {
        for (exprObj_i in seq(nrow(avail_expr))) {
            expr_list <- lapply(updated_object_list, function(gobj) {
                get_expression_values(
                    gobject = gobj,
                    spat_unit = avail_expr$spat_unit[[exprObj_i]],
                    feat_type = avail_expr$feat_type[[exprObj_i]],
                    values = avail_expr$name[[exprObj_i]],
                    output = "exprObj",
                    set_defaults = FALSE
                )
            })

            if (!.prov_match(expr_list)) 
                warning(wrap_txt("expression: provenance mismatch"))

            combmat <- .join_expression_matrices(matrix_list = lapply(
                expr_list, function(expr) expr[]))
            expr_list[[1]][] <- combmat[["matrix"]]

            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            comb_gobject <- set_expression_values(
                gobject = comb_gobject,
                values = expr_list[[1]],
                set_defaults = FALSE
            )

            comb_gobject <- set_feat_id(
                gobject = comb_gobject,
                feat_type = avail_expr$feat_type[[exprObj_i]],
                feat_IDs = combmat[["sort_all_feats"]],
                set_defaults = FALSE
            )
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

            # Moved de novo feat metadata generation to end of fxn as a catch
        }
    }




    ## spatial locations
    if (verbose) wrap_msg("3. spatial locations \n")

    available_locs <- list_spatial_locations(first_obj)

    for (slObj_i in seq(nrow(available_locs))) {
        sl_list <- lapply(updated_object_list, function(gobj) {
            get_spatial_locations(
                gobject = gobj,
                spat_unit = available_locs$spat_unit[[slObj_i]],
                spat_loc_name = available_locs$name[[slObj_i]],
                output = "spatLocsObj",
                copy_obj = FALSE
            )
        })

        if (!.prov_match(sl_list)) 
            warning(wrap_txt("spatial locations: provenance mismatch"))

        combspatlocs <- .join_spatlocs(dt_list = lapply(sl_list, 
                                                        function(sl) sl[]))
        sl_list[[1]][] <- combspatlocs

        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        comb_gobject <- set_spatial_locations(comb_gobject,
            spatlocs = sl_list[[1]],
            set_defaults = FALSE
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    }





    ## cell metadata
    if (isTRUE(verbose)) wrap_msg("4. cell metadata \n")

    for (spat_unit in names(first_obj@cell_metadata)) {
        for (feat_type in names(first_obj@cell_metadata[[spat_unit]])) {
            savelist <- list()
            for (gobj_i in seq_along(updated_object_list)) {
                cellmeta <- updated_object_list[[gobj_i
                                ]]@cell_metadata[[spat_unit]][[feat_type]][]
                savelist[[gobj_i]] <- cellmeta
            }
            combcellmeta <- .join_cell_meta(dt_list = savelist)

            S4_cell_meta <- get_cell_metadata(
                gobject = first_obj,
                spat_unit = spat_unit,
                feat_type = feat_type,
                copy_obj = TRUE,
                set_defaults = FALSE,
                output = "cellMetaObj"
            )
            S4_cell_meta[] <- combcellmeta

            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            comb_gobject <- set_cell_metadata(
                gobject = comb_gobject,
                metadata = S4_cell_meta,
                set_defaults = FALSE
            )
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        }
    }


    ## feat metadata
    # check if any already exists in first_obj
    # otherwise, skip and generate feat_metadata de novo at end
    avail_featmeta <- list_feat_metadata(gobject = first_obj)
    if (!is.null(avail_featmeta)) {
        if (isTRUE(verbose)) message("   feature metadata \n")
        for (fmObj_i in seq(nrow(avail_featmeta))) {
            fm_list <- lapply(updated_object_list, function(gobj) {
                get_feature_metadata(
                    gobject = gobj,
                    spat_unit = avail_featmeta$spat_unit[[fmObj_i]],
                    feat_type = avail_featmeta$feat_type[[fmObj_i]],
                    output = "featMetaObj",
                    copy_obj = TRUE
                )
            })

            if (!.prov_match(fm_list)) 
                warning(wrap_txt("feature metadata: provenance mismatch"))

            comb_fm <- .join_feat_meta(dt_list = lapply(fm_list, 
                                                        function(fm) fm[]))
            fm_list[[1]][] <- comb_fm

            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            comb_gobject <- set_feature_metadata(
                gobject = comb_gobject,
                metadata = fm_list[[1]],
                set_defaults = FALSE
            )
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        }
    }





    ## spatial info
    if (isTRUE(verbose)) wrap_msg("5. spatial polygon information \n")

    available_spat_info <- unique(unlist(all_spatinfo_list))

    if (isTRUE(verbose)) {
        wrap_msg("available_spat_info: \n")
        print(available_spat_info)
    }

    for (spat_info in available_spat_info) {
        savelist_vector <- list()
        savelist_centroids <- list()
        for (gobj_i in seq_along(updated_object_list)) {
            spat_information_vector <- updated_object_list[[gobj_i
                                        ]]@spatial_info[[spat_info]]@spatVector
            savelist_vector[[gobj_i]] <- spat_information_vector

            spat_information_centroids <- updated_object_list[[gobj_i
                                ]]@spatial_info[[spat_info]]@spatVectorCentroids
            savelist_centroids[[gobj_i]] <- spat_information_centroids

            # TODO: add overlaps
        }



        comb_spatvectors <- do.call("rbind", savelist_vector)
        comb_spatcentroids <- do.call("rbind", savelist_centroids)

        comb_polygon <- create_giotto_polygon_object(
            name = spat_info,
            spatVector = comb_spatvectors,
            spatVectorCentroids = comb_spatcentroids,
            overlaps = NULL
        )


        comb_gobject@spatial_info[[spat_info]] <- comb_polygon
    }



    ## feature info
    if (verbose) wrap_msg("6. spatial feature/points information \n")


    for (feat in first_features) {
        # for(feat in comb_gobject@expression_feat) {

        savelist_vector <- list()

        for (gobj_i in seq_along(updated_object_list)) {
            if (is.null(updated_object_list[[gobj_i]]@feat_info)) {
                spat_point_vector <- NULL
            } else {
                spat_point_vector <- 
                    updated_object_list[[gobj_i]]@feat_info[[feat]]@spatVector
            }

            savelist_vector[[gobj_i]] <- spat_point_vector

            # TODO: add network
        }

        comb_spatvectors <- do.call("rbind", savelist_vector)

        if (is.null(comb_spatvectors)) {
            comb_points <- NULL
        } else {
            comb_points <- create_giotto_points_object(
                feat_type = feat,
                spatVector = comb_spatvectors,
                networks = NULL
            )
        }

        comb_gobject@feat_info[[feat]] <- comb_points
    }


    ## If no feature_metadata exists, then generate now
    if (is.null(list_cell_metadata(comb_gobject))) {
        comb_gobject <- init_feat_metadata()
    }





    ## images
    if (verbose) wrap_msg("7. images \n")

    # keep individual images
    # each individual image has updated x and y locations
    # so all images can be viewed together by plotting them one-by-one
    # but images can also be easily viewed separately by grouping them
    comb_gobject@images <- all_image_list
    comb_gobject@largeImages <- all_largeImage_list


    ## TODO:
    # update giotto object with join-information
    # - list ID names
    # - xshift values

    # add option to perform yshift

    comb_gobject@join_info <- list(
        list_IDs = gobject_names,
        join_method = join_method,
        z_vals = z_vals,
        x_shift = x_shift,
        y_shift = y_shift,
        x_padding = x_padding,
        y_padding = y_padding
    )

    return(initialize(comb_gobject))
}
