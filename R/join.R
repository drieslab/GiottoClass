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
#' @param x_shift numeric vector of values to shift along x-axis if method is
#' "shift"
#' @param y_shift numeric vector of values to shift along y-axis if method is
#' "shift"
#' @param x_padding x padding between datasets if method is shift
#' @param y_padding y padding between datasets if method is shift. Only applied
#' when y shifts are given.
#' @param dry_run logical. Plot expected object locations after join, but
#' do not actually perform it.
#' @param verbose be verbose
#' Preview where each gobject will be in space with bounding polygons
#' @returns giotto object
#' @details This function joins both the expression and spatial information of
#' multiple giotto objects into a single one. Giotto supports multiple ways of
#' joining spatial information as selected through param `join_method`:
#'
#'   * **"shift"** 
#'      (default) Spatial locations of different datasets are shifted
#'      by numeric vectors of values supplied through `x_shift`,
#'      `y_shift`, `x_padding`, and `y_padding`. This is particularly useful 
#'      for data that is provided as tiles or ROIs or when analyzing multiple 
#'      spatial datasets together and keeping their spatial data separate.
#'
#'     **If shift values are given then a value is needed for each giotto
#'     object to be joined in `gobject_list`. Order matters.**
#'
#'     If a regular step value is desired instead of a specific list of values,
#'     use `x_padding` and `y_padding`. Both shift and padding values
#'     can be used at the same time.
#'
#'     When `x_shift` is `NULL`, it defaults to the x range of gobjects in the 
#'     list so that datasets are xshifted exactly next to each other with no 
#'     overlaps. An additional default `x_padding = 1000` is applied if
#'     `x_shift`, `x_padding`, `y_shift`, `y_padding` are all `NULL`.
#'   * **"z_stack"**
#'     Datasets are spatially combined with no change to x and y
#'     spatial locations, but a z value is incorporated for each dataset based
#'     on input supplied through param `z_vals`. To specify a z value for
#'     each dataset to join, a numeric vector must be given with a value for
#'     each element in `gobject_list`. Order matters.
#'
#'     Alternatively, a single numeric value can be supplied to `z_vals`
#'     in which case this input will be treated as a z step value.
#'   * **"no_change"**
#'     No changes are applied to the spatial locations of the datasets when
#'     joining.
#'
#' @concept giotto
#' @examples
#' # joining objects with no spatial information
#' m1 <- matrix(rnorm(100), nrow = 10)
#' m2 <- matrix(rnorm(100), nrow = 10)
#' colnames(m1) <- paste0("cell_", seq_len(10))
#' colnames(m2) <- paste0("cell_", seq_len(10))
#' rownames(m1) <- rownames(m2) <- paste0("feat_", seq_len(10))
#'
#' g1 <- createGiottoObject(expression = m1)
#' g2 <- createGiottoObject(expression = m2)
#'
#' joinGiottoObjects(
#'     gobject_list = list(g1, g2), 
#'     gobject_names = c("g1", "g2")
#' )
#' 
#' # dry run joining objects with spatial information
#' # a default x_padding of 1000 is applied
#' viz <- GiottoData::loadGiottoMini("viz")
#' joinGiottoObjects(
#'     list(viz, viz),
#'     gobject_names = c("v1", "v2"),
#'     dry_run = TRUE
#' )
#' 
#' # place them right next to each other
#' # note that this means generated spatial networks will be more likely to 
#' # link across the datasets
#' joinGiottoObjects(
#'     list(viz, viz),
#'     gobject_names = c("v1", "v2"),
#'     dry_run = TRUE,
#'     x_padding = 0
#' )
#' 
#' # join the spatial objects
#' joined_viz <- joinGiottoObjects(
#'     list(viz, viz),
#'     gobject_names = c("v1", "v2")
#' )
#' 
#' @export
joinGiottoObjects <- function(gobject_list,
    gobject_names = NULL,
    join_method = c("shift", "z_stack", "no_change"),
    z_vals = 1000,
    x_shift = NULL,
    y_shift = NULL,
    x_padding = NULL,
    y_padding = NULL,
    dry_run = FALSE,
    verbose = FALSE) {
    # NSE vars
    sdimz <- cell_ID <- sdimx <- sdimy <- name <- NULL

    n_gobjects <- length(gobject_list)
    gobj_idx <- seq_len(n_gobjects)

    ## check general input params
    if (n_gobjects == 0L) {
        stop(wrap_txt("A list of Giotto objects to be joined must be provided.",
            errWidth = TRUE
        ))
    }
    if (n_gobjects == 1L) {
        stop(wrap_txt("Only one gobject provided in gobject_list.",
            errWidth = TRUE
        ))
    }
    if (!is.vector(gobject_names) | !is.character(gobject_names)) {
        stop(wrap_txt("gobject_names need to be a vector with unique names for
                    the giotto objects", errWidth = TRUE))
    }
    if (n_gobjects != length(gobject_names)) {
        stop(wrap_txt("each giotto object in the list needs to have a unique
                    (short) name", errWidth = TRUE))
    }
    if (is.null(join_method)) {
        wrap_msg('No join_method given. Defaulting to "shift"')
    }


    ## determine join method
    join_method <- match.arg(
        arg = join_method,
        choices = c("shift", "z_stack", "no_change")
    )
    vmsg(.v = verbose, "Join method:", join_method)


    # **** For shift workflow ****
    if (join_method == "shift") {
        # If provided, ensure enough x_shift and y_shift values are given
        # to cover all gobjects
        if (!is.null(x_shift)) {
            if (length(x_shift) != n_gobjects) {
                stop(wrap_txt("A numeric vector with an x_shift value for each
                        gobject in gobject_list must be given.\n",
                    errWidth = TRUE
                ))
            }
        }
        if (!is.null(y_shift)) {
            if (length(y_shift) != n_gobjects) {
                stop(wrap_txt("A numeric vector with a y_shift value for each
                        gobject in gobject_list must be given.\n",
                    errWidth = TRUE
                ))
            }
        }

        # Set default x_padding = 1000 if no shift params are given
        if (is.null(x_shift) && is.null(y_shift) &&
            is.null(x_padding) && is.null(y_padding)) {
            vmsg(.v = verbose,
                 "No xy shift or specific padding values given.
                 Using defaults: x_padding = 1000
                 Set any padding value of 0 to avoid this behavior")
            x_padding <- 1000
        }
        # Assign default padding values if NULL
        x_padding <- x_padding %null% 0
        y_padding <- y_padding %null% 0

        ## find object extents
        gext <- lapply(gobject_list, function(g) ext(g, all_data = TRUE))

        # x shift value is always necessary
        # object extent can be used to provide a default for shift value

        if (is.null(x_shift)) {
            # if no x_shift provide default x_shift as object ext x range
            x_shift <- vapply(
                gobj_idx, FUN.VALUE = numeric(length = 1L),
                function(g_i) {
                    range(gext[[g_i]])[["x"]]
                }
            )
            # when x_shift is the default based on xrange, skip the first
            # x_shift since the first object can be at x0
            final_x_shift <- (x_shift * (gobj_idx - 1)) +
                (x_padding * (gobj_idx - 1))
        } else {
            # when x_shift is explicit, include the first x_shift
            final_x_shift <- x_shift + (x_padding * (gobj_idx - 1))
        }


        # NO calculated defaults for y shifts with join_method = "shift"
        # * any y shifting or padding only applied when y_shift is NOT NULL
        if (!is.null(y_shift)) {
            final_y_shift <- y_shift + (y_padding * (gobj_idx - 1))
        } else {
            final_y_shift <- rep(0, n_gobjects)
        }
    }



    # **** For no_change workflow ****
    if (join_method == "no_change") {
        join_method <- "shift"
        final_x_shift <- rep(0, n_gobjects)
        final_y_shift <- rep(0, n_gobjects)
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
                errWidth = TRUE
            ))
        }

        # expand z_vals if given as a step value
        if (length(z_vals) == 1) {
            if (isTRUE(verbose)) {
                wrap_msg("Only one value given through z_vals param
                        Treating this value as a z step")
            }
            z_vals <- (seq_len(n_gobjects) - 1) * z_vals # Find z vals stepwise
        }
    }


    # if dry run, plot expected final locations then return early
    if (isTRUE(dry_run) && join_method %in% c("no_change", "shift")) {
        gobj_polys <- lapply(seq_along(gext), function(ge_i) {
            gp <- terra::as.polygons(gext[[ge_i]])
            # perform transforms
            gp <- terra::shift(
                gp, dx = final_x_shift[[ge_i]], dy = final_y_shift[[ge_i]]
            )
            return(gp)
        })
        gobj_polys <- Reduce(f = "rbind", gobj_polys)
        terra::plot(gobj_polys, border = getRainbowColors(n_gobjects))
        return(invisible())
    }


    # perform xy shifts ("no_change" method can be ignored)
    # "z_stack" method happens elsewhere
    if (join_method == "shift") {
        gobject_list <- lapply(gobj_idx, function(g_i) {
            spatShift(
                gobject_list[[g_i]],
                dx = final_x_shift[[g_i]],
                dy = final_y_shift[[g_i]]
            )
        })
    }



    # keep instructions from first giotto object
    first_instructions <- gobject_list[[1]]@instructions

    # keep features from all giotto objects
    existing_features <- vector()
    for (obj_i in seq_len(n_gobjects)) {
        obj_i_features <- gobject_list[[obj_i]]@expression_feat
        existing_features <- c(existing_features, obj_i_features)
    }
    first_features <- unique(existing_features)



    ## 0. re-scale spatial locations ##
    ## ----------------------------- ##






    ## 1. update giotto objects ##
    ## ------------------------ ##
    vmsg(.v = verbose, "start updating objects")

    # initialize data lists for collecting info from each gobject
    all_feat_ID_list <- list()
    all_cell_ID_list <- list()
    all_image_list <- list()
    all_spatinfo_list <- list()

    # initialize list of updated gobjects
    updated_object_list <- list()


    vmsg(.v = verbose, "A) Update giotto Objects")

    # update ids across all objects
    # perform any remaining transforms
    # use for loops so that items can be appended to running lists
    for (gobj_i in seq_len(n_gobjects)) {
        gobj <- gobject_list[[gobj_i]]
        gname <- gobject_names[[gobj_i]]

        vmsg(.v = verbose, sprintf("giotto object [%s]", obj_i))


        ## 0. update cell ID and feat ID ##
        ## ------------------------------ ##
        if (verbose) wrap_msg("0. Update cell and feature IDs")
        # spat ids
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
        # feat ids
        # TODO this varies by active spat_unit and might no longer be needed....
        for (feat_type in names(gobj@feat_ID)) {
            all_feat_ID_list[[feat_type]][[gobj_i]] <- get_feat_id(
                gobject = gobj, feat_type = feat_type
            )
        }





        ## 1. update expression and all feature IDs ##
        ## ---------------------------------------- ##
        if (verbose) wrap_msg("1. Update expression IDs")

        # provide unique cell ID name
        # TODO clean this code up with ":all:" compatible accessor
        for (spat_unit in names(gobj@expression)) {
            for (feat_type in names(gobj@expression[[spat_unit]])) {
                for (matr in names(gobj@expression[[spat_unit]][[feat_type]])) {
                    colnames(gobj@expression[[spat_unit]][[feat_type]][[matr]][]) <- gobj@cell_ID[[spat_unit]]
                }
            }
        }


        ## 2. update images ##
        ## ---------------- ##
        # change individual names
        vmsg(.v = verbose, "2. Update images")

        avail_imgs <- getGiottoImage(gobj, name = ":all:")
        if (!is.null(avail_imgs)) {
            new_img_names <- paste0(gname, "-", objName(avail_imgs))
            objName(avail_imgs) <- new_img_names
            names(avail_imgs) <- new_img_names
        }

        all_image_list <- c(all_image_list, avail_imgs)


        ## 3. update spatial location ##
        ## -------------------------- ##
        vmsg(.v = verbose, "3. Update spatial locations")

        # get all spatLocsObj in the gobj
        available_locs <- getSpatialLocations(
            gobj, spat_unit = ":all:", name = ":all:", output = "spatLocsObj",
            copy_obj = TRUE, verbose = FALSE, set_defaults = FALSE,
            simplify = FALSE
        )

        # update cell IDs to joined object cell IDs
        # perform any spatShifts
        available_locs <- lapply(available_locs, function(sl) {
            # update cell_ids
            sl <- .replace_spat_ids_spatlocsobj(sl,
                ids = get_cell_id(gobj, spat_unit = spatUnit(sl))
            )

            # spatial shifts
            if (join_method == "z_stack") {
                spatShift(sl, dz = z_vals[gobj_i])
            }
            return(sl)
        })

        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobj <- setGiotto(
            gobj, available_locs, verbose = FALSE, initialize = FALSE
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



        ## 4. cell metadata ##
        ## ---------------- ##
        # * (feat metadata happens during joined object creation)
        vmsg(.v = verbose, "4. Update cell metadata")

        # update IDs
        for (spat_unit in names(gobj@cell_metadata)) {
            for (feat_type in names(gobj@cell_metadata[[spat_unit]])) {
                gobj@cell_metadata[[spat_unit]][[feat_type]]@metaDT[[
                    "cell_ID"
                ]] <- gobj@cell_ID[[spat_unit]]
                gobj@cell_metadata[[spat_unit]][[feat_type]]@metaDT[[
                    "list_ID"
                ]] <- gname
            }
        }



        ## 5. prepare spatial information ##
        ## ------------------------------ ##
        vmsg(.v = verbose, "5. prepare spatial information")

        spatinfo_vector <- vector()
        for (spat_info in names(gobj@spatial_info)) {
            # spatVector
            vmsg(.v = verbose, "-- 5.1. spatVector")
            poly_ids <- gobj@spatial_info[[spat_info]]@spatVector$poly_ID
            gobj@spatial_info[[spat_info]]@spatVector$poly_ID <- paste0(
                gname, "-", poly_ids
            )

            # spatVectorCentroids
            vmsg(.v = verbose, "-- 5.2. spatVectorCentroids")
            if (!is.null(gobj@spatial_info[[spat_info]]@spatVectorCentroids)) {
                poly_ids <- gobj@spatial_info[[
                    spat_info
                ]]@spatVectorCentroids$poly_ID
                gobj@spatial_info[[spat_info]]@spatVectorCentroids$poly_ID <-
                    paste0(gname, "-", poly_ids)
            }


            # overlaps??
            vmsg(.v = verbose, "-- 5.3. overlaps")
            # TODO

            spatinfo_vector <- c(spatinfo_vector, spat_info)
            all_spatinfo_list[[gobj_i]] <- spatinfo_vector
        }


        ## 6. prepare feature information
        vmsg(.v = verbose, "6. prepare feature information")

        for (feat_info in names(gobj@feat_info)) {
            # spatVector
            feat_ids_uniq <- gobj@feat_info[[feat_info]]@spatVector$feat_ID_uniq
            gobj@feat_info[[feat_info]]@spatVector$feat_ID_uniq <- paste0(
                gname, "-", feat_ids_uniq
            )

            # networks??
            # TODO

        }


        updated_object_list[[gobj_i]] <- gobj
    }




    ## 2. prepare for new giotto object ##
    ## -------------------------------- ##
    vmsg(.v = verbose, "B) Prepare to create new Giotto object")

    comb_gobject <- new("giotto",
        expression_feat = first_features,
        instructions = first_instructions,
        versions = .versions_info(),
        join_info = NULL
    )





    ## 3. merge updated data  ##
    ## ------------------------ ##
    vmsg(.v = verbose, "C) Merge updated data")

    first_obj <- updated_object_list[[1]]

    vmsg(.v = verbose, "1. cell and feature IDs")
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
    vmsg(.v = verbose, "2. expression data")

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

        # comb_gobject = setFeatureMetadata(gobject = comb_gobject,
        #                                     S4_feat_metadata,
        #                                     initialize = FALSE)
    } else {
        for (exprObj_i in seq(nrow(avail_expr))) {
            expr_list <- lapply(updated_object_list, function(gobj) {
                getExpression(
                    gobject = gobj,
                    spat_unit = avail_expr$spat_unit[[exprObj_i]],
                    feat_type = avail_expr$feat_type[[exprObj_i]],
                    values = avail_expr$name[[exprObj_i]],
                    output = "exprObj",
                    set_defaults = FALSE
                )
            })

            if (!.prov_match(expr_list)) {
                warning(wrap_txt("expression: provenance mismatch"))
            }

            combmat <- .join_expression_matrices(matrix_list = lapply(
                expr_list, function(expr) expr[]
            ))
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
    vmsg(.v = verbose, "3. spatial locations")

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

        if (!.prov_match(sl_list)) {
            warning(wrap_txt("spatial locations: provenance mismatch"))
        }

        combspatlocs <- .join_spatlocs(dt_list = lapply(
            sl_list,
            function(sl) sl[]
        ))
        sl_list[[1]][] <- combspatlocs

        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        comb_gobject <- set_spatial_locations(comb_gobject,
            spatlocs = sl_list[[1]],
            set_defaults = FALSE
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    }





    ## cell metadata
    vmsg(.v = verbose, "4. cell metadata")

    for (spat_unit in names(first_obj@cell_metadata)) {
        for (feat_type in names(first_obj@cell_metadata[[spat_unit]])) {
            savelist <- list()
            for (gobj_i in seq_along(updated_object_list)) {
                cellmeta <- updated_object_list[[
                    gobj_i
                ]]@cell_metadata[[spat_unit]][[feat_type]][]
                savelist[[gobj_i]] <- cellmeta
            }
            combcellmeta <- .join_cell_meta(dt_list = savelist)

            S4_cell_meta <- getCellMetadata(
                gobject = first_obj,
                spat_unit = spat_unit,
                feat_type = feat_type,
                copy_obj = TRUE,
                set_defaults = FALSE,
                output = "cellMetaObj"
            )
            S4_cell_meta[] <- combcellmeta

            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            comb_gobject <- setCellMetadata(
                gobject = comb_gobject,
                x = S4_cell_meta,
                initialize = FALSE,
                verbose = FALSE
            )
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        }
    }


    ## feat metadata
    # check if any already exists in first_obj
    # otherwise, skip and generate feat_metadata de novo at end
    avail_featmeta <- list_feat_metadata(gobject = first_obj)
    if (!is.null(avail_featmeta)) {
        vmsg(.v = verbose, "   feature metadata")
        for (fmObj_i in seq(nrow(avail_featmeta))) {
            fm_list <- lapply(updated_object_list, function(gobj) {
                getFeatureMetadata(
                    gobject = gobj,
                    spat_unit = avail_featmeta$spat_unit[[fmObj_i]],
                    feat_type = avail_featmeta$feat_type[[fmObj_i]],
                    output = "featMetaObj",
                    copy_obj = TRUE
                )
            })

            if (!.prov_match(fm_list)) {
                warning(wrap_txt("feature metadata: provenance mismatch"))
            }

            comb_fm <- .join_feat_meta(dt_list = lapply(
                fm_list,
                function(fm) fm[]
            ))
            fm_list[[1]][] <- comb_fm

            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            comb_gobject <- setFeatureMetadata(
                gobject = comb_gobject,
                x = fm_list[[1]],
                initialize = FALSE,
                verbose = FALSE
            )
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        }
    }





    ## spatial info
    vmsg(.v = verbose, "5. spatial polygon information")

    available_spat_info <- unique(unlist(all_spatinfo_list))

    if (isTRUE(verbose)) {
        wrap_msg("available_spat_info: \n")
        wrap_msg(available_spat_info)
    }

    for (spat_info in available_spat_info) {
        savelist_vector <- list()
        savelist_centroids <- list()
        for (gobj_i in seq_along(updated_object_list)) {
            spat_information_vector <- updated_object_list[[
                gobj_i
            ]]@spatial_info[[spat_info]]@spatVector
            savelist_vector[[gobj_i]] <- spat_information_vector

            spat_information_centroids <- updated_object_list[[
                gobj_i
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
    vmsg(.v = verbose, "6. spatial feature/points information")


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
    vmsg(.v = verbose, "7. images")

    # keep individual images
    # each individual image has updated x and y locations
    # so all images can be viewed together by plotting them one-by-one
    # but images can also be easily viewed separately by grouping them
    comb_gobject@images <- all_image_list


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




# internals ####

.replace_spat_ids_spatlocsobj <- function(x, ids) {
    if (length(ids) != nrow(x)) {
        stop("replacement ids must be the same length as nrow of spatLocsObj")
    }
    # update cell_ids
    x[][, "cell_ID" := as.character(ids)]
    return(x)
}


