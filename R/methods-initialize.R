#' @include classes.R
#' @include package_imports.R
NULL

# -------------------------------------------------- #




# giotto ####
# See documentation in classes.R
#' @noRd
#' @keywords internal
setMethod(
    "initialize", signature("giotto"),
    function(.Object, ..., initialize = TRUE) {
        .Object <- methods::callNextMethod(.Object, ...)
        if (!initialize || isFALSE(getOption("giotto.init", TRUE))) {
            return(.Object)
        }
        .Object <- updateGiottoObject(.Object)

        vmsg(.is_debug = TRUE, .initial = "  ", "!!giotto.initialize run!!")

        # DT vars
        spat_unit <- feat_type <- NULL

        # a = list(...)


        # TODO
        ## set slots ##
        ## --------- ##

        # if('spatial_info' %in% names(a)) {
        #   .Object = setPolygonInfo(.Object, gpolygon = a$spatial_info)
        # }
        # if('expression' %in% names(a)) {
        #   .Object = setExpression(.Object, values = a$expression)
        # }





        ## set instructions ##
        ## ---------------- ##

        # set default instructions (make sure initialize = FALSE)
        if (is.null(instructions(.Object))) {
            instructions(.Object, initialize = FALSE) <- createGiottoInstructions()
        }

        ## test python module availability if a python env is expected ##
        .check_giotto_python_modules(
            my_python_path = instructions(.Object, "python_path")
        )



        ## Slot Detection ##
        ## -------------- ##

        # detect expression and subcellular data
        avail_expr <- list_expression(.Object)
        avail_si <- list_spatial_info(.Object)
        avail_fi <- list_feature_info(.Object)

        used_spat_units <- unique(c(avail_expr$spat_unit, avail_si$spat_info))
        used_feat_types <- unique(c(avail_expr$feat_type, avail_fi$feat_info))

        # detect ID slots
        avail_cid <- list_cell_id_names(.Object)
        avail_fid <- list_cell_id_names(.Object)

        # detect metadata slots
        avail_cm <- list_cell_metadata(.Object)
        avail_fm <- list_feat_metadata(.Object)

        # detect spatial location slot
        avail_sl <- list_spatial_locations(.Object)

        # detect nearest network slot
        avail_nn <- list_nearest_networks(.Object)

        # detect dimension reduction slot
        avail_dr <- list_dim_reductions(.Object)

        # detect spatial network slot
        avail_sn <- list_spatial_networks(.Object)

        # detect spatial enrichment slot
        avail_se <- list_spatial_enrichments(.Object)


        ## Perform any subobject updates ##
        ## ----------------------------- ##

        # Feature Info #
        if (!is.null(avail_fi)) {
            info_list <- get_feature_info_list(.Object)
            # update S4 object if needed
            info_list <- lapply(info_list, function(info) {
                try_val <- try(validObject(info), silent = TRUE)
                if (inherits(try_val, "try-error")) {
                    info <- updateGiottoPointsObject(info)
                }
                return(info)
            })
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            .Object <- setFeatureInfo(
                gobject = .Object,
                x = info_list,
                verbose = FALSE,
                initialize = FALSE
            )
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        }


        # Spatial Info #
        if (!is.null(avail_si)) {
            info_list <- get_polygon_info_list(.Object)

            # update S4 object if needed
            info_list <- lapply(info_list, function(info) {
                try_val <- try(validObject(info), silent = TRUE)
                if (inherits(try_val, "try-error")) {
                    info <- updateGiottoPolygonObject(info)
                }
                return(info)
            })
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            .Object <- setPolygonInfo(
                gobject = .Object,
                x = info_list,
                verbose = FALSE,
                centroids_to_spatlocs = FALSE,
                initialize = FALSE
            )
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        }






        ## Set active/default spat_unit and feat_type ##
        ## ------------------------------------------ ##

        # detect if actives are set in giotto instructions
        active_su <- try(instructions(.Object, "active_spat_unit"), silent = TRUE)
        active_ft <- try(instructions(.Object, "active_feat_type"), silent = TRUE)

        # determine actives using defaults if data exists then set
        if (inherits(active_su, "try-error")) {
            if (!is.null(avail_expr) | !is.null(avail_si)) {
                active_su <- set_default_spat_unit(gobject = .Object)
                instructions(.Object, "active_spat_unit",
                    initialize = FALSE
                ) <- active_su
            }
        }
        if (inherits(active_ft, "try-error")) {
            if (!is.null(avail_expr) | !is.null(avail_fi)) {
                active_ft <- set_default_feat_type(
                    gobject = .Object,
                    spat_unit = active_su
                )
                instructions(.Object, "active_feat_type",
                    initialize = FALSE
                ) <- active_ft
            }
        }






        ## Set expression_feat ##
        ## ------------------- ##
        e_feat <- used_feat_types
        if ("rna" %in% e_feat) {
            rna_idx <- which(e_feat == "rna")
            e_feat <- c(e_feat[rna_idx], e_feat[-rna_idx])
        }
        .Object@expression_feat <- e_feat





        ## Ensure Consistent IDs ##
        ## --------------------- ##

        # cell IDs can be expected to be constant across a spatial unit

        # expression
        if (!is.null(avail_expr)) {
            unique_expr_sets <- unique(avail_expr[, .(spat_unit, feat_type)])

            for (set_i in nrow(unique_expr_sets)) {
                exp_list <- get_expression_values_list(
                    gobject = .Object,
                    spat_unit = unique_expr_sets$spat_unit[[set_i]],
                    feat_type = unique_expr_sets$feat_type[[set_i]],
                    output = "exprObj",
                    set_defaults = FALSE
                )

                exp_list_names <- lapply(exp_list, spatIDs)
                list_match <- vapply(
                    exp_list_names,
                    setequal,
                    exp_list_names[[1L]],
                    FUN.VALUE = logical(1L)
                )
                if (!all(list_match)) {
                    wrap_msg(list_match)
                    warning(wrap_txt(
                        "spat_unit:", unique_expr_sets$spat_unit[[set_i]], "/",
                        "feat_type:", unique_expr_sets$feat_type[[set_i]],
                        "\nNot all expression matrices share the same cell_IDs"
                    ))
                }
            }
        }




        # MIGHT BE CHANGED IN THE FUTURE
        # feat_IDs cannot be expected to be constant across spat units.







        ## ID initialization ##
        ## ----------------- ##

        # Must be after default spat_unit/feat_type are set.
        # feat_ID initialization depends on active spat_unit


        # Initialization of cell_ID and feat_ID slots. These slots hold their     #
        # respective IDs for each spatial unit and feature type respectively.     #
        #                                                                         #
        # cell_metadata and feat_metadata slots are initialized off these slots.  #
        #                                                                         #
        # expression information is PREFERRED for ID initialization.              #
        # subcellular information, being raw data may also be used.               #

        .Object <- init_cell_and_feat_IDs(gobject = .Object)





        ## Metadata initialization ##
        ## ----------------------- ##

        # Initialization of all spat_unit/feat_type combinations if the metadata  #
        # does not currently exist.                                               #

        # provenance is always updated from matched expression info if existing



        for (spatial_unit in used_spat_units) {
            for (feature_type in used_feat_types) {
                provenance <- NULL
                # get expression for provenance info
                if (!is.null(avail_expr)) {
                    if (nrow(avail_expr[spat_unit == spatial_unit &
                        feat_type == feature_type]) != 0L) {
                        provenance <- prov(getExpression(
                            gobject = .Object,
                            spat_unit = spatial_unit,
                            feat_type = feature_type,
                            output = "exprObj",
                            set_defaults = FALSE
                        ))
                    }
                }

                # initialize if no metadata exists OR none for this spat/feat

                # cell metadata
                if (is.null(avail_cm)) {
                    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                    .Object <- set_cell_metadata(
                        gobject = .Object,
                        metadata = "initialize",
                        spat_unit = spatial_unit,
                        feat_type = feature_type,
                        verbose = FALSE,
                        initialize = FALSE
                    )
                    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                } else if (nrow(avail_cm[spat_unit == spatial_unit &
                    feat_type == feature_type]) == 0L) {
                    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                    .Object <- set_cell_metadata(
                        gobject = .Object,
                        metadata = "initialize",
                        spat_unit = spatial_unit,
                        feat_type = feature_type,
                        verbose = FALSE,
                        initialize = FALSE
                    )
                    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                }

                # feature metadata
                if (is.null(avail_fm)) {
                    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                    .Object <- set_feature_metadata(
                        gobject = .Object,
                        metadata = "initialize",
                        spat_unit = spatial_unit,
                        feat_type = feature_type,
                        verbose = FALSE,
                        initialize = FALSE
                    )
                    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                } else if (nrow(avail_fm[spat_unit == spatial_unit &
                    feat_type == feature_type]) == 0L) {
                    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                    .Object <- set_feature_metadata(
                        gobject = .Object,
                        metadata = "initialize",
                        spat_unit = spatial_unit,
                        feat_type = feature_type,
                        verbose = FALSE,
                        initialize = FALSE
                    )
                    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                }


                # update provenance (always happens for all metadata objects)
                if (is.null(provenance)) next() # skip if no provenance info

                cm <- getCellMetadata(
                    gobject = .Object,
                    spat_unit = spatial_unit,
                    feat_type = feature_type,
                    output = "cellMetaObj",
                    copy_obj = FALSE,
                    set_defaults = FALSE
                )
                fm <- getFeatureMetadata(
                    gobject = .Object,
                    spat_unit = spatial_unit,
                    feat_type = feature_type,
                    output = "featMetaObj",
                    copy_obj = FALSE,
                    set_defaults = FALSE
                )
                prov(cm) <- provenance
                prov(fm) <- provenance
                ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                .Object <- set_cell_metadata(
                    gobject = .Object,
                    metadata = cm,
                    verbose = FALSE,
                    initialize = FALSE
                )
                .Object <- set_feature_metadata(
                    gobject = .Object,
                    metadata = fm,
                    verbose = FALSE,
                    initialize = FALSE
                )
                ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            }
        }


        # SLOT CHECKS ####
        
        # option to skip checks
        if (!getOption("giotto.check_valid", TRUE)) return(.Object)
        
        vmsg(.is_debug = TRUE, .initial = "  ", "!!giotto validity run!!")

        ## Metadata ##
        ## ------------- ##

        # spat_unit cross compatibility for metadata checks use spatIDs() to pull
        # the relevant set of spatIDs for that spatial unit.

        if (!is.null(avail_cm)) {
            .check_cell_metadata(gobject = .Object) # modifies by reference
        }

        if (!is.null(avail_fm)) {
            .check_feat_metadata(gobject = .Object) # modifies by reference
        }


        ## Spatial locations ##
        ## ----------------- ##

        if (!is.null(avail_expr) && !is.null(avail_sl)) {
            # 1. ensure spatial locations and expression matrices have the same
            # cell IDs
            # 2. give cell IDs if not provided
            .check_spatial_location_data(gobject = .Object) # modifies by reference
        }



        ## Spatial network ##
        ## --------------- ##

        if (!is.null(avail_sl) && !is.null(avail_sn)) {
            # 1. ensure vertices have same IDs as seen in spat_unit for gobject
            # 2. ensure spatial locations of same spat_unit exists
            .check_spatial_networks(gobject = .Object)
        }



        ## Spatial enrichment ##
        ## ------------------ ##

        if (!is.null(avail_sl) && !is.null(avail_se)) {
            # 1. ensure IDs in enrichment match gobject for same spat_unit
            # 2. ensure spatial locations exist for same spat_unit
            .check_spatial_enrichment(gobject = .Object)
        }



        ## Nearest networks ##
        ## ---------------- ##

        if (!is.null(avail_expr) && !is.null(avail_nn)) {
            .check_nearest_networks(gobject = .Object)
        }



        ## Dimension reduction ##
        ## ------------------- ##

        if (!is.null(avail_dr)) {
            .Object <- .check_dimension_reduction(gobject = .Object)
        }



        ## Spatial info ##
        ## ------------ ##

        if (!is.null(avail_si) & !is.null(avail_sl)) {
            .check_spatial_info(gobject = .Object)
        }



        ## validity check ##
        ## -------------- ##
        methods::validObject(.Object)



        .Object
    }
)










# Virtual Classes ####






## metaData ####
setMethod(
    "initialize", "metaData",
    function(.Object, ...) {
        .Object <- methods::callNextMethod()
        # prepare DT for set by reference
        if (!is.null(.Object@metaDT)) {
            .Object@metaDT <- data.table::setalloccol(.Object@metaDT)
        }
        .Object
    }
)



## enrData ####
setMethod(
    "initialize", "enrData",
    function(.Object, ...) {
        .Object <- methods::callNextMethod()
        # prepare DT for set by reference
        if (!is.null(.Object@enrichDT)) {
            .Object@enrichDT <- data.table::setalloccol(.Object@enrichDT)
        }
        .Object
    }
)





## spatNetData ####
setMethod(
    "initialize", "spatNetData",
    function(.Object, ...) {
        .Object <- methods::callNextMethod()
        # prepare DT for set by reference
        if (!is.null(.Object@networkDT)) {
            .Object@networkDT <- data.table::setalloccol(.Object@networkDT)
        }
        if (!is.null(.Object@networkDT_before_filter)) {
            .Object@networkDT_before_filter <- data.table::setalloccol(
                .Object@networkDT_before_filter
            )
        }
        .Object
    }
)





## coordDataDT ####
setMethod(
    "initialize", "coordDataDT",
    function(.Object, ...) {
        .Object <- methods::callNextMethod()
        # prepare DT for set by reference
        if (!is.null(.Object@coordinates)) {
            .Object@coordinates <- data.table::setalloccol(.Object@coordinates)
        }
        .Object
    }
)






## spatGridData ####
setMethod(
    "initialize", "spatGridData",
    function(.Object, ...) {
        .Object <- methods::callNextMethod()
        # prepare DT for set by reference
        if (!is.null(.Object@gridDT)) {
            .Object@gridDT <- data.table::setalloccol(.Object@gridDT)
        }
        .Object
    }
)


## affine2d ####
setMethod("initialize", "affine2d", function(.Object, ...) {
    .Object <- methods::callNextMethod()
    .Object@anchor <- ext(.Object@anchor) %>%
        .ext_to_num_vec()

    res <- .decomp_affine(.Object@affine)

    .Object@affine <- res$affine
    .Object@rotate <- res$rotate
    .Object@shear <- res$shear
    .Object@scale <- res$scale
    .Object@translate <- res$translate

    return(.Object)
})

## giottoLargeImage ####
setMethod("initialize", signature("giottoLargeImage"), function(.Object, ...) {
    .Object <- methods::callNextMethod()

    # defaults
    .Object@OS_platform <- .Object@OS_platform %null% .Platform[["OS.type"]]
    objName(.Object) <- objName(.Object) %null% "image"

    r <- .Object@raster_object
    if (is.null(r)) {
        return(.Object)
    } # return early if NULL

    # scale factor and res
    .Object@resolution <- terra::res(r)
    names(.Object@resolution) <- c("x", "y")
    .Object@scale_factor <- 1 / .Object@resolution

    # sample for image characteristics
    svals <- .spatraster_sample_values(r, size = 5000, verbose = FALSE)

    if (nrow(svals) != 0) {
        intensity_range <- .spatraster_intensity_range(
            raster_object = r,
            sample_values = svals
        )
    }
    .Object@min_intensity <- intensity_range[["min"]]
    .Object@max_intensity <- intensity_range[["max"]]

    # find out if image is int or floating pt
    is_int <- .spatraster_is_int(
        raster_object = r,
        sample_values = svals
    )
    .Object@is_int <- is_int

    # extent
    .Object@extent <- as.vector(terra::ext(r))
    names(.Object@extent) <- c("xmin", "xmax", "ymin", "ymax")
    .Object@overall_extent <- .Object@overall_extent %null%
        as.vector(terra::ext(r))

    # max window
    .Object@max_window <- .Object@max_window %null% .Object@max_intensity
    # .Object@max_window <- .Object@max_window %na%
    #     .bitdepth(.Object@max_intensity, return_max = TRUE)

    return(.Object)
})

## giottoAffineImage ####
setMethod("initialize", signature("giottoAffineImage"), function(.Object, ...) {
    .Object <- methods::callNextMethod()

    # default name
    if (is.null(objName(.Object))) {
        objName(.Object) <- "test"
    }

    # append associated functions


    r <- .Object@raster_object
    if (!is.null(r)) {
        # apply the image extent as anchor for affine object plotting
        .Object@affine@anchor <- ext(r)
        .Object@affine <- initialize(.Object@affine)

        # compute & set extent slot as a numeric vector
        d <- .bound_poly(r) %>%
            affine(.Object@affine)
        .Object@extent <- .ext_to_num_vec(ext(d))
    }

    .Object@funs$realize_magick <- function(filename = NULL, size = 5e5) {
        mg <- .gaffine_realize_magick(.Object, size = size)
        gimg <- .magick_preview(mg@mg_object, filename = filename) %>%
            createGiottoLargeImage()
        ext(gimg) <- ext(.Object)

        # mask image
        aff <- .Object@affine
        m <- .bound_poly(ext(aff@anchor))
        m <- affine(m, aff)
        gimg@raster_object <- terra::mask(gimg@raster_object, mask = m)

        return(gimg)
        # TODO things to be implemented for this pipeline:
        # col (the trip the magick-image flattened the image without applying col)
        # max_intensity same as above
        # the above options are also stripped when the fresh largeImage is created
    }

    return(.Object)
})









## * Initialize
# setMethod('initialize', 'nnNetObj',
#           function(.Object, ...) {
#
#             # expand args
#             a = list(.Object = .Object, ...)
#
#             # evaluate data
#             if('igraph' %in% names(a)) {
#               igraph = a$igraph
#               if(is.null(igraph)) igraph = NULL
#               else {
#                 # Convert igraph input to preferred format
#                 igraph = .evaluate_nearest_networks(igraph)
#               }
#
#               # return to arg list
#               a$igraph = igraph
#             }
#
#             .Object = do.call('methods'::'callNextMethod', a)
#             .Object
#           })





## * Initialize
# setMethod('initialize', 'spatLocsObj',
#           function(.Object, ...) {
#
#             # expand args
#             a = list(.Object = .Object, ...)
#
#             # evaluate data
#             if('coordinates' %in% names(a)) {
#               coordinates = a$coordinates
#               if(is.null(coordinates)) {
#                 coordinates = data.table::data.table(
#                   sdimx = NA_real_,
#                   sdimy = NA_real_,
#                   cell_ID = NA_character_
#                 )
#               } else {
#                 coordinates = .evaluate_spatial_locations(coordinates)
#               }
#
#               # return to arg list
#               a$coordinates = coordinates
#             }
#
#             .Object = do.call('methods'::'callNextMethod', a)
#             .Object
#           })





## * Initialize
# setMethod('initialize', 'exprObj',
#           function(.Object, ...) {
#
#             # expand args
#             a = list(.Object = .Object, ...)
#
#             # evaluate data
#             if('exprMat' %in% names(a)) {
#               exprMat = a$exprMat
#               if(is.null(exprMat)) exprMat = matrix()
#               else {
#                 # Convert matrix input to preferred format
#                 exprMat = .evaluate_expr_matrix(exprMat)
#               }
#
#               # return to arg list
#               a$exprMat = exprMat
#             }
#
#             .Object = do.call('methods'::'callNextMethod', a)
#             .Object
#           })





# helpers ####




# initialize IDs ####


#' @title Initialize cell and feature IDs
#' @name init_cell_and_feat_IDs
#' @description sets cell and feature IDs based on provided expression data.
#' Enforces that across a single spatial unit, all expression matrices MUST
#' have the same set of cell_IDs
#' @keywords internal
#' @noRd
init_cell_and_feat_IDs <- function(gobject) {
    spat_unit <- feat_type <- name <- NULL

    # wipe values
    slot(gobject, "cell_ID") <- NULL
    slot(gobject, "feat_ID") <- NULL

    # find available expr and info
    avail_expr <- list_expression(gobject)
    avail_si <- list_spatial_info(gobject)
    avail_fi <- list_feature_info(gobject)

    used_spat_units <- unique(c(avail_expr$spat_unit, avail_si$spat_info))
    used_feat_types <- unique(c(avail_expr$feat_type, avail_fi$feat_info))

    # 1. set cell_ID for each region
    # each regions can have multiple features, but the cell_IDs (spatial units)
    # should be the same
    # Select spatial unit to initialize then pass to set_cell_id
    # set_cell_id decides which data to initialize from
    for (spatial_unit in used_spat_units) {
        gobject <- set_cell_id(
            gobject = gobject,
            spat_unit = spatial_unit,
            cell_IDs = "initialize",
            verbose = FALSE,
            set_defaults = TRUE
        )
    }

    # 2. set feat_ID for each feature
    for (feature_type in used_feat_types) {
        gobject <- set_feat_id(
            gobject = gobject,
            feat_type = feature_type,
            feat_IDs = "initialize",
            verbose = FALSE,
            set_defaults = TRUE
        )
    }


    return(gobject)
}





# TODO DEPRECATE now that this functionality is covered in initialize()
# Currently only used in createSubcellular and join, but are probably redundant
#' @title Initialize cell metadata slot
#' @name init_cell_metadata
#' @description Generate cellMetaObjs to hold cell metadata for each spatial
#' unit
#' @param gobject giotto object
#' @param provenance provenance information (optional)
#' and feature type in the giotto object.
#' @returns cellMetaObjs
#' @keywords internal
init_cell_metadata <- function(
        gobject,
        provenance = NULL) {
    # data.table vars
    spat_unit <- feat_type <- NULL

    avail_expr <- list_expression(gobject)
    avail_spat_info <- list_spatial_info_names(gobject)
    avail_feat_info <- list_feature_info_names(gobject)

    # If no spatial_info then initialize for all expression matrices
    if (is.null(avail_spat_info)) {
        for (expr_i in seq(avail_expr[, .N])) {
            # initialize relevant metadata

            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            gobject <- set_cell_metadata(
                gobject = gobject,
                spat_unit = avail_expr[expr_i, spat_unit],
                feat_type = avail_expr[expr_i, feat_type],
                provenance = if (is.null(provenance)) {
                    avail_expr[expr_i, spat_unit]
                } else {
                    provenance
                },
                metadata = "initialize",
                verbose = FALSE,
                initialize = FALSE
            )
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        }
    } else {
        # if spatial_info present then initialize by spat_unit from spat_info,
        # but prefer feat_type from expression
        if (is.null(avail_expr)) {
            avail_to_use <- unique(avail_feat_info)
        } else {
            avail_to_use <- unique(avail_expr[, feat_type])
        }
        for (poly in avail_spat_info) {
            for (feature_type in unique(avail_to_use)) {
                ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                gobject <- set_cell_metadata(
                    gobject = gobject,
                    spat_unit = poly,
                    feat_type = feature_type,
                    provenance = if (is.null(provenance)) poly else provenance,
                    metadata = "initialize",
                    verbose = FALSE,
                    initialize = FALSE
                )
                ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            }
        }
    }
    return(gobject)
}




# TODO DEPRECATE now that this functionality is covered in initialize()
#' @title Initialize feature metadata slot
#' @name init_feat_metadata
#' @description Generate featMetaObjs to hold all feature metadata for each
#' spatial unit and feature type in the giotto object.
#' @param gobject giotto object
#' @param provenance provenance information (optional)
#' @returns featMetaObjs
#' @keywords internal
init_feat_metadata <- function(
        gobject,
        provenance = NULL) {
    # data.table vars
    spat_unit <- feat_type <- NULL

    avail_expr <- list_expression(gobject)
    avail_spat_info <- list_spatial_info_names(gobject)
    avail_feat_info <- list_feature_info_names(gobject)

    # If no spatial_info then initialize by expression mat
    if (is.null(avail_spat_info)) {
        for (expr_i in seq(avail_expr[, .N])) {
            # initialize relevant metadata

            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            gobject <- set_feature_metadata(
                gobject = gobject,
                spat_unit = avail_expr[expr_i, spat_unit],
                feat_type = avail_expr[expr_i, feat_type],
                provenance = if (is.null(provenance)) {
                    avail_expr[expr_i, spat_unit]
                } else {
                    provenance
                },
                metadata = "initialize",
                verbose = FALSE,
                initialize = FALSE
            )
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        }
    } else {
        # if spatial_info present then initialize by spat_unit from spat_info,
        # but prefer feat_type from expression
        if (is.null(avail_expr)) {
            avail_to_use <- unique(avail_feat_info)
        } else {
            avail_to_use <- unique(avail_expr[, feat_type])
        }
        for (poly in avail_spat_info) {
            for (feature_type in unique(avail_to_use)) {
                ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                gobject <- set_feature_metadata(
                    gobject = gobject,
                    spat_unit = poly,
                    feat_type = feature_type,
                    provenance = if (is.null(provenance)) poly else provenance,
                    metadata = "initialize",
                    verbose = FALSE,
                    initialize = FALSE
                )
                ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            }
        }
    }
    return(gobject)
}
