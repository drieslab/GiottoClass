#' @include classes.R
#' @include slot_accessors.R
#' @include package_imports.R
#' @include generics.R
NULL






#### creating Giotto objects ####

#' @title Create a giotto object
#' @name createGiottoObject
#' @description Function to create a giotto object
#' @param expression expression information
#' @param raw_exprs deprecated, use expression
#' @param expression_feat available features (e.g. rna, protein, ...)
#' @param spatial_locs data.table or data.frame with coordinates for cell
#' centroids
#' @param spatial_info list of giotto polygon objects with spatial information,
#' see \code{\link{createGiottoPolygonsFromMask}}
#' and \code{\link{createGiottoPolygonsFromDfr}}
#' @param calc_poly_centroids if spatial_info is provided, whether to also
#' calculate centroids
#' @param centroids_to_spatlocs if spatial_info is provided, whether to also
#' convert centroids to spatial locations
#' @param cell_metadata cell annotation metadata
#' @param feat_metadata feature annotation metadata for each unique feature
#' @param feat_info list of giotto point objects with feature info,
#' see \code{\link{createGiottoPoints}}
#' @param spatial_network list of spatial network(s)
#' @param spatial_grid list of spatial grid(s)
#' @param spatial_grid_name list of spatial grid name(s)
#' @param spatial_enrichment list of spatial enrichment score(s) for each
#' spatial region
#' @param dimension_reduction list of dimension reduction(s)
#' @param nn_network list of nearest neighbor network(s)
#' @param images list of images
#' @param largeImages deprecated
#' @param offset_file file used to stitch fields together (optional)
#' @param instructions list of instructions or output result
#' from \code{\link{createGiottoInstructions}}
#' @param cores how many cores or threads to use to read data if paths are
#' provided
#' @param expression_matrix_class class of expression matrix to
#' use (e.g. 'dgCMatrix', 'DelayedArray')
#' @param h5_file path to h5 file
#' @param verbose be verbose when building Giotto object
#' @returns giotto object
#' @details
#'
#' See \url{http://giottosuite.com/articles/getting_started_gobject.html} for
#' more details
#'
#' \[**Requirements**\] To create a giotto object you need to provide at least
#' a matrix with genes as row names and cells as column names. This matrix can
#' be provided as a base matrix, sparse Matrix, data.frame, data.table or as a
#' path to any of those. To include spatial information about
#' cells (or regions) you need to provide a matrix, data.table or
#' data.frame (or path to them) with coordinates for all spatial dimensions.
#' This can be 2D (x and y) or 3D (x, y, x).
#' The row order for the cell coordinates should be the same as the column
#' order for the provided expression data.
#'
#' \[**Instructions**\] Additionally an instruction file, generated manually or
#' with \code{\link{createGiottoInstructions}} can be provided to instructions,
#' if not a default instruction file will be created for the Giotto object.
#'
#' \[**Multiple fields**\] In case a dataset consists of multiple fields, like
#' seqFISH+ for example, an offset file can be provided to stitch the different
#' fields together. \code{\link{stitchFieldCoordinates}} can be used to
#' generate such an offset file.
#'
#' \[**Processed data**\] Processed count data, such as normalized data, can
#' be provided using one of the different expression
#' slots (norm_expr, norm_scaled_expr, custom_expr).
#'
#' \[**Metadata**\] Cell and gene metadata can be provided using the cell and
#' gene metadata slots. This data can also be added afterwards using
#' the \code{\link{addFeatMetadata}} or \code{\link{addCellMetadata}} functions.
#'
#' \[**Other information**\] Additional information can be provided through
#' the appropriate slots:
#' \itemize{
#'   \item{spatial networks}
#'   \item{spatial grids}
#'   \item{spatial enrichments}
#'   \item{dimensions reduction}
#'   \item{nearest neighbours networks}
#'   \item{images}
#' }
#'
#' @concept giotto
#' @examples
#' expr_matrix <- readRDS(system.file("extdata/toy_matrix.RDS",
#'     package = "GiottoClass"
#' ))
#'
#' createGiottoObject(expression = expr_matrix)
#' @export
createGiottoObject <- function(
        expression,
        expression_feat = "rna",
        spatial_locs = NULL,
        spatial_info = NULL,
        calc_poly_centroids = FALSE,
        centroids_to_spatlocs = FALSE,
        feat_info = NULL,
        cell_metadata = NULL,
        feat_metadata = NULL,
        spatial_network = NULL,
        spatial_grid = NULL,
        spatial_grid_name = NULL,
        spatial_enrichment = NULL,
        dimension_reduction = NULL,
        nn_network = NULL,
        images = NULL,
        largeImages = NULL,
        offset_file = NULL,
        instructions = NULL,
        cores = determine_cores(),
        raw_exprs = NULL,
        expression_matrix_class = c("dgCMatrix", "DelayedArray"),
        h5_file = NULL,
        verbose = FALSE) {
    debug_msg <- FALSE # for reading debug help
    initialize_per_step <- FALSE

    if (!is.null(largeImages)) {
        deprecate_warn(
            "0.3.0",
            "createGiottoObject(largeImages)",
            details = c(
                "All images should be supplied to `images` instead",
                "Names of images may not overlap"
            )
        )
        images <- c(images, largeImages)
    }

    # create minimum giotto
    gobject <- giotto(
        expression_feat = expression_feat,
        offset_file = offset_file,
        instructions = instructions,
        versions = .versions_info(),
        h5_file = h5_file
    )


    ## data.table vars
    cell_ID <- feat_ID <- NULL

    # ## check if all optional packages are installed
    # # TODO: update at the end
    # # TODO: extract from suggest field of DESCRIPTION
    # extra_packages <- c(
    #     "scran", "MAST", "png", "tiff", "biomaRt",
    #     "trendsceek", "multinet", "RTriangle", "FactoMineR"
    # )
    #
    # pack_index <- extra_packages %in% rownames(utils::installed.packages())
    # extra_installed_packages <- extra_packages[pack_index]
    # extra_not_installed_packages <- extra_packages[!pack_index]
    #
    # if (any(pack_index == FALSE) == TRUE) {
    #     wrap_msg(
    #         "Consider to install these (optional) packages to run all possible",
    #         "Giotto commands for spatial analyses: ",
    #         extra_not_installed_packages
    #     )
    #     wrap_msg(
    #         "Giotto does not automatically install all these packages as they",
    #         "are not absolutely required and this reduces the number of
    #         dependencies"
    #     )
    # }


    ## if cores is not set, then set number of cores automatically, but with
    ## limit of 10
    data.table::setDTthreads(threads = cores)


    ## spatial info ##
    ## ------------ ##
    ## place to store segmentation info in polygon format style


    if (!is.null(spatial_info)) {
        spatial_info <- readPolygonData(
            data_list = spatial_info,
            calc_centroids = calc_poly_centroids,
            verbose = debug_msg
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <- setPolygonInfo(
            gobject = gobject,
            x = spatial_info,
            centroids_to_spatlocs = centroids_to_spatlocs,
            verbose = verbose,
            initialize = initialize_per_step
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        if (isTRUE(verbose)) wrap_msg("--- finished spatial info ---\n\n")
    }



    ## feature info ##
    ## ------------ ##
    ## place to store individual feature info
    if (!is.null(feat_info)) {
        feat_info <- readFeatData(
            data_list = feat_info,
            verbose = debug_msg
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <- setFeatureInfo(
            gobject = gobject,
            x = feat_info,
            verbose = verbose,
            initialize = initialize_per_step
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        if (isTRUE(verbose)) wrap_msg("--- finished feature info ---\n\n")
    }




    ## expression data ##
    ## --------------- ##


    ## deprecated arguments
    if (!is.null(raw_exprs)) {
        expression <- raw_exprs
        warning("raw_exprs argument is deprecated, use expression argument in
                the future \n")
    }


    if (!missing(expression)) {
        expression_data <- readExprData(
            data_list = expression,
            sparse = TRUE,
            cores = cores,
            default_feat_type = expression_feat,
            verbose = debug_msg,
            expression_matrix_class = expression_matrix_class
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        ## evaluate if h5_file exists
        if (!is.null(h5_file)) {
            if (file.exists(h5_file)) {
                wrap_msg("'", h5_file, "'",
                    " file already exists and will be replaced",
                    sep = ""
                )
                file.remove(h5_file)
            } else {
                wrap_msg("Initializing file ", "'", h5_file, "'", sep = "")
            }
        }

        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <- setExpression(
            gobject = gobject,
            x = expression_data,
            verbose = verbose,
            initialize = initialize_per_step
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###





        # Set up gobject cell_ID and feat_ID slots based on expression matrices
        gobject <- init_cell_and_feat_IDs(gobject)
        # needed when initialize per step is FALSE

        if (verbose) message("--- finished expression data ---\n")
    }




    ## spatial locations ##
    ## ----------------- ##

    raw_cell_dim_list <- list()

    for (spat_unit in names(gobject@expression)) {
        for (feat_type in names(gobject@expression[[spat_unit]])) {
            raw_cell_dim_list[[spat_unit]][[feat_type]] <- ncol(
                gobject@expression[[spat_unit]][[feat_type]][[1L]]
            )
        }
    }

    # raw_cell_dim = ncol(gobject@expression[[1]][[1]][[1]]) # number of columns

    # list of spatial location data.table, each with a unique name
    # the default name = 'raw' and correspond to the real physical coordinates
    # additional spatial locations can be provided


    if (!is.null(spatial_locs)) {
        spatial_location_data <- readSpatLocsData(
            data_list = spatial_locs,
            cores = cores,
            verbose = debug_msg
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <- setSpatialLocations(
            gobject = gobject,
            x = spatial_location_data,
            verbose = verbose,
            initialize = initialize_per_step
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



        # 1. ensure spatial locations and expression matrices have the same
        # cell IDs
        # 2. give cell IDs if not provided
        .check_spatial_location_data(gobject) # modifies by reference
    } else {
        if (verbose == TRUE) warning(wrap_txt("No spatial locations have been
                                    provided, dummy locations will be created"))

        # for each spatial unit create a dummy raw spatial location matrix

        for (spat_unit in names(raw_cell_dim_list)) {
            # create square dummy coordinates
            nr_cells <- raw_cell_dim_list[[spat_unit]][[1]]
            x <- ceiling(sqrt(nr_cells))
            first_col <- rep(seq_len(x), each = x)[seq_len(nr_cells)]
            second_col <- rep(seq_len(x), times = x)[seq_len(nr_cells)]

            spatial_locs <- data.table::data.table(
                cell_ID = gobject@cell_ID[[spat_unit]],
                sdimx = first_col,
                sdimy = second_col
            )

            dummySpatLocObj <- createSpatLocsObj(
                name = "raw",
                coordinates = spatial_locs,
                spat_unit = spat_unit,
                provenance = spat_unit
            )

            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            gobject <- set_spatial_locations(gobject,
                spatlocs = dummySpatLocObj
            )
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        }
    }

    if (verbose) message("--- finished spatial location data ---\n")





    ## cell metadata ##
    ## ------------- ##
    if (!is.null(cell_metadata)) {
        cm_list <- readCellMetadata(
            data_list = cell_metadata,
            default_feat_type = expression_feat,
            verbose = debug_msg
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <- setCellMetadata(
            gobject = gobject,
            x = cm_list,
            verbose = verbose,
            initialize = initialize_per_step
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    }

    if (verbose) message("--- finished cell metadata ---\n")

    ## feat metadata ##
    ## ------------- ##
    if (!is.null(feat_metadata)) {
        fm_list <- readFeatMetadata(
            data_list = feat_metadata,
            default_feat_type = expression_feat,
            verbose = debug_msg
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <- setFeatureMetadata(
            gobject = gobject,
            x = fm_list,
            verbose = verbose,
            initialize = initialize_per_step
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    }

    if (verbose) message("--- finished feature metadata ---\n")




    ### OPTIONAL:
    ## spatial network
    if (!is.null(spatial_network)) {
        spatial_network_list <- readSpatNetData(
            data_list = spatial_network,
            verbose = debug_msg
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <- setSpatialNetwork(
            gobject = gobject,
            x = spatial_network_list,
            verbose = verbose,
            initialize = initialize_per_step
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        if (isTRUE(verbose)) wrap_msg("--- finished spatial network ---\n\n")
    } else {
        if (isTRUE(verbose)) message("No spatial network results are provided")
    }






    ## spatial grid
    if (!is.null(spatial_grid)) {
        if (is.null(spatial_grid_name) |
            length(spatial_grid) != length(spatial_grid_name)) {
            stop("\n each spatial grid must be given a unique name \n")
        } else {
            for (grid_i in seq_len(length(spatial_grid))) {
                gridname <- spatial_grid_name[[grid_i]]
                grid <- spatial_grid[[grid_i]]

                if (any(c("data.frame", "data.table") %in% class(grid))) {
                    if (all(c("x_start", "y_start", "x_end", "y_end", "gr_name") %in% colnames(grid))) {
                        if (!inherits(grid, "data.table")) {
                            data.table::as.data.table(grid)
                        }
                        grid <- new("spatialGridObj",
                            name = gridname,
                            gridDT = grid
                        )
                        # TODO Assign grid as the first spat_unit and feat_type.
                        # Assigment process will need to be improved later
                        avail_spat_feats <- list_expression(gobject)
                        gobject <- set_spatialGrid(
                            gobject = gobject,
                            spat_unit = avail_spat_feats$spat_unit[[1]],
                            feat_type = avail_spat_feats$feat_type[[1]],
                            name = gridname,
                            spatial_grid = grid
                        )
                    } else {
                        stop(
                            "\n grid ", gridname,
                            " does not have all necessary column names,
                            see details \n"
                        )
                    }
                } else {
                    stop(
                        "\n grid ", gridname,
                        " is not a data.frame or data.table \n"
                    )
                }
            }
        }
    }


    ## spatial enrichment
    if (!is.null(spatial_enrichment)) {
        spatial_enrichment <- readSpatEnrichData(
            data_list = spatial_enrichment,
            default_feat_type = expression_feat,
            verbose = debug_msg
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <- setSpatialEnrichment(
            gobject = gobject,
            x = spatial_enrichment,
            verbose = verbose,
            initialize = initialize_per_step
        )
        if (isTRUE(verbose)) wrap_msg("--- finished spatial enrichment ---\n\n")
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    } else {
        if (isTRUE(verbose)) message("No spatial enrichment results are
                                    provided")
    }




    ## dimension reduction
    if (!is.null(dimension_reduction)) {
        dimension_reduction <- readDimReducData(
            data_list = dimension_reduction,
            default_feat_type = expression_feat,
            verbose = debug_msg
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <- setDimReduction(
            gobject = gobject,
            x = dimension_reduction,
            verbose = verbose,
            initialize = initialize_per_step
        )
        if (isTRUE(verbose)) {
            wrap_msg("--- finished dimension reduction ---\n\n")
        }
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    } else {
        if (isTRUE(verbose)) {
            message("No dimension reduction results are provided")
        }
    }




    # NN network
    if (!is.null(nn_network)) {
        nn_network <- readNearestNetData(
            data_list = nn_network,
            default_feat_type = expression_feat,
            verbose = debug_msg
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <- setNearestNetwork(
            gobject = gobject,
            x = nn_network,
            verbose = verbose,
            initialize = initialize_per_step
        )
        if (isTRUE(verbose)) {
            wrap_msg("--- finished nearest neighbor network ---\n\n")
        }
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    } else {
        if (isTRUE(verbose)) {
            message("No nearest neighbor network results are provided")
        }
    }





    ## images ##
    # expect a list of giotto object images
    # prefer to make giottoImage creation separate from this function
    if (!is.null(images)) {
        # handle no names
        default_base <- "image"

        # prefer list names since they are more likely intentional
        # assign object names from @names slot to list IF list has no names
        images <- assign_objnames_2_list(images, force_replace = FALSE)
        obj_names <- names(images)

        # assign defaults for any names still missing
        if (is.null(obj_names)) obj_names <- rep(length(images), default_base)
        missing_name <- is.na(obj_names) | obj_names == ""
        obj_names[missing_name] <- default_base

        # handle naming overlaps
        obj_names <- .uniquify_dups(
            obj_names,
            what = "image name", sep = ".", verbose = verbose
        )

        # assign final names
        names(images) <- obj_names
        images <- assign_listnames_2_obj(images)

        # set image list
        gobject <- setGiotto(gobject, images, verbose = FALSE)
    }


    # other information
    # TODO

    return(initialize(gobject))
}









#' @title Create a giotto object from subcellular data
#' @name createGiottoObjectSubcellular
#' @description Function to create a giotto object starting from subcellular
#' polygon (e.g. cell) and points (e.g. transcripts) information
#' @param gpolygons giotto polygons
#' @param polygon_mask_list_params list parameters
#' for \code{\link{createGiottoPolygonsFromMask}}
#' @param polygon_dfr_list_params list parameters
#' for \code{\link{createGiottoPolygonsFromDfr}}
#' @param gpoints giotto points
#' @param cell_metadata cell annotation metadata
#' @param feat_metadata feature annotation metadata for each unique feature
#' @param spatial_network list of spatial network(s)
#' @param spatial_network_name list of spatial network name(s)
#' @param spatial_grid list of spatial grid(s)
#' @param spatial_grid_name list of spatial grid name(s)
#' @param spatial_enrichment list of spatial enrichment score(s) for each
#' spatial region
#' @param spatial_enrichment_name list of spatial enrichment name(s)
#' @param dimension_reduction list of dimension reduction(s)
#' @param nn_network list of nearest neighbor network(s)
#' @param images list of images
#' @param largeImages deprecated
#' @param largeImages_list_params image params when loading largeImages as list
#' @param instructions list of instructions or output result
#' from \code{\link{createGiottoInstructions}}
#' @param cores how many cores or threads to use to read data if paths are
#' provided
#' @param verbose be verbose when building Giotto object
#' @returns giotto object
#' @details There are two different ways to create a Giotto Object with
#' subcellular information:
#' - Starting from polygons (spatial units e.g. cell) represented by a mask
#' or dataframe file and giotto points (analyte coordinates e.g. transcripts)
#' - Starting from polygons (spatial units e.g. cell) represented by a mask
#' or dataframe file and raw intensity images (e.g. protein stains)
#' @concept giotto
#' @examples
#' x_gpolygons <- GiottoData::loadSubObjectMini("giottoPolygon")
#' x_gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
#'
#' createGiottoObjectSubcellular(
#'     gpolygons = x_gpolygons,
#'     gpoints = x_gpoints
#' )
#' @export
createGiottoObjectSubcellular <- function(gpolygons = NULL,
    polygon_mask_list_params = NULL,
    polygon_dfr_list_params = NULL,
    gpoints = NULL,
    cell_metadata = NULL,
    feat_metadata = NULL,
    spatial_network = NULL,
    spatial_network_name = NULL,
    spatial_grid = NULL,
    spatial_grid_name = NULL,
    spatial_enrichment = NULL,
    spatial_enrichment_name = NULL,
    dimension_reduction = NULL,
    nn_network = NULL,
    images = NULL,
    largeImages = NULL,
    largeImages_list_params = NULL,
    instructions = NULL,
    cores = NA,
    verbose = FALSE) {
    # data.table vars
    poly_ID <- cell_ID <- feat_ID <- x <- y <- NULL

    # create minimum giotto
    gobject <- giotto(
        expression = NULL,
        expression_feat = NULL,
        spatial_locs = NULL,
        spatial_info = NULL,
        cell_metadata = NULL,
        feat_metadata = NULL,
        feat_info = NULL,
        cell_ID = NULL,
        feat_ID = NULL,
        spatial_network = NULL,
        spatial_grid = NULL,
        spatial_enrichment = NULL,
        dimension_reduction = NULL,
        nn_network = NULL,
        images = NULL,
        parameters = NULL,
        offset_file = NULL,
        instructions = instructions,
        versions = .versions_info()
    )


    ## if cores is not set, then set number of cores automatically,
    ## but with limit
    cores <- determine_cores(cores)
    data.table::setDTthreads(threads = cores)

    if (!is.null(largeImages)) {
        deprecate_warn(
            "0.3.0",
            "createGiottoObjectSubcellular(largeImages)",
            details = c(
                "All images should be supplied to `images` instead",
                "Names of images may not overlap"
            )
        )
        images <- c(images, largeImages)
    }

    # gpolygons and features need to be provided
    if (is.null(gpolygons)) {
        stop("gpolygons = NULL, spatial polygon information needs to be
            given (e.g. cell boundary, nucleus, ...)")
    }

    if (is.null(gpoints) & is.null(images)) {
        stop("both gpoints = NULL and images = NULL: \n
        Some kind of feature information needs to be provided (e.g.
        transcript location or protein intensities)")
    }


    ## extract polygon information ##
    ## --------------------------- ##

    if (is.null(polygon_mask_list_params)) {
        polygon_mask_list_params <- list(
            mask_method = "guess",
            remove_background_polygon = TRUE,
            background_algo = c("range"),
            fill_holes = TRUE,
            ID_fmt = "cell_",
            poly_IDs = NULL,
            flip_vertical = TRUE,
            shift_vertical_step = TRUE,
            flip_horizontal = TRUE,
            shift_horizontal_step = TRUE,
            calc_centroids = FALSE
        )
    }

    if (is.null(polygon_dfr_list_params)) {
        polygon_dfr_list_params <- list(calc_centroids = FALSE)
    }

    if (verbose) message("1. Start extracting polygon information")

    polygon_res <- .extract_polygon_list(
        polygonlist = gpolygons,
        polygon_mask_list_params = polygon_mask_list_params,
        polygon_dfr_list_params = polygon_dfr_list_params
    )
    gobject@spatial_info <- polygon_res

    if (verbose) message("2. Finished extracting polygon information")


    if (verbose) message("3. Add centroid / spatial locations if available")
    for (polygon_info in list_spatial_info_names(gobject)) {
        centroidsDT <- gobject@spatial_info[[polygon_info]]@spatVectorCentroids
        if (!is.null(centroidsDT)) {
            if (verbose) {
                print(paste0(
                    " - Add centroid / spatial locations for ",
                    polygon_info
                ))
            }

            centroidsDT <- .spatvector_to_dt(centroidsDT)
            centroidsDT_loc <- centroidsDT[, .(poly_ID, x, y)]
            colnames(centroidsDT_loc) <- c("cell_ID", "sdimx", "sdimy")

            locsObj <- createSpatLocsObj(
                name = "raw",
                coordinates = centroidsDT_loc,
                spat_unit = polygon_info,
                provenance = polygon_info,
                misc = NULL
            )

            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            gobject <- setSpatialLocations(gobject, x = locsObj)
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        }
    }
    if (verbose) message("3. Finish adding centroid / spatial locations")

    ## cell ID ##
    ## ------- ##
    for (poly in names(gobject@spatial_info)) {
        unique_poly_names <- unique(
            gobject@spatial_info[[poly]]@spatVector$poly_ID
        )
        gobject@cell_ID[[poly]] <- unique_poly_names
    }


    ## extract points information ##
    ## -------------------------- ##


    if (!is.null(gpoints)) {
        vmsg(.v = verbose, "3. Start extracting spatial feature information")

        # generate named list of giottoPoints objects
        points_res <- .extract_points_list(pointslist = gpoints)
        gobject <- setGiotto(
            gobject, points_res,
            verbose = FALSE, initialize = FALSE
        )

        vmsg(
            .v = verbose,
            "4. Finished extracting spatial feature information"
        )

        ## expression features ##
        ## ------------------- ##
        gobject@expression_feat <- names(points_res)
        expression_feat <- gobject@expression_feat

        ## feat ID ##
        ## ------- ##
        for (feat in gobject@expression_feat) {
            unique_feats <- unique(gobject@feat_info[[feat]]@spatVector$feat_ID)
            gobject@feat_ID[[feat]] <- unique_feats
        }
    }









    ## parameters ##
    ## ---------- ##
    gobject@parameters <- list()

    ## set instructions ##
    ## ---------------- ##
    if (is.null(instructions)) {
        # create all default instructions
        gobject@instructions <- createGiottoInstructions()
    }



    if (!is.null(gpoints)) {
        ## cell metadata ##
        ## ------------- ##
        if (is.null(cell_metadata)) {
            # initialize metadata
            gobject <- init_cell_metadata(gobject)
        } else {
            if (length(cell_metadata) != length(expression_feat)) {
                stop("Number of different molecular features need to
                    correspond with the cell_metadata list length \n")
            }

            for (feat_type in expression_feat) {
                for (poly in names(gobject@spatial_info)) {
                    gobject@cell_metadata[[poly]][[feat_type]] <-
                        data.table::as.data.table(
                            gobject@cell_metadata[[poly]][[feat_type]]
                        )
                    gobject@cell_metadata[[poly]][[feat_type]][
                        ,
                        cell_ID := gobject@cell_ID[[poly]]
                    ]

                    # put cell_ID first
                    all_colnames <- colnames(
                        gobject@cell_metadata[[poly]][[feat_type]]
                    )
                    other_colnames <- grep("cell_ID", all_colnames,
                        invert = TRUE, value = TRUE
                    )
                    gobject@cell_metadata[[poly]][[feat_type]] <-
                        gobject@cell_metadata[[poly]][[feat_type]][,
                            c("cell_ID", other_colnames),
                            with = FALSE
                        ]
                }
            }
        }


        ## feat metadata ##
        ## ------------- ##
        if (is.null(feat_metadata)) {
            # initialize metadata
            gobject <- init_feat_metadata(gobject)
        } else {
            if (length(feat_metadata) != length(expression_feat)) {
                stop("Number of different molecular features need to
                    correspond with the feat_metadata list length \n")
            }

            for (feat_type in expression_feat) {
                gobject@feat_metadata[[feat_type]] <- data.table::as.data.table(
                    gobject@feat_metadata[[feat_type]]
                )
                gobject@feat_metadata[[feat_type]][
                    ,
                    feat_ID := gobject@feat_ID[[feat_type]]
                ]
            }
        }
    }



    ### OPTIONAL:
    ## spatial network - external input
    if (!is.null(spatial_network)) {
        if (is.null(spatial_network_name) |
            length(spatial_network) != length(spatial_network_name)) {
            stop("each spatial network must be given a unique name")
        } else {
            for (network_i in seq_len(length(spatial_network))) {
                networkname <- spatial_network_name[[network_i]]
                network <- spatial_network[[network_i]]

                if (any(c("data.frame", "data.table") %in% class(network))) {
                    if (all(c(
                        "to", "from", "weight", "sdimx_begin",
                        "sdimy_begin", "sdimx_end", "sdimy_end"
                    ) %in%
                        colnames(network))) {
                        # create spatialNetworkObj from data.table
                        network <- data.table::setDT(network)
                        # most info will be missing
                        warning(
                            "spatial_network ", network_i,
                            ' provided as data.table/frame object. Provenance and
                        spat_unit will be assumed: "',
                            names(slot(gobject, "spatial_info"))[[1]]
                        )

                        spatial_network_Obj <- createSpatNetObj(
                            name = networkname,
                            network = network,
                            spat_unit = names(
                                slot(gobject, "spatial_info")
                            )[[1]],
                            provenance = names(
                                slot(gobject, "spatial_info")
                            )[[1]]
                        ) # assumed

                        ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                        gobject <- setSpatialNetwork(gobject,
                            x = spatial_network_Obj
                        )
                        ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                    } else {
                        stop(
                            "\n network ", networkname,
                            " does not have all necessary column names,
                            see details\n"
                        )
                    }
                } else if (inherits(network, "spatialNetworkObj")) {
                    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                    gobject <- setSpatialNetwork(gobject,
                        x = network
                    )
                    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                } else {
                    stop(
                        "\n network ", networkname,
                        " is not a, spatialNetworkObj, data.frame,
                        or data.table\n"
                    )
                }
            }
        }
    }


    ## spatial grid - external input
    if (!is.null(spatial_grid)) {
        if (is.null(spatial_grid_name) |
            length(spatial_grid) != length(spatial_grid_name)) {
            stop("\n each spatial grid must be given a unique name \n")
        } else {
            for (grid_i in seq_len(length(spatial_grid))) {
                gridname <- spatial_grid_name[[grid_i]]
                grid <- spatial_grid[[grid_i]]

                if (inherits(grid, c("data.table", "data.frame"))) {
                    if (all(c(
                        "x_start", "y_start", "x_end", "y_end", "gr_name"
                    ) %in%
                        colnames(grid))) {
                        if (!inherits(grid, "data.table")) {
                            grid <- data.table::setDT(grid)
                        }
                        # Assume first spat_info and first expression_feat as
                        # spat_unit/provenance and feat_type respectively
                        warning(
                            "spatial_grid ", grid_i,
                            ' provided as data.table/frame object. Provenance and
                        spat_unit will be assumed: "',
                            names(slot(gobject, "spatial_info"))[[1]], '"\n'
                        )
                        # most info will be missing
                        grid <- create_spat_grid_obj(
                            name = gridname,
                            gridDT = grid,
                            spat_unit = names(
                                slot(gobject, "spatial_info")
                            )[[1]],
                            provenance = names(
                                slot(gobject, "spatial_info")
                            )[[1]],
                            feat_type = expression_feat[[1]]
                        )

                        ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                        gobject <- setSpatialGrid(
                            gobject = gobject,
                            spatial_grid = grid
                        )
                        ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                    } else {
                        stop(
                            "\n grid ", gridname,
                            " does not have all necessary column names,
                            see details \n"
                        )
                    }
                } else if (inherits(grid, "spatialGridObj")) {
                    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                    gobject <- setSpatialGrid(gobject, spatial_grid = grid)
                    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                } else {
                    stop(
                        "\n grid ", gridname,
                        " is not a spatialGridObj, data.frame,
                        or data.table \n"
                    )
                }
            }
        }
    }

    ## spatial enrichment
    if (!is.null(spatial_enrichment)) {
        if (is.null(spatial_enrichment_name) |
            length(spatial_enrichment) != length(spatial_enrichment_name)) {
            stop("\n each spatial enrichment data.table or data.frame must be
                given a unique name \n")
        } else {
            for (spat_enrich_i in seq_len(length(spatial_enrichment))) {
                spatenrichname <- spatial_enrichment_name[[spat_enrich_i]]
                spatenrich <- spatial_enrichment[[spat_enrich_i]]

                if (nrow(spatenrich) != nrow(gobject@cell_metadata)) {
                    stop(
                        "\n spatial enrichment ", spatenrichname,
                        " does not have the same number of rows as spots/cells,
                        see details \n"
                    )
                } else {
                    gobject@spatial_enrichment[[spatenrichname]] <- spatenrich
                }
            }
        }
    }


    ## dimension reduction
    if (!is.null(dimension_reduction)) {
        for (dim_i in seq_len(length(dimension_reduction))) {
            dim_red <- dimension_reduction[[dim_i]]

            if (all(c(
                "type", "name", "reduction_method", "coordinates", "misc"
            ) %in%
                names(dim_red))) {
                coord_data <- dim_red[["coordinates"]]

                if (all(rownames(coord_data) %in% gobject@cell_ID)) {
                    type_value <- dim_red[["type"]] # cells or genes
                    reduction_meth_value <- dim_red[["reduction_method"]]
                    # e.g. umap, tsne, ...
                    name_value <- dim_red[["name"]] # uniq name
                    misc_value <- dim_red[["misc"]] # additional data

                    gobject@dimension_reduction[[
                        type_value
                    ]][[reduction_meth_value]][[name_value]] <-
                        dim_red[c(
                            "name", "reduction_method", "coordinates",
                            "misc"
                        )]
                } else {
                    stop("\n rownames for coordinates are not found in gobject
                        IDs \n")
                }
            } else {
                stop("\n each dimension reduction list must contain all
                    required slots, see details. \n")
            }
        }
    }

    # NN network
    if (!is.null(nn_network)) {
        for (nn_i in seq_len(length(nn_network))) {
            nn_netw <- nn_network[[nn_i]]

            if (all(c("type", "name", "igraph") %in% names(nn_netw))) {
                igraph_data <- nn_netw[["igraph"]]

                if (all(names(igraph::V(igraph_data)) %in% gobject@cell_ID)) {
                    type_value <- nn_netw[["type"]] # sNN or kNN
                    name_value <- nn_netw[["name"]] # uniq name

                    gobject@nn_network[[type_value]][[name_value]][[
                        "igraph"
                    ]] <- igraph_data
                } else {
                    stop("\n igraph vertex names are not found in gobject
                        IDs \n")
                }
            } else {
                stop("\n each nn network list must contain all required slots,
                    see details. \n")
            }
        }
    }

    ## images ##
    # expect a list of inputs
    if (!is.null(images)) {
        vmsg(.v = verbose, "3. Start loading large images")

        if (is.null(largeImages_list_params)) {
            largeImages_list_params <- list(
                negative_y = TRUE,
                extent = NULL,
                use_rast_ext = FALSE,
                image_transformations = NULL,
                xmax_bound = NULL,
                xmin_bound = NULL,
                ymax_bound = NULL,
                ymin_bound = NULL,
                scale_factor = 1,
                verbose = TRUE
            )
        }

        default_base <- "image"
        images <- lapply(seq_along(images), function(img_i) {
            im <- images[[img_i]]

            # already in giotto format
            if (inherits(im, c("giottoImage", "giottoLargeImage"))) {
                return(im)
            }

            # read into giotto format
            if (inherits(im, "character") && file.exists(im)) {
                gimg <- do.call("createGiottoLargeImage", c(
                    raster_object = im,
                    name = default_base,
                    largeImages_list_params
                ))
            }

            # all others, raise warning and ignore
            warning(sprintf(
                "`image` item: %d %s",
                img_i,
                "\n is not an existing file path or a giotto image. ignored."
            ))
            return(NULL)
        })

        # prefer list names since they are more likely intentional
        # assign object names from @names slot to list IF list has no names
        images <- assign_objnames_2_list(images, force_replace = FALSE)
        obj_names <- names(images)

        # assign defaults for any names still missing
        if (is.null(obj_names)) obj_names <- rep(length(images), default_base)
        missing_name <- is.na(obj_names) | obj_names == ""
        obj_names[missing_name] <- default_base

        # handle naming overlaps
        obj_names <- .uniquify_dups(
            obj_names,
            what = "image name", sep = ".", verbose = verbose
        )

        # assign final names
        names(images) <- obj_names
        images <- assign_listnames_2_obj(images)

        # set image list
        gobject <- setGiotto(gobject, images, verbose = FALSE)

        vmsg(.v = verbose, "4. Finished loading large images")
    }

    return(gobject)
}









# constructor functions for S4 subobjects ####

#' @title Create S4 exprObj
#' @name createExprObj
#' @description Create an S4 exprObj
#' @inheritParams data_access_params
#' @param expression_data expression data
#' @param name name of exprObj
#' @param provenance origin data of expression information (if applicable)
#' @param misc misc
#' @param expression_matrix_class class of expression matrix to
#' use (e.g. 'dgCMatrix', 'DelayedArray')
#' @returns exprObj
#' @examples
#' x_expr <- readRDS(system.file("extdata/toy_matrix.RDS",
#'     package = "GiottoClass"
#' ))
#'
#' createExprObj(expression_data = x_expr)
#' @export
createExprObj <- function(
        expression_data,
        name = "test",
        spat_unit = "cell",
        feat_type = "rna",
        provenance = NULL,
        misc = NULL,
        expression_matrix_class = c("dgCMatrix", "DelayedArray")) {
    exprMat <- .evaluate_expr_matrix(expression_data,
        expression_matrix_class = expression_matrix_class,
        feat_type = feat_type
    )

    create_expr_obj(
        name = name,
        exprMat = exprMat,
        spat_unit = spat_unit,
        feat_type = feat_type,
        provenance = provenance,
        misc = misc
    )
}


#' @title create_expr_obj
#' @name create_expr_obj
#' @param exprMat matrix of expression information
#' @keywords internal
#' @returns expr_obj
#' @examples
#' x_expr <- readRDS(system.file("extdata/toy_matrix.RDS",
#'     package = "GiottoClass"
#' ))
#'
#' create_expr_obj(exprMat = x_expr)
#'
#' @export
create_expr_obj <- function(
        name = "test",
        exprMat = NULL,
        spat_unit = "cell",
        feat_type = "rna",
        provenance = NULL,
        misc = NULL) {
    deprecate_soft("3.3.0",
        what = "create_expr_obj()",
        with = "createExprObj()"
    )

    if (is.null(exprMat)) exprMat <- matrix()

    return(new("exprObj",
        name = name,
        exprMat = exprMat,
        spat_unit = spat_unit,
        feat_type = feat_type,
        provenance = provenance,
        misc = misc
    ))
}





#' @title Create S4 cellMetaObj
#' @name createCellMetaObj
#' @description Create an S4 cellMetaObj
#' @param metadata metadata info
#' @param col_desc (optional) character vector describing columns of the
#' metadata
#' @param spat_unit spatial unit of aggregated expression (e.g. 'cell')
#' @param feat_type feature type of aggregated
#' expression (e.g. 'rna', 'protein')
#' @param provenance origin data of aggregated expression
#' information (if applicable)
#' @param verbose be verbose
#' @returns cellMetaObj
#' @examples
#' df <- data.frame(cell_ID = c("cell_1", "cell_2", "cell_3"))
#'
#' createCellMetaObj(metadata = df)
#' @export
createCellMetaObj <- function(
        metadata,
        spat_unit = "cell",
        feat_type = "rna",
        provenance = NULL,
        col_desc = NULL,
        verbose = TRUE) {
    metadata <- .evaluate_cell_metadata(
        metadata = metadata,
        verbose = verbose
    )

    create_cell_meta_obj(
        metaDT = metadata,
        col_desc = col_desc,
        spat_unit = spat_unit,
        feat_type = feat_type,
        provenance = provenance
    )
}


#' @title create_cell_meta_obj
#' @name create_cell_meta_obj
#' @keywords internal
#' @returns cell_meta_obj
#'
#' @export
create_cell_meta_obj <- function(
        metaDT = NULL,
        col_desc = NA_character_,
        spat_unit = "cell",
        feat_type = "rna",
        provenance = NULL) {
    deprecate_soft("3.3.0",
        what = "create_cell_meta_obj()",
        with = "createCellMetaObj()"
    )

    if (is.null(col_desc)) col_desc <- NA_character_

    if (is.null(metaDT)) {
        metaDT <- data.table::data.table(
            cell_ID = NA_character_
        )
    }

    return(new("cellMetaObj",
        metaDT = metaDT,
        col_desc = col_desc,
        spat_unit = spat_unit,
        provenance = provenance,
        feat_type = feat_type
    ))
}





#' @title Create S4 featMetaObj
#' @name createFeatMetaObj
#' @description Create an S4 featMetaObj
#' @param metadata metadata info
#' @param col_desc (optional) character vector describing columns of the
#' metadata
#' @param spat_unit spatial unit of aggregated expression (e.g. 'cell')
#' @param feat_type feature type of aggregated
#' expression (e.g. 'rna', 'protein')
#' @param provenance origin data of aggregated expression
#' information (if applicable)
#' @param verbose be verbose
#' @returns featMetaObj
#' @examples
#' x <- data.frame(
#'     feat_ID = c("Mlc1", "Gprc5b", "Gfap"),
#'     nr_cells = c(80, 138, 151)
#' )
#'
#' createFeatMetaObj(metadata = x)
#' @export
createFeatMetaObj <- function(
        metadata,
        spat_unit = "cell",
        feat_type = "rna",
        provenance = NULL,
        col_desc = NULL,
        verbose = TRUE) {
    metadata <- .evaluate_feat_metadata(
        metadata = metadata,
        verbose = verbose
    )

    create_feat_meta_obj(
        metaDT = metadata,
        col_desc = col_desc,
        spat_unit = spat_unit,
        feat_type = feat_type,
        provenance = provenance
    )
}


#' @title create_feat_meta_obj
#' @name create_feat_meta_obj
#' @keywords internal
#' @returns feat_meta_obj
#' @export
create_feat_meta_obj <- function(
        metaDT = NULL,
        col_desc = NA_character_,
        spat_unit = "cell",
        feat_type = "rna",
        provenance = NULL) {
    deprecate_soft("3.3.0",
        what = "create_feat_meta_obj()",
        with = "createFeatMetaObj()"
    )

    if (is.null(col_desc)) col_desc <- NA_character_

    if (is.null(metaDT)) {
        metaDT <- data.table::data.table(
            feat_ID = NA_character_
        )
    }

    return(new("featMetaObj",
        metaDT = metaDT,
        col_desc = col_desc,
        spat_unit = spat_unit,
        provenance = provenance,
        feat_type = feat_type
    ))
}







#' @title Create S4 dimObj
#' @name createDimObj
#' @description Create an S4 dimObj
#' @param coordinates embedding coordinates
#' @param name name of dimObj
#' @param reduction reduction on columns (e.g. cells) or rows (e.g. features)
#' @param method method used to generate dimension reduction
#' @param spat_unit spatial unit of aggregated expression (e.g. 'cell')
#' @param feat_type feature type of aggregated
#' expression (e.g. 'rna', 'protein')
#' @param provenance origin data of aggregated expression
#' information (if applicable)
#' @param misc misc
#' @param my_rownames (optional) if needed, set coordinates rowname values here
#' @returns dimObj
#' @examples
#' x <- matrix(c(2.81, 5.59, 10.54, 2.25, 5.12, 2.79), nrow = 3)
#' rownames(x) <- c("cell_1", "cell_2", "cell_3")
#' colnames(x) <- c("Dim.1", "Dim.2")
#'
#' createDimObj(coordinates = x, name = "pca", method = "pca")
#' @export
createDimObj <- function(
        coordinates,
        name = "test",
        spat_unit = "cell",
        feat_type = "rna",
        method = NULL,
        reduction = "cells",
        provenance = NULL,
        misc = NULL,
        my_rownames = NULL) {
    coordinates <- .evaluate_dimension_reduction(coordinates)

    create_dim_obj(
        name = name,
        reduction = reduction,
        reduction_method = method,
        coordinates = coordinates,
        spat_unit = spat_unit,
        feat_type = feat_type,
        provenance = provenance,
        misc = misc,
        my_rownames = my_rownames
    )
}


#' @title create_dim_obj
#' @name create_dim_obj
#' @keywords internal
#' @returns dim_obj
#' @export
create_dim_obj <- function(
        name = "test",
        reduction = "cells",
        reduction_method = NA_character_,
        coordinates = NULL,
        spat_unit = "cell",
        feat_type = "rna",
        provenance = NULL,
        misc = NULL,
        my_rownames = NULL) {
    deprecate_soft("3.3.0",
        what = "create_dim_obj()",
        with = "createDimObj()"
    )

    if (is.null(reduction_method)) reduction_method <- NA_character_

    number_of_dimensions <- ncol(coordinates)
    colnames(coordinates) <- paste0("Dim.", seq(number_of_dimensions))

    if (!is.null(my_rownames)) {
        rownames(coordinates) <- as.character(my_rownames)
    }

    new("dimObj",
        name = name,
        reduction = reduction,
        reduction_method = reduction_method,
        coordinates = coordinates,
        spat_unit = spat_unit,
        feat_type = feat_type,
        provenance = if (is.null(provenance)) spat_unit else provenance,
        # assumed
        misc = misc
    )
}








#' @title Create S4 nnNetObj
#' @name createNearestNetObj
#' @description Create an S4 nnNetObj
#' @inheritParams data_access_params
#' @param name name of nnNetObj
#' @param nn_type type of nearest neighbor network
#' @param network igraph object or data.frame containing nearest neighbor
#' information (see details)
#' @param provenance origin of aggregated information (if applicable)
#' @param misc misc
#' @details igraph and dataframe-like inputs must include certain information.
#' For igraph, it must have, at minimum vertex 'name' attributes and 'distance'
#' edge attribute.
#' dataframe-like inputs must have 'from', 'to', and 'distance' columns
#' @returns nnNetObj
#' @examples
#' x <- GiottoData::loadSubObjectMini("nnNetObj")
#'
#' createNearestNetObj(
#'     network = slot(x, "igraph"), name = "sNN",
#'     nn_type = "sNN"
#' )
#' @export
createNearestNetObj <- function(
        name = "test",
        network,
        nn_type = NULL,
        spat_unit = "cell",
        feat_type = "rna",
        provenance = NULL,
        misc = NULL) {
    if (is.null(network)) {
        igraph <- NULL
    } else {
        # convert igraph input to preferred format
        igraph <- .evaluate_nearest_networks(network)
    }

    create_nn_net_obj(
        name = name,
        igraph = igraph,
        nn_type = nn_type,
        spat_unit = spat_unit,
        feat_type = feat_type,
        provenance = provenance,
        misc = misc
    )
}


#' @title create_nn_net_obj
#' @name create_nn_net_obj
#' @keywords internal
#' @returns nn_net_obj
#' @export
create_nn_net_obj <- function(
        name = "test",
        nn_type = NA_character_,
        igraph = NULL,
        spat_unit = "cell",
        feat_type = "rna",
        provenance = NULL,
        misc = NULL) {
    deprecate_soft("3.3.0",
        what = "create_nn_net_obj()",
        with = "createNearestNetObj()"
    )

    if (is.null(nn_type)) nn_type <- NA_character_

    new("nnNetObj",
        name = name,
        nn_type = nn_type,
        igraph = igraph,
        spat_unit = spat_unit,
        feat_type = feat_type,
        provenance = provenance,
        misc = misc
    )
}








#' @title Create S4 spatLocsObj
#' @name createSpatLocsObj
#' @description Create an S4 spatLocsObj
#' @param coordinates spatial coordinates
#' @param name name of spatLocsObj
#' @param spat_unit spatial unit of aggregated expression (e.g. 'cell')
#' @param provenance origin data of aggregated expression
#' information (if applicable)
#' @param misc misc
#' @param verbose be verbose
#' @returns spatLocsObj
#' @examples
#' x <- data.frame(
#'     cell_ID = c("cell_1", "cell_2", "cell_3"),
#'     sdimx = c(6637.881, 6471.978, 6801.610),
#'     sdimy = c(-5140.465, -4883.541, -4968.685)
#' )
#'
#' createSpatLocsObj(coordinates = x, name = "raw")
#' @export
createSpatLocsObj <- function(
        coordinates,
        name = "test",
        spat_unit = "cell",
        provenance = NULL,
        misc = NULL,
        verbose = TRUE) {
    # convert coordinates input to preferred format
    coordinates <- .evaluate_spatial_locations(
        spatial_locs = coordinates,
        verbose = verbose
    )

    create_spat_locs_obj(
        name = name,
        coordinates = coordinates,
        spat_unit = spat_unit,
        provenance = provenance,
        misc = misc
    )
}


#' @title create_spat_locs_obj
#' @name create_spat_locs_obj
#' @keywords internal
#' @returns spat_locs_obj
#' @export
create_spat_locs_obj <- function(
        name = "test",
        coordinates = NULL,
        spat_unit = "cell",
        provenance = NULL,
        misc = NULL) {
    deprecate_soft("3.3.0",
        what = "create_spat_locs_obj()",
        with = "createSpatLocsObj()"
    )

    # DT vars
    cell_ID <- NULL

    if (is.null(coordinates)) {
        coordinates <- data.table::data.table(
            sdimx = NA_real_,
            sdimy = NA_real_,
            cell_ID = NA_character_
        )
    }

    # set cell_ID col if missing to conform to spatialLocationsObj validity
    # should already never be the case after evaluation
    if (!"cell_ID" %in% colnames(coordinates)) {
        coordinates[
            ,
            cell_ID := NA_character_
        ]
    }

    new("spatLocsObj",
        name = name,
        coordinates = coordinates,
        spat_unit = spat_unit,
        provenance = provenance,
        misc = misc
    )
}








#' @title Create S4 spatialNetworkObj
#' @name createSpatNetObj
#' @param network network data with connections, distances, and weightings
#' @param name name of spatialNetworkObj
#' @param networkDT_before_filter (optional) unfiltered data.table  of
#' network connections, distances, and weightings
#' @param spat_unit spatial unit tag
#' @param method method used to generate spatial network
#' @param parameters (optional) additional method-specific parameters used
#' during spatial network generation
#' @param outputObj (optional) network geometry object
#' @param cellShapeObj (optional) network cell shape information
#' @param crossSectionObjects (optional) crossSectionObjects
#' @param provenance (optional) origin of aggregated information (if applicable)
#' @param misc misc
#' @returns spatialNetworkObj
#' @examples
#' x <- GiottoData::loadSubObjectMini("spatialNetworkObj")
#'
#' createSpatNetObj(network = slot(x, "networkDT"), name = "Delaunay_network")
#' @export
createSpatNetObj <- function(
        network,
        name = "test",
        networkDT_before_filter = NULL,
        method = NULL,
        spat_unit = "cell",
        provenance = NULL,
        parameters = NULL,
        outputObj = NULL,
        cellShapeObj = NULL,
        crossSectionObjects = NULL,
        misc = NULL) {
    networkDT <- .evaluate_spatial_network(network)

    create_spat_net_obj(
        name = name,
        method = method,
        parameters = parameters,
        outputObj = outputObj,
        networkDT = networkDT,
        networkDT_before_filter = networkDT_before_filter,
        cellShapeObj = cellShapeObj,
        crossSectionObjects = crossSectionObjects,
        spat_unit = spat_unit,
        provenance = provenance,
        misc = misc
    )
}


#' @title create_spat_net_obj
#' @name create_spat_net_obj
#' @keywords internal
#' @returns spat_net_obj
#' @export
create_spat_net_obj <- function(
        name = "test",
        method = NA_character_,
        parameters = NULL,
        outputObj = NULL,
        networkDT = NULL,
        networkDT_before_filter = NULL,
        cellShapeObj = NULL,
        crossSectionObjects = NULL,
        spat_unit = "cell",
        provenance = NULL,
        misc = NULL) {
    deprecate_soft("3.3.0",
        what = "create_spat_net_obj()",
        with = "createSpatNetObj()"
    )

    if (is.null(method)) method <- NA_character_

    new("spatialNetworkObj",
        name = name,
        method = method,
        parameters = parameters,
        outputObj = outputObj,
        networkDT = networkDT,
        networkDT_before_filter = networkDT_before_filter,
        cellShapeObj = cellShapeObj,
        crossSectionObjects = crossSectionObjects,
        spat_unit = spat_unit,
        provenance = provenance,
        misc = misc
    )
}






#' @title Create S4 spatEnrObj
#' @name createSpatEnrObj
#' @param enrichment_data spatial enrichment results, provided a
#' dataframe-like object
#' @param name name of S4 spatEnrObj
#' @param method method used to generate spatial enrichment information
#' @param spat_unit spatial unit of aggregated expression (e.g. 'cell')
#' @param feat_type feature type of aggregated
#' expression (e.g. 'rna', 'protein')
#' @param provenance origin data of aggregated expression
#' information (if applicable)
#' @param misc misc additional information about the spatial enrichment or
#' how it was generated
#' @param verbose be verbose
#' @returns spatEnrObj
#' @examples
#' x <- GiottoData::loadSubObjectMini("spatEnrObj")
#'
#' createSpatEnrObj(
#'     enrichment_data = slot(x, "enrichDT"),
#'     name = "cluster_metagene"
#' )
#' @export
createSpatEnrObj <- function(
        enrichment_data,
        name = "test",
        spat_unit = "cell",
        feat_type = "rna",
        method = NULL,
        provenance = NULL,
        misc = NULL,
        verbose = TRUE) {
    enrichDT <- .evaluate_spatial_enrichment(enrichment_data, verbose = verbose)

    create_spat_enr_obj(
        name = name,
        method = method,
        enrichDT = enrichment_data,
        spat_unit = spat_unit,
        feat_type = feat_type,
        provenance = provenance,
        misc = misc
    )
}


#' @title create_spat_enr_obj
#' @name create_spat_enr_obj
#' @keywords internal
#' @returns spat_enr_obj
#' @export
create_spat_enr_obj <- function(
        name = "test",
        method = NA_character_,
        enrichDT = NULL,
        spat_unit = "cell",
        feat_type = "rna",
        provenance = NULL,
        misc = NULL) {
    deprecate_soft("3.3.0",
        what = "create_spat_enr_obj()",
        with = "createSpatEnrObj()"
    )

    if (is.null(method)) method <- NA_character_

    new("spatEnrObj",
        name = name,
        method = method,
        enrichDT = enrichDT,
        spat_unit = spat_unit,
        feat_type = feat_type,
        provenance = provenance,
        misc = misc
    )
}













#' @title Create S4 spatialGridObj
#' @name create_spat_grid_obj
#' @param name name of spatialGridObj
#' @param method method used to generate spatial grid
#' @param parameters additional method-specific parameters used during
#' spatial grid generation
#' @param gridDT data.table holding the spatial grid information
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param provenance origin of aggregated information (if applicable)
#' @param misc misc
#' @returns spatialGridObj
#' @keywords internal
#' @examples
#' x <- GiottoData::loadSubObjectMini("spatialGridObj")
#'
#' create_spat_grid_obj(name = "test", gridDT = x)
#' @export
create_spat_grid_obj <- function(
        name = "test",
        method = NA_character_,
        parameters = NULL,
        gridDT = NULL,
        spat_unit = "cell",
        feat_type = "rna",
        provenance = NULL,
        misc = NULL) {
    return(new("spatialGridObj",
        name = name,
        method = method,
        parameters = parameters,
        gridDT = gridDT,
        spat_unit = spat_unit,
        feat_type = feat_type,
        provenance = provenance,
        misc = misc
    ))
}







#' @title Create feature network object
#' @name create_featureNetwork_object
#' @param name name to assign the created feature network object
#' @param network_datatable network data.table object
#' @param network_lookup_id network lookup id
#' @param full fully connected status
#' @keywords internal
#' @returns featureNetwork_object
create_featureNetwork_object <- function(
        name = "feat_network",
        network_datatable = NULL,
        network_lookup_id = NULL,
        full = NULL) {
    # create minimum giotto points object
    f_network <- featureNetwork(
        name = name,
        network_datatable = NULL,
        network_lookup_id = NULL,
        full = NULL
    )

    ## 1. check network data.table object
    if (!methods::is(network_datatable, "data.table")) {
        stop("network_datatable needs to be a network data.table object")
    }
    f_network@network_datatable <- network_datatable

    ## 2. provide network fully connected status
    f_network@full <- full

    ## 3. provide feature network name
    f_network@name <- name

    ## 4. network lookup id
    f_network@network_lookup_id <- network_lookup_id

    # giotoPoints object
    return(f_network)
}


# * createGiottoPoints ####
# create Giotto points from data.frame or spatVector

#' @title Create giotto points object
#' @name createGiottoPoints
#' @description Create a `giottoPoints` object that is used to represent
#' subcellular point-type features. The main values are contained within the
#' `spatVector` slot, which should contain spatial point data and also an
#' associated set of attributes for at
#' least `feat_ID` (name of the feature being described)
#' and `feat_ID_uniq` (a unique integer identifier for each specific point).
#' @param x spatVector or data.frame-like object with points coordinate
#' information (x, y, feat_ID)
#' @param feat_type character. feature type. Provide more than one value if
#' using the `split_keyword` param. For each set of keywords to split by, an
#' additional feat_type should be provided in the same order.
#' @param verbose be verbose
#' @param split_keyword list of character vectors of keywords to split the
#' giottoPoints object based on their feat_ID. Keywords will be `grepl()`
#' matched against the feature IDs information.
#' @param unique_IDs (optional) character vector of unique IDs present within
#' the spatVector data. Provided for cacheing purposes
#' @details
#' Using the manual option where you can select the names of the x, y, and
#' feat_ID columns is not compatible with a data.frame that already has the
#' names x, y, and/or feat_ID.
#' @returns giottoPoints
#'
#' @examples
#' # data.frame input
#' x <- data.frame(
#'     ID = letters[seq_len(6)],
#'     x = seq_len(6),
#'     y = seq(6, 1)
#' )
#' gpoints <- createGiottoPoints(x)
#' plot(gpoints,
#'     raster = FALSE, # don't plot with rasterization or it will be hard to see
#'     cex = 0.5
#' )
#'
#' # with a split_keyword
#' # Use this when values to read in contain multiple sets of information that
#' # should be put into separate objects.
#' gp_list <- createGiottoPoints(x,
#'     feat_type = c("feat_a", "feat_b"),
#'     split_keyword = list(c("b", "c"))
#' )
#' force(gp_list)
#'
#' # subsetting
#' gpoints[c(1, 3)] # numerical
#' gpoints[c(TRUE, FALSE, FALSE)] # logical
#' gpoints[c("a", "f")] # character subsetting is keyed on feat_ID attribute
#' gpoints[] # drop to SpatVector
#' @concept points
NULL

#' @rdname createGiottoPoints
#' @export
setMethod(
    "createGiottoPoints", signature("SpatVector"),
    function(
        x,
        feat_type = "rna",
        verbose = TRUE,
        split_keyword = NULL,
        unique_IDs = NULL) {
        checkmate::assert_character(feat_type)
        if (!is.null(split_keyword)) checkmate::assert_list(split_keyword)

        # 1. format SpatVector and create inital giottoPoints object
        gpoints <- create_giotto_points_object(
            feat_type = feat_type[[1]],
            spatVector = x,
            unique_IDs = unique_IDs
        )

        # 2. perform split if needed
        if (is.null(split_keyword)) {
            return(gpoints)
        }

        # create booleans using grepl and the given keywords
        gpoints_feat_ids <- featIDs(gpoints, uniques = FALSE)
        split_bools <- lapply(split_keyword, function(keyword) {
            grepl(paste(keyword, collapse = "|"), gpoints_feat_ids)
        })
        # default_bool is the main set of points that do not get selected by any
        # keywords. Usually the actual features being detected are here. These
        # will get mapped to the first feat_type.
        # default_bool must be made as a list for it to combine properly using
        # c() with split_bools which are already a list of logical vectors
        default_bool <- list(!Reduce("|", split_bools))
        split_bools <- c(default_bool, split_bools)
        names(split_bools) <- feat_type

        # split the created gpoints object into several using the booleans.
        gpoints_list <- lapply(split_bools, function(feat) {
            gpoints[feat]
        })

        # set object name to match the feat_type.
        for (name_i in seq_along(feat_type)) {
            objName(gpoints_list[[name_i]]) <- feat_type[[name_i]]
        }

        return(gpoints_list)
    }
)

#' @rdname createGiottoPoints
#' @param x_colname column name for x-coordinates
#' @param y_colname column name for y-coordinates
#' @param feat_ID_colname column name for feature ids
#' @export
setMethod(
    "createGiottoPoints", signature("data.frame"),
    function(
        x,
        x_colname = NULL,
        y_colname = NULL,
        feat_ID_colname = NULL,
        feat_type = "rna",
        verbose = TRUE,
        split_keyword = NULL,
        unique_IDs = NULL) {
        checkmate::assert_character(feat_type)
        if (!is.null(split_keyword)) checkmate::assert_list(split_keyword)

        # format and convert to SpatVector
        spatvec <- .create_spatvector_object_from_dfr(
            x = x,
            x_colname = x_colname,
            y_colname = y_colname,
            feat_ID_colname = feat_ID_colname,
            verbose = verbose
        )

        # pass to SpatVector method
        createGiottoPoints(
            x = spatvec,
            feat_type = feat_type,
            verbose = verbose,
            split_keyword = split_keyword,
            unique_IDs = unique_IDs
        )
    }
)



#' @title Create giotto points object
#' @name create_giotto_points_object
#' @param feat_type feature type
#' @param spatVector terra spatVector object containing point data
#' @param networks (optional) feature network object
#' @param unique_IDs (optional) unique IDs in spatVector for cacheing
#' @keywords internal
#' @returns giotto_points_object
create_giotto_points_object <- function(
        feat_type = "rna",
        spatVector = NULL,
        networks = NULL,
        unique_IDs = NULL) {
    if (is.null(feat_type)) feat_type <- NA # compliance with featData class

    # create minimum giotto points object
    g_points <- giottoPoints(
        feat_type = feat_type,
        spatVector = NULL,
        networks = NULL
    )

    ## 1. check terra spatVector object
    if (!inherits(spatVector, "SpatVector")) {
        stop("spatVector needs to be a spatVector object from the terra
            package")
    }

    terra::crs(spatVector) <- NULL
    g_points@spatVector <- spatVector

    ## 2. provide feature id
    g_points@feat_type <- feat_type

    ## 3. feature_network object
    g_points@networks <- networks

    ## 4. feat_ID cacheing
    if (is.null(unique_IDs)) {
        g_points@unique_ID_cache <- featIDs(g_points)
    } else {
        g_points@unique_ID_cache <- unique_IDs
    }

    # giottoPoints object
    return(g_points)
}







# * createGiottoPolygon ####

#' @name createGiottoPolygon
#' @title Create giotto polygons object
#' @description Create a `giottoPolygon` object that is used to represent
#' spatial annotations and polygons. Inputs can be from a structured data.frame
#' object where three of the columns should correspond to x/y vertices and the
#' polygon ID and additional columns are set as attributes, a spatial file
#' such as wkt, .shp, or .GeoJSON, or a mask file (e.g. segmentation results).
#' \cr
#' The character method is for file reading and will dispatch to specific
#' methods based on what kind of data the file was.
#' @param x input. Filepath to a .GeoJSON or a mask image file. Can also be a
#' data.frame with vertex 'x', 'y', and 'poly_ID' information.
#' @param name name for polygons
#' @param calc_centroids calculate centroids for polygons
#' @param verbose be verbose
#' @returns giottoPolygon
NULL


#' @rdname createGiottoPolygon
#' @param \dots additional params to pass. For character method, params pass to
#' SpatRaster or SpatVector methods, depending on whether x was a filepath to
#' a maskfile or a spatial file (ex: wkt, shp, GeoJSON) respectively.
#' @examples
#' # ------- create from a mask image ------- #
#' m <- system.file("extdata/toy_mask_multi.tif", package = "GiottoClass")
#' plot(terra::rast(m), col = grDevices::hcl.colors(7))
#' gp <- createGiottoPolygon(
#'     m,
#'     flip_vertical = FALSE, flip_horizontal = FALSE,
#'     shift_horizontal_step = FALSE, shift_vertical_step = FALSE,
#'     ID_fmt = "id_test_%03d",
#'     name = "test"
#' )
#' plot(gp, col = grDevices::hcl.colors(7))
#'
#' # ------- create from an shp file ------- #
#' shp <- system.file("extdata/toy_poly.shp", package = "GiottoClass")
#' # vector inputs do not have params for flipping and shifting
#' gp2 <- createGiottoPolygon(shp, name = "test")
#' plot(gp2, col = grDevices::hcl.colors(7))
#'
#' @export
setMethod(
    "createGiottoPolygon", signature("character"),
    function(x, ...) {
        checkmate::assert_file_exists(x)

        # try success means it should be mask file
        # try failure means it should be vector file
        try_rast <- tryCatch(
            {
                terra::rast(x)
            },
            error = function(e) {
                return(invisible(NULL))
            },
            warning = function(w) {
                NULL
            }
        )

        # mask workflow
        if (inherits(try_rast, "SpatRaster")) {
            return(createGiottoPolygon(try_rast, ...))
        }

        # file workflow
        return(createGiottoPolygon(x = terra::vect(x), ...))
    }
)


#' @rdname createGiottoPolygon
#' @export
setMethod(
    "createGiottoPolygon", signature("SpatVector"),
    function(x, name = "cell", calc_centroids = FALSE, verbose = TRUE) {
        res_list <- .evaluate_gpoly_spatvector(input_sv = x, verbose = verbose)
        spatvector <- res_list$spatvector
        unique_IDs <- res_list$unique_IDs

        # create giottoPolygon object
        g_polygon <- create_giotto_polygon_object(
            name = name,
            spatVector = spatvector,
            spatVectorCentroids = NULL,
            unique_IDs = NULL
        )

        # add centroids
        if (isTRUE(calc_centroids)) {
            g_polygon <- .calculate_centroids_polygons(
                gpolygon = g_polygon,
                name = "centroids",
                append_gpolygon = TRUE
            )
        }

        return(g_polygon)
    }
)

#' @rdname createGiottoPolygon
#' @export
setMethod(
    "createGiottoPolygon", signature("SpatRaster"),
    function(
        x,
        name = "cell",
        mask_method = c("guess", "single", "multiple"),
        remove_background_polygon = FALSE,
        background_algo = c("range"),
        fill_holes = TRUE,
        poly_IDs = NULL,
        ID_fmt = "cell_",
        flip_vertical = TRUE,
        shift_vertical_step = TRUE,
        flip_horizontal = TRUE,
        shift_horizontal_step = TRUE,
        remove_unvalid_polygons = TRUE,
        calc_centroids = FALSE,
        verbose = TRUE) {
        # verbose not used

        createGiottoPolygonsFromMask(
            maskfile = x,
            name = name,
            mask_method = mask_method,
            remove_background_polygon = remove_background_polygon,
            background_algo = background_algo,
            fill_holes = fill_holes,
            poly_IDs = poly_IDs,
            ID_fmt = ID_fmt,
            flip_vertical = flip_vertical,
            shift_vertical_step = shift_vertical_step,
            flip_horizontal = flip_horizontal,
            shift_horizontal_step = shift_horizontal_step,
            remove_unvalid_polygons = remove_unvalid_polygons,
            calc_centroids = calc_centroids
        )
    }
)


#' @rdname createGiottoPolygon
#' @examples
#' # ------- create from data.frame-like ------- #
#' shp <- system.file("extdata/toy_poly.shp", package = "GiottoClass")
#' gpoly <- createGiottoPolygon(shp, name = "test")
#' plot(gpoly)
#' gpoly_dt <- data.table::as.data.table(gpoly, geom = "XY")
#' needed_cols_dt <- gpoly_dt[, .(geom, part, x, y, hole, poly_ID)]
#' force(needed_cols_dt)
#'
#' out <- createGiottoPolygon(needed_cols_dt,
#'     name = "test"
#' )
#' plot(out)
#'
#' @export
setMethod(
    "createGiottoPolygon", signature("data.frame"),
    function(
        x,
        name = "cell",
        calc_centroids = FALSE,
        skip_eval_dfr = FALSE,
        copy_dt = TRUE,
        verbose = TRUE) {
        createGiottoPolygonsFromDfr(
            segmdfr = x,
            name = name,
            calc_centroids = calc_centroids,
            skip_eval_dfr = skip_eval_dfr,
            copy_dt = copy_dt,
            verbose = verbose
        )
    }
)


#' @rdname createGiottoPolygon
#' @param maskfile path to mask file, a terra `SpatRaster`, or some other
#' data class readable by [terra::rast()]
#' @param mask_method how the mask file defines individual segmentation
#' annotations. See *mask_method* section
#' @param name character. Name to assign created `giottoPolygon`
#' @param remove_background_polygon try to remove background
#' polygon (default: FALSE)
#' @param background_algo algorithm to remove background polygon
#' @param fill_holes fill holes within created polygons
#' @param poly_IDs character vector. Default = NULL. Custom unique names for
#' each polygon in the mask file.
#' @param ID_fmt character. Only applied if `poly_IDs = NULL`. Naming scheme for
#' poly_IDs. Default = "cell_". See *ID_fmt* section.
#' @param flip_vertical flip mask figure in a vertical manner
#' @param shift_vertical_step shift vertical (boolean or numerical)
#' @param flip_horizontal flip mask figure in a horizontal manner
#' @param shift_horizontal_step shift horizontal (boolean or numerical)
#' @param remove_unvalid_polygons remove unvalid polygons (default: TRUE)
#' @concept mask polygon
#' @section mask_method:
#' One of "single", "multiple", or "guess".
#' \itemize{
#'   \item{*"single"* assumes that the provided mask image is binary, with only
#'   polygon vs background being distinct values. With this kind of image, the
#'   expected generated polygons is a single multipart polygon. "single" takes
#'   this multipart polygon and breaks it apart into individual singlepart
#'   polygons. An initial simple `numeric` index as the 'nth' polygon found in
#'   the mask image will be applied as an ID (see *ID_fmt* section).}
#'   \item{*"multiple"* assumes that the provided mask image has distinct
#'   intensity values to specify the IDs of individual polygons. An initial
#'   `numeric` ID is applied as the intensity value of the pixels that made up
#'   the annotation for that polygon in the mask image (see *ID_fmt* section).}
#'   \item{*"guess"* examines the values in the image to pick the most likely
#'   appropriate method out of "single" or "multiple".}
#' }
#' @section ID_fmt:
#' Defaults to applying the input as a prefix (using `paste0()`) to the
#' numerical ID values detected by  `mask_method`. (ie: `ID_fmt = "cell_"`
#' produces `cell_1`, `cell_2`, `cell_3`, ...)\cr
#' If a "%" character is detected in the input then the input will be treated as
#' a `sprintf()` `fmt` param input instead. (ie: `ID_fmt = "cell_%03d"` produces
#' `cell_001`, `cell_002`, `cell_003`, ...)
#' @examples
#' mask_multi <- system.file("extdata/toy_mask_multi.tif",
#'     package = "GiottoClass"
#' )
#' mask_single <- system.file("extdata/toy_mask_single.tif",
#'     package = "GiottoClass"
#' )
#' plot(terra::rast(mask_multi), col = grDevices::hcl.colors(7))
#' plot(terra::rast(mask_single))
#'
#' gpoly1 <- createGiottoPolygonsFromMask(
#'     mask_multi,
#'     flip_vertical = FALSE, flip_horizontal = FALSE,
#'     shift_horizontal_step = FALSE, shift_vertical_step = FALSE,
#'     ID_fmt = "id_test_%03d",
#'     name = "multi_test"
#' )
#' plot(gpoly1, col = grDevices::hcl.colors(7))
#'
#' gpoly2 <- createGiottoPolygonsFromMask(
#'     mask_single,
#'     flip_vertical = FALSE, flip_horizontal = FALSE,
#'     shift_horizontal_step = FALSE, shift_vertical_step = FALSE,
#'     ID_fmt = "id_test_%03d",
#'     name = "single_test"
#' )
#' plot(gpoly2, col = grDevices::hcl.colors(5))
#' @export
createGiottoPolygonsFromMask <- function(maskfile,
    mask_method = c("guess", "single", "multiple"),
    name = "cell",
    remove_background_polygon = FALSE,
    background_algo = c("range"),
    fill_holes = TRUE,
    poly_IDs = NULL,
    ID_fmt = "cell_",
    flip_vertical = TRUE,
    shift_vertical_step = TRUE,
    flip_horizontal = TRUE,
    shift_horizontal_step = TRUE,
    calc_centroids = FALSE,
    remove_unvalid_polygons = TRUE,
    verbose = FALSE) {
    # data.table vars
    x <- y <- geom <- part <- NULL

    remove_unvalid_polygons <- as.logical(remove_unvalid_polygons)

    # select background algo
    background_algo <- match.arg(background_algo, choices = "range")

    # mask method
    # single: single mask value for all segmented cells
    # multiple: multiple mask values and thus a unique value for each
    # segmented cell
    mask_method <- match.arg(mask_method,
        choices = c("guess", "single", "multiple")
    )


    # if maskfile input is not a spatraster, read it in as spatraster
    # if it is spatraster, skip
    if (inherits(maskfile, "SpatRaster")) {
        terra_rast <- maskfile
    } else if (is.character(maskfile)) {
        # check if mask file exists
        maskfile <- path.expand(maskfile)
        checkmate::assert_file_exists(maskfile)
        terra_rast <- .create_terra_spatraster(maskfile)
    } else {
        # assume some other class readable by terra::rast()
        terra_rast <- .create_terra_spatraster(maskfile)
    }

    # create polygons from mask
    rast_dimensions <- dim(terra_rast)
    # value = TRUE here means that the intensity value of the mask image
    # (which usually encodes the intended polygon ID) is added to the resulting
    # SpatVector as the only attribute.
    terra_polygon <- terra::as.polygons(x = terra_rast, value = TRUE)
    val_col <- names(terra_polygon) # the only col should be from the values

    # fill holes
    if (isTRUE(fill_holes)) {
        terra_polygon <- terra::fillHoles(terra_polygon)
    }

    # handle unvalid polygons ##
    # The unvalid polys formed from as.polygons are usually very misshapen and
    # artefacted. It is impossible to fix them using `terra::makeValid()`
    if (remove_unvalid_polygons) {
        valid_index <- terra::is.valid(terra_polygon)
        terra_polygon <- terra_polygon[valid_index]
    }

    ## flip across axes ##
    if (isTRUE(flip_vertical)) {
        terra_polygon <- .flip_spatvect(terra_polygon)
    }
    if (isTRUE(flip_horizontal)) {
        terra_polygon <- .flip_spatvect(terra_polygon)
    }

    # convert to DT format since we want to be able to compare number of geoms
    # vs polys to determine correct mask method.
    # TODO only test a subset of polys here?
    spatVecDT <- .spatvector_to_dt(terra_polygon)

    ## guess mask method ##
    if (mask_method == "guess") {
        uniq_geoms <- length(unique(spatVecDT$geom))
        uniq_parts <- length(unique(spatVecDT$part))
        mask_method <- ifelse(uniq_geoms > uniq_parts, "multiple", "single")
    }
    vmsg(
        .v = verbose,
        sprintf("parsing mask using mask_method: %s", mask_method)
    )


    ## define polys and apply auto IDs ##
    naming_fun <- ifelse(grepl("%", ID_fmt), sprintf, paste0)
    # If poly_IDs are NOT provided, then terra_polygon IDs created here will be
    # `character` and the finalized ID values.
    # If poly_IDs ARE provided, the IDs are still temporary and MUST remain
    # `numeric`, pending the `poly_IDs` param being applied downstream.
    terra_polygon <- switch(mask_method,
        "multiple" = {
            names(terra_polygon) <- "poly_ID"
            if (is.null(poly_IDs)) {
                # spatVecDT[, geom := naming_fun(ID_fmt, geom)]
                # spatVecDT[, (val_col) := naming_fun(ID_fmt, get(val_col))]
                # g_polygon <- createGiottoPolygonsFromDfr(
                #   segmdfr = spatVecDT[, .(x, y, get(val_col))]
                # )
                # g_polygon@spatVector
                terra_polygon$poly_ID <- naming_fun(
                    ID_fmt,
                    terra_polygon$poly_ID
                )
            }
            terra_polygon
        },
        "single" = {
            # TODO ordering may be performed based on centroids xy instead of
            # converting the full polygon and then ordering on parts
            # May improve the speed
            if (is.null(poly_IDs)) {
                spatVecDT[, part := naming_fun(ID_fmt, part)]
            }
            g_polygon <- createGiottoPolygonsFromDfr(
                segmdfr = spatVecDT[, .(x, y, part)]
            )
            if (!is.null(poly_IDs)) {
                g_polygon@spatVector$poly_ID <- as.numeric(
                    g_polygon@spatVector$poly_ID
                )
            }

            g_polygon@spatVector
        }
    )



    ## apply spatial shifts ##
    if (identical(shift_vertical_step, TRUE)) {
        shift_vertical_step <- rast_dimensions[1] # nrows of raster
    } else if (is.numeric(shift_vertical_step)) {
        shift_vertical_step <- rast_dimensions[1] * shift_vertical_step
    } else {
        shift_vertical_step <- 0
    }
    if (identical(shift_horizontal_step, TRUE)) {
        shift_horizontal_step <- rast_dimensions[2] # ncols of raster
    } else if (is.numeric(shift_horizontal_step)) {
        shift_horizontal_step <- rast_dimensions[2] * shift_horizontal_step
    } else {
        shift_horizontal_step <- 0
    }

    terra_polygon <- terra::shift(
        terra_polygon,
        dx = shift_horizontal_step,
        dy = shift_vertical_step
    )


    ## remove background polygon ##
    if (isTRUE(remove_background_polygon)) {
        if (background_algo == "range") {
            backgr_poly_id <- .identify_background_range_polygons(
                terra_polygon
            )
            if (length(backgr_poly_id) > 1L) {
                warning("More than one background poly found.")
            }
        }

        if (length(backgr_poly_id) > 0) {
            vmsg(.v = verbose, sprintf(
                "removed background poly.\n ID was: %s",
                backgr_poly_id
            ))

            terra_polygon <- terra::subset(
                x = terra_polygon,
                terra_polygon[["poly_ID"]] != backgr_poly_id
            )
        }
    }


    ## apply custom poly_IDs ##
    if (!is.null(poly_IDs)) {
        # first sort the polys by ID to ensure that custom poly_IDs are
        # applied in a meaningful manner
        terra_polygon <- terra_polygon[order(terra_polygon$poly_ID)]

        if (isTRUE(remove_unvalid_polygons)) {
            poly_IDs <- poly_IDs[valid_index]
        }

        if (length(poly_IDs) != nrow(terra::values(terra_polygon))) {
            stop("length cell_IDs does not equal number of found polygons \n")
        }
        terra_polygon$poly_ID <- as.character(poly_IDs)
    }


    g_polygon <- create_giotto_polygon_object(
        name = name,
        spatVector = terra_polygon,
        spatVectorCentroids = NULL
    )

    # add centroids
    if (isTRUE(calc_centroids)) {
        g_polygon <- .calculate_centroids_polygons(
            gpolygon = g_polygon,
            name = "centroids",
            append_gpolygon = TRUE
        )
    }

    return(g_polygon)
}







#' @title Create giotto polygons from dataframe
#' @rdname createGiottoPolygon
#' @param segmdfr data.frame-like object with polygon coordinate
#' information (x, y, poly_ID) with x and y being vertex information for the
#' polygon referenced by poly_ID. See details for how columns are selected for
#' coordinate and ID information.
#' @param name name for the \code{giottoPolygon} object
#' @param calc_centroids (default FALSE) calculate centroids for polygons
#' @param skip_eval_dfr (default FALSE) skip evaluation of provided dataframe
#' @param copy_dt (default TRUE) if segmdfr is provided as dt, this determines
#' whether a copy is made
#' @param verbose be verbose
#' @details When determining which column within tabular data is intended to
#' provide polygon information, Giotto first checks the column names for 'x',
#' 'y', and 'poly_ID'. If any of these are discovered, they are directly
#' selected. If this is not discovered then Giotto checks the data type of the
#' columns and selects the first `'character'` type column to be 'poly_ID' and
#' the first two `'numeric'` columns as 'x' and 'y' respectively. If this is
#' also unsuccessful then poly_ID defaults to the 3rd column. 'x' and 'y' then
#' default to the 1st and 2nd columns.
#' @concept polygon
#' @export
createGiottoPolygonsFromDfr <- function(
        segmdfr,
        name = "cell",
        calc_centroids = FALSE,
        verbose = TRUE,
        skip_eval_dfr = FALSE,
        copy_dt = TRUE) {
    eval_list <- .evaluate_spatial_info(
        spatial_info = segmdfr,
        skip_eval_dfr = skip_eval_dfr,
        copy_dt = copy_dt,
        verbose = verbose
    )

    spatvector <- eval_list$spatvector
    unique_IDs <- eval_list$unique_IDs

    g_polygon <- create_giotto_polygon_object(
        name = name,
        spatVector = spatvector,
        spatVectorCentroids = NULL,
        unique_IDs = NULL
    )

    # add centroids
    if (calc_centroids == TRUE) {
        g_polygon <- .calculate_centroids_polygons(
            gpolygon = g_polygon,
            name = "centroids",
            append_gpolygon = TRUE
        )
    }

    return(g_polygon)
}







#' @title Create giotto polygons from GeoJSON
#' @rdname createGiottoPolygon
#' @param GeoJSON path to .GeoJSON file
#' @param name name for the \code{giottoPolygon} object created
#' @param calc_centroids (default FALSE) calculate centroids for polygons
#' @param verbose be verbose
#' @concept polygon
#' @export
createGiottoPolygonsFromGeoJSON <- function(
        GeoJSON,
        name = "cell",
        calc_centroids = FALSE,
        verbose = TRUE) {
    eval_list <- .evaluate_spatial_info(
        spatial_info = GeoJSON,
        verbose = verbose
    )

    spatvector <- eval_list$spatvector
    unique_IDs <- eval_list$unique_IDs

    g_polygon <- create_giotto_polygon_object(
        name = name,
        spatVector = spatvector,
        spatVectorCentroids = NULL,
        unique_IDs = NULL
    )

    # add centroids
    if (isTRUE(calc_centroids)) {
        g_polygon <- .calculate_centroids_polygons(
            gpolygon = g_polygon,
            name = "centroids",
            append_gpolygon = TRUE
        )
    }

    return(g_polygon)
}








#' @title Create a giotto polygon object
#' @name create_giotto_polygon_object
#' @param name name of polygon object
#' @param spatVector SpatVector of polygons
#' @param spatVectorCentroids (optional) SpatVector of polygon centroids
#' @param overlaps (optional) feature overlaps of polygons
#' @param unique_IDs unique polygon IDs for cacheing
#' @keywords internal
#' @returns giotto_polygon_object
create_giotto_polygon_object <- function(
        name = "cell",
        spatVector = NULL,
        spatVectorCentroids = NULL,
        overlaps = NULL,
        unique_IDs = NULL) {
    # create minimum giotto
    g_polygon <- giottoPolygon(
        name = name,
        spatVector = NULL,
        spatVectorCentroids = NULL,
        overlaps = NULL
    )

    ## 1. check spatVector object
    if (!methods::is(spatVector, "SpatVector")) {
        stop("spatVector needs to be a SpatVector object from the terra
            package")
    }

    terra::crs(spatVector) <- NULL
    g_polygon@spatVector <- spatVector


    ## 2. centroids need to be of similar length as polygons
    if (!is.null(spatVectorCentroids)) {
        if (!methods::is(spatVectorCentroids, "SpatVector")) {
            stop("spatVectorCentroids needs to be a spatVector object from the
                terra package")
        }

        l_centroids <- nrow(terra::values(spatVectorCentroids))
        l_polygons <- nrow(terra::values(spatVector))

        if (l_centroids == l_polygons) {
            g_polygon@spatVectorCentroids <- spatVectorCentroids
        } else {
            stop("number of centroids does not equal number of polygons")
        }
    }

    ## 3. overlaps info
    g_polygon@overlaps <- overlaps

    ## 4. spat_ID cacheing
    if (is.null(unique_IDs)) {
        g_polygon@unique_ID_cache <- spatIDs(g_polygon)
    } else {
        g_polygon@unique_ID_cache <- unique_IDs
    }

    # provide name
    g_polygon@name <- name

    # giotto polygon object
    return(g_polygon)
}







# giottoImage creation ####


#' @title createGiottoImage
#' @name createGiottoImage
#' @description Creates a giotto image that can be added to a Giotto object
#' and/or used to add an image to the spatial plotting functions
#' @inheritParams data_access_params
#' @param spatial_locs spatial locations (alternative if \code{gobject = NULL})
#' @param spat_loc_name name of spatial locations within gobject
#' @param mg_object magick image object
#' @param name name for the image
#' @param image_transformations vector of sequential image transformations
#' @param negative_y Map image to negative y spatial values if TRUE during
#' automatic alignment. Meaning that origin is in upper left instead of lower
#' left.
#' @param do_manual_adj (default = FALSE) flag to use manual adj values instead
#' of automatic alignment when given a gobject or spatlocs
#' @param xmax_adj,xmin_adj,ymax_adj,ymin_adj adjustment of the maximum or
#' maximum x or y-value to align the image
#' @param scale_factor scaling of image dimensions relative to spatial
#' coordinates
#' @param x_shift,y_shift shift image along x or y axes
#' @param scale_x,scale_y independently scale image in x or y direction
#' @param order perform scaling or adjustments and shifts first
#' @param xmin_set,xmax_set,ymin_set,ymax_set values to override image minmax
#' spatial anchors when doing adjustments
#' @param verbose be verbose
#' @details image_transformations: transformation options from magick library
#' \[\strong{flip_x_axis}\] flip x-axis (\code{\link[magick]{image_flop}})
#' \[\strong{flip_y_axis}\] flip y-axis (\code{\link[magick]{image_flip}})
#' Example: image_transformations = c(flip_x_axis, flip_y_axis); first flip
#' x-axis and then y-axis
#' @returns a giottoImage object
#' @examples
#' image_test <- system.file("extdata/toy_intensity.tif",
#'     package = "GiottoClass"
#' )
#'
#' createGiottoImage(mg_object = image_test)
#' @export
createGiottoImage <- function(
        gobject = NULL,
        spat_unit = NULL,
        spatial_locs = NULL,
        spat_loc_name = NULL,
        mg_object,
        name = "image",
        image_transformations = NULL,
        negative_y = TRUE,
        do_manual_adj = FALSE,
        xmax_adj = 0,
        xmin_adj = 0,
        ymax_adj = 0,
        ymin_adj = 0,
        scale_factor = 1,
        x_shift = NULL,
        y_shift = NULL,
        scale_x = NULL,
        scale_y = NULL,
        order = c("first_scale", "first_adj"),
        xmin_set = NULL,
        xmax_set = NULL,
        ymin_set = NULL,
        ymax_set = NULL,
        verbose = TRUE) {
    # Check params
    order <- match.arg(order, choices = c("first_scale", "first_adj"))
    scale_factor <- c(x = scale_factor, y = scale_factor)

    # create minimum giotto
    g_image <- new("giottoImage",
        name = name,
        mg_object = NULL,
        minmax = NULL,
        boundaries = NULL,
        scale_factor = NULL,
        resolution = NULL,
        file_path = NULL,
        OS_platform = .Platform[["OS.type"]]
    )


    ## 1.a. check magick image object
    if (!inherits(mg_object, "magick-image")) {
        if (file.exists(mg_object)) {
            g_image@file_path <- mg_object
            mg_object <- try(magick::image_read(mg_object))
            if (inherits(mg_object, "try-error")) {
                stop(mg_object, " can not be read by magick::image_read() \n")
            }
        } else {
            stop("mg_object needs to be an image object 'magick-image' from the
                magick package or \n an existing path that can be read by
                magick::image_read()")
        }
    }

    ## 1.b. check colorspace
    info <- magick::image_info(mg_object)
    mg_colorspace <- info$colorspace
    if (mg_colorspace == "Gray") {
        mg_object <- magick::image_convert(mg_object, colorspace = "rgb")
    }

    ## 1.c. perform transformations if found
    if (!is.null(image_transformations)) {
        for (transf in image_transformations) {
            if (transf == "flip_x_axis") {
                mg_object <- magick::image_flop(mg_object)
            } else if (transf == "flip_y_axis") {
                mg_object <- magick::image_flop(mg_object)
            } else {
                wrap_msg(transf, " is not a supported transformation, see
                    details")
            }
        }
    }

    g_image@mg_object <- mg_object

    ## 2. spatial minmax and adjustments -- manual OR by image dimensions (auto)
    if (verbose == TRUE) {
        if (do_manual_adj == TRUE) {
            wrap_msg(
                "do_manual_adj == TRUE \n",
                "Boundaries will be adjusted by given values."
            )
        }
    }
    # If spatlocs or gobject supplied, minmax values will always be generated
    # If do_manual_adj == TRUE, bypass followup automatic boundary value
    # generation
    if (!is.null(gobject)) {
        # Get spatial locations (or automatically take first available)
        spatlocs <- get_spatial_locations(
            gobject = gobject,
            spat_unit = spat_unit,
            spat_loc_name = spat_loc_name,
            copy_obj = FALSE,
            output = "data.table"
        )


        # Find g_image minmax (spatial) from spatial_locs in gobject
        my_xmin <- min(spatlocs$sdimx)
        my_xmax <- max(spatlocs$sdimx)
        my_ymin <- min(spatlocs$sdimy)
        my_ymax <- max(spatlocs$sdimy)

        if (do_manual_adj == FALSE) {
            # find automatic adjustment values
            img_minmax <- get_img_minmax(
                mg_img = mg_object,
                negative_y = negative_y
            )
            adj_values <- get_adj_rescale_img(
                img_minmax = img_minmax,
                spatial_locs = spatlocs,
                scale_factor = scale_factor
            )
            # Automatic g_image@boundaries values
            xmax_adj <- as.numeric(adj_values[["xmax_adj_orig"]])
            xmin_adj <- as.numeric(adj_values[["xmin_adj_orig"]])
            ymax_adj <- as.numeric(adj_values[["ymax_adj_orig"]])
            ymin_adj <- as.numeric(adj_values[["ymin_adj_orig"]])
        }
    } else if (!is.null(spatial_locs)) {
        spatlocs <- spatial_locs
        if (!all(c("sdimx", "sdimy") %in% colnames(spatlocs))) {
            stop("spatial_locs needs to be data.frame-like object with a
                sdimx and sdimy column")
        }
        # Find g_image minmax (spatial) from spatial_locs argument
        my_xmin <- min(spatlocs$sdimx)
        my_xmax <- max(spatlocs$sdimx)
        my_ymin <- min(spatlocs$sdimy)
        my_ymax <- max(spatlocs$sdimy)

        if (do_manual_adj == FALSE) {
            # find auto adjustment values
            img_minmax <- get_img_minmax(
                mg_img = mg_object,
                negative_y = negative_y
            )
            adj_values <- get_adj_rescale_img(
                img_minmax = img_minmax,
                spatial_locs = spatlocs,
                scale_factor = scale_factor
            )
            # Automatic g_image@boundaries values
            xmax_adj <- as.numeric(adj_values[["xmax_adj_orig"]])
            xmin_adj <- as.numeric(adj_values[["xmin_adj_orig"]])
            ymax_adj <- as.numeric(adj_values[["ymax_adj_orig"]])
            ymin_adj <- as.numeric(adj_values[["ymin_adj_orig"]])
        }
    } else {
        if (verbose == TRUE) {
            warning(
                "gobject or spatial locations are not provided \n",
                "Arbitrary values will be given \n"
            )
        }
        # Default g_image@minmax values if no spatial_locs provided
        my_xmin <- 0
        my_xmax <- 10
        my_ymin <- 0
        my_ymax <- 10
    }

    # Set minmax and boundary values for return
    g_image@minmax <- c(
        "xmax_sloc" = my_xmax,
        "xmin_sloc" = my_xmin,
        "ymax_sloc" = my_ymax,
        "ymin_sloc" = my_ymin
    )

    ## if do_manual == TRUE, boundary values are those given or defaulted as
    ## arguments
    ## if do_manual == FALSE, boundary values are taken from automatic
    ## processes above
    g_image@boundaries <- c(
        "xmax_adj" = xmax_adj,
        "xmin_adj" = xmin_adj,
        "ymax_adj" = ymax_adj,
        "ymin_adj" = ymin_adj
    )

    # scale factor and resolution values for return
    g_image@scale_factor <- scale_factor
    g_image@resolution <- 1 / scale_factor

    # Apply any additional manual adjustments through updateGiottoImage
    if (do_manual_adj == TRUE) {
        if (length(c(
            x_shift,
            y_shift,
            scale_x,
            scale_y,
            xmin_set,
            xmax_set,
            ymin_set,
            ymax_set
        )) > 0) {
            g_image <- updateGiottoImageMG(
                giottoImage = g_image,
                return_gobject = FALSE,
                xmax_adj = xmax_adj,
                xmin_adj = xmin_adj,
                ymax_adj = ymax_adj,
                ymin_adj = ymin_adj,
                x_shift = x_shift,
                y_shift = y_shift,
                scale_factor = scale_factor,
                order = order,
                xmin_set = xmin_set,
                xmax_set = xmax_set,
                ymin_set = ymin_set,
                ymax_set = ymax_set,
                verbose = FALSE
            )
        }
    }

    # image object
    return(g_image)
}



# giottoLargeImage creation ####


#' @title createGiottoLargeImage
#' @name createGiottoLargeImage
#' @description Creates a large giotto image that can be added to a Giotto
#' subcellular object. Generates deep copy of SpatRaster
#' @param raster_object filepath to an image, a terra `SpatRaster` or, other format
#' openable via [terra::rast()]
#' @param name name for the image
#' @param negative_y Map image to negative y spatial values if TRUE. Meaning
#' that origin is in upper left instead of lower left.
#' @param extent SpatExtent object to assign spatial extent. Takes priority
#' unless use_rast_ext is TRUE.
#' @param use_rast_ext Use extent from input raster object
#' @param image_transformations vector of sequential image
#' transformations - under construction
#' @param flip_vertical flip raster in a vertical manner
#' @param flip_horizontal flip raster in a horizontal manner
#' @param xmax_bound,xmin_bound,ymax_bound,ymin_bound assign min and max x and y
#' values for image spatial placement
#' @param scale_factor scaling of image dimensions relative to spatial
#' coordinates
#' @param verbose be verbose
#' @returns a giottoLargeImage object
#' @examples
#' image_test <- system.file("extdata/toy_intensity.tif",
#'     package = "GiottoClass"
#' )
#'
#' createGiottoLargeImage(raster_object = image_test)
#' @export
createGiottoLargeImage <- function(
        raster_object,
        name = "image",
        negative_y = TRUE,
        extent = NULL,
        use_rast_ext = FALSE,
        image_transformations = NULL,
        flip_vertical = FALSE,
        flip_horizontal = FALSE,
        xmax_bound = NULL,
        xmin_bound = NULL,
        ymax_bound = NULL,
        ymin_bound = NULL,
        scale_factor = 1,
        verbose = TRUE) {
    # create minimum giotto
    g_imageL <- new("giottoLargeImage", name = name)

    ## 1. check raster object and load as SpatRaster if necessary
    if (inherits(raster_object, "SpatRaster")) {
        # Prevent updates to original raster object input
        raster_object <- terra::deepcopy(raster_object)
    } else if (is.character(raster_object)) {
        checkmate::assert_file_exists(raster_object)
        g_imageL@file_path <- raster_object
        raster_object <- .create_terra_spatraster(raster_object)
    } else {
        # assume class readable by terra rast
        raster_object <- .create_terra_spatraster(raster_object)
    }


    ## 2. image bound spatial extent
    if (use_rast_ext) {
        extent <- terra::ext(raster_object)
        vmsg(.v = verbose, "use_rast_ext == TRUE
        extent from input raster_object will be used.")
    }

    # By extent object (priority)
    if (!is.null(extent)) {
        if (inherits(extent, "SpatExtent")) {
            terra::ext(raster_object) <- extent
        } else {
            stop("extent argument only accepts terra SpatExtent objects")
        }
    } else { # OR by manual OR by image dimensions (auto)

        # Check if manual adj values were given
        # Assign default values for any that were not manually given
        if (all(
            is.null(xmax_bound),
            is.null(xmin_bound),
            is.null(ymax_bound),
            is.null(ymin_bound)
        )) {
            im_dim <- dim(raster_object)[2:1]

            # Apply scale_factor
            im_dim <- im_dim * scale_factor

            # Automatic extent values
            xmax_bound <- im_dim[1]
            xmin_bound <- 0
            if (negative_y == TRUE) {
                ymax_bound <- 0
                ymin_bound <- -im_dim[2]
            } else if (negative_y == FALSE) {
                ymax_bound <- im_dim[2]
                ymin_bound <- 0
            }
        } else {
            # Manual extent values
            if (is.null(xmax_bound) == TRUE) xmax_bound <- 1
            if (is.null(xmin_bound) == TRUE) xmin_bound <- 0
            if (negative_y == TRUE) {
                if (is.null(ymax_bound) == TRUE) ymax_bound <- 0
                if (is.null(ymin_bound) == TRUE) ymin_bound <- -1
            } else if (negative_y == FALSE) {
                if (is.null(ymax_bound) == TRUE) ymax_bound <- 1
                if (is.null(ymin_bound) == TRUE) ymin_bound <- 0
            }
        }
        terra::ext(raster_object) <- c(
            xmin_bound, xmax_bound, ymin_bound, ymax_bound
        )
    }


    ## transformations

    ## flip axes ##
    if (flip_vertical == TRUE) {
        raster_object <- terra::flip(raster_object, direction = "vertical")
    }

    if (flip_horizontal == TRUE) {
        raster_object <- terra::flip(raster_object, direction = "horizontal")
    }

    ## 3. Assign raster_object to giottoLargeImage
    g_imageL@raster_object <- raster_object

    ## 4. return image object
    return(initialize(g_imageL))
}






#' @title createGiottoLargeImageList
#' @name createGiottoLargeImageList
#' @description Creates a list of large giotto images that can be added to a
#' Giotto object. Generates deep copy of SpatRaster
#' @param raster_objects vector of image paths or terra SpatRaster image objects
#' @param names vector of names for the images
#' @param negative_y Map image to negative y spatial values if TRUE. Meaning
#' that origin is in upper left instead of lower left.
#' @param extent SpatExtent object to assign spatial extent. Takes priority
#' unless use_rast_ext is TRUE.
#' @param use_rast_ext Use extent from input raster object
#' @param image_transformations vector of sequential image
#' transformations - under construction
#' @param flip_vertical flip raster in a vertical manner
#' @param flip_horizontal flip raster in a horizontal manner
#' @param xmax_bound,xmin_bound,ymax_bound,ymin_bound assign min and max x and y
#'   values for image spatial placement
#' @param scale_factor scaling of image dimensions relative to spatial
#' coordinates
#' @param verbose be verbose
#' @details See \code{\link{createGiottoLargeImage}}
#' @returns a list with giottoLargeImage objects
#' @examples
#' image_test <- system.file("extdata/toy_intensity.tif",
#'     package = "GiottoClass"
#' )
#'
#' createGiottoLargeImageList(raster_objects = image_test)
#' @export
createGiottoLargeImageList <- function(
        raster_objects,
        names = "image",
        negative_y = TRUE,
        extent = NULL,
        use_rast_ext = FALSE,
        image_transformations = NULL,
        flip_vertical = FALSE,
        flip_horizontal = FALSE,
        xmax_bound = NULL,
        xmin_bound = NULL,
        ymax_bound = NULL,
        ymin_bound = NULL,
        scale_factor = 1,
        verbose = TRUE) {
    l_images <- length(raster_objects)
    l_image_names <- length(unique(names))

    if (l_image_names != l_image_names) {
        stop("length of raster_objects and unique names must be the same")
    }

    result_list <- list()

    for (i in seq_len(l_images)) {
        image_res <- createGiottoLargeImage(
            raster_object = raster_objects[[i]],
            name = names[[i]],
            negative_y = negative_y,
            extent = extent,
            use_rast_ext = use_rast_ext,
            image_transformations = image_transformations,
            flip_vertical = flip_vertical,
            flip_horizontal = flip_horizontal,
            xmax_bound = xmax_bound,
            xmin_bound = xmin_bound,
            ymax_bound = ymax_bound,
            ymin_bound = ymin_bound,
            scale_factor = scale_factor,
            verbose = verbose
        )

        result_list[[i]] <- image_res
    }

    return(result_list)
}






# Possibly to be implemented ####
# icfObject
