## Show functions ####

#' @title showGiottoExpression
#' @name showGiottoExpression
#' @description shows the available matrices
#' @param gobject giotto object
#' @param nrows number of rows to print for each
#' matrix (ignored for sparse matrices)
#' @param ncols number of columns to print for each
#' matrix (ignored for sparse matrices)
#' @returns prints the name and small subset of available matrices
#' @family functions to show data in giotto object
#' @keywords show
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' showGiottoExpression(g)
#' @export
showGiottoExpression <- function(gobject, nrows = 4, ncols = 4) {
    # import print styling
    ch <- box_chars()
    ct <- color_tag()

    # 1. check inputs
    if (is.null(gobject)) stop("A giotto object needs to be provided \n")

    # 2. get availability matrix
    available_data <- list_expression(gobject = gobject)
    if (is.null(available_data)) {
        cat("No expression data available \n")
    } else {
        # 3.1 set up object printouts
        objPrints <- list()
        for (obj_i in seq(nrow(available_data))) {
            # get object
            dataObj <- get_expression_values(
                gobject = gobject,
                values = available_data$name[[obj_i]],
                spat_unit = available_data$spat_unit[[obj_i]],
                feat_type = available_data$feat_type[[obj_i]],
                output = "exprObj"
            )

            # collect object prints
            if (inherits(dataObj[], "sparseMatrix")) {
                objPrints[[obj_i]] <- capture.output(show(dataObj))
            } else if (inherits(
                dataObj[], c("denseMatrix", "matrix", "data.frame")
            )) {
                objPrints[[obj_i]] <- capture.output(
                    .abbrev_mat(dataObj, nrows, ncols)
                )
            } else {
                # directly print slot (catch)
                objPrints[[obj_i]] <- capture.output(
                    show(slot(dataObj, "exprMat"))
                )
            }
        }

        # object printblock edits
        objPrints <- lapply(
            objPrints,
            function(x) paste0(ch$s, x)
        ) # add indent
        objPrints <- lapply(
            objPrints,
            function(x) paste(x, collapse = ("\n"))
        )
        # linearize print

        # append to availability table
        available_data$values <- unlist(objPrints)

        # 3.2 setup general prints
        if (isTRUE(use_color_text())) {
            available_data$spat_unit <- paste0(
                'Spatial unit "', ct$b, available_data$spat_unit, ct$x, '"'
            )
            available_data$feat_type <- paste0(
                'Feature type "', ct$r, available_data$feat_type, ct$x, '"'
            )
            available_data$name <- paste0(
                'Expression data "', ct$t, available_data$name, ct$x,
                '" values:'
            )
        } else {
            available_data$spat_unit <- paste0(
                'Spatial unit "', available_data$spat_unit, '"'
            )
            available_data$feat_type <- paste0(
                'Feature type "', available_data$feat_type, '"'
            )
            available_data$name <- paste0(
                'Expression data "', available_data$name, '" values:'
            )
        }


        # 4. print information
        .print_leaf(
            level_index = 1,
            availableDT = available_data,
            inherit_last = TRUE,
            indent = ""
        )
    }
}



#' @title showGiottoCellMetadata
#' @name showGiottoCellMetadata
#' @description shows the available cell metadata
#' @param gobject giotto object
#' @param nrows number of rows to print for each metadata
#' @returns prints the name and small subset of available metadata
#' @family functions to show data in giotto object
#' @keywords show
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' showGiottoCellMetadata(g)
#' @export
showGiottoCellMetadata <- function(gobject,
    nrows = 4) {
    # import print styling
    ch <- box_chars()
    ct <- color_tag()

    # 1. check inputs
    if (is.null(gobject)) stop("A giotto object needs to be provided \n")

    # 2. get availability matrix
    available_data <- list_cell_metadata(gobject = gobject)
    if (is.null(available_data)) {
        cat("No cell metadata available \n")
    } else {
        # 3.1 set up object printouts
        objPrints <- objRows <- list()
        for (obj_i in seq(nrow(available_data))) {
            # get object
            dataObj <- getCellMetadata(
                gobject = gobject,
                spat_unit = available_data$spat_unit[[obj_i]],
                feat_type = available_data$feat_type[[obj_i]],
                output = "cellMetaObj",
                copy_obj = TRUE
            )

            # collect object prints
            objRows[[obj_i]] <- nrow(dataObj[])

            objPrints[[obj_i]] <-
                dataObj[seq_len(if (nrows <= objRows[[obj_i]]) {
                    nrows
                } else {
                    objRows[[obj_i]]
                }), ] %>%
                print() %>%
                capture.output()
        }

        # object printblock edits
        objPrints <- lapply(objPrints, function(x) paste0(ch$s, x)) # add indent
        objPrints <- lapply(objPrints, function(x) paste(x, collapse = ("\n")))
        # linearize print

        # append to availability table
        available_data$values <- unlist(objPrints)

        # 3.2 setup general prints
        if (isTRUE(use_color_text())) {
            available_data$spat_unit <- paste0(
                'Spatial unit "', ct$b, available_data$spat_unit, ct$x, '"'
            )
            available_data$feat_type <- paste0(
                'Feature type "', ct$r, available_data$feat_type, ct$x, '"'
            )
        } else {
            available_data$spat_unit <- paste0(
                'Spatial unit "', available_data$spat_unit, '"'
            )
            available_data$feat_type <- paste0(
                'Feature type "', available_data$feat_type, '"'
            )
        }


        # 4. print information
        .print_leaf(
            level_index = 1,
            availableDT = available_data,
            inherit_last = TRUE,
            indent = ""
        )
    }
}


#' @title showGiottoFeatMetadata
#' @name showGiottoFeatMetadata
#' @description shows the available feature metadata
#' @param gobject giotto object
#' @param nrows number of rows to print for each metadata
#' @returns prints the name and small subset of available metadata
#' @family functions to show data in giotto object
#' @keywords show
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' showGiottoFeatMetadata(g)
#' @export
showGiottoFeatMetadata <- function(gobject,
    nrows = 4) {
    # import print styling
    ch <- box_chars()
    ct <- color_tag()

    # 1. check inputs
    if (is.null(gobject)) stop("A giotto object needs to be provided \n")

    # 2. get availability matrix
    available_data <- list_feat_metadata(gobject = gobject)
    if (is.null(available_data)) {
        cat("No feature metadata available \n")
    } else {
        # 3.1 set up object printouts
        objPrints <- objRows <- list()
        for (obj_i in seq(nrow(available_data))) {
            # get object
            dataObj <- getFeatureMetadata(
                gobject = gobject,
                spat_unit = available_data$spat_unit[[obj_i]],
                feat_type = available_data$feat_type[[obj_i]],
                output = "featMetaObj",
                copy_obj = TRUE
            )

            # collect object prints
            objRows[[obj_i]] <- nrow(dataObj[])

            objPrints[[obj_i]] <-
                dataObj[seq_len(if (nrows <= objRows[[obj_i]]) {
                    nrows
                } else {
                    objRows[[obj_i]]
                }), ] %>%
                print() %>%
                capture.output()
        }

        # object printblock edits
        objPrints <- lapply(objPrints, function(x) paste0(ch$s, x)) # add indent
        objPrints <- lapply(objPrints, function(x) paste(x, collapse = ("\n")))
        # linearize print

        # append to availability table
        available_data$values <- unlist(objPrints)

        # 3.2 setup general prints
        if (isTRUE(use_color_text())) {
            available_data$spat_unit <- paste0(
                'Spatial unit "', ct$b, available_data$spat_unit, ct$x, '"'
            )
            available_data$feat_type <- paste0(
                'Feature type "', ct$r, available_data$feat_type, ct$x, '"'
            )
        } else {
            available_data$spat_unit <- paste0(
                'Spatial unit "', available_data$spat_unit, '"'
            )
            available_data$feat_type <- paste0(
                'Feature type "', available_data$feat_type, '"'
            )
        }


        # 4. print information
        .print_leaf(
            level_index = 1,
            availableDT = available_data,
            inherit_last = TRUE,
            indent = ""
        )
    }
}



#' @title showGiottoSpatLocs
#' @name showGiottoSpatLocs
#' @description shows the available spatial locations
#' @param gobject giotto object
#' @param nrows number of rows to print for each spatial location data.table
#' @returns prints the name and small subset of available data.table
#' @family functions to show data in giotto object
#' @keywords show
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' showGiottoSpatLocs(g)
#' @export
showGiottoSpatLocs <- function(gobject,
    nrows = 4) {
    # import print styling
    ch <- box_chars()
    ct <- color_tag()

    # 1. check inputs
    if (is.null(gobject)) stop("A giotto object needs to be provided \n")

    # 2. get availability matrix
    available_data <- list_spatial_locations(gobject = gobject)
    if (is.null(available_data)) {
        cat("No spatial locations available \n")
    } else {
        # 3.1 set up object printouts
        objPrints <- objRows <- list()
        for (obj_i in seq(nrow(available_data))) {
            # get object
            dataObj <- get_spatial_locations(
                gobject = gobject,
                spat_unit = available_data$spat_unit[[obj_i]],
                spat_loc_name = available_data$name[[obj_i]],
                output = "spatLocsObj",
                copy_obj = TRUE
            )

            # collect object prints
            objRows[[obj_i]] <- nrow(dataObj[])

            objPrints[[obj_i]] <- capture.output(
                .abbrev_spatlocs(dataObj, nrows)
            )
        }

        # object printblock edits
        objPrints <- lapply(objPrints, function(x) paste0(ch$s, x)) # add indent
        objPrints <- lapply(objPrints, function(x) paste(x, collapse = ("\n")))
        # linearize print

        # append to availability table
        available_data$values <- unlist(objPrints)

        # 3.2 setup general prints
        if (isTRUE(use_color_text())) {
            available_data$spat_unit <- paste0(
                'Spatial unit "', ct$b, available_data$spat_unit, ct$x, '"'
            )
            available_data$name <- paste0(
                'S4 spatLocsObj "', ct$t, available_data$name, ct$x,
                '" coordinates:'
            )
        } else {
            available_data$spat_unit <- paste0(
                'Spatial unit "', available_data$spat_unit, '"'
            )
            available_data$name <- paste0(
                'S4 spatLocsObj "', available_data$name, '" coordinates:'
            )
        }
        for (obj_i in seq(nrow(available_data))) {
            available_data$name[[obj_i]] <- paste0(
                available_data$name[[obj_i]],
                ch$s, "(", objRows[[obj_i]], " rows)"
            )
        }

        # 4. print information
        .print_leaf(
            level_index = 1,
            availableDT = available_data,
            inherit_last = TRUE,
            indent = ""
        )
    }

    # if(is.null(gobject)) stop('A giotto object needs to be provided \n')
    #
    # available_data = list_spatial_locations(gobject = gobject)
    # if(is.null(available_data)) cat('No spatial locations available \n')
    #
    # for(spatial_unit in unique(available_data$spat_unit)) {
    #
    #   cat('Spatial unit: ', spatial_unit, ' \n\n')
    #
    #   for(spatlocname in
    #   available_data[available_data$spat_unit == spatial_unit,]$name) {
    #     if(inherits(gobject@spatial_locs[[spatial_unit]][[spatlocname]],
    #     'data.frame')) {
    #       cat('--> Name: ', spatlocname, ' \n\n')
    #       print(gobject@spatial_locs[[spatial_unit]][[spatlocname
    #       ]][seq_len(nrows),])
    #       cat('\n')
    #     }
    #     if(inherits(gobject@spatial_locs[[spatial_unit]][[spatlocname]],
    #     'spatLocsObj')) {
    #       cat('--> Name: ', spatlocname, ' \n\n')
    #       cat('An object of class spatLocsObj\n')
    #       cat('Provenance: ', slot(gobject@spatial_locs[[spatial_unit
    #       ]][[spatlocname]], 'provenance'), ' \n')
    #       print(slot(gobject@spatial_locs[[spatial_unit]][[spatlocname]],
    #       'coordinates')[seq_len(nrows),])
    #       cat('\n')
    #     }
    #   }
    # }
}


#' @title showGiottoSpatEnrichments
#' @name showGiottoSpatEnrichments
#' @description shows the available spatial enrichment results
#' @param gobject giotto object
#' @param nrows number of rows to print for each spatial enrichment data.table
#' @returns prints the name and small subset of available data.table
#' @family functions to show data in giotto object
#' @keywords show
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' showGiottoSpatEnrichments(g)
#' @export
showGiottoSpatEnrichments <- function(gobject,
    nrows = 4) {
    # NSE vars
    spat_unit <- feat_type <- name <- NULL

    if (is.null(gobject)) stop("A giotto object needs to be provided \n")

    available_data <- list_spatial_enrichments(gobject = gobject)

    if (is.null(available_data)) cat("No spatial enrichments available \n")

    for (spatial_unit in unique(available_data$spat_unit)) {
        cat("Spatial unit: ", spatial_unit, " \n\n")

        for (feature_type in
            available_data[spat_unit == spatial_unit, unique(feat_type)]) {
            cat("--> Feature type: ", feature_type, " \n")

            for (spatenrichname in
                available_data[
                    spat_unit == spatial_unit
                ][
                    feat_type == feature_type, unique(name)
                ]) {
                cat("\n----> Name ", spatenrichname, ": \n")

                print(gobject@spatial_enrichment[[spatial_unit]][[
                    feature_type
                ]][[spatenrichname]][][
                    seq_len(nrows),
                ])
            }
        }
    }
}



#' @title showGiottoDimRed
#' @name showGiottoDimRed
#' @description shows the available dimension reductions
#' @param gobject giotto object
#' @param nrows number of coordinates rows to print
#' @param ncols number of coordinates columns to print
#' @returns prints the name and small subset of available dimension reduction
#' coordinates
#' @family functions to show data in giotto object
#' @keywords show
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' showGiottoDimRed(g)
#' @export
showGiottoDimRed <- function(gobject,
    nrows = 3,
    ncols = 2) {
    # Define for data.table
    data_type <- NULL

    # import print styling
    ch <- box_chars()
    ct <- color_tag()

    # 1. Check inputs
    if (is.null(gobject)) stop("A giotto object needs to be provided \n")

    # 2. Get availability matrix
    available_data <- list_dim_reductions(gobject)
    if (is.null(available_data)) {
        cat("No dimensional reductions available \n")
    } else {
        # 3.1 Set up object printouts
        objPrints <- objRows <- objCols <- list()
        for (obj_i in seq(nrow(available_data))) {
            # Get object
            dataObj <- get_dimReduction(
                gobject = gobject,
                reduction = available_data$data_type[[obj_i]],
                spat_unit = available_data$spat_unit[[obj_i]],
                feat_type = available_data$feat_type[[obj_i]],
                reduction_method = available_data$dim_type[[obj_i]],
                name = available_data$name[[obj_i]],
                output = "data.table"
            )

            # Collect object prints
            objRows[[obj_i]] <- nrow(dataObj)
            objCols[[obj_i]] <- ncol(dataObj)

            objPrints[[obj_i]] <-
                dataObj[
                    seq_len(if (nrows <= objRows[[obj_i]]) {
                        nrows
                    } else {
                        objRows[[obj_i]]
                    }),
                    seq_len(if (ncols <= objCols[[obj_i]]) ncols else objCols[[obj_i]])
                ] %>%
                print() %>%
                capture.output()
        }

        # object printblock edits
        objPrints <- lapply(objPrints, function(x) paste0(ch$s, x)) # Add indent
        objPrints <- lapply(objPrints, function(x) paste(x, collapse = ("\n")))
        # Linearize print

        # Append to availability table
        available_data$values <- unlist(objPrints)

        # 3.2 Setup general prints
        if (isTRUE(use_color_text())) {
            available_data$spat_unit <- paste0(
                'Spatial unit "', ct$b, available_data$spat_unit, ct$x, '"'
            )
            available_data$feat_type <- paste0(
                'Feature type "', ct$r, available_data$feat_type, ct$x, '"'
            )
            available_data$dim_type <- paste0(
                'Dim reduction type "', ct$p, available_data$dim_type, ct$x, '"'
            )
            available_data$name <- paste0(
                'S4 dimObj "', ct$t, available_data$name, ct$x,
                '" coordinates:'
            )
        } else {
            available_data$spat_unit <- paste0(
                'Spatial unit "', available_data$spat_unit, '"'
            )
            available_data$feat_type <- paste0(
                'Feature type "', available_data$feat_type, '"'
            )
            available_data$dim_type <- paste0(
                'Dim reduction type "', available_data$dim_type, '"'
            )
            available_data$name <- paste0(
                'S4 dimObj "', available_data$name, '" coordinates:'
            )
        }
        for (obj_i in seq(nrow(available_data))) {
            available_data$name[[obj_i]] <- paste0(
                available_data$name[[obj_i]],
                ch$s, "(", objRows[[obj_i]], " rows ",
                objCols[[obj_i]], " cols)"
            )
        }

        # 4. Print information
        for (data_type_red in unique(available_data$data_type)) {
            data_type_subset <- available_data$data_type == data_type_red

            if (isTRUE(use_color_text())) {
                if (data_type_red == "feats") {
                    cat(paste0(
                        "Dim reduction on ", ct$y, "features", ct$x, ":"
                    ))
                }
                if (data_type_red == "cells") {
                    cat(paste0(
                        "Dim reduction on ", ct$y, "cells", ct$x, ":"
                    ))
                }
            } else {
                if (data_type_red == "feats") {
                    cat(paste0(
                        "Dim reduction on ", "features", ":"
                    ))
                }
                if (data_type_red == "cells") {
                    cat(paste0(
                        "Dim reduction on ", "cells", ":"
                    ))
                }
            }

            cat(
                "\n",
                "-------------------------",
                "\n\n.\n"
            )

            .print_leaf(
                level_index = 2, # skip over dim reduction layer
                availableDT = available_data[data_type == data_type_red],
                inherit_last = TRUE,
                indent = ""
            )
        }
    }
}




#' @title showGiottoNearestNetworks
#' @name showGiottoNearestNetworks
#' @description shows the available nearest neighbor networks
#' @param gobject giotto object
#' @param nrows number of network rows to print
#' @returns prints the name and small subset of available nearest neighbor
#' network info
#' @family functions to show data in giotto object
#' @keywords show
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' showGiottoNearestNetworks(g)
#' @export
showGiottoNearestNetworks <- function(gobject,
    nrows = 3) {
    # import print styling
    ch <- box_chars()
    ct <- color_tag()

    # 1. check input
    if (is.null(gobject)) stop("A giotto object needs to be provided \n")

    # 2. get availability matrix
    available_data <- list_nearest_networks(gobject)
    if (is.null(available_data)) {
        cat("No nearest neighbor networks available \n")
    } else {
        # 3.1 Set up object printouts
        objPrints <- objRows <- list()
        for (obj_i in seq(nrow(available_data))) {
            # Get object
            dataObj <- get_NearestNetwork(
                gobject = gobject,
                spat_unit = available_data$spat_unit[[obj_i]],
                feat_type = available_data$feat_type[[obj_i]],
                nn_network_to_use = available_data$nn_type[[obj_i]],
                network_name = available_data$name[[obj_i]],
                output = "data.table"
            )

            # Collect object prints
            objRows[[obj_i]] <- nrow(dataObj)

            objPrints[[obj_i]] <-
                dataObj[seq_len(if (nrows <= objRows[[obj_i]]) {
                    nrows
                } else {
                    objRows[[obj_i]]
                }), ] %>%
                print() %>%
                capture.output()
        }

        # object printblock edits
        objPrints <- lapply(objPrints, function(x) paste0(ch$s, x)) # Add indent
        objPrints <- lapply(objPrints, function(x) paste(x, collapse = ("\n")))
        # Linearize print

        # Append to availability table
        available_data$values <- unlist(objPrints)

        # 3.2 Setup general prints
        if (isTRUE(use_color_text())) {
            available_data$spat_unit <- paste0(
                'Spatial unit "', ct$b, available_data$spat_unit, ct$x, '"'
            )
            if (!is.null(available_data$feat_type)) {
                available_data$feat_type <- paste0(
                    'Feature type "', ct$r, available_data$feat_type, ct$x, '"'
                )
                # Check to be deprecated
            } else {
                warning("Only networks from the deprecated nesting will be
                        shown")
            }
            available_data$nn_type <- paste0(
                'NN network type "', ct$p, available_data$nn_type, ct$x, '"'
            )
            available_data$name <- paste0(
                'S4 nnNetObj "', ct$t, available_data$name, ct$x, '"'
            )
        } else {
            available_data$spat_unit <- paste0(
                'Spatial unit "', available_data$spat_unit, '"'
            )
            if (!is.null(available_data$feat_type)) {
                available_data$feat_type <- paste0(
                    'Feature type "', available_data$feat_type, '"'
                )
                # Check to be deprecated
            } else {
                warning("Only networks from the deprecated nesting will
                        be shown")
            }
            available_data$nn_type <- paste0(
                'NN network type "', available_data$nn_type, '"'
            )
            available_data$name <- paste0(
                'S4 nnNetObj "', available_data$name, '"'
            )
        }
        for (obj_i in seq(nrow(available_data))) {
            available_data$name[[obj_i]] <- paste0(
                available_data$name[[obj_i]],
                ch$s, "(", objRows[[obj_i]], " rows)"
            )
        }

        # 4. Print information
        .print_leaf(
            level_index = 1,
            availableDT = available_data,
            inherit_last = TRUE,
            indent = ""
        )
    }
}




#' @title showGiottoSpatialInfo
#' @name showGiottoSpatialInfo
#' @description show the available giotto spatial polygon information
#' @param gobject giotto object
#' @returns SpatVector
#' @family functions to show data in giotto object
#' @keywords show
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' showGiottoSpatialInfo(g)
#' @export
showGiottoSpatialInfo <- function(gobject) {
    if (is.null(gobject)) stop("A giotto object needs to be provided \n")

    available_data <- list_spatial_info(gobject = gobject)
    if (is.null(available_data)) cat("No spatial info available \n")

    for (info in available_data$spat_info) {
        cat("For Spatial info: ", info, "\n\n")
        print(gobject@spatial_info[[info]])
        cat("-----------------------------")
        cat("\n \n")
    }
}


#' @title showGiottoFeatInfo
#' @name showGiottoFeatInfo
#' @description show the available giotto spatial feature information
#' @param gobject giotto object
#' @family functions to show data in giotto object
#' @keywords show
#' @returns SpatVector
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' showGiottoFeatInfo(g)
#' @export
showGiottoFeatInfo <- function(gobject) {
    if (is.null(gobject)) stop("A giotto object needs to be provided \n")

    available_data <- list_feature_info(gobject = gobject)
    if (is.null(available_data)) cat("No feature info available \n")

    for (info in available_data$feat_info) {
        cat("For Feature info: ", info, "\n\n")
        print(gobject@feat_info[[info]])
        cat("-----------------------------")
        cat("\n \n")
    }
}




#' @title showGiottoSpatNetworks
#' @name showGiottoSpatNetworks
#' @description Prints the available spatial networks that are attached to the
#' Giotto object
#' @param gobject a giotto object
#' @param nrows number of rows to print
#' @returns prints names and small subset of available spatial network info
#' @family functions to show data in giotto object
#' @keywords show
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' showGiottoSpatNetworks(g)
#' @export
showGiottoSpatNetworks <- function(gobject,
    nrows = 4) {
    # import print styling
    ch <- box_chars()
    ct <- color_tag()

    # 1. Check input
    if (is.null(gobject)) stop("A giotto object needs to be provided \n")

    # 2. Get availability matrix
    available_data <- list_spatial_networks(gobject = gobject)
    if (is.null(available_data)) {
        cat("No spatial networks are available \n")
    } else {
        # 3.1 Set up object printouts
        objPrints <- objRows <- list()
        for (obj_i in seq(nrow(available_data))) {
            # Get object
            dataObj <- get_spatialNetwork(
                gobject = gobject,
                spat_unit = available_data$spat_unit[[obj_i]],
                name = available_data$name[[obj_i]],
                output = "networkDT"
            )

            # Collect object prints
            objRows[[obj_i]] <- nrow(dataObj)

            objPrints[[obj_i]] <-
                dataObj[seq_len(if (nrows <= objRows[[obj_i]]) {
                    nrows
                } else {
                    objRows[[obj_i]]
                }), ] %>%
                print() %>%
                capture.output()
        }

        # object printblock edits
        objPrints <- lapply(objPrints, function(x) paste0(ch$s, x)) # Add indent
        objPrints <- lapply(objPrints, function(x) paste(x, collapse = ("\n")))
        # Linearize print

        # Append to availability table
        available_data$values <- unlist(objPrints)

        # 3.2 Setup general prints
        if (isTRUE(use_color_text())) {
            available_data$spat_unit <- paste0(
                'Spatial unit "', ct$b, available_data$spat_unit, ct$x, '"'
            )
            available_data$name <- paste0(
                'S4 spatialNetworkObj "', ct$t, available_data$name, ct$x, '"'
            )
        } else {
            available_data$spat_unit <- paste0(
                'Spatial unit "', available_data$spat_unit, '"'
            )
            available_data$name <- paste0(
                'S4 spatialNetworkObj "', available_data$name, '"'
            )
        }
        for (obj_i in seq(nrow(available_data))) {
            available_data$name[[obj_i]] <- paste0(
                available_data$name[[obj_i]],
                ch$s, "(", objRows[[obj_i]], " rows)"
            )
        }

        # 4. Print information
        .print_leaf(
            level_index = 1,
            availableDT = available_data,
            inherit_last = TRUE,
            indent = ""
        )
    }
}


#' @title Show networks
#' @name showNetworks
#' @inheritDotParams showGiottoSpatNetworks
#' @returns prints names and small subset of available spatial network info
#' @seealso \code{\link{showGiottoSpatNetworks}}
#' @export
showNetworks <- function(...) {
    .Deprecated(new = "showGiottoSpatNetworks")

    showGiottoSpatNetworks(...)
}


#' @title showGiottoSpatGrids
#' @name showGiottoSpatGrids
#' @description Prints the available spatial grids that are attached to the
#' Giotto object
#' @param gobject giotto object
#' @param nrows number of rows to print
#' @returns prints name of available spatial grids
#' @family functions to show data in giotto object
#' @keywords show
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' g <- createSpatialGrid(g, sdimx_stepsize = 5, sdimy_stepsize = 5)
#'
#' showGiottoSpatGrids(g)
#' @export
showGiottoSpatGrids <- function(gobject,
    nrows = 4) {
    # import boxchars
    ch <- box_chars()

    # 1. check input
    if (is.null(gobject)) stop("A giotto object needs to be provided \n")

    # 2. get availability matrix
    available_data <- list_spatial_grids(gobject = gobject)
    if (is.null(available_data)) {
        cat("No available spatial grids \n")
    } else {
        # 3.1 Set up object printouts
        objPrints <- objRows <- list()
        for (obj_i in seq(nrow(available_data))) {
            # Get object
            dataObj <- get_spatialGrid(
                gobject = gobject,
                spat_unit = available_data$spat_unit[[obj_i]],
                name = available_data$name[[obj_i]],
                return_grid_Obj = FALSE
            )

            # Collect object prints
            objRows[[obj_i]] <- nrow(dataObj)

            objPrints[[obj_i]] <-
                dataObj[seq_len(if (nrows <= objRows[[obj_i]]) {
                    nrows
                } else {
                    objRows[[obj_i]]
                }), ] %>%
                print() %>%
                capture.output()
        }

        # object printblock edits
        objPrints <- lapply(
            objPrints,
            function(x) paste0(ch$s, x)
        ) # Add indent
        objPrints <- lapply(
            objPrints,
            function(x) paste(x, collapse = ("\n"))
        )
        # Linearize print

        # Append to availability table
        available_data$values <- unlist(objPrints)

        # 3.2 Setup general prints
        available_data$spat_unit <- paste0(
            'Spatial unit "', available_data$spat_unit, '"'
        )
        if (!is.null(available_data$feat_type)) {
            available_data$feat_type <- paste0(
                'Feature type "', available_data$feat_type, '"'
            )
            # Check to be deprecated
        } else {
            warning("Only networks from the deprecated nesting will be shown")
        }
        available_data$name <- paste0(
            'S4 spatialGridObj "', available_data$name, '"'
        )
        for (obj_i in seq(nrow(available_data))) {
            available_data$name[[obj_i]] <- paste0(
                available_data$name[[obj_i]],
                ch$s, "(", objRows[[obj_i]], " rows)"
            )
        }

        # 4. Print information
        .print_leaf(
            level_index = 1,
            availableDT = available_data,
            inherit_last = TRUE,
            indent = ""
        )
    }
}


#' @title Show Spatial Grids
#' @name showGrids
#' @inheritDotParams showGiottoSpatGrids
#' @returns prints name of available spatial grids
#' @seealso \code{\link{showGiottoSpatGrids}}
#' @export
showGrids <- function(...) {
    .Deprecated(new = "showGiottoSpatGrids")

    showGiottoSpatGrids(...)
}


#' @title showGiottoImageNames
#' @name showGiottoImageNames
#' @description Prints the available giotto images that are attached to the
#' Giotto object
#' @param gobject a giotto object
#' @returns prints names of available giotto image objects
#' @family functions to show data in giotto object
#' @keywords show
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' showGiottoImageNames(g)
#' @export
showGiottoImageNames <- function(gobject) {
    if (is.null(gobject)) stop("A giotto object needs to be provided \n")

    available_data <- list_images(gobject = gobject)
    if (is.null(available_data)) cat("No available images \n")

    for (image_type in unique(available_data$img_type)) {
        cat("Image type:", image_type, "\n\n")

        for (image_name in
            available_data[available_data$img_type == image_type, ]$name) {
            cat("--> Name:", image_name, "\n")
        }
        cat("\n")
    }
}



## internals ####

#' @title Hierarchical tree printing
#' @name .print_leaf
#' @param level_index Which col of availability matrix to start print from
#' @param availableDT availability matrix given as data.table
#' @param inherit_last (boolean) determine behavior from previous level for
#' last level (intended for values print)
#' Only TRUE behavior defined. #TODO
#' @param indent indent characters to print for this level (top level is '')
#' @keywords internal
#' @details Much inspiration taken from https://rdrr.io/cran/fs/src/R/tree.R
#' @returns Hierarchical tree
#' @keywords internal
.print_leaf <- function(level_index,
    availableDT,
    inherit_last = TRUE,
    indent) {
    ch <- box_chars()

    leafs <- unique(unlist(availableDT[, level_index, with = FALSE]))
    # Read unique items for this level into 'leafs'
    for (i in seq_along(leafs)) {
        if (isTRUE(inherit_last) & level_index == ncol(availableDT)) {
            # values layer has no special indent and ends
            writeLines(paste0(indent, capture.output(cat(leafs[[i]]))))
            cat(indent, "\n", sep = "")
        } else {
            if (i == length(leafs)) {
                cat(indent, ch$b, leafs[[i]], "\n", sep = "")
                .print_leaf(
                    level_index = level_index + 1, # increment level_index
                    availableDT = availableDT[as.vector(availableDT[,
                        level_index,
                        with = FALSE
                    ] == leafs[[i]])], # pass subset

                    inherit_last = inherit_last,
                    indent = paste0(indent, "   ")
                )
            } else {
                cat(indent, ch$t, leafs[[i]], "\n", sep = "")
                .print_leaf(
                    level_index = level_index + 1,
                    availableDT = availableDT[as.vector(availableDT[,
                        level_index,
                        with = FALSE
                    ] == leafs[[i]])], # pass subset

                    inherit_last = inherit_last,
                    indent = paste0(indent, paste0(ch$v, "  "))
                )
            }
        }
    }
}



#' @title Print abbreviated matrix
#' @name .abbrev_mat
#' @description print abbreviated matrix exprObj. Works for Matrix pkg denseMatrix,
#' matrix, data.frame and classes that inherit them.
#' @keywords internal
#' @returns abbreviated matrix exprObj
.abbrev_mat <- function(exprObj, nrows, ncols, header = TRUE) {
    mat <- as.matrix(exprObj[])
    four_names <- head(colnames(mat), 4)
    mat_cols <- ncol(mat)
    mat_rows <- nrow(mat)

    # suppress colnames
    mat <- mat[
        seq_len(if (nrows <= mat_rows) nrows else mat_rows),
        seq_len(if (ncols <= mat_cols) ncols else mat_cols)
    ]
    colnames(mat) <- NULL

    # prints
    if (isTRUE(header)) {
        cat("An object of class", class(exprObj), "\n")
        cat(paste0(
            'for spatial unit: "', exprObj@spat_unit,
            '" and feature type: "', exprObj@feat_type, '"\n'
        ))
    }
    cat("  Provenance:", exprObj@provenance)
    if (isTRUE(header)) {
        cat("\n\ncontains:\n")
    } else {
        cat("\n")
    }
    cat(paste0(
        mat_rows, " x ", mat_cols, ' dense matrix of class "',
        class(exprObj[]), '"\n\n'
    ))
    print(mat)
    cat("\n First four colnames:")
    cat("\n", wrap_txt(four_names, strWidth = 40), "\n")
}



#' @title Print abbreviated spatlocs
#' @name .abbrev_spatlocs
#' @description print abbreviated spatLocsObj
#' @keywords internal
#' @returns abbreviated spatLocsObj
.abbrev_spatlocs <- function(spatLocsObj, nrows) {
    # data.table vars
    sdimx <- sdimy <- NULL

    DT_rows <- nrow(spatLocsObj[])
    spatlocs <- spatLocsObj[][seq_len(if (nrows <= DT_rows) nrows else DT_rows), ]

    # prints
    cat("An object of class", class(spatLocsObj), "\n")
    cat("provenance:", slot(spatLocsObj, "provenance"))
    cat("\n    ------------------------\n")
    print(spatlocs)
    cat("\nranges:\n")
    try(
        expr = print(vapply(
            slot(spatLocsObj, "coordinates")[, .(sdimx, sdimy)],
            range,
            FUN.VALUE = numeric(2L)
        )),
        silent = TRUE
    )

    cat("\n\n")
}
