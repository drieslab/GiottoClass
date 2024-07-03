# List functions ####

#' @title list_giotto_data
#' @name list_giotto_data
#' @description list the available data within specified giotto object slot
#' @param gobject giotto object
#' @param slot giotto object slot of
#' interest (e.g. "expression", "spatial_locs", etc.)
#' @param ... additional params to pass
#' @returns names and locations of data within giotto object slot
#' @keywords internal
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' list_giotto_data(gobject = g, slot = "expression")
#' @export
list_giotto_data <- function(gobject = NULL,
    slot = NULL,
    ...) {
    if (slot == "expression") {
        return(list_expression(gobject = gobject, ...))
    }
    if (slot == "cell_metadata") {
        return(list_cell_metadata(gobject = gobject, ...))
    }
    if (slot == "feat_metadata") {
        return(list_feat_metadata(gobject = gobject, ...))
    }
    if (slot == "spatial_locs") {
        return(list_spatial_locations(gobject = gobject, ...))
    }
    if (slot == "spatial_enrichment") {
        return(list_spatial_enrichments(gobject = gobject, ...))
    }
    if (slot == "dimension_reduction") {
        return(list_dim_reductions(gobject = gobject, ...))
    }
    if (slot == "nn_network") {
        return(list_nearest_networks(gobject = gobject, ...))
    }
    if (slot == "spatial_info") {
        return(list_spatial_info(gobject = gobject))
    }
    if (slot == "feat_info") {
        return(list_feature_info(gobject = gobject))
    }
    if (slot == "spatial_network") {
        return(list_spatial_networks(gobject = gobject, ...))
    }
    if (slot == "spatial_grid") {
        return(list_spatial_grids(gobject = gobject, ...))
    }
    if (slot == "images") {
        return(list_images_names(gobject = gobject, ...))
    }
}


#' @title list_expression
#' @name list_expression
#' @description lists the available matrices
#' @inheritParams data_access_params
#' @returns names and locations of available matrices as data.table.
#' col order matters.
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' list_expression(g)
#' @export
list_expression <- function(gobject,
    spat_unit = NULL,
    feat_type = NULL) {
    availableExpr <- data.table()
    for (spatial_unit in names(gobject@expression)) {
        for (feature_type in names(gobject@expression[[spatial_unit]])) {
            for (mat_i in
                names(gobject@expression[[spatial_unit]][[feature_type]])) {
                availableExpr <- rbind(
                    availableExpr,
                    list(
                        spat_unit = spatial_unit,
                        feat_type = feature_type,
                        name = mat_i
                    )
                )
            }
        }
    }

    # check if a specific category is desired
    if (!is.null(spat_unit)) {
        spat_unit_subset <- availableExpr$spat_unit %in%
            spat_unit
    } else {
        spat_unit_subset <- TRUE
    }
    if (!is.null(feat_type)) {
        feat_type_subset <- availableExpr$feat_type %in%
            feat_type
    } else {
        feat_type_subset <- TRUE
    }

    availableExpr <- availableExpr[spat_unit_subset & feat_type_subset, ]

    # return data.table (NULL if empty)
    if (nrow(availableExpr) == 0) {
        return(NULL)
    } else {
        return(availableExpr)
    }
}



#' @title list_expression_names
#' @name list_expression_names
#' @description lists the available matrices names for a given spatial unit
#' and feature type
#' @inheritParams data_access_params
#' @returns vector with names of available matrices
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' list_expression_names(g, spat_unit = "cell", feat_type = "rna")
#' @export
list_expression_names <- function(gobject,
    spat_unit = NULL,
    feat_type = NULL) {
    if (is.null(spat_unit)) stop("spat_unit must be given\n")
    if (is.null(feat_type)) stop("feat_type must be given\n")

    expression_names <- names(gobject@expression[[spat_unit]][[feat_type]])

    return(expression_names)
}



#' @title List cell ID names
#' @name list_cell_id_names
#' @description lists the available cell id names. In effect, these names are
#' the spat_units and poly info in the gobject
#' @inheritParams data_access_params
#' @returns vector with names of available sets of cell_IDs
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' list_cell_id_names(g)
#' @export
list_cell_id_names <- function(gobject) {
    return(names(gobject@cell_ID))
}


#' @title List feat ID names
#' @name list_feat_id_names
#' @description lists the available feat id names In effect, these names are the
#' feat_types and feature info in the gobject
#' @inheritParams data_access_params
#' @returns vector with names of available sets of feat_IDs
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' list_feat_id_names(g)
#' @export
list_feat_id_names <- function(gobject) {
    return(names(gobject@feat_ID))
}


#' @title list_cell_metadata
#' @name list_cell_metadata
#' @description lists the available cell metadata.
#' @inheritParams data_access_params
#' @returns names and locations of available cell metadata as data.table
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' list_cell_metadata(g)
#' @export
list_cell_metadata <- function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    return_uniques = FALSE) {
    availableCMet <- data.table()
    uniques <- list()
    for (spatial_unit in names(gobject@cell_metadata)) {
        uniques$spat_unit <- c(uniques$spat_unit, spatial_unit)
        for (feature_type in names(gobject@cell_metadata[[spatial_unit]])) {
            uniques$feat_type <- c(uniques$feat_type, feature_type)
            availableCMet <- rbind(
                availableCMet,
                list(
                    spat_unit = spatial_unit,
                    feat_type = feature_type
                )
            )
        }
    }

    # check if a specific category is desired
    if (!is.null(spat_unit)) {
        spat_unit_subset <- availableCMet$spat_unit %in%
            spat_unit
    } else {
        spat_unit_subset <- TRUE
    }
    if (!is.null(feat_type)) {
        feat_type_subset <- availableCMet$feat_type %in%
            feat_type
    } else {
        feat_type_subset <- TRUE
    }

    availableCMet <- availableCMet[spat_unit_subset & feat_type_subset, ]

    if (!isTRUE(return_uniques)) {
        # return data.table (NULL if empty)
        if (nrow(availableCMet) == 0) {
            return(NULL)
        } else {
            return(availableCMet)
        }
    } else {
        return(lapply(uniques, unique))
    }
}



#' @title list_feat_metadata
#' @name list_feat_metadata
#' @description lists the available feature metadata
#' @inheritParams data_access_params
#' @returns names and locations of available feature metadata as data.table
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' list_feat_metadata(g)
#' @export
list_feat_metadata <- function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    return_uniques = FALSE) {
    availableFMet <- data.table()
    uniques <- list()
    for (spatial_unit in names(gobject@feat_metadata)) {
        uniques$spat_unit <- c(uniques$spat_unit, spatial_unit)
        for (feature_type in names(gobject@feat_metadata[[spatial_unit]])) {
            uniques$feat_type <- c(uniques$feat_type, feature_type)
            availableFMet <- rbind(
                availableFMet,
                list(
                    spat_unit = spatial_unit,
                    feat_type = feature_type
                )
            )
        }
    }

    # check if a specific category is desired
    if (!is.null(spat_unit)) {
        spat_unit_subset <- availableFMet$spat_unit %in%
            spat_unit
    } else {
        spat_unit_subset <- TRUE
    }
    if (!is.null(feat_type)) {
        feat_type_subset <- availableFMet$feat_type %in%
            feat_type
    } else {
        feat_type_subset <- TRUE
    }

    availableFMet <- availableFMet[spat_unit_subset & feat_type_subset, ]

    if (!isTRUE(return_uniques)) {
        # return data.table (NULL if empty)
        if (nrow(availableFMet) == 0) {
            return(NULL)
        } else {
            return(availableFMet)
        }
    } else {
        return(lapply(uniques, unique))
    }
}



#' @title list_spatial_locations
#' @name list_spatial_locations
#' @description shows the available spatial locations
#' @inheritParams data_access_params
#' @returns names and locations of available data.table as data.table
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' list_spatial_locations(g)
#' @export
list_spatial_locations <- function(gobject,
    spat_unit = NULL,
    return_uniques = FALSE) {
    availableSpatLocs <- data.table()
    uniques <- list()
    for (spatial_unit in names(gobject@spatial_locs)) {
        uniques$spat_unit <- c(uniques$spat_unit, spatial_unit)
        for (spatloc_name in names(gobject@spatial_locs[[spatial_unit]])) {
            uniques$name <- c(uniques$name, spatloc_name)
            if (inherits(
                slot(gobject, "spatial_locs")[[spatial_unit]][[spatloc_name]],
                c("data.table", "spatLocsObj")
            )) {
                availableSpatLocs <- rbind(
                    availableSpatLocs,
                    list(
                        spat_unit = spatial_unit,
                        name = spatloc_name
                    )
                )
            }
        }
    }

    # check if a specific category is desired
    if (!is.null(spat_unit)) {
        spat_unit_subset <- availableSpatLocs$spat_unit %in%
            spat_unit
    } else {
        spat_unit_subset <- TRUE
    }

    availableSpatLocs <- availableSpatLocs[spat_unit_subset, ]

    if (!isTRUE(return_uniques)) {
        if (nrow(availableSpatLocs) == 0) {
            return(NULL)
        } else {
            return(availableSpatLocs)
        }
    } else {
        return(lapply(uniques, unique))
    }
}




#' @title list_spatial_locations_names
#' @name list_spatial_locations_names
#' @description lists the available spatial location names for a given spatial
#' unit
#' @inheritParams data_access_params
#' @returns vector with names of available spatial locations
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' list_spatial_locations_names(g, spat_unit = "cell")
#' @export
list_spatial_locations_names <- function(gobject,
    spat_unit = NULL) {
    if (is.null(spat_unit)) stop("spat_unit must be given\n")

    spatlocs_names <- names(gobject@spatial_locs[[spat_unit]])

    return(spatlocs_names)
}



#' @title list_spatial_enrichments
#' @name list_spatial_enrichments
#' @description return the available spatial enrichment results
#' @inheritParams data_access_params
#' @returns names and locations of available data as data.table
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' list_spatial_enrichments(g)
#' @export
list_spatial_enrichments <- function(gobject,
    spat_unit = NULL,
    feat_type = NULL) {
    availableSpatEnr <- data.table()

    for (spatial_unit in names(gobject@spatial_enrichment)) {
        for (feature_type in
            names(gobject@spatial_enrichment[[spatial_unit]])) {
            for (spatenr_name in
                names(gobject@spatial_enrichment[[
                    spatial_unit
                ]][[feature_type]])) {
                availableSpatEnr <- rbind(
                    availableSpatEnr,
                    list(
                        spat_unit = spatial_unit,
                        feat_type = feature_type,
                        name = spatenr_name
                    )
                )
            }
        }
    }

    # check if a specific category is desired
    if (!is.null(spat_unit)) {
        spat_unit_subset <- availableSpatEnr$spat_unit %in%
            spat_unit
    } else {
        spat_unit_subset <- TRUE
    }
    if (!is.null(feat_type)) {
        feat_type_subset <- availableSpatEnr$feat_type %in%
            feat_type
    } else {
        feat_type_subset <- TRUE
    }

    availableSpatEnr <- availableSpatEnr[spat_unit_subset & feat_type_subset, ]

    if (nrow(availableSpatEnr) == 0) {
        return(NULL)
    } else {
        return(availableSpatEnr)
    }
}





#' @title list_spatial_enrichments_names
#' @name list_spatial_enrichments_names
#' @description returns the available spatial enrichment names for a given
#' spatial unit
#' @inheritParams data_access_params
#' @returns vector of names for available spatial enrichments
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' list_spatial_enrichments_names(g, spat_unit = "aggregate", feat_type = "rna")
#' @export
list_spatial_enrichments_names <- function(gobject,
    spat_unit = NULL,
    feat_type = NULL) {
    if (is.null(spat_unit)) stop("spat_unit must be given\n")
    if (is.null(feat_type)) stop("feat_type must be given\n")

    spatenr_names <- names(gobject@spatial_enrichment[[spat_unit]][[feat_type]])

    return(spatenr_names)
}





#' @title list_dim_reductions
#' @name list_dim_reductions
#' @description return the available dimension reductions
#' @inheritParams data_access_params
#' @param data_type "cells" or "feats" data used in dim reduction
#' @param dim_type dimensional reduction method (e.g. "pca", "umap")
#' @returns names and locations of dimension reduction as a data.table
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' list_dim_reductions(g)
#' @export
list_dim_reductions <- function(gobject,
    data_type = NULL,
    spat_unit = NULL,
    feat_type = NULL,
    dim_type = NULL) {
    availableDimRed <- data.table()
    for (dataType in names(slot(gobject, "dimension_reduction"))) {
        for (spatUnit in
            names(slot(gobject, "dimension_reduction")[[dataType]])) {
            for (featType in names(slot(gobject, "dimension_reduction")[[dataType]][[spatUnit]])) {
                for (dimType in names(slot(gobject, "dimension_reduction")[[dataType]][[spatUnit]][[featType]])) {
                    for (subType in names(slot(gobject, "dimension_reduction")[[dataType]][[spatUnit]][[featType]][[dimType]])) {
                        if (inherits(slot(gobject, "dimension_reduction")[[dataType]][[spatUnit]][[
                            featType
                        ]][[dimType]][[subType]], "dimObj")) {
                            availableDimRed <- rbind(
                                availableDimRed,
                                list(
                                    data_type = dataType,
                                    spat_unit = spatUnit,
                                    feat_type = featType,
                                    dim_type = dimType,
                                    name = subType
                                )
                            )
                        }
                    }
                }
            }
        }
    }

    # check if a specific category is desired
    if (!is.null(data_type)) {
        data_type_subset <- availableDimRed$data_type %in%
            data_type
    } else {
        data_type_subset <- TRUE
    }
    if (!is.null(spat_unit)) {
        spat_unit_subset <- availableDimRed$spat_unit %in%
            spat_unit
    } else {
        spat_unit_subset <- TRUE
    }
    if (!is.null(feat_type)) {
        feat_type_subset <- availableDimRed$feat_type %in%
            feat_type
    } else {
        feat_type_subset <- TRUE
    }
    if (!is.null(dim_type)) {
        dimred_type_subset <- availableDimRed$dim_type %in%
            dim_type
    } else {
        dimred_type_subset <- TRUE
    }

    availableDimRed <- availableDimRed[data_type_subset & spat_unit_subset &
        feat_type_subset & dimred_type_subset, ]

    # NULL if there is no data
    if (nrow(availableDimRed) == 0) {
        return(NULL)
    } else {
        return(availableDimRed)
    }
}



#' @title list_dim_reductions_names
#' @name list_dim_reductions_names
#' @description return the available dimension reductions object names
#' @inheritParams data_access_params
#' @param data_type cells or feats dim reduction
#' @param dim_type dimensional reduction type (method)
#' @returns names of dimension reduction object
#' @details function that can be used to find which names have been used
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' list_dim_reductions_names(g,
#'     spat_unit = "cell", feat_type = "rna",
#'     dim_type = "pca"
#' )
#' @export
list_dim_reductions_names <- function(gobject,
    data_type = "cells",
    spat_unit = NULL,
    feat_type = NULL,
    dim_type = NULL) {
    if (is.null(spat_unit)) stop("spat_unit must be given\n")
    if (is.null(feat_type)) stop("feat_type must be given\n")
    if (is.null(dim_type)) stop("dim_type must be given\n")

    dim_red_object_names <- names(slot(gobject, "dimension_reduction")[[data_type]][[spat_unit]][[feat_type]][[dim_type]])

    return(dim_red_object_names)
}



#' @title list_nearest_networks
#' @name list_nearest_networks
#' @description return the available nearest neighbor network information
#' @inheritParams data_access_params
#' @param nn_type nearest neighbor method (e.g. "sNN", "kNN")
#' @returns names and locations of nearest neighbor networks as a data.table
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' list_nearest_networks(g)
#' @export
list_nearest_networks <- function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    nn_type = NULL,
    return_uniques = FALSE) {
    # TODO remove uniques

    availableNN <- data.table()
    uniques <- list()
    for (spatUnit in names(slot(gobject, "nn_network"))) {
        uniques$spat_unit <- c(uniques$spat_unit, spatUnit)
        for (featType in names(slot(gobject, "nn_network")[[spatUnit]])) {
            uniques$feat_type <- c(uniques$feat_type, featType)
            for (nnType in names(slot(gobject, "nn_network")[[spatUnit]][[featType]])) {
                uniques$nn_type <- c(uniques$nn_type, nnType)
                for (nnNet in names(slot(gobject, "nn_network")[[spatUnit]][[featType]][[nnType]])) {
                    uniques$name <- c(uniques$name, nnNet)
                    if (inherits(
                        slot(gobject, "nn_network")[[spatUnit]][[featType]][[nnType]][[nnNet]],
                        c("igraph", "nnData")
                    )) {
                        availableNN <- rbind(
                            availableNN,
                            list(
                                spat_unit = spatUnit,
                                feat_type = featType,
                                nn_type = nnType,
                                name = nnNet
                            )
                        )
                    }
                }
            }
        }
    }

    # check if a specific category is desired
    if (!is.null(spat_unit)) {
        spat_unit_subset <- availableNN$spat_unit %in%
            spat_unit
    } else {
        spat_unit_subset <- TRUE
    }
    if (!is.null(feat_type)) {
        feat_type_subset <- availableNN$feat_type %in%
            feat_type
    } else {
        feat_type_subset <- TRUE
    }
    if (!is.null(nn_type)) {
        nn_type_subset <- availableNN$nn_type %in%
            nn_type
    } else {
        nn_type_subset <- TRUE
    }

    availableNN <- availableNN[spat_unit_subset &
        feat_type_subset & nn_type_subset, ]

    if (!isTRUE(return_uniques)) {
        # NULL if there is no data
        if (nrow(availableNN) == 0) {
            return(NULL)
        } else {
            return(availableNN)
        }
    } else {
        return(lapply(uniques, unique))
    }
}



#' @title list_nearest_networks_names
#' @name list_nearest_networks_names
#' @description return the available nearest neighbor network object names
#' @inheritParams data_access_params
#' @param nn_type nearest neighbor method (e.g. "sNN", "kNN")
#' @returns names of nearest neighbor network object
#' @details function that can be used to find which names have been used
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' list_nearest_networks_names(g,
#'     spat_unit = "cell", feat_type = "rna",
#'     nn_type = "sNN"
#' )
#' @export
list_nearest_networks_names <- function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    nn_type = NULL) {
    if (is.null(spat_unit)) stop("spat_unit must be given\n")
    if (is.null(feat_type)) stop("feat_type must be given\n")
    if (is.null(nn_type)) stop("nn_type must be given\n")

    nn_object_names <- names(
        slot(gobject, "nn_network")[[spat_unit]][[feat_type]][[nn_type]]
    )

    return(nn_object_names)
}



#' @title list_spatial_info
#' @name list_spatial_info
#' @description return the available giotto spatial polygon information
#' @param gobject giotto object
#' @returns names of available spatial polygon information
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' list_spatial_info(g)
#' @export
list_spatial_info <- function(gobject) {
    availableSpatInfo <- data.table()
    for (info in names(gobject@spatial_info)) {
        availableSpatInfo <- rbind(
            availableSpatInfo,
            list(spat_info = info)
        )
    }

    if (nrow(availableSpatInfo) == 0) {
        return(NULL)
    } else {
        return(availableSpatInfo)
    }
}




#' @title list_spatial_info_names
#' @name list_spatial_info_names
#' @description return the available names for giotto spatial polygon
#' information
#' @param gobject giotto object
#' @returns vector with names of available spatial polygon information
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' list_spatial_info_names(g)
#' @export
list_spatial_info_names <- function(gobject) {
    spat_info_names <- names(gobject@spatial_info)

    return(spat_info_names)
}



#' @title list_feature_info
#' @name list_feature_info
#' @description return the available giotto spatial feature information
#' @param gobject giotto object
#' @returns names of available feature information
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' list_feature_info(g)
#' @export
list_feature_info <- function(gobject) {
    availableFeatInfo <- data.table()
    for (info in names(gobject@feat_info)) {
        availableFeatInfo <- rbind(
            availableFeatInfo,
            list(feat_info = info)
        )
    }

    if (nrow(availableFeatInfo) == 0) {
        return(NULL)
    } else {
        return(availableFeatInfo)
    }
}


#' @title list_feature_info_names
#' @name list_feature_info_names
#' @description return the available names for giotto feature information
#' @param gobject giotto object
#' @returns vector with names of available feature information
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' list_feature_info_names(g)
#' @export
list_feature_info_names <- function(gobject) {
    feat_info_names <- names(gobject@feat_info)

    return(feat_info_names)
}



#' @title list_spatial_networks
#' @name list_spatial_networks
#' @description return the available spatial networks that are attached to
#' the Giotto object
#' @inheritParams data_access_params
#' @returns data.table of names and locations of available spatial networks,
#' col order matters or list of unique nestings
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' list_spatial_networks(g)
#' @export
list_spatial_networks <- function(gobject,
    spat_unit = NULL,
    return_uniques = FALSE) {
    availableSpatNetworks <- data.table()
    uniques <- list()
    for (spatial_unit in names(gobject@spatial_network)) {
        uniques$spat_unit <- c(uniques$spat_unit, spatial_unit)
        for (spat_network_name in
            names(gobject@spatial_network[[spatial_unit]])) {
            uniques$name <- c(uniques$name, spat_network_name)
            if (inherits(gobject@spatial_network[[
                spatial_unit
            ]][[spat_network_name]], "spatialNetworkObj")) {
                availableSpatNetworks <- rbind(
                    availableSpatNetworks,
                    list(
                        spat_unit = spatial_unit,
                        name = spat_network_name
                    )
                )
            }
        }
    }

    # check if a specific category is desired
    if (!is.null(spat_unit)) {
        spat_unit_subset <- availableSpatNetworks$spat_unit %in%
            spat_unit
    } else {
        spat_unit_subset <- TRUE
    }

    availableSpatNetworks <- availableSpatNetworks[spat_unit_subset, ]

    if (!isTRUE(return_uniques)) {
        if (nrow(availableSpatNetworks) == 0) {
            return(NULL)
        } else {
            return(availableSpatNetworks)
        }
    } else {
        return(lapply(uniques, unique))
    }
}


#' @title list_spatial_networks_names
#' @name list_spatial_networks_names
#' @description return the available names for giotto feature information
#' @inheritParams data_access_params
#' @returns vector with names of available feature information
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' list_spatial_networks_names(g, spat_unit = "cell")
#' @export
list_spatial_networks_names <- function(gobject,
    spat_unit = NULL) {
    if (is.null(spat_unit)) stop("spat_unit must be given\n")

    spat_network_names <- names(gobject@spatial_network[[spat_unit]])

    return(spat_network_names)
}




#' @title list_spatial_grids
#' @name list_spatial_grids
#' @description return the available spatial grids that are attached to the
#' Giotto object
#' @inheritParams data_access_params
#' @returns data.table of names and locations of available spatial grids.
#' col order matters
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' g <- createSpatialGrid(g, sdimx_stepsize = 5, sdimy_stepsize = 5)
#'
#' list_spatial_grids(g)
#' @export
list_spatial_grids <- function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    return_uniques = FALSE) {
    availableSpatGrids <- data.table()
    uniques <- list()
    for (spatial_unit in names(gobject@spatial_grid)) {
        uniques$spat_unit <- c(uniques$spat_unit, spatial_unit)
        for (feature_type in names(gobject@spatial_grid[[spatial_unit]])) {
            uniques$feat_type <- c(uniques$feat_type, feature_type)
            for (grid_names in
                names(gobject@spatial_grid[[spatial_unit]][[feature_type]])) {
                uniques$name <- c(uniques$name, grid_names)
                if (inherits(gobject@spatial_grid[[
                    spatial_unit
                ]][[feature_type]][[grid_names]], "spatialGridObj")) {
                    availableSpatGrids <- rbind(
                        availableSpatGrids,
                        list(
                            spat_unit = spatial_unit,
                            feat_type = feature_type,
                            name = grid_names
                        )
                    )
                }
            }
        }
    }


    # check if a specific category is desired
    if (!is.null(spat_unit)) {
        spat_unit_subset <- availableSpatGrids$spat_unit %in%
            spat_unit
    } else {
        spat_unit_subset <- TRUE
    }
    if (!is.null(feat_type)) {
        feat_type_subset <- availableSpatGrids$feat_type %in%
            feat_type
    } else {
        feat_type_subset <- TRUE
    }

    availableSpatGrids <- availableSpatGrids[spat_unit_subset &
        feat_type_subset, ]

    if (!isTRUE(return_uniques)) {
        if (nrow(availableSpatGrids) == 0) {
            return(NULL)
        } else {
            return(availableSpatGrids)
        }
    } else {
        return(lapply(uniques, unique))
    }
}




#' @title list_spatial_grids_names
#' @name list_spatial_grids_names
#' @description return the available spatial grids name for a given spatial
#' unit that are attached to the Giotto object
#' @inheritParams data_access_params
#' @param return_uniques return unique nesting
#' names (ignores if final object exists/is correct class)
#' @returns vector with names of available spatial grids names
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' g <- createSpatialGrid(g, sdimx_stepsize = 5, sdimy_stepsize = 5)
#'
#' list_spatial_grids_names(g, spat_unit = "cell", feat_type = "rna")
#' @export
list_spatial_grids_names <- function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    return_uniques = FALSE) {
    if (is.null(spat_unit)) stop("spat_unit must be given\n")
    if (is.null(feat_type)) stop("feat_type must be given\n")

    spat_grid_names <- names(gobject@spatial_grid[[spat_unit]][[feat_type]])

    return(spat_grid_names)
}


#' @title list_images
#' @name list_images
#' @description Prints the available giotto images that are attached to the
#' Giotto object
#' @param gobject giotto object
#' @param img_type "image" or "largeImage"
#' @returns data.table of giotto image names attached to the giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#' list_images(g)
#' list_images(g, img_type = "largeImage")
#' @export
list_images <- function(gobject,
    img_type = NULL) {
    img_info <- lapply(
        slot(gobject, "images"),
        function(img) {
            data.table::data.table(
                name = objName(img),
                img_type = class(img)
            )
        }
    )
    avail_imgs <- data.table::rbindlist(img_info)

    # change to shortnames for img_type
    if (nrow(avail_imgs) > 0) {
        avail_imgs[img_type == "giottoLargeImage", img_type := "largeImage"]
        avail_imgs[img_type == "giottoImage", img_type := "image"]
    }

    # check if a specific category is desired
    if (!is.null(img_type)) {
        img_type_subset <- avail_imgs$img_type %in% img_type
    } else {
        img_type_subset <- TRUE
    }

    avail_imgs <- avail_imgs[img_type_subset, ]

    # NULL if there is no data
    if (nrow(avail_imgs) == 0) {
        return(NULL)
    } else {
        return(avail_imgs)
    }
}



#' @title list_images_names
#' @name list_images_names
#' @description return the available image names for a given image type that
#' are attached to the Giotto object
#' @param gobject a giotto object
#' @param img_type passing NULL (default) gets all image names. Can further
#' specify "image" or "largeImage"
#' @returns vector with names of available image names
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#' list_images_names(g)
#' list_images_names(g, img_type = "largeImage")
#' @export
list_images_names <- function(gobject,
    img_type = NULL) {
    if (!is.null(img_type)) {
        img_type <- match.arg(img_type, choices = c("image", "largeImage"))
    }

    img_names <- list_images(gobject, img_type = img_type)$name
    return(img_names)
}
