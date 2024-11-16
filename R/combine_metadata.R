# Combine metadata functions combine information from other slots with metadata
# information to create a single report.




# Combine metadata ####

#' @title combineMetadata
#' @name combineMetadata
#' @description This function combines the cell metadata with spatial locations
#' and enrichment results from runSpatialEnrich.
#' @param gobject Giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param spat_loc_name name of spatial locations to include
#' @param spat_enr_names names of spatial enrichment results to include
#' @param verbose verbosity
#' @returns Extended cell metadata in data.table format.
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' combineMetadata(g)
#' @export
combineMetadata <- function(
        gobject,
        spat_unit = NULL,
        feat_type = NULL,
        spat_loc_name = "raw",
        spat_enr_names = NULL,
        verbose = TRUE) {
    # DT vars
    cell_ID <- NULL

    # Set feat_type and spat_unit
    spat_unit <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = spat_unit
    )
    feat_type <- set_default_feat_type(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type
    )

    # cell metadata
    metadata <- getCellMetadata(
        gobject,
        spat_unit = spat_unit,
        feat_type = feat_type,
        output = "data.table"
    )

    # spatial locations
    if (!is.null(spat_loc_name)) {
        spatial_locs <- getSpatialLocations(
            gobject = gobject,
            spat_unit = spat_unit,
            name = spat_loc_name,
            output = "data.table",
            copy_obj = TRUE,
            verbose = verbose
        )
    } else {
        spatial_locs <- NULL
    }

    if (!is.null(spatial_locs)) {
        # metadata = cbind(metadata, spatial_locs[, cell_ID := NULL])
        metadata <- data.table::merge.data.table(
            metadata,
            spatial_locs,
            by = "cell_ID"
        )
    }

    # cell/spot enrichment data
    final_meta <- .merge_spatial_enrich_info(
        gobject = gobject,
        comb_dt = metadata,
        spat_unit = spat_unit,
        feat_type = feat_type,
        spat_enr_names = spat_enr_names
    )

    return(final_meta)
}





#' @title combineSpatialCellMetadataInfo
#' @name combineSpatialCellMetadataInfo
#' @description Combine cell metadata with spatial cell
#' information (e.g. polygon)
#' @param gobject Giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type(s)
#' @details
#' The returned data.table has the following columns: \cr
#' \itemize{
#'   \item{sdimx: spatial feature location on the x-axis}
#'   \item{sdimy: spatial feature location on the y-axis}
#'   \item{cell_ID: unique cell ID}
#'   \item{feat: selected feature(s)}
#'   \item{other columns that are part of the cell metadata}
#' }
#' @returns list of data.table(s)
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' combineSpatialCellMetadataInfo(g, spat_unit = "aggregate", feat_type = "rna")
#' @export
combineSpatialCellMetadataInfo <- function(
        gobject,
        spat_unit = NULL,
        feat_type = NULL) {
    # combine
    # 1. spatial morphology information ( = polygon)
    # 2. cell metadata

    # Set feat_type and spat_unit
    spat_unit <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = spat_unit
    )
    feat_type <- set_default_feat_type(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type
    )

    # get spatial cell information
    spatial_cell_info <- data.table::as.data.table(
        gobject@spatial_info[[spat_unit]]
    )

    colnames(spatial_cell_info)[1] <- "cell_ID"


    res_list <- list()
    for (feat in unique(feat_type)) {
        # get spatial cell metadata
        cell_meta <- pDataDT(gobject,
            spat_unit = spat_unit,
            feat_type = feat
        )

        # merge
        spatial_cell_info_meta <- merge.data.table(spatial_cell_info,
            cell_meta,
            by = "cell_ID"
        )

        spatial_cell_info_meta[, "feat" := feat]

        res_list[[feat]] <- spatial_cell_info_meta
    }

    return(res_list)
}







#' @title combineCellData
#' @name combineCellData
#' @description combine cell data information
#' @param gobject giotto object
#' @param feat_type feature type
#' @param include_spat_locs include information about spatial locations
#' @param spat_loc_name spatial location name
#' @param include_poly_info include information about polygon
#' @param poly_info polygon information name
#' @param include_spat_enr include information about spatial enrichment
#' @param spat_enr_names names of spatial enrichment results to include
#' @concept combine cell metadata
#' @returns data.table with combined spatial information
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' combineCellData(g, poly_info = "aggregate")
#' @export
combineCellData <- function(
        gobject,
        feat_type = "rna",
        include_spat_locs = TRUE,
        spat_loc_name = "raw",
        include_poly_info = TRUE,
        poly_info = "cell",
        include_spat_enr = TRUE,
        spat_enr_names = NULL) {
    # combine
    # 1. spatial morphology information ( = polygon)
    # 2. cell metadata

    # specify feat_type
    # Set feat_type and spat_unit
    poly_info <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = poly_info
    )
    feat_type <- set_default_feat_type(
        gobject = gobject,
        spat_unit = poly_info,
        feat_type = feat_type
    )


    ## spatial locations ##
    if (isTRUE(include_spat_locs)) {
        spat_locs_dt <- getSpatialLocations(
            gobject = gobject,
            spat_unit = poly_info,
            name = spat_loc_name,
            output = "data.table",
            copy_obj = TRUE
        )
    } else {
        spat_locs_dt <- NULL
    }


    ## spatial poly ##
    if (isTRUE(include_poly_info)) {
        # get spatial poly information
        spatial_cell_info_spatvec <- getPolygonInfo(
            gobject = gobject,
            polygon_name = poly_info,
            return_giottoPolygon = FALSE
        )
        spatial_cell_info_dt <- data.table::as.data.table(
            spatial_cell_info_spatvec,
            geom = "XY",
            include_values = TRUE
        )
        data.table::setnames(spatial_cell_info_dt,
            old = "poly_ID",
            new = "cell_ID"
        )
    } else {
        spatial_cell_info_dt <- NULL
    }


    # combine spatloc and poly information if desired
    if (!is.null(spat_locs_dt) &&
        !is.null(spatial_cell_info_dt)) {
        comb_dt <- data.table::merge.data.table(
            spat_locs_dt,
            spatial_cell_info_dt,
            by = "cell_ID"
        )
    } else if (!is.null(spat_locs_dt)) {
        comb_dt <- spat_locs_dt
    } else if (!is.null(spatial_cell_info_dt)) {
        comb_dt <- spatial_cell_info_dt
    } else {
        comb_dt <- NULL
    }

    ## spat enrichment ##
    if (isTRUE(include_spat_enr)) {
        comb_dt <- .merge_spatial_enrich_info(
            gobject = gobject,
            comb_dt = comb_dt,
            spat_unit = poly_info,
            feat_type = feat_type,
            spat_enr_names = spat_enr_names
        )
    }

    ## cell metadata ##
    # iterate across each requested feat_type
    res_list <- list()
    for (feat in unique(feat_type)) {
        # get spatial cell metadata
        cell_meta <- getCellMetadata(
            gobject = gobject,
            spat_unit = poly_info,
            feat_type = feat,
            output = "data.table"
        )

        # merge
        if (!is.null(comb_dt)) {
            spatial_cell_info_meta <- data.table::merge.data.table(
                comb_dt,
                cell_meta,
                by = "cell_ID"
            )
        } else {
            spatial_cell_info_meta <- cell_meta
        }

        spatial_cell_info_meta[, "feat" := feat]

        res_list[[feat]] <- spatial_cell_info_meta
    }

    return(res_list)
}


#' @title combineFeatureData
#' @name combineFeatureData
#' @description combine feature data information
#' @param gobject giotto object
#' @param feat_type feature type
#' @param spat_unit spatial unit
#' @param sel_feats selected features (default: NULL or no selection)
#' @concept combine feature metadata
#' @returns data.table with combined spatial feature information
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' combineFeatureData(g, spat_unit = "aggregate", feat_type = "rna")
#' @export
combineFeatureData <- function(
        gobject,
        feat_type = NULL,
        spat_unit = NULL,
        sel_feats = NULL) {
    # data.table variables
    feat_ID <- NULL

    spat_unit <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = spat_unit
    )
    feat_type <- set_default_feat_type(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type
    )

    res_list <- list()
    for (feat in unique(feat_type)) {
        for (spat in unique(spat_unit)) {
            # feature meta
            feat_meta <- getFeatureMetadata(
                gobject = gobject,
                spat_unit = spat_unit,
                feat_type = feat,
                output = "data.table"
            )

            if (!is.null(sel_feats[[feat]])) {
                selected_features <- sel_feats[[feat]]
                feat_meta <- feat_meta[feat_ID %in% selected_features]
            }


            # feature info
            feat_info_spatvec <- getFeatureInfo(
                gobject = gobject,
                feat_type = feat,
                return_giottoPoints = FALSE
            )
            feat_info <- .spatvector_to_dt(feat_info_spatvec)
            if (!is.null(sel_feats[[feat]])) {
                selected_features <- sel_feats[[feat]]
                feat_info <- feat_info[feat_ID %in% selected_features]
            }

            comb_dt <- data.table::merge.data.table(
                x = feat_meta,
                y = feat_info,
                by = "feat_ID"
            )

            comb_dt[, "feat" := feat]
            comb_dt[, "spat_unit" := spat]
        }

        res_list[[feat]] <- comb_dt
    }

    return(res_list)
}





#' @title combineFeatureOverlapData
#' @name combineFeatureOverlapData
#' @description combine feature data information
#' @param gobject giotto object
#' @param feat_type feature type
#' @param sel_feats selected features (default: NULL or no selection)
#' @param poly_info polygon information name
#' @concept combine feature metadata
#' @returns data.table with combined spatial polygon information
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' combineFeatureOverlapData(g, poly_info = "aggregate")
#' @export
combineFeatureOverlapData <- function(
        gobject,
        feat_type = "rna",
        sel_feats = NULL,
        poly_info = "cell") {
    # data.table vars
    feat_ID <- NULL

    poly_info <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = poly_info
    )
    feat_type <- set_default_feat_type(
        gobject = gobject,
        spat_unit = poly_info,
        feat_type = feat_type
    )


    res_list <- list()
    for (feat in unique(feat_type)) {
        for (spat in unique(poly_info)) {
            feat_meta <- getFeatureMetadata(
                gobject = gobject,
                spat_unit = spat,
                feat_type = feat,
                output = "data.table"
            )

            if (!is.null(sel_feats[[feat]])) {
                selected_features <- sel_feats[[feat]]
                feat_meta <- feat_meta[feat_ID %in% selected_features]
            }

            # overlap poly and feat info
            poly_list <- list()
            for (poly in poly_info) {
                feat_overlap_info_spatvec <- getPolygonInfo(
                    gobject = gobject,
                    polygon_name = poly,
                    polygon_overlap = feat
                )
                feat_overlap_info <- .spatvector_to_dt(
                    feat_overlap_info_spatvec
                )

                if (!is.null(sel_feats[[feat]])) {
                    selected_features <- sel_feats[[feat]]
                    feat_overlap_info <- feat_overlap_info[
                        feat_ID %in% selected_features
                    ]
                }

                feat_overlap_info[, "poly_info" := poly]
                poly_list[[poly]] <- feat_overlap_info
            }

            poly_list_res <- data.table::rbindlist(poly_list, fill = TRUE)

            comb_dt <- data.table::merge.data.table(
                x = feat_meta,
                y = poly_list_res,
                by = "feat_ID"
            )
        }


        comb_dt[, "feat" := feat]
        res_list[[feat]] <- comb_dt
    }

    return(res_list)
}








#' @title calculateSpatCellMetadataProportions
#' @name calculateSpatCellMetadataProportions
#' @description calculates a proportion table for a cell metadata
#' column (e.g. cluster labels) for all the spatial neighbors of a source cell.
#' In other words it calculates the niche composition for a given annotation
#' for each cell.
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param spat_network spatial network
#' @param metadata_column metadata column to use
#' @param name descriptive name for the calculated proportions
#' @param return_gobject return giotto object
#' @returns giotto object (default) or enrichment object if
#' return_gobject = FALSE
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' calculateSpatCellMetadataProportions(g,
#'     spat_network = "Delaunay_network", metadata_column = "leiden_clus"
#' )
#' @export
calculateSpatCellMetadataProportions <- function(
        gobject,
        spat_unit = NULL,
        feat_type = NULL,
        spat_network = NULL,
        metadata_column = NULL,
        name = "proportion",
        return_gobject = TRUE) {
    # DT vars
    proptable <- target_clus <- source_clus <- network <- target <- NULL

    if (is.null(spat_network)) stop("spat_network = NULL, you need to provide
                                    an existing spatial network")
    if (is.null(metadata_column)) stop("metadata_column = NULL, you need to
                                    provide an existing cell metadata column")

    # Set feat_type and spat_unit
    spat_unit <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = spat_unit
    )
    feat_type <- set_default_feat_type(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type
    )

    # get spatial network to use
    sp_network <- get_spatialNetwork(
        gobject = gobject,
        spat_unit = spat_unit,
        name = spat_network,
        output = "networkDT"
    )

    # convert spatial network to a full spatial network
    sp_network <- convert_to_full_spatial_network(
        reduced_spatial_network_DT = sp_network
    )

    # get cell metadata
    cell_meta <- getCellMetadata(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type,
        output = "data.table"
    )

    # merge spatial network and cell metadata
    network_annot <- data.table::merge.data.table(
        x = sp_network,
        y = cell_meta[, c("cell_ID", metadata_column), with = FALSE],
        by.x = "source", by.y = "cell_ID"
    )
    setnames(network_annot, old = metadata_column, "source_clus")
    network_annot <- data.table::merge.data.table(
        x = network_annot,
        y = cell_meta[, c("cell_ID", metadata_column), with = FALSE],
        by.x = "target", by.y = "cell_ID"
    )
    setnames(network_annot, old = metadata_column, "target_clus")

    # create self information: source cell is its own neighbor
    source_annot_info <- unique(network_annot[, .(source, source_clus)])
    setnames(source_annot_info, "source_clus", "label")
    source_annot_info[, target := source]
    source_annot_info <- source_annot_info[, .(source, target, label)]

    # network information: source cells and other neighbors
    target_annot_info <- unique(network_annot[, .(source, target, target_clus)])
    setnames(target_annot_info, "target_clus", "label")

    # combine: provides most detailed information about neighbors
    final_annot_info <- rbindlist(list(source_annot_info, target_annot_info))



    # calculate proportions of neighbors
    tableres <- final_annot_info[, names(table(label)), by = "source"]
    setnames(tableres, "V1", "tablelabels")
    propensities <- final_annot_info[, prop.table(table(label)), by = "source"]
    setnames(propensities, "V1", "proptable")


    # data.table variables
    label <- NULL
    propensities[, "label" := tableres$tablelabels]
    propensities[, "proptable" := as.numeric(proptable)]
    proportions_mat <- dcast.data.table(propensities,
        formula = "source~label", fill = 0,
        value.var = "proptable"
    )
    data.table::setnames(x = proportions_mat, old = "source", new = "cell_ID")

    # convert to matrix
    # proportions_matrix = dt_to_matrix(proportions_mat)
    # proportions_matrix[seq_len(4), seq_len(10)]

    # create spatial enrichment object
    enrObj <- create_spat_enr_obj(
        name = name,
        method = "rank",
        enrichDT = proportions_mat,
        spat_unit = spat_unit,
        feat_type = feat_type,
        provenance = NULL,
        misc = NULL
    )


    ## return object or results ##
    if (return_gobject == TRUE) {
        spenr_names <- list_spatial_enrichments_names(
            gobject = gobject,
            spat_unit = spat_unit,
            feat_type = feat_type
        )


        if (name %in% spenr_names) {
            wrap_msg(name, " has already been used, will be overwritten")
        }

        ## update parameters used ##
        parameters_list <- gobject@parameters
        number_of_rounds <- length(parameters_list)
        update_name <- paste0(number_of_rounds, "_spatial_enrichment")


        ## update parameters used ##
        gobject <- update_giotto_params(gobject, description = "_enrichment")

        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <- set_spatial_enrichment(
            gobject = gobject,
            spatenrichment = enrObj
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

        return(gobject)
    } else {
        return(enrObj)
    }
}








# internals ####



#' @title .merge_spatial_enrich_info
#' @name .merge_spatial_enrich_info
#' @description take a combine data.table result and append spatial enrichment
#' results.
#' @keywords internal
#' @returns data.table
# spat_unit and feat_type are expected to not be NULL.
.merge_spatial_enrich_info <- function(
        gobject,
        comb_dt,
        spat_unit,
        feat_type,
        spat_enr_names = NULL) {
    if (is.null(spat_enr_names)) {
        return(comb_dt)
    } # skip if not requested
    if (is.null(comb_dt)) {
        return(comb_dt)
    } # skip if no comb_dt to build on

    # detect available cell/spot enrichment data
    available_enr <- list_spatial_enrichments_names(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type
    )

    # output warning if requested spatial enrichment is not found
    not_available <- spat_enr_names[!spat_enr_names %in% available_enr]
    if (length(not_available) > 0L) {
        warning(wrap_txt(
            "These spatial enrichment results have not been found:\n",
            not_available
        ))
    }

    spat_enr_names <- spat_enr_names[spat_enr_names %in% available_enr]

    if (length(spat_enr_names) == 0L) {
        return(comb_dt)
    } # skip if none available

    # get requested spatial enrichments and append them
    result_list <- lapply(spat_enr_names, function(enr_name) {
        getSpatialEnrichment(
            gobject = gobject,
            spat_unit = spat_unit,
            feat_type = feat_type,
            name = enr_name,
            output = "data.table",
            copy_obj = TRUE,
            set_defaults = FALSE
        )
    })

    final_meta <- Reduce(
        function(x, y) data.table::merge.data.table(x, y, by = "cell_ID"),
        c(list(comb_dt), result_list)
    )

    # detect and warn about any col naming duplications
    duplicates <- final_meta %>%
        colnames() %>%
        duplicated() %>%
        sum()
    if (duplicates > 0) {
        wrap_msg(
            "Some column names are not unique.
                If you add results from multiple enrichments,",
            "consider giving the signatures unique names"
        )
    }

    return(final_meta)
}
