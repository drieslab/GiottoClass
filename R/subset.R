# subset slots ####


#' @title Subset expression data
#' @name .subset_expression_data
#' @description Subset expression data from giotto object
#' @keywords internal
#' @noRd
.subset_expression_data <- function(
        gobject,
        feat_type = ":all:",
        spat_unit = ":all:",
        spat_unit_fsub = ":all:",
        feat_type_ssub = ":all:",
        cell_ids = NULL,
        feat_ids = NULL) {
    # DT vars
    subset_cells <- subset_feats <- NULL

    # find expression sets to use
    if (is.null(cell_ids)) {
        avail_cex <- NULL
    } else {
        if (isTRUE(feat_type_ssub == ":all:")) feat_type_ssub <- NULL
        if (isTRUE(spat_unit == ":all:")) spat_unit <- NULL
        avail_cex <- list_expression(
            gobject = gobject,
            spat_unit = spat_unit,
            feat_type = feat_type_ssub
        )
    }

    if (is.null(feat_ids)) {
        avail_fex <- NULL
    } else {
        if (isTRUE(spat_unit_fsub == ":all:")) spat_unit_fsub <- NULL
        if (isTRUE(feat_type == ":all:")) feat_type <- NULL
        avail_fex <- list_expression(
            gobject = gobject,
            feat_type = feat_type,
            spat_unit = spat_unit_fsub
        )
    }


    # if no expr or no need to subset, directly return
    if (is.null(avail_cex) && is.null(avail_fex)) {
        return(gobject)
    }


    # merge availability tables for easier iterating
    # left
    if (!is.null(avail_cex) && is.null(avail_fex)) {
        avail_cex <- avail_cex[, subset_feats := FALSE]
        avail_ex <- avail_cex[, subset_cells := TRUE]
    }
    # right
    else if (is.null(avail_cex) && !is.null(avail_fex)) {
        avail_fex <- avail_fex[, subset_cells := FALSE]
        avail_ex <- avail_fex[, subset_feats := TRUE]
    } else {
        # both
        avail_cex[, subset_cells := TRUE]
        avail_fex[, subset_feats := TRUE]
        avail_ex <- merge(avail_cex, avail_fex,
            by = c("spat_unit", "feat_type", "name"),
            all = TRUE
        )
    }


    # for each selected expr, perform up to 2 subsets #
    lapply(seq(nrow(avail_ex)), function(ex_i) {
        ex <- getExpression(
            gobject = gobject,
            spat_unit = avail_ex[ex_i]$spat_unit,
            feat_type = avail_ex[ex_i]$feat_type,
            values = avail_ex[ex_i]$name,
            output = "exprObj"
        )

        # perform matrix subset #
        ## cell only subsets
        if (isTRUE(avail_ex[ex_i]$subset_cells) &&
            !isTRUE(avail_ex[ex_i]$subset_feats)) {
            filter_bool_cells <- spatIDs(ex) %in% cell_ids
            ex[] <- .finalize_expr_subset(
                ex[][, filter_bool_cells, drop = FALSE]
            )
        }

        ## feat only subsets
        if (!isTRUE(avail_ex[ex_i]$subset_cells) &&
            isTRUE(avail_ex[ex_i]$subset_feats)) {
            filter_bool_feats <- featIDs(ex) %in% feat_ids
            ex[] <- .finalize_expr_subset(
                ex[][filter_bool_feats, , drop = FALSE]
            )
        }

        ## cell and feat subsets
        if (isTRUE(avail_ex[ex_i]$subset_cells) &&
            isTRUE(avail_ex[ex_i]$subset_feats)) {
            filter_bool_cells <- spatIDs(ex) %in% cell_ids
            filter_bool_feats <- featIDs(ex) %in% feat_ids
            ex[] <- .finalize_expr_subset(ex[][
                filter_bool_feats,
                filter_bool_cells,
                drop = FALSE
            ])
        }

        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <<- set_expression_values(
            gobject = gobject,
            values = ex,
            verbose = FALSE
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

        return(NULL) # ignore this
    })

    return(gobject)
}






#' @title Subset spatial locations
#' @name .subset_spatial_locations
#' @description Subset location data from giotto object
#' @keywords internal
#' @noRd
.subset_spatial_locations <- function(
        gobject,
        spat_unit = ":all:",
        cell_ids = NULL) {
    if (isTRUE(spat_unit == ":all:")) spat_unit <- NULL
    avail_locs <- list_spatial_locations(gobject, spat_unit = spat_unit)

    # if no spatial locations or subset needed, directly return
    if (is.null(avail_locs) || is.null(cell_ids)) {
        return(gobject)
    }

    # for each selected spatlocs, perform subset
    lapply(seq(nrow(avail_locs)), function(sl_i) {
        spatObj <- get_spatial_locations(
            gobject = gobject,
            spat_unit = avail_locs[sl_i]$spat_unit,
            spat_loc_name = avail_locs[sl_i]$name,
            output = "spatLocsObj",
            copy_obj = FALSE
        )

        ## filter index
        filter_bool_cells <- spatIDs(spatObj) %in% cell_ids
        spatObj[] <- spatObj[][filter_bool_cells]

        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <<- set_spatial_locations(gobject,
            spatlocs = spatObj,
            verbose = FALSE
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        # not yet possible to row subset data.tables by reference.
        # Must be set back in.

        return(NULL) # ignore this
    })

    return(gobject)
}


#' @title Subset cell metadata
#' @name .subset_cell_metadata
#' @description Subset cell metadata from giotto object
#' @inheritParams data_access_params
#' @param cell_ids cell ids to keep
#' @keywords internal
#' @noRd
.subset_cell_metadata <- function(
        gobject,
        spat_unit = ":all:",
        feat_type_ssub = ":all:",
        cell_ids = NULL) {
    # cell meta contains cell (row) by meta (col)
    # only uses cell_ids subsetting

    if (isTRUE(feat_type_ssub == ":all:")) feat_type_ssub <- NULL
    if (isTRUE(spat_unit == ":all:")) spat_unit <- NULL
    avail_cm <- list_cell_metadata(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type_ssub
    )

    # if no cell meta or subset needed, directly return
    if (is.null(avail_cm) || is.null(cell_ids)) {
        return(gobject)
    }

    # for each selected cellmeta, perform subset
    lapply(seq(nrow(avail_cm)), function(cm_i) {
        cm <- get_cell_metadata(
            gobject = gobject,
            spat_unit = avail_cm[cm_i]$spat_unit,
            feat_type = avail_cm[cm_i]$feat_type,
            output = "cellMetaObj"
        )

        ## filter index
        filter_bool_cells <- spatIDs(cm) %in% cell_ids
        cm[] <- cm[][filter_bool_cells]

        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <<- setCellMetadata(gobject,
            x = cm,
            verbose = FALSE,
            initialize = FALSE
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

        return(FALSE) # ignore this
    })

    return(gobject)
}


#' @title Subset feature metadata
#' @name .subset_feature_metadata
#' @description Subset feature metadata from giotto object
#' @inheritParams data_access_params
#' @param feat_ids feature ids to keep
#' @keywords internal
#' @noRd
.subset_feature_metadata <- function(
        gobject,
        feat_type = ":all:",
        spat_unit_fsub = ":all:",
        feat_ids = NULL) {
    if (isTRUE(spat_unit_fsub == ":all:")) spat_unit_fsub <- NULL
    if (isTRUE(feat_type == ":all:")) feat_type <- NULL
    avail_fm <- list_feat_metadata(
        gobject = gobject,
        feat_type = feat_type,
        spat_unit = spat_unit_fsub
    )

    # return directly if no cellmeta or no subset needed
    if (is.null(avail_fm) || is.null(feat_ids)) {
        return(gobject)
    }

    # for each selected featmeta, perform subset
    lapply(seq(nrow(avail_fm)), function(fm_i) {
        fm <- getFeatureMetadata(
            gobject = gobject,
            spat_unit = avail_fm[fm_i]$spat_unit,
            feat_type = avail_fm[fm_i]$feat_type,
            output = "featMetaObj"
        )

        ## filter index
        filter_bool_feats <- featIDs(fm) %in% feat_ids
        fm[] <- fm[][filter_bool_feats]

        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <<- setFeatureMetadata(gobject,
            x = fm,
            verbose = FALSE,
            initialize = FALSE
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

        return(NULL) # ignore this
    })

    return(gobject)
}



#' @title Subset spatial network
#' @name .subset_spatial_network
#' @description subset ALL spatial networks from giotto object of the given
#' spat_unit
#' @keywords internal
#' @noRd
.subset_spatial_network <- function(
        gobject,
        spat_unit = ":all:",
        cell_ids = NULL) {
    # DT vars
    to <- from <- NULL

    # Find existing networks and return as DT
    if (isTRUE(spat_unit == ":all:")) spat_unit <- NULL
    avail_sn <- list_spatial_networks(gobject, spat_unit = spat_unit)

    # if no networks or no subset needed, return directly
    if (is.null(avail_sn) || is.null(cell_ids)) {
        return(gobject)
    }

    # for each selected spatnet, perform subset
    lapply(seq(nrow(avail_sn)), function(sn_i) {
        sn <- get_spatialNetwork(
            gobject = gobject,
            spat_unit = avail_sn[sn_i]$spat_unit,
            name = avail_sn[sn_i]$name,
            output = "spatialNetworkObj"
        )

        # Within each spatialNetworkObj, subset only the cells_to_keep
        sn[] <- sn[][to %in% cell_ids & from %in% cell_ids]

        # Set the spatialNetworkObj back into the gobject
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <<- set_spatialNetwork(
            gobject = gobject,
            spatial_network = sn,
            verbose = FALSE
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

        return(NULL) # ignore this
    })

    return(gobject)
}



#' @title Subset dimension reduction
#' @name .subset_dimension_reduction
#' @description Subset dimension reduction results from giotto object
#' @keywords internal
#' @noRd
.subset_dimension_reduction <- function(
        gobject,
        spat_unit = ":all:",
        feat_type = ":all:",
        spat_unit_fsub = ":all:",
        feat_type_ssub = ":all:",
        cell_ids = NULL,
        feat_ids = NULL) {
    # find available dim reductions


    if (isTRUE(feat_type_ssub == ":all:")) feat_type_ssub <- NULL
    if (isTRUE(spat_unit == ":all:")) spat_unit <- NULL
    avail_cdr <- list_dim_reductions(
        gobject = gobject,
        data_type = "cells",
        spat_unit = spat_unit,
        feat_type = feat_type_ssub
    )

    if (isTRUE(spat_unit_fsub == ":all:")) spat_unit_fsub <- NULL
    if (isTRUE(feat_type == ":all:")) feat_type <- NULL
    avail_fdr <- list_dim_reductions(
        gobject = gobject,
        data_type = "feats",
        feat_type = feat_type,
        spat_unit = spat_unit_fsub
    )

    # if no dim reducs, or no need to subset directly return
    if ((is.null(avail_cdr) || is.null(cell_ids)) &&
        (is.null(avail_fdr) || is.null(feat_ids))) {
        return(gobject)
    }

    # for each selected dimreduc, perform subset #
    # there should be no overlap over cell dim reducs (cdr)
    # and feature dim reducs (fdr), as in the two types of info are always
    # entirely different objects. Subset operation only needs to be performed
    # once per object, so there should be no limitations on mirai usage.
    lapply(seq(nrow(avail_cdr)), function(cdr_i) {
        cdr <- get_dimReduction(
            gobject = gobject,
            spat_unit = avail_cdr[cdr_i]$spat_unit,
            feat_type = avail_cdr[cdr_i]$feat_type,
            reduction = "cells",
            reduction_method = avail_cdr[cdr_i]$dim_type,
            name = avail_cdr[cdr_i]$name,
            output = "dimObj"
        )

        cdr[] <- cdr[][rownames(cdr[]) %in% cell_ids, ]

        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <<- set_dimReduction(
            gobject = gobject,
            dimObject = cdr,
            verbose = FALSE
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        return(NULL) # ignore this
    })

    lapply(seq(nrow(avail_fdr)), function(fdr_i) {
        fdr <- get_dimReduction(
            gobject = gobject,
            spat_unit = avail_fdr[fdr_i]$spat_unit,
            feat_type = avail_fdr[fdr_i]$feat_type,
            reduction = "feats",
            reduction_method = avail_fdr[fdr_i]$dim_type,
            name = avail_fdr[fdr_i]$name,
            output = "dimObj"
        )

        fdr[] <- fdr[][rownames(fdr[]) %in% feat_ids, ]

        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <<- set_dimReduction(
            gobject = gobject,
            dimObject = fdr,
            verbose = FALSE
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        return(NULL) # ignore this
    })

    return(gobject)
}




# TODO expand this to include NNs that might be made from feature dim reducs
#' @title Subset nearest network
#' @name .subset_nearest_network
#' @description Subset nearest network results from giotto object
#' @keywords internal
#' @noRd
.subset_nearest_network <- function(
        gobject,
        spat_unit = ":all:",
        feat_type_ssub = ":all:",
        cell_ids = NULL) {
    if (isTRUE(feat_type_ssub == ":all:")) feat_type_ssub <- NULL
    if (isTRUE(spat_unit == ":all:")) spat_unit <- NULL
    avail_nn <- list_nearest_networks(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type_ssub
    )

    # if no nearest nets or subset needed, directly return
    if (is.null(avail_nn) || is.null(cell_ids)) {
        return(gobject)
    }

    # for each selected nearest net, perform subset
    lapply(seq(nrow(avail_nn)), function(nn_i) {
        nnObj <- get_NearestNetwork(
            gobject = gobject,
            spat_unit = avail_nn[nn_i]$spat_unit,
            feat_type = avail_nn[nn_i]$feat_type,
            nn_network_to_use = avail_nn[nn_i]$nn_type,
            network_name = avail_nn[nn_i]$name,
            output = "nnNetObj"
        )

        # vertices_to_keep = igraph::V(nnObj[])[filter_bool_cells]
        vids <- which(spatIDs(nnObj) %in% cell_ids)
        nnObj[] <- igraph::induced_subgraph(graph = nnObj[], vids = vids)

        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <<- set_NearestNetwork(gobject,
            nn_network = nnObj,
            verbose = FALSE
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    })

    return(gobject)
}



#' @title Subset spatial enrichment
#' @name .subset_spatial_enrichment
#' @description Subset spatial enrichment results from giotto object
#' @keywords internal
#' @noRd
.subset_spatial_enrichment <- function(
        gobject,
        spat_unit = ":all:",
        feat_type_ssub = ":all:",
        cell_ids = NULL) {
    if (isTRUE(feat_type_ssub == ":all:")) feat_type_ssub <- NULL
    if (isTRUE(spat_unit == ":all:")) spat_unit <- NULL
    avail_enr <- list_spatial_enrichments(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type_ssub
    )

    # if no spatial enrichments or subset needed, directly return
    if (is.null(avail_enr) || is.null(cell_ids)) {
        return(gobject)
    }

    # for each selected spatial enrichment, perform subset
    lapply(seq(nrow(avail_enr)), function(enr_i) {
        spatEnrObj <- get_spatial_enrichment(
            gobject = gobject,
            spat_unit = avail_enr[enr_i]$spat_unit,
            feat_type = avail_enr[enr_i]$feat_type,
            enrichm_name = avail_enr[enr_i]$name,
            output = "spatEnrObj"
        )

        filter_bool_cells <- spatIDs(spatEnrObj) %in% cell_ids
        spatEnrObj[] <- spatEnrObj[][filter_bool_cells]

        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <<- set_spatial_enrichment(gobject,
            spatenrichment = spatEnrObj,
            verbose = FALSE
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    })

    return(gobject)
}










#' @title Subset spatial info data
#' @name .subset_spatial_info_data
#' @description Subset all spatial info (polygon) data.
#' @param spatial_info contents of the Giotto spatial_info slot
#' @param cell_ids character. cell ids to keep
#' @param poly_info character. polygon(s) to subset
#' @param feat_type feature type of overlaps to subset if they exist within the
#' giottoPolygon
#' @param feat_ids character. feat ids to keep
#' @keywords internal
#' @noRd
.subset_spatial_info_data <- function(
        spatial_info,
        cell_ids = NULL,
        poly_info = "cell",
        feat_type = NULL,
        feat_ids = NULL,
        # poly_whitelist = NULL,
        verbose = TRUE) {
    if (isTRUE(poly_info == ":all:")) poly_info <- names(spatial_info)

    # set feat type
    if (is.null(feat_type)) {
        feat_type <- "rna"
    }

    res_list <- list()
    # iterate through all spatial info entries...
    for (spat_info in names(spatial_info)) {
        if (verbose) wrap_msg("for ", spat_info)
        si <- spatial_info[[spat_info]]

        # if the spatial info is one selected in poly_info:
        if (spat_info %in% poly_info) {
            if (verbose) {
                wrap_msg(
                    "--> ", spat_info, " found back in polygon layer: ",
                    poly_info
                )
            }

            # # whitelist check
            # # intended to check for mismatch. A NULL whitelist just means
            # # that no spatial locations
            # # or expression info was discovered for the selected
            # # spat_unit

            # check if cell_ids to keep and poly_ids of polygon have any overlap
            # warn if none
            if (!any(spatIDs(si) %in% cell_ids)) {
                warning(wrap_txt(
                    paste0("[", objName(si), "]"),
                    "subset poly: no overlap between",
                    "poly_IDs and cell_ids to keep"
                ))
            }

            # subset the giottoPolygon object for the cell_ids and the specified
            # feat_type overlap information (if existing) for the feat_ids
            spat_subset <- .subset_giotto_polygon_object(
                gpolygon = si,
                cell_ids = cell_ids,
                feat_ids = feat_ids,
                feat_type = feat_type
            )

            res_list[[spat_info]] <- spat_subset
        } else {
            # even if the spatial info is not one selected directly through
            # poly_info,
            # still subset subset any existing feature overlaps matching the
            # feat_type
            # for the feat_ids
            if (!is.null(si@overlaps) &&
                !is.null(feat_ids)) {
                if (isTRUE(feat_type) == ":all:") {
                    feat_type <- names(si@overlaps)
                }

                for (feat in names(si@overlaps)) {
                    if (isTRUE(feat %in% feat_type)) {
                        feat_id_bool <- terra::as.list(
                            si@overlaps[[feat]]
                        )$feat_ID %in% feat_ids
                        si@overlaps[[feat]] <- si@overlaps[[feat]][feat_id_bool]
                    }
                }
            }

            res_list[[spat_info]] <- si
        }
    }
    return(res_list)
}






#' @title Subset feature info data
#' @name .subset_feature_info_data
#' @description Subset all spatial feature (points) data
#' @param feat_info contents of giotto object feat_info slot
#' @param feat_ids character. feat ids to keep
#' @param feat_type character vector. feature type(s) to subset
#' @param x_min,x_max,y_min,y_max spatial bounds to subset by
#' @param verbose be verbose
#' @keywords internal
#' @noRd
.subset_feature_info_data <- function(
        feat_info,
        feat_ids = NULL,
        feat_type = "rna",
        x_min = NULL,
        x_max = NULL,
        y_min = NULL,
        y_max = NULL,
        verbose = FALSE) {
    if (isTRUE(feat_type == ":all:")) feat_type <- names(feat_info)

    res_list <- list()
    for (feat in names(feat_info)) {
        if (isTRUE(feat %in% feat_type)) {
            if (verbose) wrap_msg("subset feature info:", feat)

            feat_subset <- .subset_giotto_points_object(
                gpoints = feat_info[[feat]],
                feat_ids = feat_ids,
                x_min = x_min,
                x_max = x_max,
                y_min = y_min,
                y_max = y_max,
                verbose = verbose
            )

            res_list[[feat]] <- feat_subset
        } else {
            res_list[[feat]] <- feat_info[[feat]]
        }
    }
    return(res_list)
}





# subset giotto object ####


# explanation of spat_unit and feat_type in subsetting
# for a set of su and ft, the su and ft define the scope across which the ID
# selections are applied. su and ft limitations are applied independently.
# ie: su: a, ft: b is NOT those sets of info where su == a & ft == b,
# but instead cell_ID subset is applied only to su == a
# and feat_ID subset is applied only to ft == b.


#' @name .subset_giotto
#' @title Subset Giotto
#' @description
#' Internal to subset Giotto object including previous analyses for a specific
#' spatial unit and feature type.\cr
#' This is the master workflow for aggregate data subsetting.
#' [subsetGiotto], [subsetGiottoLocs], and [subsetGiottoLocsMulti]
#' only provide the params to pass to this function. This function also
#' hides confusing but needed internal params from the exported functions.\cr
#'
#' The x_max, x_min, y_max, y_min params are only provided for subsetting of the
#' feature info downstream of [subsetGiottoLocs].\cr
#'
#' About polygon subsetting:\cr
#' Polygons to subset are only subset using their poly_ID values which MUST
#' match cell_ids, otherwise incorrect or empty polygons can be returned.
#'
#' For subsetting the subcellular information only without editing the aggregate
#' information, use [subsetGiottoLocsSubcellular]
#' @inheritParams data_access_params
#' @param spat_unit,feat_type character vector. (default = ':all:') One or more
#' spatial units or feature types to subset. Accepts ':all:' as a token
#' to subset across all available.
#' @param cell_ids character. cell IDs to keep
#' @param feat_ids character. feature IDs to keep
#' @param poly_info character. polygon info(s) to subset if present. (defaults
#' to be the same as the spat_unit)
#' @param spat_unit_fsub character vector. (default = ':all:') limit feat_id
#' subsets to these spat_units
#' @param feat_type_ssub character vector. (default = ':all:') limit cell_id
#' subsets to these feat_types
#' @param x_max,x_min,y_max,y_min For feature info only. minimum and maximum x
#' and y coordinates to keep
#' @param verbose be verbose
#' @param toplevel_params expected stackframe from which parameters should be
#' extracted
#' @returns giotto object
#' @keywords internal
.subset_giotto <- function(
        gobject,
        spat_unit = ":all:",
        feat_type = "rna", # see note below
        spat_unit_fsub = ":all:",
        feat_type_ssub = ":all:",
        cell_ids = NULL,
        feat_ids = NULL,
        poly_info = spat_unit,
        x_max = NULL,
        x_min = NULL,
        y_max = NULL,
        y_min = NULL,
        verbose = FALSE,
        toplevel_params = 2) {
    # NOTE:
    # spat_unit = ':all:', but feat_type is hardcoded to 'rna'
    # This is since most people will be running analyses that look at rna
    # information through the lens of multiple spatial units

    # update ID slots now performed by intialization


    # mirai::daemons(n = GiottoUtils::determine_cores())
    #
    # # mirai cleanup
    # on.exit({
    #   mirai::daemons(0) # reset
    # })



    # # filter cell_ID and feat_ID
    # g_cell_IDs = get_cell_id(gobject, spat_unit = spat_unit)
    # g_feat_IDs = get_feat_id(gobject, feat_type = feat_type)


    ## filter index
    # if(!is.null(cell_ids)) {
    #   filter_bool_cells = g_cell_IDs %in% cell_ids
    #   cell_ids = g_cell_IDs[filter_bool_cells]
    # } else {
    #   # set cell ids to all if not provided
    #   filter_bool_cells = g_cell_IDs %in% g_cell_IDs
    #   cell_ids = g_cell_IDs[filter_bool_cells]
    # }
    #
    # if(!is.null(feat_ids)) {
    #   filter_bool_feats = g_feat_IDs %in% feat_ids
    #   feat_ids = g_feat_IDs[filter_bool_feats]
    # } else {
    #   # set feat ids to all if not provided
    #   filter_bool_feats = g_feat_IDs %in% g_feat_IDs
    #   feat_ids = g_feat_IDs[filter_bool_feats]
    # }




    if (verbose) wrap_msg("completed 1: preparation")


    ## FILTER ##
    # filter expression data
    gobject <- .subset_expression_data(
        gobject = gobject,
        cell_ids = cell_ids,
        feat_ids = feat_ids,
        spat_unit = spat_unit,
        feat_type = feat_type,
        spat_unit_fsub = spat_unit_fsub,
        feat_type_ssub = feat_type_ssub
    )

    if (verbose) wrap_msg("completed 2: subset expression data")


    # filter spatial locations

    gobject <- .subset_spatial_locations(
        gobject = gobject,
        cell_ids = cell_ids,
        spat_unit = spat_unit
    )

    if (verbose) wrap_msg("completed 3: subset spatial locations")



    ## cell & feature metadata ##
    # cell metadata
    gobject <- .subset_cell_metadata(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type_ssub = feat_type_ssub,
        cell_ids = cell_ids
    )

    if (verbose) wrap_msg("completed 4: subset cell metadata")

    # feature metadata
    gobject <- .subset_feature_metadata(
        gobject = gobject,
        feat_type = feat_type,
        spat_unit_fsub = spat_unit_fsub,
        feat_ids = feat_ids
    )

    if (verbose) wrap_msg("completed 5: subset feature metadata")


    ## spatial network & grid ##
    # cell spatial network
    gobject <- .subset_spatial_network(
        gobject = gobject,
        spat_unit = spat_unit,
        cell_ids = cell_ids
    )


    if (verbose) wrap_msg("completed 6: subset spatial network(s)")

    # spatial grid
    # need to be recomputed


    ## dimension reduction ##
    # cell dim reduction
    gobject <- .subset_dimension_reduction(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type,
        spat_unit_fsub = spat_unit_fsub,
        feat_type_ssub = feat_type_ssub,
        cell_ids = cell_ids,
        feat_ids = feat_ids
    )
    # likely also needs to be recomputed

    if (verbose) wrap_msg("completed 7: subsetted dimension reductions")


    ## nn network ##
    gobject <- .subset_nearest_network(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type_ssub = feat_type_ssub,
        cell_ids = cell_ids
    )

    if (verbose) wrap_msg("completed 8: subsetted nearest network(s)")


    ## spatial enrichment ##
    gobject <- .subset_spatial_enrichment(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type_ssub = feat_type_ssub,
        cell_ids = cell_ids
    )

    if (verbose) wrap_msg("completed 9: subsetted spatial enrichment results")

    ## spatial info
    if (!is.null(gobject@spatial_info)) {
        # # get poly whitelist
        # # whitelist is based on the provenance and spat_unit of any matching
        # # aggregate spatial locations and expression information
        # #
        # # Should only be checked for cell_ids subsets
        
        attached_polys <- list_spatial_info_names(gobject)
        
        if (is.null(poly_info)) {
            poly_info <- spat_unit[spat_unit %in% attached_polys]
        }
        
        if (isTRUE(poly_info == ":all:")) {
            poly_info <- attached_polys
        }

        for (select_poly_info in poly_info) {
            # For each entry entry in poly_info, subset using cell_ids.
            # Note that even if no poly_info is selected, the overlaps slots
            # that match the feat_type param will be subset using feat_ids.
            #
            # A warning check is performed to see if there is any overlap
            # between poly_IDs and the cell_ids to keep for each polygon.
            # Lack of overlap means that all polygon geometries will be removed
            # for that giottoPolygon object.
            gobject@spatial_info <- .subset_spatial_info_data(
                spatial_info = gobject@spatial_info,
                feat_type = feat_type,
                cell_ids = cell_ids,
                feat_ids = feat_ids,
                poly_info = select_poly_info,
                # poly_whitelist = poly_whitelist,
                verbose = verbose
            )
        }

        if (verbose) {
            wrap_msg("completed 10: subsetted spatial information data")
        }
    }


    ## feature info
    if (!is.null(gobject@feat_info)) {
        gobject@feat_info <- .subset_feature_info_data(
            feat_info = gobject@feat_info,
            feat_ids = feat_ids,
            feat_type = feat_type,
            x_max = x_max,
            x_min = x_min,
            y_max = y_max,
            y_min = y_min,
            verbose = verbose
        )

        if (verbose) wrap_msg("completed 11: subsetted spatial feature data")
    }



    ## update parameters used ##

    parameters_info <- update_giotto_params(
        gobject,
        description = "_subset",
        return_gobject = FALSE,
        toplevel = toplevel_params
    )

    ## TODO - this is no longer easily doable since multiple spatial units being
    ## subset means that cells and feats removed are variable
    # extra parameters to include
    # cells_removed = length(filter_bool_cells[filter_bool_cells==FALSE])
    # feats_removed = length(filter_bool_feats[filter_bool_feats==FALSE])

    parameters_list <- parameters_info[["plist"]]
    # update_name = parameters_info[['newname']]
    #
    # parameters_list[[update_name]] = c(parameters_list[[update_name]],
    #                                    'cells removed' = cells_removed,
    #                                    'feats removed' = feats_removed)
    gobject@parameters <- parameters_list




    #   # mirai
    #   mirai_res = get_mirai_list(gobject)
    #   if (length(mirai_res > 0)) {
    #
    #     lapply(mirai_res, function(res) {
    #       # wait until everything is done evaluating
    #       mirai::call_mirai(res)
    #
    #       # handle errors
    #       if(mirai::is_error_value(res)) stop(wrap_txt('mirai', res$data))
    #
    #       # set value
    #       gobject <<- setGiotto(gobject, x = res$data, verbose = FALSE)
    #
    #       return(NULL) # lapply itself should not return anything
    #     })
    #
    #     # TODO clear results
    #     gobject@mirai <- list()
    #     gobject <<- gobject
    #
    #   }



    return(initialize(gobject))
}






# Spatial locations subsetting workflow
# This function only spatially subsets one spatial unit at a time
# As much as possible, the aggregate spatial locations are prioritized for this
# subsetting. The cell_IDs that result from the spatial subset are then applied
# to the rest of the Giotto object using .subset_giotto().
#
# The subset for the rest of the object is cell_id-based even for the polygons
# because of possible differences in the centroid placements across different
# sets of polygons. As such, the polygons to remove through poly_info MUST have
# matching poly_IDs for the aggregate spat_unit being used in the subset.
.subset_giotto_locs <- function(
        gobject,
        spat_unit = NULL,
        feat_type = NULL,
        feat_type_ssub = ":all:",
        spat_loc_name = NULL,
        x_max = NULL,
        x_min = NULL,
        y_max = NULL,
        y_min = NULL,
        z_max = NULL,
        z_min = NULL,
        poly_info = NULL,
        return_gobject = TRUE,
        verbose = FALSE,
        toplevel_params = 5L) {
    spat_unit <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = spat_unit
    )
    feat_type <- set_default_feat_type(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type
    )

    # check :all: params
    if (length(feat_type) > 1L ||
        isTRUE(feat_type == ":all:")) {
        .gstop(
            "subsetGiottoLocs: multiple values or :all: passed to param
            feat_types. Giotto subset functions use params spat_unit and
            feat_type to define on which spatial units and feature types
            cell_id and feat_id subsets should be performed, respectively.
            -- Spatial subsets can only subset on cell_id.\n
            subsetGiottoLocs() uses feat_type only to define which set of
            combined metadata info to generate if return_gobject = FALSE.\n
            Use param feat_type_ssub instead to define which feature types
            the cell_id subset is applied across."
        )
    }

    # Usage of spat_unit = :all: IS NOT allowed for this function
    # throw error if more than one supplied or ":all:" is passed
    checkmate::assert_character(spat_unit)
    if (length(spat_unit) > 1 ||
        isTRUE(spat_unit == ":all:")) {
        .gstop(
            "subsetGiottoLocs: Length of spat_unit > 1
            Use subsetGiottoLocsMulti() for spatial subsets across
            multiple spat_units"
        )
    }

    # Usage of poly_info = :all: IS allowed for this function, but requires that
    # the polys share cell_IDs/poly_IDs with the aggregate information,
    # otherwise wrong or erroneously empty results will be returned.
    #
    # A warning check for empty gpolys after subsetting is performed in
    # .subset_giotto_polygon_object()
    # A warning check for no overlap between cell_ids to keep and poly_IDs is
    # performed in .subset_spatial_info_data()
    if (isTRUE(poly_info == ":all:")) {
        poly_info <- list_spatial_info_names(gobject)
    }
    if (is.null(poly_info)) {
        # if no value supplied, default to spat unit
        poly_info <- spat_unit
    }


    # 1. Check spatial params
    .valid_spat_subset_params(
        x_min = x_min, x_max = x_max,
        y_min = y_min, y_max = y_max,
        z_min = z_min, z_max = z_max
    )


    # function requires spat_loc_name
    if (is.null(spat_loc_name)) {
        # first check spatial locations
        if (!is.null(slot(gobject, "spatial_locs"))) {
            # get name of first available spatial unit if none provided
            spat_loc_name <- list_spatial_locations_names(
                gobject = gobject,
                spat_unit = spat_unit
            )[[1]]

            # if spatlocs missing, use alternate method with spatial_info
        } else if (!is.null(slot(gobject, "spatial_info"))) {
            subset_object <- .subset_giotto_polygons_workflow(
                gobject = gobject,
                return_gobject = return_gobject,
                spat_unit = spat_unit,
                feat_type = feat_type,
                poly_info = poly_info,
                feat_type_ssub = feat_type_ssub,
                x_min = x_min, # pass subsetting params to feat info
                x_max = x_max,
                y_min = y_min,
                y_max = y_max,
                verbose = verbose,
                toplevel_params = toplevel_params
            )
            return(subset_object)
        } else {
            spat_loc_name <- NULL
            wrap_msg("No spatial locations or spatial info have been found \n")
            return(NULL)
        }
    }


    # spatial subsets are performed using this the combined metadata.
    # the cell_ids from the combined metdata subset are then used to subset
    # the giotto object
    comb_metadata <- combineMetadata(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type,
        spat_loc_name = spat_loc_name
    )
    comb_colnames <- colnames(comb_metadata)

    # x spatial dimension
    if ("sdimx" %in% comb_colnames) {
        if (is.null(x_max)) x_max <- max(comb_metadata[["sdimx"]])
        if (is.null(x_min)) x_min <- min(comb_metadata[["sdimx"]])

        comb_metadata <- comb_metadata[get("sdimx") < x_max &
            get("sdimx") > x_min]
    }

    # y spatial dimension
    if ("sdimy" %in% comb_colnames) {
        if (is.null(y_max)) y_max <- max(comb_metadata[["sdimy"]])
        if (is.null(y_min)) y_min <- min(comb_metadata[["sdimy"]])

        comb_metadata <- comb_metadata[get("sdimy") < y_max &
            get("sdimy") > y_min]
    }

    # z spatial dimension
    if ("sdimz" %in% comb_colnames) {
        if (is.null(z_max)) z_max <- max(comb_metadata[["sdimz"]])
        if (is.null(z_min)) z_min <- min(comb_metadata[["sdimz"]])

        comb_metadata <- comb_metadata[get("sdimz") < z_max &
            get("sdimz") > z_min]
    }

    if (return_gobject) {
        filtered_cell_IDs <- comb_metadata[["cell_ID"]]

        # assumes that all spatlocs within a spat unit contain the same cell_IDs
        # additionally, all values passed to poly_info must use matching
        # cell_IDs
        subset_object <- .subset_giotto(
            gobject = gobject,
            spat_unit = spat_unit,
            feat_type = feat_type,
            feat_type_ssub = feat_type_ssub,
            cell_ids = filtered_cell_IDs,
            poly_info = poly_info,
            x_max = x_max, # pass subsetting params to feat_info
            x_min = x_min,
            y_max = y_max,
            y_min = y_min,
            verbose = verbose,
            toplevel_params = toplevel_params
        )

        return(subset_object)
    } else {
        return(comb_metadata)
    }
}





# workflow for spatially subsetting more than one set of spatial units. This
# operation iterates across each spat_unit desired. Any polygons provided
# through poly_info MUST be matched to the spat_unit it is paired to using
# the names of a named list style input.
# Since polygons are provided through named list, any polygons with no matching
# aggregate information should be subsetted with subsetGiottoLocsSubcellular().
.subset_giotto_locs_multi <- function(
        gobject,
        spat_unit = ":all:",
        feat_type = NULL,
        feat_type_ssub = ":all:",
        spat_loc_name = NULL,
        x_max = NULL,
        x_min = NULL,
        y_max = NULL,
        y_min = NULL,
        z_max = NULL,
        z_min = NULL,
        poly_info = NULL,
        return_gobject = TRUE,
        verbose = TRUE,
        toplevel_params = 5L) {
    if (!is.null(spat_unit)) checkmate::assert_character(spat_unit)
    if (!is.null(feat_type)) checkmate::assert_character(feat_type)
    # poly_info check is more detailed and performed below

    if (length(feat_type) > 1L ||
        isTRUE(feat_type == ":all:")) {
        stop(wrap_txt(
            "subsetGiottoLocsMulti: multiple values or :all: passed to param
            feat_types.
            Giotto subset functions use params spat_unit and feat_type to
            define on which spatial units and feature types cell_id and feat_id
            subsets should be performed, respectively.
            -- Spatial subsets can only subset on cell_id.\n
            subsetGiottoLocs() and subsetGiottoLocsMulti() use feat_type only
            to define which set of combined metadata info to generate if
            return_gobject = FALSE.\n
            Use param feat_type_ssub instead to define which feature types the
            cell_id subset is applied across.",
            errWidth = TRUE
        ))
    }

    # if :all:, find and subset all existing spat units with aggregate
    # spatial information
    if (isTRUE(spat_unit == ":all:")) {
        avail_su <- c(
            list_spatial_locations(gobject)$spat_unit,
            list_spatial_info(gobject)$spat_unit
        )

        spat_unit <- unique(avail_su)
    }

    # check poly_info input
    # if not given, subset all polys that match spat_unit names
    if (is.null(poly_info)) {
        poly_info <- as.list(spat_unit)
        names(poly_info) <- spat_unit
    }
    if (!inherits(poly_info, "list") ||
        length(names(poly_info)) == 0) {
        # if not named list, throw error
        # Guessing is not attempted since this may be an expensive operation and
        # there will always be more than one spat_unit in this function.
        stop(wrap_txt(
            "poly_info must be a named list of character vectors referring to
            polygons to spatially subset.
            List names should correspond to spat_units to subset the polygons
            with."
        ))
    }


    res_list <- list()

    for (spat_unit_selected in spat_unit) {
        poly_info_selected <- poly_info[[spat_unit_selected]]


        if (verbose) {
            wrap_msg(
                "\n\nStart subset on locations for spatial unit: ",
                spat_unit_selected,
                "and polygon information layers: ", poly_info_selected, "\n"
            )
        }


        if (return_gobject) {
            # multiple subsets upon same object
            gobject <- subsetGiottoLocs(
                gobject = gobject,
                spat_unit = spat_unit_selected,
                feat_type = feat_type,
                feat_type_ssub = feat_type_ssub,
                spat_loc_name = spat_loc_name,
                x_max = x_max,
                x_min = x_min,
                y_max = y_max,
                y_min = y_min,
                z_max = z_max,
                z_min = z_min,
                poly_info = poly_info_selected,
                return_gobject = return_gobject,
                verbose = verbose,
                toplevel_params = toplevel_params
            )
        } else {
            # accumulate list of combined metadata tables
            res_list[[spat_unit_selected]] <- subsetGiottoLocs(
                gobject = gobject,
                spat_unit = spat_unit_selected,
                feat_type = feat_type,
                feat_type_ssub = feat_type_ssub,
                spat_loc_name = spat_loc_name,
                x_max = x_max,
                x_min = x_min,
                y_max = y_max,
                y_min = y_min,
                z_max = z_max,
                z_min = z_min,
                poly_info = poly_info_selected,
                return_gobject = return_gobject,
                verbose = verbose,
                toplevel_params = toplevel_params
            )
        }
    }

    if (return_gobject) {
        return(gobject)
    } else {
        return(res_list)
    }
}





# exported subset functions ####


# Overview of subset functions #
#
# subsetGiotto
#   Main subsetting pipeline that subsets based on the aggregated information
#   available. It determines a set of IDs (cell_ids or feat_ids) to subset with
#   and then walks through each of the slots, performing the subset.
# subsetGiottoLocs
#   Pulls cell_IDs information and spatial locations and performs a spatial
#   subset. The cell_IDs that are selected are then fed back into subsetGiotto()
#   This operation is performed only for a single spat_unit
# subsetGiottoLocsMulti
#   Performs more than one subset operation using subsetGiottoLocs
# subsetGiottoLocsSubcellular
#   Performs the spatial subset only on the subcellular spatial info (polygons)
#   and feature info (points). This is useful for situations in which aggregate
#   information has not been created but the subcellular data is present.


# subsetGiotto (by IDs) and subsetGiottoLocs (by spatial bounds) are separate
# functions because of the difference in the scope of how these subsets can be
# applied.
# - ID subsets are only make sense relative which spat_units/feat_types they are
#   part of.
# - spatial bound subsets should be possible to apply across multiple spatunits/
#   feat_types and should be agnostic to them.
#
# These differences result in different defaults needed for scope of spat_units
# and feat_types they affect.






# TODO
# Consider an `across_spat_units` and `across_feat_types` for finer control
# with a special ':all:' input that will apply to all spat_units/feat_types

# Hierarchical subsetting?


#' @title subsetGiotto
#' @description Subsets Giotto object including previous analyses. For
#' subsetting the subcellular information only without editing the aggregate
#' information, use [subsetGiottoLocsSubcellular]
#' @inheritParams data_access_params
#' @param spat_unit,feat_type character vector. (default = ':all:') One or more
#' spatial units or feature types to subset. Accepts ':all:' as a token
#' to subset across all available.
#' @param cell_ids character. cell IDs to keep
#' @param feat_ids character. feature IDs to keep
#' @param poly_info character. polygon info(s) to subset if present. (defaults
#' to be the same as the spat_unit)
#' @param all_spat_units deprecated. use spat_unit_fsub = ':all:'
#' @param all_feat_types deprecated. use feat_type_ssub = ':all:'
#' @param spat_unit_fsub character vector. (default = ':all:') limit feat_id
#' subsets to these spat_units
#' @param feat_type_ssub character vector. (default = ':all:') limit cell_id
#' subsets to these feat_types
#' @param verbose be verbose
#' @param toplevel_params parameters to extract
#' @returns giotto object
#' @details Subsets a Giotto object for a specific spatial unit and feature type
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' subsetGiotto(g, cell_ids = c("AACTCGATGGCGCAGT-1", "GGCTGGCTAGCTTAAA-1"))
#' @export
subsetGiotto <- function(
        gobject,
        spat_unit = ":all:",
        feat_type = "rna",
        cell_ids = NULL,
        feat_ids = NULL,
        poly_info = spat_unit,
        all_spat_units = NULL,
        all_feat_types = NULL,
        spat_unit_fsub = ":all:",
        feat_type_ssub = ":all:",
        verbose = FALSE,
        toplevel_params = 2) {
    # handle deprecations
    if (!is.null(all_spat_units)) {
        if (all_spat_units) {
            spat_unit_fsub <- ":all:"
        } else {
            spat_unit_fsub <- spat_unit
        }

        warning(wrap_txt(
            'subsetGiotto: all_spat_units param is deprecated. Please use
            spat_unit_fsub = \":all:\" instead. (this is the default)'
        ))
    }
    if (!is.null(all_feat_types)) {
        if (all_feat_types) {
            feat_type_ssub <- ":all:"
        } else {
            feat_type_ssub <- feat_type
        }

        warning(wrap_txt(
            'subsetGiotto: all_feat_types param is deprecated. Please use
            feat_type_ssub = \":all:\" instead. (this is the default)'
        ))
    }

    .subset_giotto(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type,
        cell_ids = cell_ids,
        feat_ids = feat_ids,
        poly_info = poly_info,
        spat_unit_fsub = spat_unit_fsub,
        feat_type_ssub = feat_type_ssub,
        x_max = NULL,
        x_min = NULL,
        y_max = NULL,
        y_min = NULL,
        verbose = verbose,
        toplevel_params = toplevel_params + 1
    )
}













#' @title Subset by spatial locations
#' @name subsetGiottoLocs
#' @description Subsets Giotto object spatially by defining a set of
#' cropping bounds. The information to be subset is preferred to be from spatial
#' locations. If no `spat_loc_name` is given, the first available set of
#' spatial locations for the `spat_unit` will be picked.
#' If no spatial locations are available, the polygon information will be
#' subset. Spatial IDs surviving the crop are then applied to the rest of the
#' Giotto object using [subsetGiotto].
#' @inheritParams data_access_params
#' @param spat_loc_name name of spatial locations to use within spat_unit
#' @param spat_unit spatial unit to subset
#' @param feat_type (optional) feat type to use if `return_gobject = TRUE` and
#' a combined data.table output is desired.
#' @param feat_type_ssub which feat types across which to apply the spatial
#' subset
#' @param x_max,x_min,y_max,y_min,z_max,z_min minimum and maximum x, y, and z
#' coordinates to subset to
#' @param poly_info character. polygon information to subset (considered
#' separately from the spat_units to subset)
#' @param return_gobject return Giotto object
#' @param verbose be verbose
#' @param toplevel_params parameters to extract
#' @returns giotto object
#' @details If `return_gobject = FALSE`, then a filtered combined metadata
#' data.table will be returned
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' subsetGiottoLocs(g, x_max = 4000, y_max = -1000)
#' @export
subsetGiottoLocs <- function(
        gobject,
        spat_unit = NULL,
        feat_type = NULL,
        feat_type_ssub = ":all:",
        spat_loc_name = NULL,
        x_max = NULL,
        x_min = NULL,
        y_max = NULL,
        y_min = NULL,
        z_max = NULL,
        z_min = NULL,
        poly_info = NULL,
        return_gobject = TRUE,
        verbose = FALSE,
        toplevel_params = 5) {
    args_list <- get_args_list()

    if (length(spat_unit) > 1L ||
        isTRUE(spat_unit == ":all:")) {
        # for subsetting across multiple
        do.call(.subset_giotto_locs_multi, args = args_list)
    } else {
        do.call(.subset_giotto_locs, args = args_list)
    }
}








#' @name subsetGiottoLocsMulti
#' @title deprecated
#' @inheritParams subsetGiottoLocs
#' @param poly_info named list of character vectors. names correspond to
#' spat_unit and character vectors are the polygons associated with those
#' spat units
#' @returns giotto object
#' @export
subsetGiottoLocsMulti <- function(
        gobject,
        spat_unit = ":all:",
        feat_type = NULL,
        feat_type_ssub = ":all:",
        spat_loc_name = NULL,
        x_max = NULL,
        x_min = NULL,
        y_max = NULL,
        y_min = NULL,
        z_max = NULL,
        z_min = NULL,
        poly_info = NULL,
        return_gobject = TRUE,
        verbose = TRUE) {
    # deprecation
    warning(wrap_txt(
        "subsetGiottoLocsMulti() is deprecated. Use subsetGiottoLocs() in
        the future."
    ))

    args_list <- get_args_list()

    do.call(subsetGiottoLocs, args = args_list)
}




#' @title Subset raw subcellular information by location
#' @name subsetGiottoLocsSubcellular
#' @description Subsets Giotto object based on spatial coordinates. This
#' subset function is intended for poly_info and feat_types for which there
#' are no matching aggregated information.
#' @param poly_info character. which polygons to spatially subset
#' @param feat_type character. which feature info to spatially subset
#' @inheritParams subsetGiottoLocs
#' @details
#' This is separate from [subsetGiottoLocs] because spat_units with attached
#' aggregated information should be removed using a cell_ids-based approach in
#' order to ensure that paired information is removed together. Polygons and
#' points information without associated aggregated values do not have this
#' issue and can be directly spatially subset.
#' @returns giotto object
#' @export
subsetGiottoLocsSubcellular <- function(
        gobject,
        poly_info = NULL,
        feat_type = NULL,
        x_min = NULL,
        x_max = NULL,
        y_min = NULL,
        y_max = NULL,
        z_max = NULL,
        z_min = NULL,
        verbose = FALSE) {
    checkmate::assert_class(gobject, "giotto")
    if (!is.null(poly_info)) checkmate::assert_character(poly_info)
    if (!is.null(feat_type)) checkmate::assert_character(feat_type)


    # only to be used if there is no aggregated information #
    if (!is.null(gobject@expression)) {
        stop(wrap_txt("Aggregated information was found in gobject.
                  Use subsetGiottoLocs() instead"))
    }

    # Check spatial params
    .valid_spat_subset_params(
        x_min = x_min, x_max = x_max,
        y_min = y_min, y_max = y_max,
        z_min = z_min, z_max = z_max
    )


    # first subset feature ids based on location
    # this information could be needed for spatial_info if overlaps were
    # calculated


    ## 1. feature info ##
    ## --------------- ##
    if (!is.null(gobject@feat_info)) {
        # get the gpoints that are selected using feat_type as named list
        gpoints_list <- get_feature_info_list(
            gobject = gobject,
            return_giottoPoints = TRUE
        )
        if (isTRUE(feat_type == ":all:")) feat_type <- names(gpoints_list)
        gpoints_list <- gpoints_list[feat_type]

        # perform crop across named list and return to gobject
        cropped_gpoints <- lapply(gpoints_list, function(x) {
            crop(x,
                DT = TRUE,
                xmin = x_min, xmax = x_max,
                ymin = y_min, ymax = y_max
            )
            # TODO add cropping for z values as well
        })
        for (gpoints in cropped_gpoints) {
            gobject <- setFeatureInfo(
                gobject = gobject,
                x = gpoints,
                verbose = FALSE,
                initialize = FALSE
            )
        }

        # extract the cropped feature IDs for use with spatial info overlaps
        # as a named list of feat_ids to keep.
        cropped_feats <- lapply(cropped_gpoints, function(cropped_x) {
            cropped_x@unique_ID_cache
        })

        if (verbose) wrap_msg("subsetted spatial feature data")
    } else {
        cropped_feats <- NULL
    }


    ## 2. spatial info ###
    ## ---------------- ##
    if (!is.null(gobject@spatial_info)) {
        # get gpolys that are selected using poly_info as a named list.
        gpolys_list <- get_polygon_info_list(
            gobject = gobject,
            return_giottoPolygon = TRUE
        )
        if (isTRUE(poly_info == ":all:")) poly_info <- names(gpolys_list)
        gpolys_list <- gpolys_list[poly_info]

        # perform spatial crop on named list
        cropped_gpolys <- lapply(gpolys_list, function(x) {
            x <- crop(x,
                DT = TRUE,
                xmin = x_min, xmax = x_max,
                ymin = y_min, ymax = y_max
            )

            # remove features in overlaps that might have been removed during
            # the gpoints spatial crop.

            # return directly if no overlaps exist or if there are no features
            # to remove from overlaps.
            if (is.null(x@overlaps) || is.null(cropped_feats)) {
                return(x)
            }

            gpoly_overlap_names <- names(x@overlaps)
            for (overlap_feat in gpoly_overlap_names) {
                if (isTRUE(overlap_feat %in% names(cropped_feats))) {
                    feat_id_bool <- terra::as.list(
                        x@overlaps[[overlap_feat]]
                    )$feat_ID %in%
                        cropped_feats[[overlap_feat]]
                    x@overlaps[[overlap_feat]] <- x@overlaps[[
                        overlap_feat
                    ]][feat_id_bool]
                }
            }
            x
        })
        for (gpoly in cropped_gpolys) {
            gobject <- setPolygonInfo(
                gobject = gobject,
                x = cropped_gpolys,
                verbose = FALSE,
                initialize = FALSE
            )
        }

        if (verbose) wrap_msg("subsetted spatial information data")
    }


    # TODO: update parameters

    return(initialize(gobject))
}




# helpers ####

.valid_spat_subset_params <- function(x_min = NULL,
    x_max = NULL,
    y_min = NULL,
    y_max = NULL,
    z_min = NULL,
    z_max = NULL) {
    # Check spatial params
    spatError <- NULL
    if (!is.null(x_min) && !is.null(x_max)) {
        if (x_min > x_max) {
            spatError <- append(spatError, "x max must be larger than x min \n")
        }
    }
    if (!is.null(y_min) && !is.null(y_max)) {
        if (y_min > y_max) {
            spatError <- append(spatError, "y max must be larger than y min \n")
        }
    }
    if (!is.null(z_min) && !is.null(z_max)) {
        if (z_min > z_max) {
            spatError <- append(spatError, "z max must be larger than z min \n")
        }
    }

    if (!is.null(spatError)) {
        stop("Invalid spatial subset params:\n",
            spatError,
            call. = FALSE
        )
    }
}







# see .subset_giotto_locs()
# This is only ever used with a single spat_unit at a time.
# This is part of the spatial subsetting pipeline and thus has no need for
# most of the feature subsetting-related params
#   feat_type is used in case combined_metadata needs to be generated
#   feat_type_ssub allows finer control over which aggregate information is
#     also subset for cell_ids within the spatial subset.
.subset_giotto_polygons_workflow <- function(
        gobject,
        return_gobject,
        spat_unit,
        feat_type,
        poly_info,
        feat_type_ssub,
        x_min,
        x_max,
        y_min,
        y_max,
        verbose,
        toplevel_params = 4L) {
    checkmate::assert_character(spat_unit, len = 1L)
    if (isTRUE(spat_unit == ":all:")) {
        stop(wrap_txt(
            "subset_giotto_locs: polygons workflow is incompatible with
            multiple spat_units at a time."
        ))
    }

    if (!return_gobject) {
        stop(wrap_txt(
            "subsetGiottoLocs: No spatial locations for spat_unit",
            paste0("'", spat_unit, "'"), "have been found.
            return_gobject = FALSE is not possible when defaulting to polygon
            info subsetting.",
            errWidth = TRUE
        ))
    }

    # EXCEPTION: if no spatlocs found but polys exist, find cell_IDs from polys
    # by spatially scanning across all of them. The resulting cell_ID list
    # can be expected not be specific for the spatial unit, but this should
    # be fine in most cases since subsetGiotto() cell_ID input is which cells
    # to keep as opposed to which to exclude.
    polys_list <- slot(gobject, "spatial_info")
    cropped_IDs <- lapply(polys_list, function(x) {
        terra::crop(x, terra::ext(x_min, x_max, y_min, y_max))@unique_ID_cache
        # TODO add cropping for z values as well
    })

    cropped_IDs <- unique(unlist(cropped_IDs))

    subset_object <- .subset_giotto(
        gobject = gobject,
        spat_unit = spat_unit, # should be only one at a time
        feat_type = feat_type,
        cell_ids = cropped_IDs,
        poly_info = poly_info,
        x_max = x_max, # pass subsetting to feat_info
        x_min = x_min,
        y_max = y_max,
        y_min = y_min,
        feat_type_ssub = ":all:",
        verbose = verbose,
        toplevel_params = toplevel_params
    )

    return(subset_object)
}




## object subsetting ####

#' @title Subset giotto polygon object
#' @name .subset_giotto_polygon_object
#' @description Subset a single giotto polygon object for cell_ids and feat_ids
#' @param gpolygon giottoPolygon object to subset
#' @param cell_ids character. cell_ids to keep
#' @param feat_ids character. feat_ids to keep
#' @param feat_type character. feature type to subset feat_ids from if overlaps
#' are present within the giottoPolygon object
#' @keywords internal
#' @noRd
.subset_giotto_polygon_object <- function(
        gpolygon,
        cell_ids = NULL,
        feat_ids = NULL,
        feat_type = NULL) {
    # cell ID only subsets
    if (!is.null(cell_ids)) {
        if (!is.null(gpolygon@spatVector)) {
            poly_IDs <- spatIDs(gpolygon, uniques = FALSE)
            cell_id_bool <- poly_IDs %in% cell_ids
            gpolygon@spatVector <- gpolygon@spatVector[cell_id_bool]
            gpolygon@unique_ID_cache <- unique(poly_IDs[cell_id_bool])
            # update cache

            # warning if all values are removed
            if (length(gpolygon@unique_ID_cache) == 0) {
                warning(wrap_txt(
                    paste0("[", objName(gpolygon), "]"),
                    "no polygons remaining after subset."
                ))
            }
        }

        if (!is.null(gpolygon@spatVectorCentroids)) {
            # assume identical ordering
            gpolygon@spatVectorCentroids <- gpolygon@spatVectorCentroids[
                cell_id_bool
            ]
        }
    }

    # cell ID and feat ID subsets
    if (!is.null(gpolygon@overlaps)) {
        if (isTRUE(feat_type) == ":all:") feat_type <- names(gpolygon@overlaps)

        for (feat in names(gpolygon@overlaps)) {
            # TODO check this for intensity image overlaps
            if (!is.null(cell_ids)) {
                cell_id_bool <- terra::as.list(
                    gpolygon@overlaps[[feat]]
                )$poly_ID %in% cell_ids
                gpolygon@overlaps[[feat]] <- gpolygon@overlaps[[
                    feat
                ]][cell_id_bool]
            }

            if (!is.null(feat_ids) &&
                isTRUE(feat %in% feat_type)) {
                feat_id_bool <- terra::as.list(
                    gpolygon@overlaps[[feat]]
                )$feat_ID %in% feat_ids
                gpolygon@overlaps[[feat]] <- gpolygon@overlaps[[
                    feat
                ]][feat_id_bool]
            }
        }
    }
    return(gpolygon)
}







#' @title Subset giotto points object
#' @name .subset_giotto_points_object
#' @description Subset a single giotto points object
#' @details Subset on feature ids and on x,y coordinates
#' @keywords internal
#' @noRd
.subset_giotto_points_object <- function(
        gpoints,
        feat_ids = NULL,
        x_min = NULL,
        x_max = NULL,
        y_min = NULL,
        y_max = NULL,
        verbose = FALSE) {
    # data.table vars
    x <- y <- feat_ID <- NULL

    # 0. check if spatial feature information exists
    if (is.null(gpoints@spatVector)) {
        return(gpoints) # return without change since there is no points info
    }


    # 1. ID based subsetting #
    # ---------------------- #
    if (!is.null(feat_ids)) {
        feat_id_bool <- featIDs(gpoints, uniques = FALSE) %in% feat_ids
        gpoints@spatVector <- gpoints@spatVector[feat_id_bool]
    }

    # 2. Spatial subsetting #
    # --------------------- #
    # 2.1 if NO spatial subset information available,
    # ie: if all spat subset params are NULL, return directly because there are
    # no following steps
    if (all(vapply(
        list(x_min, x_max, y_min, y_max),
        is.null,
        FUN.VALUE = logical(1L)
    ))) {
        # even if no spatial subsetting happened, if ID subsetting occurred, the
        # unique_ID_cache needs to be updated
        gpoints@unique_ID_cache <- featIDs(
            gpoints,
            use_cache = FALSE, uniques = TRUE
        )
        return(gpoints)
    }

    # 2.2 otherwise use DT crop method
    gpoints <- crop(gpoints,
        xmin = x_min, xmax = x_max,
        ymin = y_min, ymax = y_max,
        DT = TRUE
    )
    # unique_ID_cache needs to be updated
    gpoints@unique_ID_cache <- featIDs(
        gpoints,
        use_cache = FALSE, uniques = TRUE
    )

    return(gpoints)
}





# see .subset_expression_data
# runs expression finalizer, if any
.finalize_expr_subset <- function(ex_mat) {
    if (methods::is(ex_mat[], "HDF5Array")) {
        ex_mat[] <- DelayedArray::realize(ex_mat[], "HDF5Array")
    }
    return(ex_mat)
}





# # used with cell_ids subsetting
# # ensure that the polygons to be subset match the spat_unit or provenance of
# # aggregate information that it is paired with during the subset
# # TODO expand this to test more than just expression and spatlocs
# poly_provenance_whitelist = function(gobject, spat_unit) {
#
#   if (isTRUE(spat_unit == ":all:")) spat_unit = NULL
#   avail_ex = list_expression(
#     gobject = gobject,
#     spat_unit = spat_unit
#   )
#   avail_sl = list_spatial_locations(
#     gobject = gobject,
#     spat_unit = spat_unit
#   )
#
#   prov_ex = lapply(seq(nrow(avail_ex)), function(ex_i) {
#     ex = getExpression(
#       gobject = gobject,
#       spat_unit = avail_ex[ex_i]$spat_unit,
#       feat_type = avail_ex[ex_i]$feat_type,
#       values = avail_ex[ex_i]$name,
#       output = 'exprObj'
#     )
#
#     c(spatUnit(ex), prov(ex))
#   })
#   prov_sl = lapply(seq(nrow(avail_sl)), function(sl_i) {
#     sl = getSpatialLocations(
#       gobject = gobject,
#       spat_unit = avail_sl[sl_i]$spat_unit,
#       name = avail_sl[sl_i]$name,
#       output = 'spatLocsObj'
#     )
#
#     c(spatUnit(sl), prov(sl))
#   })
#
#   c(prov_ex, prov_sl) %>%
#     unlist(recursive = TRUE) %>%
#     unique()
# }
