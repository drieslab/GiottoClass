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
combineMetadata <- function(gobject,
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
combineSpatialCellMetadataInfo <- function(gobject,
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
#' @description Produce a table of information about the cells, including
#' the geometry and centroids information. This function will be simplified
#' in the future with [spatValues()].
#' @param gobject giotto object
#' @param feat_type feature type
#' @param include_spat_locs include information about spatial locations
#' @param spat_loc_name spatial location name
#' @param include_poly_info include information about polygon
#' @param poly_info polygon information name
#' @param include_spat_enr include information about spatial enrichment
#' @param spat_enr_names names of spatial enrichment results to include
#' @param ext numeric or SpatExtent (optional). A cropping extent to apply to
#' to the geometries.
#' @param xlim,ylim numeric length of 2 (optional). x or y bounds to apply.
#' @param remove_background_polygon logical (default = `TRUE`). `crop()` may
#' sometimes produce extent-filling polygons when the original geometry is
#' problematic or invalid. Set `TRUE` to remove these, based on whether a
#' polygon fills up most of the x and y range.
#' @concept combine cell metadata
#' @returns data.table with combined spatial information
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' combineCellData(g, poly_info = "aggregate")
#' @export
combineCellData <- function(gobject,
    feat_type = "rna",
    include_spat_locs = TRUE,
    spat_loc_name = "raw",
    include_poly_info = TRUE,
    poly_info = "cell",
    include_spat_enr = TRUE,
    spat_enr_names = NULL,
    ext = NULL,
    xlim = NULL,
    ylim = NULL,
    remove_background_polygon = TRUE) {

    checkmate::assert_numeric(xlim, len = 2L, null.ok = TRUE)
    checkmate::assert_numeric(ylim, len = 2L, null.ok = TRUE)

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
        sv <- getPolygonInfo(
            gobject = gobject,
            polygon_name = poly_info,
            return_giottoPolygon = FALSE
        )

        e <- ext(sv)
        need_crop <- FALSE
        if (!is.null(xlim)) {
            need_crop <- TRUE
            e[c(1, 2)] <- xlim
        }
        if (!is.null(ylim)) {
            need_crop <- TRUE
            e[c(3, 4)] <- ylim
        }
        if (!is.null(ext)) {
            need_crop <- TRUE
            ext <- ext(ext)
            e <- intersect(e, ext)
        }
        if (need_crop) {
            sv <- crop(sv, e)
            if (remove_background_polygon) {
                sv <- .remove_background_polygon(sv, verbose = FALSE)
            }
            if (nrow(sv) == 0) {
                warning("no geometries left after crop", call. = FALSE)
            }
        }

        spatial_cell_info_dt <- data.table::as.data.table(
            sv,
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
combineFeatureData <- function(gobject,
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
combineFeatureOverlapData <- function(gobject,
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



#' @title Calculate Proportions of Labels Per Observation Group
#' @name calculateLabelProportions
#' @description Calculate a proportion table for a cell metadata column
#' (e.g. celltype labels) based on defined groupings of cells. These groups
#' can be defined in one of 3 ways:
#'
#' * `"table"` - explicitly provide a `data.frame` of relationships between a
#' grouping column and cell IDs or provide a colname in cell metadata with
#' grouping information. Method-specific params are:
#'   * `groups` - the `data.frame` or `character` input
#'   * `column_group_id` - column in `groups` defining the groups
#'   * `column_cell_id` - column in `groups` defining the grouped cell_IDs
#' * `"spatialnetwork"` - Use a spatial network to find groups of cells, where
#' the groups are the cells and their network neighbors. Method-specific params
#' are:
#'   * `spatial_network_name` - name of spatial network to use
#'   * `weights` - whether to consider proportion contribution of neighbors
#'   based on network weights (`TRUE`) or adjacency (`FALSE`)
#'   * `alpha` - weighting between 0 and 1 to use for the cell itself. This is
#'   independent from the `weights` param.
#' * `"polygon"` - Use a set of polygons indicated using `spat_info` to select
#' underlying cells of the `spat_unit`. This is determined with
#' `relate(relation = "intersects")`, where the underlying cells may be
#' represented by either the polygons, their centroids, if `centroids = TRUE`,
#' or the spatial locations if a specific `spat_loc_name` is provided.
#'   * `spat_info` - name of polygons to select with.
#'   * `select_on` - one of `"spatial_locs"` or `"polygons"`, determining
#'   whether the cells to be selected will be represented by their spatial
#'   locations (further specified via `spat_loc_name`) or their polygons.
#'   * `centroids` - if `select_on = "polygons"`, further specify whether to
#'   perform selection on polygon centroids.
#'   * `spat_loc_name` - if `select_on = "spatial_locs"`, further specify the
#'   set of spatial locations to use.
#' @inheritParams data_access_params
#' @param spat_unit spatial unit to perform grouping selection and calculation
#' on.
#' @param labels character. Metadata column gobject with labels to use
#' @param group_method character, one of `"table"`, `"spatialnetwork"`,
#' `"polygon"`. Method used to find groups of cell_IDs to perform proportion
#' calculation on. See description.
#' @param groups character or data.frame. If character, groups are assumed to
#' be a metadata column to use. If data.frame, a 2 column table of relations
#' between groups and cell_IDs in those groups. The values of group column will
#' be used as the group names.
#' @param spatial_network_name character. Name of spatial network to use to
#' group cell_ID values to use.
#' @param alpha numeric. Value between 0 and 1 inclusive that defines weighting
#' for self-self network connections.
#' @param weights logical. Whether to use the `"weight"` information included
#' with spatial networks as part of the proportions calculation.
#' @param spat_info character. Name of polygon information to use to group
#' @param select_on character. One of `"spatial_locs"` or `"polygons"`. Whether
#' to perform the polygon grouping on the spatial locations information or the
#' polygons.
#' @param centroids logical. When `select_on = "polygons"`, whether to use
#' the polygon centroids instead of the polygon for the spatial intersects
#' operation.
#' @param spat_loc_name character. Name of spatial locations to use in
#' spatial intersects operation.
#' @param column_cell_id character. Name of column in `groups` that contains
#' cell_ID values to use.
#' @param column_group_id character. Name of column in `groups` that contains
#' the group ids. If not provided, the first character col in `groups` that is
#' not `column_cell_id` will be used.
#' @param name character. Name to assign to the `spatEnrObj` results if `output`
#' is either `"spatEnrObj"` or `"gobject"`.
#' @param output character. Type of data to return. One of `"data.table"`,
#' `"matrix"`, `"gobject"`, or `"spatEnrObj"`
#' @param verbose verbosity.
#' @returns `gobject` with `spatEnrObj` of results attached, `data.table`,
#' `matrix`, or `spatEnrObj` depending on `output` param.
#' @export
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#' activeSpatUnit(g) <- "aggregate"
#' rels <- data.frame(
#'     grp = rep(LETTERS[1:10], length.out = ncol(g)),
#'     cid = colnames(g)
#' )
#' # calculate and return as data.table
#' calculateLabelProportions(g,
#'     labels = "leiden_clus", groups = rels, column_cell_id = "cid",
#'     spat_unit = "aggregate", output = "data.table"
#' )
#' # return as matrix
#' calculateLabelProportions(g,
#'     labels = "leiden_clus", groups = rels, column_cell_id = "cid",
#'     spat_unit = "aggregate", output = "matrix"
#' )
#' # calculate with groups from another column in metadata
#' calculateLabelProportions(g,
#'     labels = "louvain_clus", groups = "leiden_clus",
#'     spat_unit = "aggregate", output = "matrix"
#' )
#' # calculate proportions across all cells
#' calculateLabelProportions(g,
#'     labels = "leiden_clus",
#'     groups = data.frame(
#'         id = "all", # this is an arbitrary name
#'         cell_ID = colnames(g)
#'     ),
#'     spat_unit = "aggregate",
#'     output = "matrix"
#' )
#'
#' # network
#' g <- createSpatialNetwork(g,
#'     name = "knn_k8_r30",
#'     maximum_distance_knn = 30,
#'     k = 8
#'  )
#' calculateLabelProportions(g, labels = "leiden_clus",
#'     group_method = "spatialnetwork", spatial_network_name = "knn_k8_r30",
#'     spat_unit = "aggregate", output = "spatEnrObj"
#' )
#' # add to gobject
#' g <- calculateLabelProportions(g, labels = "leiden_clus",
#'     group_method = "spatialnetwork", spatial_network_name = "knn_k8_r30",
#'     spat_unit = "aggregate", output = "gobject"
#' )
#' # with weighted contributions and alpha = 0 (no self-self contribution)
#' g <- calculateLabelProportions(g, labels = "leiden_clus",
#'     group_method = "spatialnetwork", spatial_network_name = "knn_k8_r30",
#'     spat_unit = "aggregate", output = "gobject", alpha = 0, weights = TRUE
#' )
#'
#' # polygon
#' hex <- tessellate(
#'     extent = ext(g), shape = "hexagon", shape_size = 20, gap = -5,
#'     name = "hex"
#' )
#' g <- setGiotto(g, hex)
#' g <- calculateLabelProportions(g, labels = "leiden_clus",
#'     group_method = "polygon", spat_info = "hex",
#'     spat_unit = "aggregate", output = "gobject"
#' )
calculateLabelProportions <- function(gobject, labels,
    group_method = c("table", "spatialnetwork", "polygon"),
    # table
    groups = NULL,
    column_cell_id = "cell_ID",
    column_group_id = NULL,
    # spatial network
    spatial_network_name = NULL,
    alpha = 1,
    weights = FALSE,
    # polygon
    spat_info, # not NULL for autoselect since this should be intentional
    select_on = c("spatial_locs", "polygons"),
    centroids = TRUE,
    spat_loc_name = NULL,
    # general/return args
    name = "proportions",
    spat_unit = NULL,
    feat_type = NULL,
    output = c("data.table", "matrix", "spatEnrObj", "gobject"),
    verbose = NULL) {
    checkmate::assert_class(gobject, "giotto")
    checkmate::assert_character(labels, len = 1L)
    checkmate::assert_character(name, len = 1L)
    checkmate::assert_character(column_cell_id, len = 1L)
    checkmate::assert_character(column_group_id, len = 1L, null.ok = TRUE)

    fname <- "[calculateLabelProportions]" # for printing
    group_method <- match.arg(
        group_method, choices = c("table", "spatialnetwork", "polygon")
    )
    output <- match.arg(
        output, choices = c("data.table", "matrix", "spatEnrObj", "gobject")
    )
    gm_dt_incompat <- c("spatEnrObj", "gobject")
    if (output %in% gm_dt_incompat && group_method == "table") {
        stop(wrap_txtf("%s %s outputs are not available for %s",
            fname, paste(gm_dt_incompat, collapse = " and "),
            "group_method = \"table\""
        ), call. = FALSE)
    }

    spat_unit = set_default_spat_unit(
        gobject = gobject, spat_unit = spat_unit
    )
    feat_type = set_default_feat_type(
        gobject = gobject, spat_unit = spat_unit, feat_type = feat_type
    )

    # get group values
    groups <- switch(group_method,
        "table" = .clp_group_table(
            gobject = gobject,
            groups = groups,
            spat_unit = spat_unit,
            feat_type = feat_type,
            verbose = verbose
        ),
        "spatialnetwork" = .clp_group_spatialnetwork(
            gobject = gobject,
            spat_unit = spat_unit,
            spatial_network_name = spatial_network_name,
            alpha = alpha,
            weights = weights,
            verbose = verbose
        ),
        "polygon" = .clp_group_polygon(
            gobject = gobject,
            spat_unit = spat_unit,
            spat_info = spat_info,
            select_on = select_on,
            centroids = centroids,
            spat_loc_name = spat_loc_name
        )
    )

    # spatnet and poly specific hardcoded col settings
    if (group_method %in% c("spatialnetwork", "polygon")) {
        column_cell_id <- "cell_ID"
        column_group_id <- "group"
    }

    # label values
    labs <- spatValues(gobject,
        feats = labels,
        spat_unit = spat_unit,
        feat_type = feat_type,
        verbose = FALSE
    )

    # validate input colnames
    if (!column_cell_id %in% colnames(groups)) {
        stop(wrap_txt(
            fname, "'column_cell_id' must be a colname",
            "of 'groups' table\n"), call. = FALSE)
    }
    if (!is.null(column_group_id) && # if present and ID is not in colnames
        isTRUE(!column_group_id %in% colnames(groups))) {
        stop(wrap_txt(
            fname, "if provided, 'column_group_id' must be a colname",
            "of 'groups' table\n"), call. = FALSE)
    }

    # find column_group_id if not provided
    groups_col <- .clp_detect_group_col(
        groups, column_cell_id, column_group_id
    )

    data.table::setnames(labs, old = "cell_ID", new = column_cell_id)
    comb_table <- merge(groups, labs, by = column_cell_id, all.x = TRUE)
    if ("weight" %in% colnames(comb_table)) { # weight
        labs_per_group <- comb_table[, sum(weight), by = c(groups_col, labels)]
        n_per_group <- comb_table[, sum(weight), by = groups_col]
        data.table::setnames(labs_per_group, old = "V1", new = ".LPG")
        data.table::setnames(n_per_group, old = "V1", new = ".NPG")
    } else { # adjacency
        labs_per_group <- comb_table[, .N, by = c(groups_col, labels)]
        n_per_group <- comb_table[, .N, by = groups_col]
        data.table::setnames(labs_per_group, old = "N", new = ".LPG")
        data.table::setnames(n_per_group, old = "N", new = ".NPG")
    }
    prop_table <- merge(labs_per_group, n_per_group, by = groups_col)
    prop_table[, "prop" := .LPG / .NPG]
    res <- data.table::dcast(prop_table,
        formula = paste(groups_col, labels, sep = "~"),
        fill = 0,
        value.var = "prop"
    )
    if (output %in% c("spatEnrObj", "gobject")) {
        data.table::setnames(res, old = "group", new = "cell_ID")
        enr <- createSpatEnrObj(res,
            name = name,
            spat_unit = spat_unit,
            feat_type = feat_type,
            method = "calculateLabelProportions",
            verbose = FALSE
        )
        if (group_method == "polygon") {
            spatUnit(enr) <- spat_info
            has_sl <- isTRUE(nrow(list_giotto_data(gobject,
                slot = "spatial_locs", spat_unit = spat_info
            )) >= 1)
            if (!has_sl) {
                gobject <- addSpatialCentroidLocations(gobject,
                    poly_info = spat_info, verbose = FALSE
                )
            }
        }
        switch(output,
            "spatEnrObj" = return(enr),
            "gobject" = {
                gobject <- setGiotto(gobject, enr, verbose = verbose)
                ## update parameters used ##
                gobject <- update_giotto_params(gobject,
                    description = "_proportions"
                )
                return(gobject)
            }
        )
    } else {
        switch(output,
            "data.table" = return(res),
            "matrix" = return(dt_to_matrix(res))
        )
    }
}

.clp_detect_group_col <- function(groups, column_cell_id, column_group_id) {
    if (!is.null(column_group_id)) return(column_group_id)
    fname <- "[calculateLabelProportions]" # for printing
    vmsg(.v = verbose, fname, "no 'column_group_id' provided.
        Autodetecting...'")
    candidate_cols <- which(
        colnames(groups) != column_cell_id & colnames(groups) != "weight"
    )
    if (length(candidate_cols) == 0L) {
        stop(wrap_txt(
            fname, "Cannot detect 'column_group_id'.
            No character col other than 'column_cell_id' exists"
        ))
    }
    column_group_id <- colnames(groups)[candidate_cols[1L]]
    vmsg(.v = verbose, sprintf("%s selecting \"%s\" as 'column_group_id'",
        fname, column_group_id)
    )
    column_group_id
}

.clp_group_table <- function(gobject, groups, spat_unit, feat_type,
    verbose = NULL) {
    if (inherits(groups, "data.frame")) {
        data.table::setDT(groups)
    } else {
        groups <- spatValues(gobject,
            feats = groups,
            spat_unit = spat_unit,
            feat_type = feat_type,
            verbose = verbose
        )
    }
    groups
}

.clp_group_spatialnetwork <- function(gobject, spat_unit,
    spatial_network_name = NULL,
    alpha = 1,
    weights = FALSE,
    verbose = NULL) {
    checkmate::assert_numeric(alpha, lower = 0, upper = 1, len = 1L)
    checkmate::assert_logical(weights, len = 1L)
    sn <- getSpatialNetwork(gobject,
        spat_unit = spat_unit,
        name = spatial_network_name,
        output = "networkDT",
        copy_obj = TRUE,
        verbose = verbose
    )
    # ensure for every A -> B, there is both A -> B and B -> A
    sn <- convert_to_full_spatial_network(reduced_spatial_network_DT = sn)
    # extract only needed info
    needed_cols <- c("source", "target")
    if ("weight" %in% colnames(sn)) needed_cols <- c(needed_cols, "weight")
    sn <- sn[, needed_cols, with = FALSE]
    if (isFALSE(weights) && alpha == 1) {
        sn <- sn[, c("source", "target")] # drop weights info if any
    } else if (!"weight" %in% colnames(sn) || isFALSE(weights)) {
        warning(wrap_txt("No 'weight' information present in spatial network.
                         Using adjacency instead."), call. = FALSE)
        sn[, "weight" := 1] # fallback if no weight info exists
    }
    # ensure unique
    rels <- unique(sn)
    # create self information: source cell is its own neighbor
    if (alpha != 0) {
        src <- unique(sn$source)
        self_rels <- data.table::data.table(
            source = src,
            target = src
        )
        if (alpha != 1 || "weight" %in% colnames(rels)) {
            self_rels[, "weight" := alpha]
        }
        rels <- rbind(rels, self_rels)
    }
    data.table::setnames(rels, # standardize naming
        old = c("source", "target"),
        new = c("group", "cell_ID")
    )
    rels
}

.clp_group_polygon <- function(gobject, spat_unit, spat_info,
    select_on = c("spatial_locs", "polygons"),
    centroids = TRUE,
    spat_loc_name = NULL,
    verbose = NULL) {
    checkmate::assert_character(spat_info, len = 1L)
    select_on <- match.arg(select_on, choices = c("spatial_locs", "polygons"))

    x <- getPolygonInfo(gobject,
        polygon_name = spat_info,
        return_giottoPolygon = TRUE,
        verbose = TRUE
    )
    vmsg(.v = verbose, "using", objName(x), "polygons to select")

    switch(select_on,
        "spatial_locs" = {
            sl <- getSpatialLocations(gobject,
                spat_unit = spat_unit,
                name = spat_loc_name,
                output = "spatLocsObj",
                verbose = FALSE
            )
            y <- as.points(sl)
            y <- createGiottoPoints(y)
        },
        "polygons" = {
            y <- getPolygonInfo(gobject,
                polygon_name = spat_unit,
                return_giottoPolygon = TRUE,
                verbose = FALSE
            )
            if (centroids) {
                y <- centroids(y)
                y <- createGiottoPoints(y)
            }
        }
    )
    rels <- relate(x, y, relation = "intersects")
    data.table::setnames(rels, # standardize naming
        old = c("x", "y"),
        new = c("group", "cell_ID")
    )
    rels
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
calculateSpatCellMetadataProportions <- function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    spat_network = NULL,
    metadata_column = NULL,
    name = "proportion",
    return_gobject = TRUE) {

    deprecate_warn(
        when = "0.4.8",
        what = "calculateSpatCellMetadataProportions()",
        with = "calculateLabelProportions()"
    )

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
.merge_spatial_enrich_info <- function(gobject,
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
