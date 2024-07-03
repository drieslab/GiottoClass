## * spatial queries ####

# If the polys are to be clipped, then the returned info MUST be a new polygon
# object

#' @title Spatially query polygons within Giotto object
#' @name spatQueryGiottoPolygons
#' @description Recursively select polygons based on a list of spatial filters.
#' Results will be returned as a new polygon-based spatial unit with selection
#' information recorded in the associated cell metadata. The final item in
#' provided in param \code{filters} is the layer of information being queried.
#' @param gobject Giotto object
#' @param filters list of characters. Named list of IDs to query on as spatial
#' filters where the names designate the spatial unit to use and the character
#' values should either be 'all' or a vector of cell_IDs to use.
#' @param name (optional) character. If not NULL, a new spatial unit of this
#' name will be generated from the results
#' @param feat_type (optional) May be changed in future. Determines which
#' feature type metadata in which hierarchical selection information is stored.
#' @param clip boolean. Default = FALSE. Whether final round of querying should
#' return polygons clipped by the polygons used to select them. If TRUE, a value
#' must be provided to \code{name} param to generate a new spatial unit
#' @returns giottoPolygon
#' @seealso [spatQueryGiottoSpatLocs()
#' @export
spatQueryGiottoPolygons <- function(gobject,
    filters,
    name = "query_polys",
    feat_type = NULL,
    clip = TRUE) {
    assert_giotto(gobject)
    if (!is.null(name)) checkmate::assert_character(name)
    checkmate::assert_list(filters, types = "character")
    if (!length(filters <= 2)) {
        stop(wrap_txt("At least two elements in filters are needed."))
    }

    if (isTRUE(clip) & is.null(name)) {
        stop(wrap_txt("If clip is true, a value to 'name' param should be
                    provided."))
    }

    # check spat units input
    spat_units <- names(filters)
    if (any(vapply(spat_units, is_empty_char, FUN.VALUE = logical(1L)))) {
        stop(wrap_txt("All elements in filters list must be named by the
                    spatial units being used."))
    }
    avail_polys <- list_spatial_info_names(gobject)
    missing_polys <- spat_units[!spat_units %in% avail_polys]

    last_info <- tail(spat_units, 1) # get final spatial info layer
    if (is.null(name)) name <- last_info
    # replace poly and meta if name not supplied
    feat_type <- set_default_feat_type(
        gobject = gobject,
        spat_unit = last_info,
        feat_type = feat_type
    )
    # cell_meta = getCellMetadata(gobject = gobject,
    #                             spat_unit = last_info,
    #                             feat_type = feat_type,
    #                             output = 'cellMetaObj',
    #                             copy_obj = TRUE)
    # spatUnit(cell_meta) = name
    # prov(cell_meta) = name

    # function to get subsetted spatvector
    get_sv <- function(gobject, spat_unit, cell_id) {
        # 'all' is passed, use all spatIDs found for that spat unit
        if (identical(cell_id, "all")) {
            IDs <- spatIDs(gobject, spat_unit = spat_unit)
        } else {
            IDs <- cell_id
        }
        sv <- getPolygonInfo(
            gobject = gobject,
            polygon_name = spat_unit,
            return_giottoPolygon = FALSE
        )
        sv[sv$poly_ID %in% IDs]
    }

    # get first poly
    sv1 <- get_sv(
        gobject = gobject,
        spat_unit = spat_units[1L],
        cell_id = filters[[spat_units[1L]]]
    )

    # iterate
    # sv1 is the filter poly
    # sv2 is the data poly
    for (unit in spat_units[2:length(spat_units)]) {
        sv2 <- get_sv(
            gobject = gobject,
            spat_unit = unit,
            cell_id = filters[[unit]]
        )
        sv1 <- terra::intersect(sv1, sv2)
    }

    names(sv1) <- c("poly_ID", rev(spat_units)[2:length(spat_units)])
    poly <- giottoPolygon(
        spatVector = sv1,
        name = name,
        unique_ID_cache = unique(sv1$poly_ID)
    )

    # extract relationships which have been appended to sv1 for each intersect
    # info for each new layer appended on the left, with at least the 'poly_ID'
    # column being added each time. Expected layout:
    # final_data_lyr, ..., filter_lyr4, filter_lyr3, filter_lyr2, filter_lyr1
    #
    # final_data_lyr should remain named as poly_ID, but the others should be
    # renamed as their respective spatial units

    # rels = terra::values(sv1) %>%
    #   data.table::setDT()
    #
    # hierarchy_info_idx = which(names(rels) == 'poly_ID')
    # rels = rels[, ..hierarchy_info_idx]
    # data.table::setnames(rels, new = c('cell_ID',
    # rev(spat_units)[2:length(spat_units)]))

    # merge in relationship info
    # cell_meta[] = merge(cell_meta[], rels)


    # set values
    gobject <- setPolygonInfo(gobject = gobject, x = poly, initialize = FALSE)
    # gobject = setCellMetadata(gobject = gobject, x = cell_meta)


    return(gobject)
}
