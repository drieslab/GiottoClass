
#' @include generics.R
NULL

## spatIDs and featIDs generic ####
#' @title Spatial and feature IDs
#' @name spatIDs-generic
#' @description Get the cell IDs (termed spatial IDs to better reflect when not at
#' the single-cell level) and feature IDs of a giotto object or subobject
#' @aliases spatIDs featIDs
#' @param x an object
#' @param spat_unit (optional) specify which spatial unit
#' @param feat_type (optional) specify which feature type
#' @param ... additional parameters to pass
#' @include classes.R
#' @usage
#' spatIDs(x, spat_unit, ...)
#'
#' featIDs(x, feat_type, ...)
#'
#' ## Default S4 method for signatures:
#' ## 'giotto', 'exprObj', 'spatLocsObj', 'cellMetaObj', 'spatialNetworkObj' 'dimObj'
NULL




# spatIDs ####

#' @rdname spatIDs-generic
#' @export
setMethod('spatIDs', signature(x = 'giotto', spat_unit = 'missing'),
          function(x, ...) {
            as.character(get_cell_id(gobject = x, ...))
          })
#' @rdname spatIDs-generic
#' @export
setMethod('spatIDs', signature(x = 'giotto', spat_unit = 'character'),
          function(x, spat_unit, ...) {
            as.character(get_cell_id(gobject = x, spat_unit, ...))
          })
#' @rdname spatIDs-generic
#' @export
setMethod('spatIDs', signature(x = c('exprObj'), spat_unit = 'missing'),
          function(x, ...) {
            as.character(colnames(x[]))
          })
#' @rdname spatIDs-generic
#' @export
setMethod('spatIDs', signature(x = c('spatLocsObj'), spat_unit = 'missing'),
          function(x, ...) {
            as.character(x[]$cell_ID)
          })
#' @rdname spatIDs-generic
#' @export
setMethod('spatIDs', signature(x = c('cellMetaObj'), spat_unit = 'missing'),
          function(x, ...) {
            as.character(x[]$cell_ID)
          })
#' @rdname spatIDs-generic
#' @export
setMethod('spatIDs', signature(x = c('spatialNetworkObj'), spat_unit = 'missing'),
          function(x, ...) {
            as.character(unique(c(x[]$from, x[]$to)))
          })
#' @rdname spatIDs-generic
#' @export
setMethod('spatIDs', signature(x = 'dimObj', spat_unit = 'missing'),
          function(x, ...) {
            as.character(rownames(x@coordinates))
          })
#' @rdname spatIDs-generic
#' @param use_cache use cached IDs if available (gpoly and gpoints only)
#' @export
setMethod('spatIDs', signature(x = 'giottoPolygon', spat_unit = 'missing'),
          function(x, use_cache = TRUE, ...) {
            # getting as list first is more performant
            if(!all(is.na(x@unique_ID_cache)) & isTRUE(use_cache)) as.character(x@unique_ID_cache)
            else as.character(unique(terra::as.list(x@spatVector)$poly_ID))
          })
#' @rdname spatIDs-generic
#' @export
setMethod('spatIDs', signature(x = 'spatEnrObj', spat_unit = 'missing'),
          function(x, ...) {
            as.character(x@enrichDT$cell_ID)
          })
#' @rdname spatIDs-generic
#' @export
setMethod('spatIDs', signature(x = 'nnNetObj', spat_unit = 'missing'),
          function(x, ...) {
            as.character(unique(names(igraph::V(x@igraph))))
          })






# featIDs ####

#' @rdname spatIDs-generic
#' @export
setMethod('featIDs', signature(x = 'giotto', feat_type = 'missing'),
          function(x, ...) {
            as.character(get_feat_id(gobject = x, ...))
          })
#' @rdname spatIDs-generic
#' @export
setMethod('featIDs', signature(x = 'giotto', feat_type = 'character'),
          function(x, feat_type, ...) {
            as.character(get_feat_id(gobject = x, feat_type, ...))
          })
#' @rdname spatIDs-generic
#' @export
setMethod('featIDs', signature(x = 'exprObj', feat_type = 'missing'),
          function(x, ...) {
            as.character(rownames(x[]))
          })
#' @rdname spatIDs-generic
#' @export
setMethod('featIDs', signature(x = 'giottoPoints', feat_type = 'missing'),
          function(x, use_cache = TRUE, ...) {
            # getting as list is more performant than directly using `$`
            if(!all(is.na(x@unique_ID_cache)) & isTRUE(use_cache)) as.character(x@unique_ID_cache)
            else as.character(unique(terra::as.list(x@spatVector)$feat_ID))
          })
#' @rdname spatIDs-generic
#' @export
setMethod('featIDs', signature(x = 'spatEnrObj', feat_type = 'missing'),
          function(x, ...) {
            as.character(colnames(x@enrichDT[, -'cell_ID']))
          })
# no features generic for dimObj


