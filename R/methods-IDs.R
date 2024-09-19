#' @include generics.R
NULL

## spatIDs and featIDs generic ####
#' @title Spatial and feature IDs
#' @name spatIDs-generic
#' @description Get the cell
#' IDs (termed spatial IDs to better reflect when not at the single-cell level)
#' and feature IDs of a giotto object or subobject.
#'
#' \[**`giotto` object specific**\]
#' When applied on a `giotto` object, these functions pull from the `cell_ID`
#' and `feat_ID` slots. The values within these slots are updated whenever the
#' object is data is changed and, importantly, whenever the active spat_unit and
#' feat_type is set (see [activeSpatUnit()] and [activeFeatType()]). New values
#' for these slots are specific to the active spat_unit and feat_type and are
#' detected from either the *subcellular*
#' level (`giottoPolygon` and `giottoPoints`) or the *aggregate*
#' level (expression matrix) data, with a preference for the latter if it
#' exists. Be aware that with this current behavior, values returned
#' by`spatIDs()` and `featIDs()` should be regarded as the minimal set of
#' expected IDs within all `giotto` slots, and not always the exact set or
#' ordering.
#'
#' @aliases spatIDs featIDs
#' @param x an object
#' @param subset logical expression to find a subset of features.
#' @param \dots additional params to pass when used with the `subset` param.
#' For `spatID()`, these pass to [spatValues()]. For `featID()`, these 
#' currently only pass to `fDataDT()`.
#' @returns character vector of cell/spatial IDs or feature IDs
#' @include classes.R
#' @examples
#' g <- GiottoData::loadGiottoMini("vis")
#' spatIDs(g)
#' spatIDs(g, subset = nr_feats <= 200)
#' spatIDs(g, subset = Dim.1 > 25, dim_reduction_to_use = "umap")
#' 
#' featIDs(g)
#' featIDs(g, subset = nr_cells < 100)
#' 
#' gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
#' featIDs(gpoints)
#'
NULL




# spatIDs ####


#' @rdname spatIDs-generic
#' @param spat_unit (optional) specify which spatial unit
#' @export
setMethod(
    "spatIDs", signature(x = "giotto"),
    function(x, spat_unit = NULL, subset, ...) {

        if (missing(subset)) {
            res <- as.character(get_cell_id(gobject = x, spat_unit, ...))
            return(res)
        }
        
        sub_s <- substitute(subset)
        vars <- all.vars(sub_s)
        vals <- lapply(vars, function(v) {
            spatValues(x,
                feats = v,
                spat_unit = spat_unit,
                verbose = FALSE,
                ...
            )
        })
        .dtjoin <- function(x, y) {
            x[y, on = "cell_ID"]
        }
        vals_dt <- Reduce(.dtjoin, vals)
        if (identical(getOption("giotto.verbose"), "debug")) {
            message("data.table used in subset")
            print(vals_dt)
        }
        sids <- subset.data.frame(vals_dt, subset = eval(sub_s))$cell_ID
        return(sids)
    }
)
#' @rdname spatIDs-generic
#' @export
setMethod(
    "spatIDs", signature(x = c("exprObj")),
    function(x, ...) {
        as.character(colnames(x[]))
    }
)
#' @rdname spatIDs-generic
#' @export
setMethod(
    "spatIDs", signature(x = c("spatLocsObj")),
    function(x, ...) {
        as.character(x[]$cell_ID)
    }
)
#' @rdname spatIDs-generic
#' @export
setMethod(
    "spatIDs", signature(x = c("cellMetaObj")),
    function(x, ...) {
        as.character(x[]$cell_ID)
    }
)
#' @rdname spatIDs-generic
#' @export
setMethod(
    "spatIDs", signature(x = c("spatialNetworkObj")),
    function(x, ...) {
        as.character(unique(c(x[]$from, x[]$to)))
    }
)
#' @rdname spatIDs-generic
#' @export
setMethod(
    "spatIDs", signature(x = "dimObj"),
    function(x, ...) {
        as.character(rownames(x@coordinates))
    }
)
#' @rdname spatIDs-generic
#' @param use_cache use cached IDs if available (gpoly and gpoints only)
#' @param uniques return unique ID values
#' only (currently gpoly and gpoints only)
#' @export
setMethod(
    "spatIDs", signature(x = "giottoPolygon"),
    function(x, use_cache = TRUE, uniques = TRUE, ...) {
        if (!all(is.na(x@unique_ID_cache)) &&
            isTRUE(use_cache) &&
            isTRUE(uniques)) {
            return(as.character(x@unique_ID_cache))
        }

        # getting as list first is more performant
        out <- as.character(terra::as.list(x@spatVector)$poly_ID)
        if (isTRUE(uniques)) out <- unique(out)
        return(out)
    }
)
#' @rdname spatIDs-generic
#' @export
setMethod(
    "spatIDs", signature(x = "spatEnrObj"),
    function(x, ...) {
        as.character(x@enrichDT$cell_ID)
    }
)
#' @rdname spatIDs-generic
#' @export
setMethod(
    "spatIDs", signature(x = "nnNetObj"),
    function(x, ...) {
        as.character(unique(names(igraph::V(x@igraph))))
    }
)






# featIDs ####


#' @rdname spatIDs-generic
#' @param feat_type (optional) specify which feature type
#' @export
setMethod(
    "featIDs", signature(x = "giotto"),
    function(x, feat_type = NULL, subset, ...) {
        if (missing(subset)) {
            res <- as.character(get_feat_id(gobject = x, feat_type, ...))
            return(res)
        }
        sub_s <- substitute(subset)
        fx <- fDataDT(x, feat_type = feat_type, ...)
        fids <- subset.data.frame(fx, subset = eval(sub_s))$feat_ID
        return(fids)
    }
)
#' @rdname spatIDs-generic
#' @export
setMethod(
    "featIDs", signature(x = "exprObj"),
    function(x, ...) {
        as.character(rownames(x[]))
    }
)
#' @rdname spatIDs-generic
#' @export
setMethod(
    "featIDs", signature(x = "featMetaObj"),
    function(x, ...) {
        as.character(x[]$feat_ID)
    }
)
#' @rdname spatIDs-generic
#' @export
setMethod(
    "featIDs", signature(x = "giottoPoints"),
    function(x, use_cache = TRUE, uniques = TRUE, ...) {
        if (!all(is.na(x@unique_ID_cache)) &&
            isTRUE(use_cache) &&
            isTRUE(uniques)) {
            return(as.character(x@unique_ID_cache))
        }

        # getting as list is more performant than directly using `$`
        out <- as.character(terra::as.list(x@spatVector)$feat_ID)
        if (isTRUE(uniques)) out <- unique(out)
        return(out)
    }
)
#' @rdname spatIDs-generic
#' @export
setMethod(
    "featIDs", signature(x = "spatEnrObj"),
    function(x, ...) {
        as.character(colnames(x@enrichDT[, -"cell_ID"]))
    }
)
# no features generic for dimObj
