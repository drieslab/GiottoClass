# docs ----------------------------------------------------------- #
#' @title Spatial relationships between geometries
#' @name relate
#' @description `relate()` returns a logical matrix indicating the presence or
#'  absence of a specific spatial relationships between the geometries in 
#'  x and y.
#' @param x spatial object with records to test
#' @param y spatial object records to test relations against
#' @param ... additional args to pass
#' @param output character. `"data.table"` or `"matrix"`. `"data.table"` is
#' only possible when `pairs=TRUE`
#' @param use_names logical. If `TRUE`, `pairs=TRUE`, and `output="data.table"`
#' the IDs of the geometries will be used.
#' @returns `data.table` if `output="data.table"`. `matrix` if `output="matrix"`
#' @examples
#' g <- GiottoData::loadGiottoMini("viz")
#' activeSpatUnit(g) <- "aggregate"
#' sl <- g[["spatial_locs"]][[1]]
#' gpoints <- g[["spatial_info]][[1]]
#' gpoly <- g[["feat_info]][[1]]
#'
#' relate(gpoints, gpoly, relation = "intersects")
#' relate(gpoints, gpoly, relation = "intersects", use_names = FALSE)
#' 
#' selection <- system.file("extdata/viz_interactive_select.csv",
#'     package = "GiottoClass"
#' )
#' select_polys <- createGiottoPolygon(data.table::fread(selection))
#' res <- relate(g, select_polys, relation = "intersects")
#' g[,res[y == "polygon1", x]]
#' g[,res[y == "polygon2", x]]
#' g[,res[y == "polygon3", x]]
NULL
# ---------------------------------------------------------------- #

#' @rdname relate
#' @inheritParams terra::relate
#' @export
setMethod(
    "relate", signature(x = "giottoSpatial", y = "giottoSpatial"), 
    function(x, y, relation, 
        pairs = TRUE, 
        na.rm = TRUE,
        output = c("data.table", "matrix"), 
        use_names = TRUE, 
        ...) {
        output <- match.arg(output, choices = c("data.table", "matrix"))

        if (inherits(x, "spatLocsObj")) x_use <- as.points(x)
        if (inherits(y, "spatLocsObj")) y_use <- as.points(y)
        if (inherits(x, "giottoSpatial")) x_use <- x[]
        if (inherits(x, "giottoSpatial")) y_use <- y[]
        
        res <- relate(x_use, y_use, relation, pairs, na.rm, ...)

        if (pairs && output == "data.table") {
            res <- data.table::as.data.table(res)
            data.table::setnames(res, new = c("x", "y"))
            
            if (use_names) {
                x_ids <- .get_ids(x, res$x)
                y_ids <- .get_ids(y, res$y)
                res[, x := x_ids]
                res[, y := y_ids]
            }
        }
        
        return(res)
    }
)

#' @rdname relate
#' @param what character. Which type of spatial data in the `giotto` object to
#' relate. One of "polygon", "spatlocs", "points"
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param spat_locs_name name of spatlocs to use if what = "spatlocs"
#' @export
setMethod(
    "relate", signature(x = "giotto", y = "giottoSpatial"),
    function(x, y, ..., 
             what = c("polygon", "spatlocs", "points"), 
             spat_unit = NULL,
             feat_type = NULL,
             spat_locs_name = NULL) {
        
        what <- match.arg(what, c("polygon", "spatlocs", "points"))
        
        spat_unit <- set_default_spat_unit(x, spat_unit = spat_unit)
        feat_type <- set_default_feat_type(
            x, spat_unit = spat_unit, feat_type = feat_type
        )
        
        x <- switch(what,
            "polygon" = {
                getPolygonInfo(x, 
                    polygon_name = spat_unit, 
                    return_giottoPolygon = TRUE
                )
            },
            "points" = {
                getFeatureInfo(x, 
                    feat_type = feat_type,
                    return_giottoPoints = TRUE
                )
            },
            "spatlocs" = {
                getSpatialLocations(x,
                    spat_unit = spat_unit,
                    output = "spatLocsObj",
                    name = spat_locs_name
                )
            }
        )
        
        res <- relate(x, y, ...)
        return(res)
    }
)






# internals ####

.get_ids <- function(x, idx) {
    ids <- x[idx]$cell_ID
    ids <- ids %null% x[idx]$feat_ID
    ids <- ids %null% x[idx]$poly_ID
    if (is.null(ids)) {
        stop("no ids found for an object. `use_names` might not work", 
             call. = FALSE)
    }
    return(ids)
}
