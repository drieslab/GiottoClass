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
#' sl <- GiottoData::loadSubObjectMini("spatLocsObj")
#' gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
#' gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")
#'
#' relate(gpoints, gpoly, relation = "intersects")
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
        output = c("data.table", "list", "matrix"), 
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
