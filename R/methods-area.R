# docs ----------------------------------------------------------- #
#' @title Get the area of individual polygons
#' @name area
#' @description Compute the area covered by polygons
#' @param x `giottoPolygon`
#' @param ... additional args to pass
#' @returns `numeric` vector of spatial area
#' @examples
#' sl <- GiottoData::loadSubObjectMini("spatLocsObj")
#' gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")
#' gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
#'
#' # area of polygons
#' area(gpoly)
#'
#' # area of the convex hull
#' area(convHull(sl))
#' feature_hulls <- convHull(gpoints, by = "feat_ID")
#' area(feature_hulls)
#'
NULL
# ---------------------------------------------------------------- #

#' @rdname area
#' @export
setMethod("area", signature("giottoPolygon"), function(x, ...) {
    # handle warning about missing CRS
    handle_warnings(area(x[], ...))$result
})

#' @rdname area
#' @export
setMethod("area", signature("SpatVector"), function(x, ...) {
    # handle warning about missing CRS
    handle_warnings(terra::expanse(x, transform = FALSE, ...))$result
})
