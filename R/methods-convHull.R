# docs ----------------------------------------------------------- #
#' @title Convex hull, minimal bounding rotated rectangle, and minimal bounding circle
#' @name convHull
#' @aliases minRect minCircle
#' @description Get the convex hull, the minimal bounding rotated rectangle,
#' or minimal bounding circle of a Giotto spatial object or terra SpatVector
#' @param x any of giotto image, giottoPolygon, giottoPoints, spatLocsObj, SpatVector
#' @param by character (variable name), to get a new geometry for groups of input geometries
#' @param \dots additional parameters to pass
#' @examples
#' sl <- GiottoData::loadSubObjectMini("spatLocsObj")
#' gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
#'
#' h <- convHull(sl)
#' plot(h)
#'
#' r <- minRect(sl)
#' plot(r)
#'
#' circ <- minCircle(gpoints, by = "feat_ID")
#' plot(circ, border = rainbow(100))
#'
#' @returns SpatVector
NULL
# ---------------------------------------------------------------- #

#' @rdname convHull
#' @export
setMethod("convHull", signature("spatLocsObj"), function(x, by = "", ...) {
    convHull(x = as.points(x), by = by, ...)
})
#' @rdname convHull
#' @export
setMethod("convHull", signature("giottoSpatial"), function(x, by = "", ...) {
    convHull(x[], by = by, ...)
})


#' @rdname convHull
#' @export
setMethod("minRect", signature("spatLocsObj"), function(x, by = "", ...) {
    minRect(x = as.points(x), by = by, ...)
})
#' @rdname convHull
#' @export
setMethod("minRect", signature("giottoSpatial"), function(x, by = "", ...) {
    minRect(x[], by = by, ...)
})


#' @rdname convHull
#' @export
setMethod("minCircle", signature("spatLocsObj"), function(x, by = "", ...) {
    minCircle(x = as.points(x), by = by, ...)
})
#' @rdname convHull
#' @export
setMethod("minCircle", signature("giottoSpatial"), function(x, by = "", ...) {
    minCircle(x[], by = by, ...)
})
