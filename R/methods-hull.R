# docs ----------------------------------------------------------- #
#' @title Convex, concave, rectangular and circular hulls
#' @name hull
#' @aliases minRect minCircle convHull
#' @description Compute a hull around Giotto spatial object or terra SpatVector.
#' The concaveness of the concave hull can be specified in different ways.
#' @param x any of giotto image, giottoPolygon, giottoPoints, spatLocsObj, SpatVector
#' @param by character (variable name), to get a new geometry for groups of input geometries
#' @inheritParams terra::hull
#' @inheritDotParams terra::hull
#' @examples
#' sl <- GiottoData::loadSubObjectMini("spatLocsObj")
#' gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
#'
#' h <- hull(sl)
#' plot(h)
#'
#' r <- hull(sl, type = "rectangle")
#' plot(r)
#'
#' circ <- hull(gpoints, type = "circle", by = "feat_ID")
#' plot(circ, border = rainbow(100))
#'
#' plot(hull(sl, type = "concave_ratio", param = 0.15, allowHoles = FALSE))
#'
#' @returns SpatVector
NULL
# ---------------------------------------------------------------- #

#' @rdname hull
#' @export
setMethod("hull", signature("spatLocsObj"), function(x, by = "", param = 1, allowHoles = TRUE, tight = TRUE, ...) {
    hull(
        x = as.points(x),
        by = by,
        param = param,
        allowHoles = allowHoles,
        tight = tight,
        ...
    )
})
#' @rdname hull
#' @export
setMethod("hull", signature("giottoSpatial"), function(x, by = "", param = 1, allowHoles = TRUE, tight = TRUE, ...) {
    hull(
        x = x[],
        by = by,
        param = param,
        allowHoles = allowHoles,
        tight = tight,
        ...
    )
})

#' @rdname hull
#' @export
minRect <- function(x, ...) {
    warning("minRect() is deprecated. Please use hull() in the future.",
            call. = FALSE)
    hull(x, type = "rectangle", ...)
}

#' @rdname hull
#' @export
minCircle <- function(x, ...) {
    warning("minCircle() is deprecated. Please use hull() in the future.",
            call. = FALSE)
    hull(x, type = "circle", ...)
}

#' @rdname hull
#' @export
convHull <- function(x, ...) {
    warning("convHull() is deprecated. Please use hull() in the future.",
            call. = FALSE)
    hull(x, type = "convex", ...)
}
