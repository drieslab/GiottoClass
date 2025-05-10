# docs ----------------------------------------------------------- #
#' @title Get the area of individual polygons
#' @name area
#' @aliases area
#' @description Compute the area covered by polygons
#' @details
#' Giotto's methods do not hook into terra's `area()` generic. This is because
#' `area()` in terra is deprecated in favor of `expanse()`. Additionally,
#' Giotto suppresses warnings about unrecognized CRS, which are currently not
#' as relevant for biological data.
#'
#' @param x `giottoPolygon`
#' @inheritDotParams terra::expanse
#' @returns `numeric` vector of spatial area
#' @examples
#' sl <- GiottoData::loadSubObjectMini("spatLocsObj")
#' gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")
#' gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
#'
#' # area of polygons
#' head(area(gpoly))
#'
#' # area of the convex hull
#' area(hull(sl))
#' feature_hulls <- hull(gpoints, by = "feat_ID")
#' head(area(feature_hulls))
#'
NULL
# ---------------------------------------------------------------- #

#' @rdname area
#' @export
setMethod("area", signature("giottoPolygon"), function(x, ...) {
    area(x[], ...)
})

#' @rdname area
#' @export
setMethod("area", signature("SpatVector"), function(x, ...) {
    area_params <- list(x, ...)
    area_params$transform <- area_params$transform %null% FALSE
    # handle warning about missing CRS
    handle_warnings({
        do.call(terra::expanse, args = area_params)
    })$result
})
