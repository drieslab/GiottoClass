# docs ----------------------------------------------------------- #
#' @title Spatial binary operations
#' @name spatial_binary_ops
#' @aliases snap erase symdif union intersect
#' @description Perform geometric binary operations on Giotto spatial classes
#' (`giottoPolygon`, `giottoPoints` and `spatLocsObj`) and underlying
#' representations (only terra `SpatVector` right now.)
#' @param x spatial object 1
#' @param y spatial object 2 (can be missing or NULL)
#' @param ... additional args to pass
#' @returns The same class as `x`
#' @usage
#' # S4 methods for giottoPolygon, giottoPoints, spatLocsObj, SpatVector #
#' @examples
#' gpoly1 <- GiottoData::loadSubObjectMini("giottoPolygon")
#' epoly <- as.polygons(ext(c(6600, 6800, -5000, -4800)))
#' gpoly2 <- spatShift(gpoly1, dx = 20)
#'
#' plot(gpoly1)
#' plot(union(gpoly1, gpoly2))
#' plot(erase(gpoly1, epoly))
#'
#' plot(union(gpoly1, epoly))
#'
#' plot(symdif(gpoly1, epoly))
#'
#' plot(intersect(gpoly1, epoly))
#'
#' if (FALSE) {
#'     # takes a long time so don't run in checks
#'     plot(snap(gpoly1, tolerance = 0.2))
#' }
NULL
# ---------------------------------------------------------------- #


# erase ####
#' @rdname spatial_binary_ops
#' @export
setMethod("erase", signature(x = "spatialClasses", y = "spatialClasses"), function(x, y, ...) {
    x0 <- x
    if (inherits(x, "spatLocsObj")) x <- as.points(x)
    if (inherits(y, "spatLocsObj")) y <- as.points(y)
    if (inherits(x, "giottoSpatial")) x <- x[]
    if (inherits(y, "giottoSpatial")) y <- y[]
    res <- erase(x, y, ...)
    x0[] <- res
    x0@unique_ID_cache <- spatIDs(x0, use_cache = FALSE, uniques = TRUE)
    return(x0)
})

# snap ####
#' @rdname spatial_binary_ops
#' @inheritParams terra::snap
#' @export
setMethod("snap", signature("giottoSpatial"), function(x, y = NULL, tolerance, ...) {
    if (inherits(x, "spatLocsObj")) x_use <- as.points(x)
    if (inherits(x, "giottoSpatial")) x_use <- x[]
    if (!is.null(y)) {
        if (inherits(y, "spatLocsObj")) y <- as.points(y)
        if (inherits(y, "giottoSpatial")) y <- y[]
    }
    res <- snap(x_use, y, tolerance, ...)
    x[] <- res
    return(res)
})


# symdif ####
#' @rdname spatial_binary_ops
#' @export
setMethod("symdif", signature(x = "spatialClasses", y = "spatialClasses"), function(x, y, ...) {
    x0 <- x
    if (inherits(x, "spatLocsObj")) x <- as.points(x)
    if (inherits(y, "spatLocsObj")) y <- as.points(y)
    if (inherits(x, "giottoSpatial")) x <- x[]
    if (inherits(y, "giottoSpatial")) y <- y[]
    res <- symdif(x, y, ...)
    x0[] <- res
    x0@unique_ID_cache <- spatIDs(x0, use_cache = FALSE, uniques = TRUE)
    return(x0)
})

# union ####
#' @rdname spatial_binary_ops
#' @export
setMethod("union", signature(x = "spatialClasses", y = "spatialClasses"), function(x, y) {
    x0 <- x
    if (inherits(x, "spatLocsObj")) x <- as.points(x)
    if (inherits(y, "spatLocsObj")) y <- as.points(y)
    if (inherits(x, "giottoSpatial")) x <- x[]
    if (inherits(y, "giottoSpatial")) y <- y[]
    res <- terra::union(x, y)
    x0[] <- res
    x0@unique_ID_cache <- spatIDs(x0, use_cache = FALSE, uniques = TRUE)
    return(x0)
})


# intersect ####

#' @rdname spatial_binary_ops
#' @export
setMethod("intersect", signature(x = "spatialClasses", y = "spatialClasses"), function(x, y) {
    x0 <- x
    if (inherits(x, "spatLocsObj")) x <- as.points(x)
    if (inherits(y, "spatLocsObj")) y <- as.points(y)
    if (inherits(x, "giottoSpatial")) x <- x[]
    if (inherits(y, "giottoSpatial")) y <- y[]
    res <- terra::intersect(x, y)
    x0[] <- res
    x0@unique_ID_cache <- spatIDs(x0, use_cache = FALSE, uniques = TRUE)
    return(x0)
})
