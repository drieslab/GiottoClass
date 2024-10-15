# docs ----------------------------------------------------------- #
#' @title Spatial binary operations
#' @name spat_binary_ops
#' @description Perform geometric binary operations on Giotto spatial classes
#' (`giottoPolygon`, `giottoPoints` and `spatLocsObj`) and underlying
#' representations (only terra `SpatVector` right now.)
#' @param x,e1 spatial object 1
#' @param y,e2 spatial object 2 (can be missing or NULL)
#' @param ... additional args to pass
#' @returns The same class as `e1` or `x`
#' @usage
#' # S4 methods for giottoPolygon, giottoPoints, spatLocsObj, SpatVector #
#' @examples
#' gpoly1 <- GiottoData::loadSubObjectMini("giottoPolygon")
#' epoly <- as.polygons(ext(c(6600, 6800, -5000, -4800)))
#' gpoly2 <- spatShift(gpoly1, dx = 20)
#' 
#' plot(gpoly1)
#' plot(gpoly1 - gpoly2)
#' plot(gpoly1 - epoly)
#' 
#' plot(gpoly1 + epoly)
#' 
#' plot(symdif(gpoly1, epoly))
#' 
#' plot(intersect(gpoly1, epoly))
#' 
#' if (FALSE) {
#'   # takes a long time so don't run in checks
#'   plot(snap(gpoly1, tolerance = 0.2))
#' }
NULL
# ---------------------------------------------------------------- #


# + ####
#' @rdname spat_binary_ops
#' @usage e1 + e2
#' @export
setMethod("+", signature(e1 = "spatialClasses", e2 = "spatialClasses"), function(e1, e2) {
    e0 <- e1
    if (inherits(e1, "spatLocsObj")) e1 <- as.points(e1)
    if (inherits(e2, "spatLocsObj")) e2 <- as.points(e2)
    if (inherits(e1, "giottoSpatial")) e1 <- e1[]
    if (inherits(e2, "giottoSpatial")) e2 <- e2[]
    res <- e1 + e2
    e0[] <- res
    initialize(e0)
    e0@unique_ID_cache <- spatIDs(e0, use_cache = FALSE, uniques = TRUE)
    return(e0)
})




# - / erase ####
#' @rdname spat_binary_ops
#' @usage e1 - e2
#' @export
setMethod("-", signature(e1 = "spatialClasses", e2 = "spatialClasses"), function(e1, e2) {
    e0 <- e1
    if (inherits(e1, "spatLocsObj")) e1 <- as.points(e1)
    if (inherits(e2, "spatLocsObj")) e2 <- as.points(e2)
    if (inherits(e1, "giottoSpatial")) e1 <- e1[]
    if (inherits(e2, "giottoSpatial")) e2 <- e2[]
    res <- e1 - e2
    e0[] <- res
    e0@unique_ID_cache <- spatIDs(e0, use_cache = FALSE, uniques = TRUE)
    return(e0)
})


#' @rdname spat_binary_ops
#' @inheritParams terra::snap
#' @usage snap(x, y=NULL, tolerance)
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
#' @rdname spat_binary_ops
#' @usage symdif(x, y)
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
#' @rdname spat_binary_ops
#' @usage union(x, y)
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

#' @rdname spat_binary_ops
#' @usage intersect(x, y)
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


