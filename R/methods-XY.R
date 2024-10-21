# docs ----------------------------------------------------------- #
#' @title Spatial coordinates
#' @name XY
#' @aliases XY<-
#' @description Directly get and set the xy(z) coordinates of spatial
#' subobjects (currently `spatLocsObj`, `giottoPoints`, `giottoPolygon`).
#' coordinate values are retrieved and set as `matrix`.
#' @param x object
#' @param value matrix. xy(z) coordinates to set
#' @param ... additional args to pass
#' @returns same class as `x`
#' @examples
#' sl <- GiottoData::loadSubObjectMini("spatLocsObj")
#' gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")
#' gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
#'
#' m1 <- XY(sl)
#' plot(sl)
#' XY(sl) <- m1 + 1000
#' plot(sl)
#' 
#' m2 <- XY(gpoints)
#' plot(gpoints)
#' XY(gpoints) <- m2 * 2 + 1000
#' plot(gpoints)
#' 
#' m3 <- XY(gpoly)
#' plot(gpoly)
#' XY(gpoly) <- m3 / 2
#' plot(gpoly)
#' 
#' XY(gpoly[1:10]) # vertices from first 10 polys
NULL
# ---------------------------------------------------------------- #



# * spatLocsObj ####

#' @rdname XY
#' @export
setMethod("XY", signature("spatLocsObj"), function(x, ...) {
    m <- x[][, colnames(sl) != "cell_ID", with = F] |>
        as.matrix(...)
    if (ncol(m) == 2L) colnames(m) <- c("x", "y")
    if (ncol(m) == 3L) colnames(m) <- c("x", "y", "z")
    return(m)
})

#' @rdname XY
#' @export
setMethod(
    "XY<-", signature(x = "spatLocsObj", value = "matrix"), 
    function(x, value) {
        dt <- data.table::as.data.table(value)
        if (ncol(dt) == 2L) 
            data.table::setnames(dt, new = c("sdimx", "sdimy"))
        if (ncol(dt) == 3L)
            data.table::setnames(dt, new = c("sdimx", "sdimy", "sdimx"))
        x[] <- cbind(dt, x[][, "cell_ID"])
        return(x)
    })

# * giottoPoints & giottoPolygon ####

#' @rdname XY
#' @export
setMethod("XY", signature("giottoPoints"), function(x, ...) {
    return(XY(x[], ...))
})

#' @rdname XY
#' @export
setMethod(
    "XY<-", signature(x = "giottoPoints", value = "ANY"), 
    function(x, ..., value) {
        XY(x[]) <- value
        return(x)
    })

#' @rdname XY
#' @export
setMethod("XY", signature("giottoPolygon"), function(x, ...) {
    return(XY(x[], ...))
})

#' @rdname XY
#' @export
setMethod(
    "XY<-", signature(x = "giottoPolygon", value = "ANY"), 
    function(x, ..., value) {
        XY(x[]) <- value
        return(x)
    })

# * SpatVector ####

#' @rdname XY
#' @param include_geom logical. Whether `geom`, `part`, and `hole` from the
#' terra geometry matrix should be included.
#' @export
setMethod("XY", signature("SpatVector"), function(x, include_geom = FALSE, ...) {
    m <- terra::geom(x, ...)
    if (!include_geom) {
        m <- m[, c("x", "y")]
    }
    return(m)
})

#' @rdname XY
#' @export
setMethod("XY<-", signature(x = "SpatVector", value = "matrix"), function(x, ..., value) {
    switch(terra::geomtype(x),
        "points" = .xy_sv_points_set(x, ..., value = value),
        "polygons" = .xy_sv_polys_set(x, ..., value = value)
    )
})



# internals ####


.xy_sv_points_set <- function(x, ..., value) {
    atts <- terra::values(x)
    v <- terra::vect(value, type = "points", ..., atts = atts)
    return(v)
}

.xy_sv_polys_set <- function(x, ..., value) {
    atts <- terra::values(x)
    if (identical(colnames(x), c("geom", "part", "x", "y", "hole"))) {
        # the entire geom matrix is given. Directly use it.
        v <- terra::vect(value, type = "polygons", ..., atts = atts)
    } else {
        # replace xy values in geom matrix
        m <- terra::geom(x)
        m[, "x"] <- value[, "x"]
        m[, "y"] <- value[, "y"]
        v <- terra::vect(m, type = "polygons", ..., atts = atts)
    }
}







