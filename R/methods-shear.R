#' @include generics.R
#' @include classes.R

# docs ----------------------------------------------------------- #
#' @title Apply a shear tranform
#' @name shear
#' @description Apply shear transformation to a spatial object.
#' Currently only works for 2D transforms. This implementation
#' applies a shear along one axis by adding the value of the other
#' axis after a multiplicative factor `fx` or `fy` is applied.
#' @param x object
#' @param fx numeric. x shear
#' @param fy numeric. y shear
#' @param x0 numeric. x-origin of shear
#' @param y0 numeric. y-origin of shear
#' @param ... additional args to pass (none implemented)
#' @returns shear transformed object
#' @examples
#' sl <- GiottoData::loadSubObjectMini("spatLocsObj")
#' 
#' plot(shear(sl, fx = 2))
#' 
#' # equivalent affine transform
#' shear_m <- diag(rep(1, 3))
#' shear_m[2, 1] <- 2
#' plot(affine(sl, shear_m))
#' plot(shear(sl, fx = 2, x0 = 0, y0 = 0))
NULL
# ---------------------------------------------------------------- #

# * spatLocsObj ####
#' @rdname shear
#' @export
setMethod("shear", signature("spatLocsObj"), function(
        x, fx = 0, fy = 0, x0, y0, ...
) {
    a <- get_args_list(...)
    a$x <- x[]
    x[] <- do.call(.shear_dt, args = a)
    return(x)
})

# * SpatVector ####
#' @rdname shear
#' @export
setMethod("shear", signature("SpatVector"), function(
        x, fx = 0, fy = 0, x0, y0, ...
) {
    a <- get_args_list(...)
    do.call(.shear_sv, args = a)
})

# * giottoPoints ####
#' @rdname shear
#' @export
setMethod("shear", signature("giottoPoints"), function(
        x, fx = 0, fy = 0, x0, y0, ...
) {
    a <- get_args_list(...)
    a$x <- x[]
    res <- do.call(.shear_sv, args = a)
    x[] <- res
    return(x)
})

# * giottoPolygon ####
#' @rdname shear
#' @export
setMethod("shear", signature("giottoPolygon"), function(
        x, fx = 0, fy = 0, x0, y0, ...
) {
    a <- get_args_list(...)
    a$x <- NULL
    .do_gpoly(x, what = .shear_sv, args = a)
})

# * giottoLargeImage ####
#' @rdname shear
#' @export
setMethod("shear", signature("giottoLargeImage"), function(
        x, fx = 0, fy = 0, x0, y0, ...
) {
    a <- get_args_list(...)
    a$x <- as(x, "giottoAffineImage") # convert to giottoAffineImage
    res <- do.call(shear, args = a)
    return(res)
})

# * giottoAffineImage ####
#' @rdname shear
#' @export
setMethod("shear", signature("giottoAffineImage"), function(
        x, fx = 0, fy = 0, x0, y0, ...
) {
    a <- get_args_list(...)
    a$x <- x@affine
    # update affine
    x@affine <- do.call(shear, args = a)
    
    return(initialize(x))
})

# * affine2d ####
setMethod("shear", signature("affine2d"), function(
         x, fx = 0, fy = 0, x0, y0, ...
    ) {
    a <- get_args_list(...)
 
    # update linear
    shear_x <- matrix(c(1, fx, 0, 1), ncol = 2)
    shear_y <- matrix(c(1, 0, fy, 1), ncol = 2)
    old_aff <- new_aff <- x@affine
    .aff_linear_2d(new_aff) <- .aff_linear_2d(new_aff) %*% shear_x %*% shear_y
    
    ## calc shifts ##
    # create dummy
    d <- .bound_poly(x@anchor)
    # perform transforms so far
    a$x <- affine(d, old_aff)
    # perform new transform
    post <- do.call(shear, args = a)
    
    # perform affine & transform without shifts
    b <- a
    b$x0 <- b$y0 <- 0
    b$x <- affine(d, .aff_linear_2d(old_aff))
    pre <- do.call(shear, args = b)
    
    # find xyshift by comparing tfs so far vs new tf
    xyshift <- .get_centroid_xy(post) - .get_centroid_xy(pre)
    
    # update translate
    .aff_shift_2d(new_aff) <- xyshift
    
    x@affine <- new_aff
    return(initialize(x))
})



# internals ####

.shear_dt <- function(
        x, fx = 0, fy = 0, x0, y0, geom = c("sdimx", "sdimy", "sdimz"), ...
) {
    x <- data.table::copy(x)
    xyz <- tail(letters, 3L)
    if (is.null(names(geom))) names(geom) <- xyz
    if (!all(names(geom) %in% xyz)) stop("geom value names not recognized")
    geom_col <- geom # avoid name collisions with terra "geom" ID column

    # find center
    if (missing(x0)) x0 <- x[, mean(range(get(geom_col[["x"]])))]
    if (missing(y0)) y0 <- x[, mean(range(get(geom_col[["y"]])))]
    scenter <- c(x0, y0)
    
    if (!all(scenter == c(0, 0))) {
        # center values
        x <- spatShift(x, dx = -x0, dy = -y0, geom = geom)
    }
    
    # perform shears
    if (fx != 0) {
        x[, (geom[["x"]]) := get(geom_col[["y"]]) * fx + get(geom_col[["x"]])]
    }
    if (fy != 0) {
        x[, (geom[["y"]]) := get(geom_col[["x"]]) * fy + get(geom_col[["y"]])]
    }
    
    if (!all(scenter == c(0, 0))) {
        # return values to original positions
        x <- spatShift(x, dx = x0, dy = y0, geom = geom)
    }
    
    return(x)
}

.shear_sv <- function(
        x, fx = 0, fy = 0, x0, y0, geom = tail(letters, 3L), ...
) {
    a <- get_args_list(...)
    gtype <- terra::geomtype(x)
    a$x <- data.table::as.data.table(x, geom = "XY")
    res <- do.call(.shear_dt, args = a)
    
    res <- switch(gtype,
        "points" = terra::vect(res, geom = c("x", "y")),
        "polygons" = terra::as.polygons(res)
    )
    
    return(res)
}

