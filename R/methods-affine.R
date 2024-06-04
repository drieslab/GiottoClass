#' @include generics.R
#' @include classes.R

# docs ----------------------------------------------------------- #
#' @title Apply an affine tranform
#' @name affine
#' @description Apply an affine transformation matrix to a spatial object.
#' Currently only works for 2D transforms.
#' @param x object
#' @param m `matrix` or coercible to `matrix`. Should be a matrix with either
#' 2 or 3 columns (linear or affine).
#' @param ... additional args to pass (none implemented)
#' @returns affine transformed object
#' @examples
#' m <- diag(rep(1, 3))
#' trans_m <- matrix(c(1, 0, 0, 0, 1, 0, 200, 300, 1), nrow = 3)
#' scale_m <- matrix(c(2, 0, 0, 0, 3, 0, 0, 0, 1), nrow = 3)
#' aff_m <- matrix(c(2, 3, 0.43, 0.2, 3, 0, 100, 29, 1), nrow = 3)
#' 
#' gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
#' gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")
#' sl <- GiottoData::loadSubObjectMini("spatLocsObj")
#'
#' # giottoPoints ##############################################
#' plot(affine(gpoints, m))
#' plot(affine(gpoints, trans_m))
#' plot(affine(gpoints, scale_m))
#' plot(affine(gpoints, aff_m))
#' 
#' # giottoPolygon #############################################
#' plot(affine(gpoly, m))
#' plot(affine(gpoly, trans_m))
#' plot(affine(gpoly, scale_m))
#' plot(affine(gpoly, aff_m))
#'
#' # spatLocsObj ###############################################
#' plot(affine(sl, m))
#' plot(affine(sl, trans_m))
#' plot(affine(sl, scale_m))
#' plot(affine(sl, aff_m))
NULL
# ---------------------------------------------------------------- #

#' @rdname affine
#' @export
setMethod("affine", signature("SpatVector"), function(x, m, ...) {
    .affine_sv(x, m, ...)
})

#' @rdname affine
#' @export
setMethod(
    "affine", signature("giottoPoints"),
    function(x, m, ...) {
        x[] <- .affine_sv(x = x[], m = m, ...)
        return(x)
    }
)

#' @rdname affine
#' @export
setMethod(
    "affine", signature("giottoPolygon"),
    function(x, m, ...) {
        .do_gpoly(x, what = .affine_sv, args = list(m = m, ...))
    }
)

#' @rdname affine
#' @export
setMethod(
    "affine", signature("spatLocsObj"),
    function(x, m, ...) {
        x[] <- .affine_dt(x = x[], m = m, xcol = "sdimx", ycol = "sdimy", ...)
        return(x)
    }
)




# internals ####

# 2D only
.affine_sv <- function(x, m, ...) {
    m <- as.matrix(m)
    gtype <- terra::geomtype(x)
    xdt <- data.table::as.data.table(x, geom = "XY")
    
    xdt <- .affine_dt(
        x = xdt, m = m, xcol = "x", ycol = "y", ...
    )
    
    res <- switch(gtype,
                  "points" = terra::vect(xdt, geom = c("x", "y")),
                  "polygons" = terra::as.polygons(xdt)
    )
    
    return(res)
}

.affine_dt <- function(x, m, xcol = "sdimx", ycol = "sdimy", ...) {
    x <- data.table::as.data.table(x)
    m <- as.matrix(m)
    xm <- as.matrix(x[, c(xcol, ycol), with = FALSE])
    
    # linear transforms
    aff_m <- m[seq(2), seq(2)]
    xm <- xm %*% aff_m
    
    # translations (if any)
    if (ncol(m) > 2) {
        translation <- m[seq(2), 3] # class: numeric
        xm <- t(t(xm) + translation)
    }
    
    x[, (xcol) := xm[, 1L]]
    x[, (ycol) := xm[, 2L]]
    
    return(x)
}



#' @name parse_affine
#' @title Read affine matrix for linear transforms
#' @description Affine transforms are linear transformations that cover scaling,
#' rotation, shearing, and translations. They can be represented as matrices of
#' 2x3 or 3x3 values. This function reads the matrix and extracts the values
#' needed to perform them.
#' @param x object coercible to matrix with a 2x3 or 3x3 affine matrix
#' @returns a list of transforms information.
#' @keywords internal
#' @examples
#' m <- diag(rep(1, 3))
#' trans_m <- matrix(c(1, 0, 0, 0, 1, 0, 200, 300, 1), nrow = 3)
#' scale_m <- matrix(c(2, 0, 0, 0, 3, 0, 0, 0, 1), nrow = 3)
#' 
#' aff_m <- matrix(c(2, 3, 0.43, 0.2, 3, 0, 100, 29, 1), nrow = 3)
#' 
#' parse_affine(m)
#' parse_affine(trans_m)
#' parse_affine(scale_m)
#' parse_affine(aff_m)
parse_affine <- function(x) {
    # should be matrix or coercible to matrix
    x <- as.matrix(x)
    
    scale_x <- x[[1, 1]]
    shear_x <- x[[1, 2]]
    translate_x <- x[[1, 3]]
    scale_y <- x[[2, 2]]
    shear_y <- x[[2, 1]]
    translate_y <- x[[2, 3]]
    
    structure(
        .Data =     list(
            scale = c(x = scale_x, y = scale_y),
            rotate = atan(shear_x / scale_x) + atan(shear_y / scale_y),
            shear = c(x = shear_x, y = shear_y),
            translate = c(x = translate_x, y = translate_y),
            matrix = x
        ),
        class = "affine"
    )
}

