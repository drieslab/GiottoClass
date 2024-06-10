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
#' @param inv logical. Whether the inverse of the affine transform should
#' be applied.
#' @param ... additional args to pass (none implemented)
#' @returns affine transformed object
#' @examples
#' m <- diag(rep(1, 3))
#' trans_m <- matrix(c(1, 0, 0, 0, 1, 0, 200, 300, 1), nrow = 3)
#' scale_m <- matrix(c(2, 0, 0, 0, 3, 0, 0, 0, 1), nrow = 3)
#' aff_m <- matrix(c(2, 3, 0, 0.2, 3, 0, 100, 29, 1), nrow = 3)
#' 
#' gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
#' gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")
#' sl <- GiottoData::loadSubObjectMini("spatLocsObj")
#'
#' # giottoPoints ##############################################
#' plot(gpoints)
#' plot(affine(gpoints, trans_m))
#' 
#' # giottoPolygon #############################################
#' plot(gpoly)
#' plot(affine(gpoly, scale_m))
#'
#' # spatLocsObj ###############################################
#' plot(affine(sl, m))
#' plot(affine(sl, trans_m))
#' plot(affine(sl, scale_m))
#' # this transformation can be inverted
#' aff_sl <- affine(sl, aff_m)
#' plot(aff_sl)
#' plot(affine(aff_sl, aff_m, inv = TRUE))
NULL
# ---------------------------------------------------------------- #

#' @rdname affine
#' @export
setMethod("affine", signature("SpatVector"), function(x, m, inv = FALSE, ...) {
    .affine_sv(x, m, inv, ...)
})

#' @rdname affine
#' @export
setMethod(
    "affine", signature("giottoPoints"),
    function(x, m, inv = FALSE, ...) {
        x[] <- .affine_sv(x = x[], m = m, inv = inv, ...)
        return(x)
    }
)

#' @rdname affine
#' @export
setMethod(
    "affine", signature("giottoPolygon"),
    function(x, m, inv = FALSE, ...) {
        .do_gpoly(x, what = .affine_sv, args = list(m = m, inv = inv, ...))
    }
)

#' @rdname affine
#' @export
setMethod(
    "affine", signature("spatLocsObj"),
    function(x, m, inv = FALSE, ...) {
        x[] <- .affine_dt(
            x = x[], m = m, xcol = "sdimx", ycol = "sdimy", inv = inv, ...
        )
        return(x)
    }
)




# internals ####

# 2D only
.affine_sv <- function(x, m, inv = FALSE, ...) {
    m <- as.matrix(m)
    gtype <- terra::geomtype(x)
    xdt <- data.table::as.data.table(x, geom = "XY")
    
    xdt <- .affine_dt(
        x = xdt, m = m, xcol = "x", ycol = "y", inv = inv, ...
    )
    
    res <- switch(gtype,
      "points" = terra::vect(xdt, geom = c("x", "y")),
      "polygons" = terra::as.polygons(xdt)
    )
    
    return(res)
}

.affine_dt <- function(
        x, m, xcol = "sdimx", ycol = "sdimy", inv = FALSE, ...
) {
    x <- data.table::as.data.table(x)
    m <- as.matrix(m)
    xm <- as.matrix(x[, c(xcol, ycol), with = FALSE])
    
    # translations (if any)
    translation <- NULL
    if (ncol(m) > 2) {
        translation <- m[seq(2), 3] # class: numeric
        if (isTRUE(inv)) translation <- -translation
        if (all(translation == c(0, 0))) translation <- NULL
    }
    
    # inv translation
    if (!is.null(translation) && isTRUE(inv)) {
        xm <- t(t(xm) + translation)
    }
    
    # linear transforms
    aff_m <- m[seq(2), seq(2)]
    if (isTRUE(inv)) aff_m <- solve(aff_m)
    xm <- xm %*% aff_m

    # normal translation
    if (!is.null(translation) && !isTRUE(inv)) {
        xm <- t(t(xm) + translation)
    }

    x[, (xcol) := xm[, 1L]]
    x[, (ycol) := xm[, 2L]]
    
    return(x)
}





#' @name decomp_affine
#' @title Decompose affine matrix into scale, rotation, and shear operations
#' @description Affine transforms are linear transformations that cover scaling,
#' rotation, shearing, and translations. They can be represented as matrices of
#' 2x3 or 3x3 values. This function reads the matrix and extracts the values
#' needed to perform them as a list of class `affine`. Works only for 2D
#' transforms. Logic from \url{https://math.stackexchange.com/a/3521141}
#' @param x object coercible to matrix with a 2x3 or 3x3 affine matrix
#' @returns a list of transforms information.
#' @keywords internal
#' @examples
#' # affine transform matrices
#' m <- diag(rep(1, 3))
#' shear_m <- trans_m <- m
#' trans_m[seq(2), 3] <- c(200, 300)
#' scale_m <- diag(c(2, 3, 1))
#' shear_m[2, 1] <- 2
#' aff_m <- matrix(c(
#'     2, 0.5, 1000, 
#'     -0.3, 3, 20,
#'     100, 29, 1
#' ), nrow = 3, byrow = TRUE)
#' 
#' # show decomps
#' # values are shown in order of operations
#' decomp_affine(m)
#' decomp_affine(trans_m)
#' decomp_affine(scale_m)
#' s <- decomp_affine(shear_m)
#' a <- decomp_affine(aff_m)
#' force(a)
#' 
#' # perform piecewise transforms with decomp
#' 
#' sl_shear_piecewise <- sl %>%
#'     spin(GiottoUtils::degrees(s$rotate), x0 = 0, y0 = 0) %>%
#'     shear(fx = s$shear[["x"]], fy = s$shear[["y"]], x0 = 0, y0 = 0) %>%
#'     rescale(fx = s$scale[["x"]], fy = s$scale[["y"]], x0 = 0, y0 = 0) %>%
#'     spatShift(dx = s$translate[["x"]], dy = s$translate[["y"]])
#' 
#' sl_aff_piecewise <- sl %>%
#'     spin(GiottoUtils::degrees(a$rotate), x0 = 0, y0 = 0) %>%
#'     shear(fx = a$shear[["x"]], fy = a$shear[["y"]], x0 = 0, y0 = 0) %>%
#'     rescale(fx = a$scale[["x"]], fy = a$scale[["y"]], x0 = 0, y0 = 0) %>%
#'     spatShift(dx = a$translate[["x"]], dy = a$translate[["y"]])
#'     
#' plot(affine(sl, shear_m))
#' plot(sl_shear_piecewise)
#' plot(affine(sl, aff_m))
#' plot(sl_aff_piecewise)
#' 
#' @export
#' @seealso [solve.affine_decomp()]
decomp_affine <- function(x) {
    # should be matrix or coercible to matrix
    x <- as.matrix(x)

    a11 <- x[[1, 1]]
    a21 <- x[[2, 1]]
    a12 <- x[[1, 2]]
    a22 <- x[[2, 2]]
    
    res_x <- .decomp_affine_xshear(a11, a21, a12, a22)
    res_y <- .decomp_affine_yshear(a11, a21, a12, a22)

    res_x_s <- .decomp_affine_simplicity(res_x)
    res_y_s <- .decomp_affine_simplicity(res_y)
    
    if (res_y_s > res_x_s) {
        res <- res_y
    } else {
        res <- res_x
    }
    
    # apply xy translations
    if (ncol(x) == 3) {
        res$translate = res$translate + x[seq(2), 3]
    }
    
    res <- structure(
        .Data = c(res, list(matrix = x)), class = "affine_decomp"
    )
    
    return(res)
}

# score decomp solutions based on how simple they are
.decomp_affine_simplicity <- function(affine_res) {
    a <- affine_res

    score <- 0
    score <- score + sum(a$scale == c(1, 1))
    score <- score + sum(a$shear == c(0, 0))
    score <- score + sum(a$rotate == 0)
    
    return(score)
}

.decomp_affine_yshear <- function(a11, a21, a12, a22) {
    sx <- sqrt(a11^2 + a21^2) # scale x
    r <- atan(a21 / a11) # rotation
    msy <- a12 * cos(r) + a22 * sin(r)
    if (sin(r) != 0) { # scale y
        sy <- (msy * cos(r) - a12) / sin(r)
    } else {
        sy <- (a22 - msy * sin(r)) / cos(r)
    }
    m <- msy / sy # y shear (no x shear)
    
    list(
        scale = c(x = sx, y = sy),
        rotate = r,
        shear = c(x = 0, y = m),
        translate = c(x = 0, y = 0),
        order = c("rotate", "shear", "scale", "translate")
    )
}

.decomp_affine_xshear <- function(a11, a21, a12, a22) {
    sy <- sqrt(a12^2 + a22^2) # scale y
    r <- atan(-(a12 / a22)) # rotation
    msx <- a21 * cos(r) - a11 * sin(r)
    if (sin(r) != 0) { # scale y
        sx <- (a21 - msx * cos(r)) / sin(r)
    } else {
        sx <- (a11 + msx * sin(r)) / cos(r)
    }
    m <- msx / sx # y shear (no x shear)
    
    list(
        scale = c(x = sx, y = sy),
        rotate = r,
        shear = c(x = m, y = 0),
        translate = c(x = 0, y = 0),
        order = c("rotate", "shear", "scale", "translate")
    )
}



#' @name print.affine_decomp
#' @title affine_decomp print method
#' @param x object to print
#' @param \dots additional params to pass (none implemented)
#' @keywords internal
#' @export
print.affine_decomp <- function(x, ...) {
    cat("<affine_decomp>\n")
    # reorder in order of piecewise transforms
    x <- x[x$order]
    print_list(x)
}

#' @name solve.affine_decomp
#' @title Solve for inverted 2D affine decomp
#' @param a `affine_decomp` object
#' @param b not used
#' @param \dots additional params to pass (none implemented)
#' @examples
#' sl <- GiottoData::loadSubObjectMini("spatLocsObj")
#' 
#' m <- matrix(c(
#'     2, 0.5, 1000, 
#'     -0.3, 3, 20,
#'     100, 29, 1
#' ), nrow = 3, byrow = TRUE)
#' 
#' a <- decomp_affine(m)
#' i <- solve(decomp) # inverted
#' 
#' aff_sl <- affine(sl, aff_m)
#' inv_sl <- aff_sl %>%
#'     spatShift(dx = i$translate[["x"]], dy = i$translate[["y"]]) %>%
#'     spin(GiottoUtils::degrees(i$rotate), x0 = 0, y0 = 0) %>%
#'     shear(fx = i$shear[["x"]], fy = i$shear[["y"]], x0 = 0, y0 = 0) %>%
#'     rescale(fx = i$scale[["x"]], fy = i$scale[["y"]], x0 = 0, y0 = 0)
#' 
#' plot(aff_sl)
#' plot(sl)
#' plot(inv_sl) # same as original
#' 
#' @export
solve.affine_decomp <- function(a, b, ...) {
    m <- a$matrix[seq(2), seq(2)]
    m <- solve(m)
    # translations <- NULL
    translation <- -a$translate

    inv_m <- rbind(m, c(0,0)) %>%
        cbind(c(translation, 1))
    
    res <- decomp_affine(inv_m)
    res$order <- c("translate", "rotate", "shear", "scale")
    class(res) <- "affine_decomp"
    return(res)
}








