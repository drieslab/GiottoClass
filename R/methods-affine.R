#' @include generics.R
#' @include classes.R

# docs ----------------------------------------------------------- #
#' @title Affine transformations
#' @name affine
#' @description Apply an affine transformation matrix to a spatial object.
#' Currently only works for 2D transforms.
#' @param x object to affine transform or a `matrix`
#' @param y `matrix` or coercible to `matrix` (such as `affine2d`). Should
#' be a matrix with either 2 or 3 columns (linear or affine).
#' @param inv logical. Whether the inverse of the affine transform should
#' be applied.
#' @param ... additional args to pass (none implemented)
#' @returns affine transformed object or an `affine2d` if a `matrix` was
#' passed to `x`
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
#' # creation of affine2d
#' aff <- affine(m)
#' aff <- spin(flip(shear(aff, fx = 0.2)), 45)
#' plot(aff) # blue is start, red is end
#'
#' # giottoPoints ##############################################
#' plot(gpoints)
#' plot(affine(gpoints, trans_m))
#' 
#' # giottoPolygon #############################################
#' plot(gpoly)
#' plot(affine(gpoly, scale_m))
#' plot(affine(gpoly, aff)) # affine() with `affine2d`
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

# * giotto ####
#' @rdname affine
#' @param spat_unit character vector. spatial units to affect. The :all: token
#' to affect all can be used.
#' @param feat_type character vector. feature types to affect. The :all: token
#' to affect all can be used.
#' @param images character vector. Images to affect. The :all: token
#' to affect all can be used.
#' @export
setMethod(
    "affine", signature(x = "giotto", y = "matrix"), function(
        x, y, inv = FALSE, 
        spat_unit = ":all:", feat_type = ":all:", images = ":all:", 
        ...
    ) {
        a <- list(y = y, inv = inv, ...)
        
        spat_unit <- set_default_spat_unit(
            gobject = x, spat_unit = spat_unit
        )
        feat_type <- set_default_feat_type(
            gobject = x, spat_unit = spat_unit, feat_type = feat_type
        )
        
        all_su <- spat_unit == ":all:"
        all_ft <- feat_type == ":all:"
        
        # polygons --------------------------------------------------------- #
        polys <- get_polygon_info_list(
            gobject = x, return_giottoPolygon = TRUE
        )
        if (!all_su) {
            polys <- polys[spatUnit(polys) %in% spat_unit]
        }
        if (!is.null(polys)) {
            for(poly in polys) {
                poly <- do.call(affine, args = c(list(x = poly), a))
                x <- setGiotto(x, poly, verbose = FALSE, initialize = FALSE)
            }
        }
        
        # spatlocs --------------------------------------------------------- #
        sls <- get_spatial_locations_list(
            gobject = x,
            spat_unit = ":all:",
            output = "spatLocsObj",
            copy_obj = FALSE
        )
        if (!all_su) {
            sls[spatUnit(sls) %in% spat_unit]
        }
        if (!is.null(sls)) {
            for (sl in sls) {
                sl <- do.call(affine, args = c(list(x = sl), a))
                x <- setGiotto(x, sl, verbose = FALSE, initialize = FALSE)
            }
            
            # TODO remove this after spatial info is removed from
            # spatialNetwork objs
            sn_list <- get_spatial_network_list(
                gobject = x,
                spat_unit = ":all:",
                output = "spatialNetworkObj",
                copy_obj = FALSE
            )
            if (length(sn_list) > 0) {
                warning(wrap_txt("spatial locations have been modified.
                                Relevant spatial networks may need to be
                                regenerated"), call. = FALSE)
            }
        }
        
        # points ----------------------------------------------------------- #
        
        pts <- get_feature_info_list(
            gobject = x, return_giottoPoints = TRUE
        )
        if (!all_ft) {
            pts <- pts[featType(pts) %in% feat_type]
        }
        if (!is.null(pts)) {
            for(pt in pts) {
                pt <- do.call(affine, args = c(list(x = pt), a))
                x <- setGiotto(x, pt, verbose = FALSE, initialize = FALSE)
            }
        }
        # images ----------------------------------------------------------- #
        
        imgs <- get_giotto_image_list(x)
        if (!is.null(imgs)) {
            if (!inherits(imgs, "list")) imgs <- list(imgs)
            for (img in imgs) {
                img <- do.call(affine, args = c(list(x = img), a))
                x <- setGiotto(x, img, verbose = FALSE)
            }
        }
        
        return(initialize(x)) # init not necessarily needed
    }
)

# * missing, missing ####
#' @rdname affine
#' @export
setMethod("affine", signature(x = "missing", y = "missing"), function(x) {
    new("affine2d", affine = diag(c(1, 1)))
})


# * ANY, missing ####
#' @rdname affine
#' @export
setMethod("affine", signature(x = "ANY", y = "missing"), function(x) {
    x <- as.matrix(x)
    if (ncol(x) <= 3) {
        res <- new("affine2d", affine = x)
    }
    return(res)
})

# * ANY, affine2d ####
#' @rdname affine
#' @export
setMethod("affine", signature(x = "ANY", y = "affine2d"), function(x, y, ...) {
    a <- get_args_list(...)
    a$y <- y@affine
    do.call(affine, args = a)
})

# * SpatVector, matrix ####
#' @rdname affine
#' @export
setMethod("affine", signature(x = "SpatVector", y = "matrix"), 
          function(x, y, inv = FALSE, ...) {
    .affine_sv(x, m = y, inv, ...)
})

# * giottoPoints, matrix ####
#' @rdname affine
#' @export
setMethod(
    "affine", signature(x = "giottoPoints", y = "matrix"),
    function(x, y, inv = FALSE, ...) {
        x[] <- .affine_sv(x = x[], m = y, inv = inv, ...)
        return(x)
    }
)

# * giottoPolygon, matrix ####
#' @rdname affine
#' @export
setMethod(
    "affine", signature(x = "giottoPolygon", y = "matrix"),
    function(x, y, inv = FALSE, ...) {
        .do_gpoly(x, what = .affine_sv, args = list(m = y, inv = inv, ...))
    }
)

# * spatLocsObj, matrix ####
#' @rdname affine
#' @export
setMethod(
    "affine", signature("spatLocsObj", y = "matrix"),
    function(x, y, inv = FALSE, ...) {
        x[] <- .affine_dt(
            x = x[], m = y, xcol = "sdimx", ycol = "sdimy", inv = inv, ...
        )
        return(x)
    }
)

# * giottoLargeImage, matrix ####
#' @rdname affine
#' @export
setMethod("affine", signature(x = "giottoLargeImage", y = "matrix"), function(
        x, y, inv = FALSE, ...
) {
    a <- get_args_list(...)
    a$x <- as(x, "giottoAffineImage") # convert to giottoAffineImage
    res <- do.call(affine, args = a)
    return(res)
})

# * giottoAffineImage, matrix ####
#' @rdname affine
#' @export
setMethod("affine", signature(x = "giottoAffineImage", y = "matrix"), function(
        x, y, inv = FALSE, ...
) {
    a <- get_args_list(...)
    aff <- x@affine
    a$x <- aff
    # update affine
    x@affine <- do.call(affine, args = a)
    return(initialize(x))
})

# * affine2d, matrix ####
#' @rdname affine
#' @export
setMethod("affine", signature(x = "affine2d", y = "matrix"), function(
    x, y, inv = FALSE, ...   
) {
    a <- get_args_list()
    # update linear
    m <- .aff_linear_2d(y)
    if (isTRUE(inv)) m <- solve(m)
    old_aff <- new_aff <- x@affine
    .aff_linear_2d(new_aff) <- .aff_linear_2d(new_aff) %*% m
    
    ## calc shifts ##
    # create dummy
    d <- .bound_poly(x@anchor)
    # perform transforms so far
    a$x <- affine(d, old_aff)
    # perform new transform
    post <- do.call(affine, args = a)
    
    # perform affine & transform without shifts
    b <- a
    b$y <- .aff_linear_2d(y)
    b$x <- affine(d, .aff_linear_2d(old_aff))
    pre <- do.call(affine, args = b)
    
    # find xyshift by comparing tfs so far vs new tf
    xyshift <- .get_centroid_xy(post) - .get_centroid_xy(pre)
    
    # update translate
    .aff_shift_2d(new_aff) <- xyshift 

    x@affine <- new_aff
    return(initialize(x))
})


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


# perform the actual affine operation
.gaffine_realize_magick <- function(x, size = 5e5, ...) {
    mg <- .spatraster_sample_values(x, output = "magick", size = size, ...)
    aff <- x@affine
    
    # create a dummy spatLocsObj to act as control points
    # pt1: bottom left
    # pt2: top left
    # pt3: bottom right
    dummy_sl <- .magick_image_corners(mg)
    aff_dummy_sl <- dummy_sl %>%
        affine(.aff_linear_2d(aff)) %>%
        flip() %>%
        rescale(fx = 1 / abs(aff$scale[["x"]]), #*see below
                fy = 1 / abs(aff$scale[["y"]]))
    # *no scaling should be performed at this step. Otherwise magick
    # will generate a differently sized image during distortion
    # To prevent the scaling change, we use the decomposed scale values.
    # However, flips ARE desired, so we make sure the use the abs() values.
    
    .sl_to_mat <- function(x) {
        x[][, c("sdimx", "sdimy")] %>% t()
    }
    
    # convert spatlocs of dummy points to matrix
    ctrl_pts_a <- .sl_to_mat(flip(dummy_sl))
    ctrl_pts_b <- .sl_to_mat(aff_dummy_sl)
    
    ctrl_pts <- c(
        ctrl_pts_a[, 1], ctrl_pts_b[, 1],
        ctrl_pts_a[, 2], ctrl_pts_b[, 2],
        ctrl_pts_a[, 3], ctrl_pts_b[, 3]
    )
    names(ctrl_pts) <- NULL
    
    mg_aff <- magick::image_distort(
        mg, distortion = "Affine", coordinates = ctrl_pts, bestfit = TRUE
    )
    
    d <- .bound_poly(x) %>%
        affine(aff)
    
    affine_gimg <- giottoImage(
        name = paste(objName(x), "affine", collapse = "_"),
        mg_object = mg_aff,
        minmax = c(xmax_sloc = 10, xmin_sloc = 0, 
                   ymax_sloc = 10, ymin_sloc = 0),
        boundaries = c(xmax_adj = 0, xmin_adj = 0,
                       ymax_adj = 0, ymin_adj = 0)
    )
    
    # assign ext from dummy
    ext(affine_gimg) <- ext(d)
    
    return(initialize(affine_gimg))
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
#' # create affine objects
#' # values are shown in order of operations
#' affine(m)
#' affine(trans_m)
#' affine(scale_m)
#' s <- affine(shear_m)
#' a <- affine(aff_m)
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
.decomp_affine <- function(x) {
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
    } else {
        # append translations
        x <- cbind(x, rep(0, 2L)) %>%
            rbind(c(0, 0, 1))
    }
    
    res$affine <- x
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

## affine matrix manipulation ####
# internal accessors for the relevant parts of 2D affine matrices
.aff_linear_2d <- function(x) {
    if (inherits(x, "affine2d")) x <- x[]
    x[][seq(2), seq(2)]
}

`.aff_linear_2d<-` <- function(x, value) {
    checkmate::assert_matrix(value, nrows = 2L, ncols = 2L)
    if (inherits(x, "affine2d")) {
        x[][seq(2), seq(2)] <- value
        x <- initialize(x)
    }
    else x[seq(2), seq(2)] <- value
    
    return(x)
}

.aff_shift_2d <- function(x) {
    if (inherits(x, "affine2d")) x <- x[]
    x[seq(2), 3]
}

`.aff_shift_2d<-` <- function(x, value) {
    checkmate::assert_numeric(value, len = 2L)
    if (inherits(x, "affine2d")) {
        x[][seq(2), 3] <- value
        x <- initialize(x)
    } else {
        x[seq(2), 3] <- value
    }
    
    return(x)
}




