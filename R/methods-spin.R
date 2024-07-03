## spin ####

#' @title Spin an object
#' @name spin
#' @description Spin (rotate) an object spatially (limited to xy rotations)
#' @param x object
#' @param angle numeric. Angle of rotation in degrees
#' @param x0 numeric. x-coordinate of the center of rotation. Defaults to
#' center x val if not given.
#' @param y0 numeric. y-coordinate of the center of rotation. Defaults to
#' center y val if not given.
#' @returns spun object
#' @examples
#' g <- GiottoData::loadSubObjectMini("spatLocsObj")
#'
#' spin(g)
NULL


#' @rdname spin
#' @param spat_unit character vector. spatial units to affect
#' @param feat_type character vector. feature types to affect
#' (giottoPoints only).
#' @export
setMethod(
    "spin", signature(x = "giotto"),
    function(
        x, angle, x0 = NULL, y0 = NULL, spat_unit = ":all:",
        feat_type = ":all:") {
        a <- list(angle = angle, x0 = x0, y0 = y0)

        checkmate::assert_character(spat_unit)
        checkmate::assert_character(feat_type)
        all_su <- spat_unit == ":all:"
        all_ft <- feat_type == ":all:"

        # no need to set default spat_unit and feat_type. NULL is
        # acceptable input

        # polygons --------------------------------------------------------- #
        poly <- get_polygon_info_list(
            gobject = x, return_giottoPolygon = TRUE
        )
        if (!all_su) {
            poly <- poly[spatUnit(poly) %in% spat_unit]
        }
        if (!is.null(poly)) {
            for (p in poly) {
                p <- do.call(spin, args = c(list(x = p), a))
                x <- setPolygonInfo(x, p, verbose = FALSE, initialize = FALSE)
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
                sl <- do.call(spin, args = c(list(x = sl), a))
                x <- setSpatialLocations(x, sl,
                    verbose = FALSE,
                    initialize = FALSE
                )
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
            for (pt in pts) {
                pt <- do.call(spin, args = c(list(x = pt), a))
                x <- setFeatureInfo(x, pt, verbose = FALSE, initialize = FALSE)
            }
        }


        return(initialize(x)) # init not necessarily needed
    }
)





#' @rdname spin
#' @export
setMethod(
    "spin", signature(x = "giottoPolygon"),
    function(x, angle, x0 = NULL, y0 = NULL) {
        if (is.null(x0)) x0 <- terra::mean(terra::ext(x@spatVector))[1]
        if (is.null(y0)) y0 <- terra::mean(terra::ext(x@spatVector))[2]
        return(.do_gpoly(
            x = x, what = "terra"::"spin",
            args = list(angle = angle, x0 = x0, y0 = y0)
        ))
    }
)

#' @rdname spin
#' @export
setMethod(
    "spin", signature(x = "giottoPoints"),
    function(x, angle, x0 = NULL, y0 = NULL) {
        if (is.null(x0)) x0 <- terra::mean(terra::ext(x@spatVector))[1]
        if (is.null(y0)) y0 <- terra::mean(terra::ext(x@spatVector))[2]
        x@spatVector <- terra::spin(x@spatVector,
            angle = angle,
            x0 = x0,
            y0 = y0
        )
        return(x)
    }
)

#' @rdname spin
#' @param z0 spatLocsObj specific. Numeric. z-coordinate of the center of
#' rotation. Depending on if z data is present, defaults to either 0 or
#' center z val if not given.
#' @param xy_angle spatLocsObj specific. xy plane rotation in degrees.
#' Overrides angle param
#' @param zy_angle spatLocsObj specific. zy plane rotation
#' @param xz_angle spatLocsObj specific. xz plane rotation
#' @export
setMethod(
    "spin", signature(x = "spatLocsObj"),
    function(x, angle = NULL, x0 = NULL, y0 = NULL, z0 = NULL,
    xy_angle = NULL, zy_angle = NULL, xz_angle = NULL) {
        argslist <- get_args_list()
        argslist$x <- x[]

        # pass to data.frame method
        x[] <- do.call(spin, argslist)

        return(x)
    }
)


# TODO can this one be made internal?
#' @rdname spin
#' @param geom character. Named vector of colnames of x, y, (z) coordinate
#' columns. Default is `c("sdimx", "sdimy", "sdimz")`
#' @export
setMethod(
    "spin",
    signature(x = "data.frame"),
    function(x, angle = NULL, x0 = NULL, y0 = NULL, z0 = NULL,
    xy_angle = NULL, zy_angle = NULL, xz_angle = NULL,
    geom = c("sdimx", "sdimy", "sdimz")) {
        x <- data.table::as.data.table(x)

        if (!is.null(angle)) xy_angle <- angle
        if (is.null(xy_angle)) xy_angle <- 0
        if (is.null(zy_angle)) zy_angle <- 0
        if (is.null(xz_angle)) xz_angle <- 0
        angles <- c(xy = xy_angle, zy = zy_angle, xz = xz_angle)
        angles <- radians(angles)

        # get rotation centers
        if (is.null(x0)) x0 <- x[, mean(range(get(geom[1L]))), ]
        if (is.null(y0)) y0 <- x[, mean(range(get(geom[2L])))]

        if (geom[3L] %in% colnames(x)) {
            if (is.null(z0)) {
                z0 <- x[, mean(range(get(geom[3L])))]
            } else {
                z0 <- 0
            }
        }

        # perform rotation(s)
        x <- .rotate_spatial_locations(
            spatlocs = x,
            rotateradians = angles,
            rcenter = c(x = x0, y = y0, z = z0)
        )
        return(x)
    }
)


#' @rdname spin
#' @export
setMethod("spin", signature("giottoLargeImage"), function(
        x, angle = NULL, x0 = NULL, y0 = NULL, ...
) {
    a <- get_args_list(...)
    a$x <- as(x, "giottoAffineImage") # convert to giottoAffineImage
    res <- do.call(spin, args = a)
    return(res)
})

#' @rdname spin
#' @export
setMethod("spin", signature("giottoAffineImage"), function(
        x, angle = NULL, x0 = NULL, y0 = NULL, ...
) {
    a <- get_args_list(...)
    a$x <- x@affine
    # update affine
    x@affine <- do.call(spin, args = a)
    
    return(initialize(x))
})

#' @rdname spin
#' @export
setMethod("spin", signature("affine2d"), function(
        x, angle = NULL, x0 = NULL, y0 = NULL
) {
    a <- get_args_list()
    # remove from args list if not provided
    if (is.null(x0)) a$x0 <- NULL
    if (is.null(y0)) a$y0 <- NULL
    # update linear
    r <- radians(angle)
    rotate_m <- matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2L)
    old_aff <- new_aff <- x@affine
    .aff_linear_2d(new_aff) <- .aff_linear_2d(new_aff) %*% rotate_m
    
    ## calc shifts ##
    # create dummy
    d <- .bound_poly(x@anchor)
    # perform transforms so far
    a$x <- affine(d, old_aff)
    # perform new transform
    post <- do.call(spin, args = a)
    
    # perform affine & transform without shifts
    b <- a
    b$x0 <- b$y0 <- 0
    b$x <- affine(d, .aff_linear_2d(old_aff))
    pre <- do.call(spin, args = b)
    
    # find xyshift by comparing tfs so far vs new tf
    xyshift <- .get_centroid_xy(post) - .get_centroid_xy(pre)
    
    # update translate
    .aff_shift_2d(new_aff) <- xyshift
    
    x@affine <- new_aff
    return(initialize(x))
})

# internals ####

# Accepts a data.table with coordinates information
# 2D rotations always involve values across two coordinate axes. In this
# function, the 2 axes across which the rotation are applied are referred to
# as x and y. The columns in the data.table that contain the coordinate
# values are provided through the xy param.
# Either rotate_rad or rotate_deg may be provided. Internally, the function
# converts everything to radians.
.rotate_2d <- function(
        DT, xy = c("x", "y"), rotate_rad = NULL,
        rotate_deg = NULL) {
    # send error if both angle inputs exist or both are missing
    if (is.null(rotate_rad) && is.null(rotate_deg) ||
        !is.null(rotate_rad) && !is.null(rotate_deg)) {
        .gstop("rotation must be supplied through one of 'rotate_rad'
            or 'rotate_deg'")
    }

    # ensure that rotate_rad exists
    if (!is.null(rotate_deg)) rotate_rad <- radians(deg = rotate_deg)

    # perform coordinate rotation
    xvals <- DT[, xy[[1L]], with = FALSE]
    yvals <- DT[, xy[[2L]], with = FALSE]
    DT[, (xy[[1L]]) := xvals * cos(rotate_rad) + yvals * sin(rotate_rad)]
    DT[, (xy[[2L]]) := -xvals * sin(rotate_rad) + yvals * cos(rotate_rad)]
    return(DT)
}



#' @title Rotate spatial locations
#' @name .rotate_spatial_locations
#' @description Rotate given spatlocs by given radians
#' @param spatlocs spatial locations to use
#' @param rotateradians Named vector of radians for rotation along each of
#' the 3 coordinate axes. If only a single value is provided, it will be
#' treated as xy rotation.
#' @param rcenter center of rotation given as vector xy(z)
#' coordinates (defaults to coordinate center)
#' @param geom character. Named vector of colames of x, y, z coordinate columns.
#' Default is `c("sdimx", "sdimy", "sdimz")`
#' @returns spatial locations
#' @details Radians are provided through \code{rotateradians} param as a named
#' vector with values for \code{xy} (yaw), \code{zy} (pitch), \code{xz} (roll)
#' @keywords internal
.rotate_spatial_locations <- function(spatlocs,
    rotateradians = c(xy = 0, zy = 0, xz = 0),
    rcenter = c(0, 0, 0),
    geom = c("sdimx", "sdimy", "sdimz")) {
    checkmate::assert_data_table(spatlocs)

    xyz <- c("x", "y", "z")
    if (is.null(names(rcenter))) names(rcenter) <- xyz
    if (is.null(names(geom))) names(geom) <- xyz

    if (length(rotateradians) == 1) {
        rotateradians <- c(
            xy = rotateradians, zy = 0, xz = 0
        )
    }
    if (!all(names(rotateradians) %in% c("xy", "zy", "xz"))) {
        stop("rotateradians value names not recognized")
    }
    if (!all(names(rcenter) %in% xyz)) {
        stop("rcenter value names not recognized")
    }
    if (!all(names(geom) %in% xyz)) stop("geom value names not recognized")

    hasZ <- geom[["z"]] %in% colnames(spatlocs)

    # xy center of rotation adjustment
    spatlocs[, (geom[["x"]]) := get(geom[["x"]]) - rcenter[["x"]]]
    spatlocs[, (geom[["y"]]) := get(geom[["y"]]) - rcenter[["y"]]]

    # Perform rotation XY
    if (rotateradians[["xy"]] != 0) {
        spatlocs <- .rotate_2d(
            spatlocs,
            xy = c("sdimx", "sdimy"),
            rotate_rad = rotateradians[["xy"]]
        )
    }

    # if z values are available
    if (hasZ) {
        # z center of rotation adjustment
        spatlocs[, (geom[["z"]]) := get(geom[["z"]]) - rcenter[["z"]]]

        zvals <- spatlocs$sdimz

        # Perform rotations
        if (rotateradians[["zy"]] != 0) {
            spatlocs <- .rotate_2d(
                spatlocs,
                xy = c(geom[["z"]], geom[["y"]]),
                rotate_rad = rotateradians[["zy"]]
            )
        }

        if (rotateradians[["xz"]] != 0) {
            spatlocs <- .rotate_2d(
                spatlocs,
                xy = c(geom[["x"]], geom[["z"]]),
                rotate_rad = rotateradians[["xz"]]
            )
        }

        # Revert z center of rotation adjustment
        spatlocs[, (geom[["z"]]) := get(geom[["z"]]) + rcenter[["z"]]]
    }

    # Revert xy center of rotation adjustment
    spatlocs[, (geom[["x"]]) := get(geom[["x"]]) + rcenter[["x"]]]
    spatlocs[, (geom[["y"]]) := get(geom[["y"]]) + rcenter[["y"]]]

    return(spatlocs)
}
