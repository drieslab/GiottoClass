#' @name spatShift
#' @title Spatially shift an object
#' @param x object
#' @param dx numeric. The shift on the x axis
#' @param dy numeric. The shift on the y axis
#' @param dz numeric. The shift on the z axis
#' @param copy_obj Default = TRUE
#' @param ... additional params to pass to methods
#' @details
#' With the `giotto` object, the ":all:" token can be passed to `spat_unit`,
#' `feat_type`, and `images` arguments to affect all available items.
#'
#' @returns object with shifted spatial locations
#' @description Shift the spatial locations of an object
#' @examples
#' g <- GiottoData::loadSubObjectMini("spatLocsObj")
#'
#' spatShift(g)
NULL




## spatShift ####


#' @rdname spatShift
#' @param spat_unit character vector. spatial units to affect
#' @param feat_type character vector. feature types to affect
#' @param images character vector. Images to affect.
#' @export
setMethod(
    "spatShift",
    signature = "giotto",
    function(
        x, dx = 0, dy = 0,
        spat_unit = ":all:", feat_type = ":all:", images = ":all:",
        space = NULL) {
        # Indirect-usage path: `space = <name|giottoSpace>` records as a
        # recipe step rather than mutating subobjects eagerly.
        if (!is.null(space)) {
            return(.record_space_on_gobject(x, space, "spatShift",
                list(dx = dx, dy = dy)))
        }
        a <- list(dx = dx, dy = dy)

        spat_unit <- set_default_spat_unit(
            gobject = x, spat_unit = spat_unit
        )
        feat_type <- set_default_feat_type(
            gobject = x, spat_unit = spat_unit, feat_type = feat_type
        )

        su_filter <- if (spat_unit == ":all:") NULL else spat_unit
        ft_filter <- if (feat_type == ":all:") NULL else feat_type

        # polygons ---------------------------------------------------------- #
        poly <- x[["spatial_info", spat_unit = su_filter]]
        if (!is.null(poly)) {
            for (p in poly) {
                p <- do.call(spatShift, args = c(list(x = p), a))
                x <- setPolygonInfo(x, p, verbose = FALSE, initialize = FALSE)
            }
        }

        # spatlocs --------------------------------------------------------- #
        sls <- x[["spatial_locs", spat_unit = su_filter]]
        if (!is.null(sls)) {
            for (sl in sls) {
                sl <- do.call(spatShift, args = c(list(x = sl), a))
                x <- setSpatialLocations(x, sl,
                    verbose = FALSE,
                    initialize = FALSE
                )
            }

            # TODO remove this after spatial info is removed from
            # spatialNetwork objs
            sn_list <- x[["spatial_network"]]
            if (length(sn_list) > 0) {
                warning(wrap_txt("spatial locations have been modified.
                                Relevant spatial networks may need to be
                                regenerated"), call. = FALSE)
            }
        }


        # points ----------------------------------------------------------- #
        pts <- x[["feat_info", feat_type = ft_filter]]
        if (!is.null(pts)) {
            for (pt in pts) {
                pt <- do.call(spatShift, args = c(list(x = pt), a))
                x <- setFeatureInfo(x, pt, verbose = FALSE, initialize = FALSE)
            }
        }

        # images ----------------------------------------------------------- #
        imgs <- getGiottoImage(x, name = images)
        if (!is.null(imgs)) {
            if (!inherits(imgs, "list")) imgs <- list(imgs)
            for (img in imgs) {
                img <- do.call(spatShift, args = c(list(x = img), a))
                x <- setGiottoImage(x, img, verbose = FALSE)
            }
        }

        return(initialize(x)) # init not necessarily needed
    }
)

#' @rdname spatShift
#' @export
setMethod(
    "spatShift", signature("SpatExtent"),
    function(x, dx = 0, dy = 0) {
        terra::shift(x, dx = dx, dy = dy)
    }
)


#' @rdname spatShift
#' @export
setMethod(
    "spatShift", signature("spatLocsObj"),
    function(x, dx = 0, dy = 0, dz = 0, copy_obj = TRUE, ...) {
        argslist <- get_args_list()
        argslist$x <- x[]
        argslist$geom <- c("sdimx", "sdimy", "sdimz")

        # pass to data.frame method
        x[] <- do.call(spatShift, argslist)

        return(x)
    }
)

#' @rdname spatShift
#' @param geom character. Named vector of colnames of x, y, (z) coordinate
#' columns. Default is `c("sdimx", "sdimy", "sdimz")`
#' @export
setMethod(
    "spatShift", signature("data.frame"),
    function(
        x, dx = 0, dy = 0, dz = 0, copy_obj = TRUE,
        geom = c("sdimx", "sdimy", "sdimz"), ...) {
        x <- data.table::as.data.table(x)
        x <- .shift_spatial_locations(
            spatlocs = x,
            dx = dx, dy = dy, dz = dz,
            copy_obj = copy_obj,
            geom = geom
        )

        return(x)
    }
)


#' @rdname spatShift
#' @export
setMethod(
    "spatShift", signature("giottoPolygon"),
    function(x, dx = 0, dy = 0, copy_obj = FALSE, ...) {
        .shift_gpoly(gpoly = x, dx = dx, dy = dy, copy_obj = copy_obj, ...)
    }
)

#' @rdname spatShift
#' @export
setMethod(
    "spatShift", signature("giottoPoints"),
    function(x, dx = 0, dy = 0, copy_obj = FALSE, ...) {
        .shift_gpoints(
            gpoints = x, dx = dx, dy = dy, copy_obj = copy_obj, ...
        )
    }
)

#' @rdname spatShift
#' @export
setMethod(
    "spatShift", signature("giottoLargeImage"),
    function(x, dx = 0, dy = 0, copy_obj = FALSE, ...) {
        .shift_large_image(
            image = x, dx = dx, dy = dy, copy_obj = copy_obj, ...
        )
    }
)

#' @rdname spatShift
#' @export
setMethod(
    "spatShift", signature("giottoImage"),
    function(x, dx = 0, dy = 0, ...) {
        .shift_image(image = x, dx = dx, dy = dy, ...)
    }
)

#' @rdname spatShift
#' @export
setMethod(
    "spatShift", signature("giottoAffineImage"),
    function(x, dx = 0, dy = 0, ...) {
        x@affine <- spatShift(x@affine, dx = dx, dy = dy, ...)
        return(initialize(x))
    }
)

#' @rdname spatShift
#' @export
setMethod(
    "spatShift", signature("affine2d"),
    function(x, dx = 0, dy = 0, ...) {
        aff <- x@affine
        aff[seq(2), 3] <- aff[seq(2), 3] + c(dx, dy)
        x@affine <- aff
        return(initialize(x))
    }
)



# internals ####


#' @title Shift spatial locations
#' @name .shift_spatial_locations
#' @description Shift given coordinates by given translation values
#' @param spatlocs spatial locations to use
#' @param dx value to shift coordinates in the positive x direction
#' @param dy value to shift coordinates in the positive y direction
#' @param dz value to shift coordinates in the positive z direction
#' @param geom character vector. Expected colnames of x, y, z coords
#' @param copy_obj logical. copy/duplicate object (default = TRUE)
#' @returns spatial locations
#' @keywords internal
.shift_spatial_locations <- function(spatlocs,
    dx = 0,
    dy = 0,
    dz = 0,
    geom = c("sdimx", "sdimy", "sdimz"),
    copy_obj = TRUE) {
    # catch NULL inputs
    dx <- dx %null% 0
    dy <- dy %null% 0
    dz <- dz %null% 0

    if (copy_obj) spatlocs <- data.table::copy(spatlocs)

    xyz <- c("x", "y", "z")

    if (is.null(names(geom))) names(geom) <- xyz
    if (!all(names(geom) %in% xyz)) stop("geom value names not recognized")
    geom_col <- geom # avoid name collisions with terra "geom" ID column

    spatlocs[, (geom_col[["x"]]) := get(geom_col[["x"]]) + dx]
    spatlocs[, (geom_col[["y"]]) := get(geom_col[["y"]]) + dy]
    if (dz == 0) {
        return(spatlocs)
    } # return early if no z shift

    if (geom_col[["z"]] %in% colnames(spatlocs)) {
        # existing z info
        spatlocs[, (geom_col[["z"]]) := get(geom_col[["z"]]) + dz]
    } else {
        # initialize z info
        spatlocs[, (geom_col[["z"]]) := dz]
    }

    # fix col ordering
    data.table::setcolorder(spatlocs, c(geom_col, "cell_ID"))

    return(spatlocs)
}














#' @rdname spatShift
#' @param ... additional params to pass
#' @keywords internal
#' @noRd
.shift_large_image <- function(
        image,
        dx = 0,
        dy = 0,
        copy_obj = FALSE,
        ...) {
    if (copy_obj) image@raster_object <- terra::deepcopy(image@raster_object)

    if (all(dx == 0, dy == 0)) {
        return(image)
    }

    image@raster_object <- terra::shift(
        image@raster_object,
        dx = dx, dy = dy, ...
    )

    return(image)
}

#' @rdname spatShift
#' @param ... additional params to pass
#' @keywords internal
#' @noRd
.shift_image <- function(
        image,
        dx = 0,
        dy = 0,
        ...) {
    if (all(dx == 0, dy == 0)) {
        return(image)
    }
    e <- ext(image)
    e_shift <- terra::shift(e, dx = dx, dy = dy)
    ext(image) <- e_shift
    return(image)
}

#' @rdname spatShift
#' @keywords internal
#' @noRd
.shift_gpoints <- function(
        gpoints,
        dx = 0,
        dy = 0,
        copy_obj = FALSE,
        ...) {
    if (copy_obj) gpoints@spatVector <- terra::deepcopy(gpoints@spatVector)

    if (!all(dx == 0, dy == 0)) {
        gpoints@spatVector <- terra::shift(
            gpoints@spatVector,
            dx = dx, dy = dy, ...
        )
    }
    gpoints
}

#' @rdname spatShift
#' @keywords internal
#' @noRd
.shift_gpoly <- function(
        gpoly,
        dx = 0,
        dy = 0,
        copy_obj = FALSE,
        ...) {
    if (copy_obj) gpoly@spatVector <- terra::deepcopy(gpoly@spatVector)

    if (!all(dx == 0, dy == 0)) {
        gpoly <- .do_gpoly(
            gpoly,
            what = terra::shift,
            args = list(
                dx = dx,
                dy = dy,
                ...
            )
        )
    }
    gpoly
}
