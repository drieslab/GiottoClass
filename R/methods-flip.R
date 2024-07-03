# docs ----------------------------------------------------------- #
#' @title Flip an object
#' @name flip
#' @description Flip an object over a designated x or y value depending on
#' direction param input. Note that this behavior may be different from terra's
#' @param x object
#' @param direction character. Direction to flip. Should be either partial
#' match to 'vertical' or 'horizontal'
#' @param x0 x value to flip horizontally over (ignored for vertical). Pass NULL
#' to flip over the extent
#' @param y0 y value to flip vertically over (ignored for horizontal). Pass NULL
#' to flip over the extent
#' @param ... additional args to pass
#' @returns flipped object
#' @examples
#' g <- GiottoData::loadSubObjectMini("spatLocsObj")
#'
#' flip(g)
NULL
# ---------------------------------------------------------------- #

#' @rdname flip
#' @param spat_unit character vector. spatial units to affect
#' @param feat_type character vector. feature types to affect
#' @export
setMethod(
    "flip", signature("giotto"),
    function(x, direction = "vertical",
    x0 = 0, y0 = 0,
    spat_unit = ":all:", feat_type = ":all:",
    ...) {
        a <- list(direction = direction, x0 = x0, y0 = y0, ...)

        checkmate::assert_character(spat_unit)
        checkmate::assert_character(feat_type)
        all_su <- spat_unit == ":all:"
        all_ft <- feat_type == ":all:"

        # no need to set default spat_unit and feat_type. NULL is acceptable
        # input

        # polygons --------------------------------------------------------- #
        poly <- get_polygon_info_list(
            gobject = x, return_giottoPolygon = TRUE
        )
        if (!all_su) {
            poly <- poly[spatUnit(poly) %in% spat_unit]
        }
        if (!is.null(poly)) {
            for (p in poly) {
                p <- do.call(flip, args = c(list(x = p), a))
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
                sl <- do.call(flip, args = c(list(x = sl), a))
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
                pt <- do.call(flip, args = c(list(x = pt), a))
                x <- setFeatureInfo(x, pt, verbose = FALSE, initialize = FALSE)
            }
        }


        return(initialize(x)) # init not necessarily needed
    }
)

#' @describeIn flip Flip a giottoPolygon object
#' @export
setMethod(
    "flip", signature(x = "giottoPolygon"),
    function(x, direction = "vertical", x0 = 0, y0 = 0, ...) {
        .flip_gpoly(gpoly = x, direction = direction, x0 = x0, y0 = y0)
    }
)

#' @describeIn flip Flip a giottoPoints object
#' @export
setMethod(
    "flip", signature(x = "giottoPoints"),
    function(x, direction = "vertical", x0 = 0, y0 = 0, ...) {
        .flip_gpoints(gpoints = x, direction = direction, x0 = x0, y0 = y0)
    }
)

#' @describeIn flip Flip a spatLocsObj
#' @export
setMethod(
    "flip", signature(x = "spatLocsObj"),
    function(x, direction = "vertical", x0 = 0, y0 = 0, ...) {
        .flip_spatlocs(sl = x, direction = direction, x0 = x0, y0 = y0)
    }
)

#' @describeIn flip Flip a spatialNetworkObj
#' @export
setMethod(
    "flip", signature(x = "spatialNetworkObj"),
    function(x, direction = "vertical", x0 = 0, y0 = 0, ...) {
        .flip_spatnet(sn = x, direction = direction, x0 = x0, y0 = y0)
    }
)

# TODO apply as instructions for lazy eval after crop/resampling
#' @describeIn flip Flip a giottoLargeImage
#' @export
setMethod(
    "flip", signature(x = "giottoLargeImage"),
    function(x, direction = "vertical", x0 = 0, y0 = 0, ...) {
        .flip_large_image(image = x, direction = direction, x0 = x0, y0 = y0)
    }
)

#' @describeIn flip Flip a SpatExtent
#' @export
setMethod(
    "flip", signature(x = "SpatExtent"),
    function(x, direction = "vertical", x0 = 0, y0 = 0) {
        .flip_extent(e = x, direction = direction, x0 = x0, y0 = y0)
    }
)



# internals ####
#
#
#' @name .flip_gpoly
#' @title Flip a giottoPolygon object
#' @description Flip a giottoPolygon over a designated x or y value depending on
#' direction param input. Note that this behavior is different from terra's
#' implementation of flip for SpatVectors where flips happen over the extent
#' @param gpoly giottoPolygon
#' @param direction character. Direction to flip. Should be either partial
#' match to 'vertical' or 'horizontal'
#' @param x0 x value to flip horizontally over (ignored for vertical). Pass NULL
#' to flip over the extent
#' @param y0 y value to flip vertically over (ignored for horizontal). Pass NULL
#' to flip over the extent
#' @keywords internal
#' @noRd
.flip_gpoly <- function(gpoly,
    direction = "vertical",
    x0 = 0,
    y0 = 0) {
    checkmate::assert_class(gpoly, "giottoPolygon")
    checkmate::assert_character(direction)
    if (!is.null(x0)) {
        checkmate::assert_numeric(x0)
    }
    if (!is.null(y0)) {
        checkmate::assert_numeric(y0)
    }

    # 1. perform flip
    # This initial flip may move the polys and centroids different distances
    # depending on extent min, so the following shift steps must be processed
    # indpendently with the respective extents
    e_p <- terra::ext(gpoly@spatVector) # p = poly
    if (!is.null(gpoly@spatVectorCentroids)) {
        e_c <- terra::ext(gpoly@spatVectorCentroids) # c = centroid
    }
    gpoly <- .do_gpoly(
        x = gpoly,
        what = terra::flip,
        args = list(direction = direction)
    )

    # 2. perform shift to match line of symmetry
    if (grepl(direction, "vertical")) { # ------------------------------- #
        y_min_p <- as.numeric(e_p$ymin)

        if (is.null(y0)) {
            # flip about p extent
            # poly - no change
            # centroid
            if (!is.null(gpoly@spatVectorCentroids)) {
                y_min_c <- as.numeric(e_c$ymin)
                dy_c <- y_min_p - y_min_c
                gpoly@spatVectorCentroids <- terra::shift(
                    gpoly@spatVectorCentroids,
                    dy = 2 * dy_c
                )
            }
        } else {
            # flip about y0
            # poly
            dy_p <- y0 - y_min_p
            gpoly@spatVector <- terra::shift(
                gpoly@spatVector,
                dy = 2 * dy_p
            )
            # centroid
            if (!is.null(gpoly@spatVectorCentroids)) {
                y_min_c <- as.numeric(e_c$ymin)
                dy_c <- y0 - y_min_c
                gpoly@spatVectorCentroids <- terra::shift(
                    gpoly@spatVectorCentroids,
                    dy = 2 * dy_c
                )
            }
        }
    }
    if (grepl(direction, "horizontal")) { # ------------------------------- #
        x_min_p <- as.numeric(e_p$xmin)

        if (is.null(x0)) {
            # flip about p extent
            # poly - no change
            # centroid
            if (!is.null(gpoly@spatVectorCentroids)) {
                x_min_c <- as.numeric(e_c$xmin)
                dx_c <- x_min_p - x_min_c
                gpoly@spatVectorCentroids <- terra::shift(
                    gpoly@spatVectorCentroids,
                    dx = 2 * dx_c
                )
            }
        } else {
            # flip about x0
            # poly
            dx_p <- x0 - x_min_p
            gpoly@spatVector <- terra::shift(
                gpoly@spatVector,
                dx = 2 * dx_p
            )
            # centroid
            if (!is.null(gpoly@spatVectorCentroids)) {
                x_min_c <- as.numeric(e_c$xmin)
                dx_c <- x0 - x_min_c
                gpoly@spatVectorCentroids <- terra::shift(
                    gpoly@spatVectorCentroids,
                    dx = 2 * dx_c
                )
            }
        }
    }

    # 3. return
    return(gpoly)
}




.flip_spatvect <- function(x, direction = "vertical", x0 = 0, y0 = 0) {
    checkmate::assert_class(x, "SpatVector")
    if (!is.null(x0)) {
        checkmate::assert_numeric(x0)
    }
    if (!is.null(y0)) {
        checkmate::assert_numeric(y0)
    }

    # 1. perform flip
    e <- terra::ext(x)
    x <- terra::flip(x, direction = direction)

    x <- switch(direction,
        "vertical" = {
            if (!is.null(y0)) { # flip about y0 if not NULL
                ymin <- as.numeric(e$ymin)
                dy <- y0 - ymin
                terra::shift(x, dy = 2 * dy)
            }
        },
        "horizontal" = {
            if (!is.null(x0)) { # flip about x0 if not NULL
                xmin <- as.numeric(e$xmin)
                dx <- x0 - xmin
                terra::shift(x, dx = 2 * dx)
            }
        }
    )

    # 3. return
    return(x)
}




#' @name .flip_large_image
#' @title Flip a giottoLargeImage object
#' @description Flip a giottoPoints over a designated x or y value depending on
#' direction param input. Note that this behavior is different from terra's
#' implementation of flip for SpatVectors where flips happen over the extent
#' @param image giottoLargeImage
#' @param direction character. Direction to flip. Should be either partial
#' match to 'vertical' or 'horizontal'
#' @param x0 x value to flip horizontally over (ignored for vertical). Pass NULL
#' to flip over the extent
#' @param y0 y value to flip vertically over (ignored for horizontal). Pass NULL
#' to flip over the extent
#' @keywords internal
#' @noRd
.flip_large_image <- function(image,
    direction = "vertical",
    x0 = 0,
    y0 = 0) {
    checkmate::assert_class(image, "giottoLargeImage")
    checkmate::assert_character(direction)
    if (!is.null(x0)) {
        checkmate::assert_numeric(x0)
    }
    if (!is.null(y0)) {
        checkmate::assert_numeric(y0)
    }

    # 1. perform flip
    e <- ext(image)
    image@raster_object <- terra::flip(image@raster_object,
        direction = direction
    )

    # 2. perform shift to match line of symmetry
    if (grepl(direction, "vertical") & !is.null(y0)) {
        y_range <- as.numeric(c(e$ymin, e$ymax))
        dy <- 2 * y0 - y_range[1] - y_range[2]
        image <- spatShift(x = image, dy = dy)
    }
    if (grepl(direction, "horizontal") & !is.null(x0)) {
        x_range <- as.numeric(c(e$xmin, e$xmax))
        dx <- 2 * x0 - x_range[1] - x_range[2]
        image <- spatShift(x = image, dx = dx)
    }

    # 3. return
    return(image)
}


#' @name .flip_gpoints
#' @title Flip a giottoPoints object
#' @description Flip a giottoPoints over a designated x or y value depending on
#' direction param input. Note that this behavior is different from terra's
#' implementation of flip for SpatVectors where flips happen over the extent
#' @param gpoly giottoPoints
#' @param direction character. Direction to flip. Should be either partial
#' match to 'vertical' or 'horizontal'
#' @param x0 x value to flip horizontally over (ignored for vertical). Pass NULL
#' to flip over the extent
#' @param y0 y value to flip vertically over (ignored for horizontal). Pass NULL
#' to flip over the extent
#' @keywords internal
#' @noRd
.flip_gpoints <- function(gpoints,
    direction = "vertical",
    x0 = 0,
    y0 = 0) {
    checkmate::assert_class(gpoints, "giottoPoints")
    checkmate::assert_character(direction)
    if (!is.null(x0)) {
        checkmate::assert_numeric(x0)
    }
    if (!is.null(y0)) {
        checkmate::assert_numeric(y0)
    }

    # !will need to update for networks information!

    # 1. perform flip
    e <- terra::ext(gpoints@spatVector)
    gpoints@spatVector <- terra::flip(gpoints@spatVector,
        direction = direction
    )

    # 2. perform shift to match line of symmetry
    if (grepl(direction, "vertical") & !is.null(y0)) {
        y_min <- as.numeric(e$ymin)
        dy <- y0 - y_min
        gpoints@spatVector <- terra::shift(
            x = gpoints@spatVector,
            dy = 2 * dy
        )
    }
    if (grepl(direction, "horizontal") & !is.null(x0)) {
        x_min <- as.numeric(e$xmin)
        dx <- x0 - x_min
        gpoints@spatVector <- terra::shift(
            x = gpoints@spatVector,
            dx = 2 * dx
        )
    }

    # 3. return
    return(gpoints)
}





#' @name .flip_spatlocs
#' @param sl spatLocsObj
#' @param direction character. Direction to flip. Should be either partial
#' match to 'vertical' or 'horizontal'
#' @param x0 x value to flip horizontally over (ignored for vertical). Pass NULL
#' to flip over the extent
#' @param y0 y value to flip vertically over (ignored for horizontal). Pass NULL
#' to flip over the extent
#' @keywords internal
#' @noRd
.flip_spatlocs <- function(sl,
    direction = "vertical",
    x0 = 0,
    y0 = 0,
    copy_obj = TRUE) {
    sdimy <- sdimx <- NULL

    checkmate::assert_class(sl, "spatLocsObj")
    checkmate::assert_character(direction)
    if (!is.null(x0)) {
        checkmate::assert_numeric(x0)
    }
    if (!is.null(y0)) {
        checkmate::assert_numeric(y0)
    }

    if (isTRUE(copy_obj)) sl <- copy(sl)

    if (grepl(direction, "vertical")) {
        y_min <- sl[][, min(sdimy)]
        if (is.null(y0)) y0 <- y_min
        sl[][, sdimy := -sdimy + (2 * y0)]
    }
    if (grepl(direction, "horizontal")) {
        x_min <- sl[][, min(sdimx)]
        if (is.null(x0)) x0 <- x_min
        sl[][, sdimx := -sdimx + (2 * x0)]
    }

    return(sl)
}




#' @name .flip_spatnet
#' @param sn spatialNetworkObj
#' @param direction character. Direction to flip. Should be either partial
#' match to 'vertical' or 'horizontal'
#' @param x0 x value to flip horizontally over (ignored for vertical). Pass NULL
#' to flip over the extent
#' @param y0 y value to flip vertically over (ignored for horizontal). Pass NULL
#' to flip over the extent
#' @keywords internal
#' @noRd
.flip_spatnet <- function(sn,
    direction = "vertical",
    x0 = 0,
    y0 = 0,
    copy_obj = TRUE) {
    sdimy_begin <- sdimy_end <- sdimx_begin <- sdimx_end <- NULL

    checkmate::assert_class(sn, "spatialNetworkObj")
    checkmate::assert_character(direction)
    if (!is.null(x0)) {
        checkmate::assert_numeric(x0)
    }
    if (!is.null(y0)) {
        checkmate::assert_numeric(y0)
    }

    if (isTRUE(copy_obj)) sn <- copy(sn)

    if (grepl(direction, "vertical")) {
        y_min <- sn[][, min(sdimy_begin, sdimy_end)]
        if (is.null(y0)) y0 <- y_min
        sn[][, c("sdimy_begin", "sdimy_end") := .(
            -sdimy_begin + (2 * y0),
            -sdimy_end + (2 * y0)
        )]
        if (!is.null(sn@networkDT_before_filter)) {
            sn@networkDT_before_filter[
                ,
                c("sdimy_begin", "sdimy_end") := .(
                    -sdimy_begin + (2 * y0),
                    -sdimy_end + (2 * y0)
                )
            ]
        }
    }
    if (grepl(direction, "horizontal")) {
        x_min <- sn[][, min(sdimx_begin, sdimx_end)]
        if (is.null(x0)) x0 <- x_min
        sn[][, c("sdimx_begin", "sdimx_end") := .(
            -sdimx_begin + (2 * x0),
            -sdimx_end + (2 * x0)
        )]
        if (!is.null(sn@networkDT_before_filter)) {
            sn@networkDT_before_filter[
                ,
                c("sdimx_begin", "sdimx_end") := .(
                    -sdimx_begin + (2 * x0),
                    -sdimx_end + (2 * x0)
                )
            ]
        }
    }

    return(sn)
}




#' @name .flip_extent
#' @title Flip a SpatExtent
#' @param e extent
#' @param direction character. Direction to flip. Should be either partial
#' match to 'vertical' or 'horizontal'
#' @param x0 x value to flip horizontally over (ignored for vertical). Pass NULL
#' to flip over the extent
#' @param y0 y value to flip vertically over (ignored for horizontal). Pass NULL
#' to flip over the extent
#' @keywords internal
#' @noRd
.flip_extent <- function(e,
    direction = "vertical",
    x0 = 0,
    y0 = 0) {
    checkmate::assert_class(e, "SpatExtent")
    checkmate::assert_character(direction)
    if (!is.null(x0)) {
        checkmate::assert_numeric(x0)
    }
    if (!is.null(y0)) {
        checkmate::assert_numeric(y0)
    }

    y_vals <- as.numeric(c(e$ymin, e$ymax))
    x_vals <- as.numeric(c(e$xmin, e$xmax))

    if (grepl(direction, "vertical")) {
        if (is.null(y0)) y0 <- y_vals[1] # set bound min as line of
        # sym (terra default)
        y_vals <- -y_vals + (2 * y0)
    }
    if (grepl(direction, "horizontal")) {
        if (is.null(x0)) x0 <- x_vals[1] # set bound min as line of
        # sym (terra default)
        x_vals <- -x_vals + (2 * x0)
    }

    terra::ext(c(sort(x_vals), sort(y_vals)))
}
