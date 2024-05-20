# docs -------------------------------------------------------------- #
#' @title Rescale an object
#' @name rescale
#' @description Rescale an object spatially. Z dimension scaling is supported
#' for some types of subobjects.
#' @param x object
#' @param fx numeric > 0. The horizontal scaling factor
#' @param fy numeric > 0. The vertical scaling factor
#' @param fz numeric > 0. The z scaling factor (only for supported objects)
#' @param x0 numeric. x-coordinate of the center of rescaling. If missing,
#' the center of the extent of x is used
#' @param y0 numeric. y-coordinate of the center of rescaling. If missing,
#' the center of the extent of x is used
#' @param z0 numeric. z-coordinate of the center of rescaling. If missing,
#' the center of the extent of x is used (only for supported objects)
#' @details
#' With the `giotto` object, the ":all:" token can be passed to `spat_unit`,
#' `feat_type`, and `images` arguments to affect all available items.
#'
#' @returns re-scaled object
#' @examples
#' g <- GiottoData::loadSubObjectMini("spatLocsObj")
#'
#' rescale(g)
NULL
# ------------------------------------------------------------------- #


#' @rdname rescale
#' @param spat_unit character vector. spatial units to affect
#' @param feat_type character vector. feature types to affect
#' @param images character vector. Images to affect
#' @export
setMethod(
    "rescale", signature("giotto"),
    function(x, fx = 1, fy = fx, x0, y0, spat_unit = ":all:",
             feat_type = ":all:", images = ":all:"
    ) {
        # scalefactor settings
        a <- list(fx = fx, fy = fy)

        if (!missing(x0)) a$x0 <- x0
        if (!missing(y0)) a$y0 <- y0

        spat_unit <- set_default_spat_unit(
            gobject = x, spat_unit = spat_unit
        )
        feat_type <- set_default_feat_type(
            gobject = x, spat_unit = spat_unit, feat_type = feat_type
        )

        # if no rescale center provided, use center of gobject data to use
        if (is.null(a$x0) || is.null(a$y0)) {
            # find center
            centroid <- ext(x,
                spat_unit = spat_unit,
                feat_type = feat_type,
                all_data = TRUE
            ) %>%
                as.polygons() %>%
                centroids() %>%
                data.table::as.data.table(geom = "XY")

            if (is.null(a$x0)) a$x0 <- centroid$x
            if (is.null(a$y0)) a$y0 <- centroid$y
        }

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
                p <- do.call(rescale, args = c(list(x = p), a))
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
                sl <- do.call(rescale, args = c(list(x = sl), a))
                x <- setSpatialLocations(x, sl,
                    verbose = FALSE, initialize = FALSE
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
                pt <- do.call(rescale, args = c(list(x = pt), a))
                x <- setFeatureInfo(x, pt, verbose = FALSE, initialize = FALSE)
            }
        }

        # images ----------------------------------------------------------- #
        imgs <- getGiottoImage(x, name = images)
        if (!is.null(imgs)) {
            if (!inherits(imgs, "list")) imgs <- list(imgs)
            for(img in imgs) {
                img <- do.call(rescale, args = c(list(x = img), a))
                x <- setGiottoImage(x, img, verbose = FALSE)
            }
        }

        return(initialize(x)) # init not necessarily needed
    }
)


#' @rdname rescale
#' @export
setMethod(
    "rescale", signature("spatLocsObj"),
    function(x, fx = 1, fy = fx, fz = fx, x0, y0, z0) {
        argslist <- GiottoUtils::get_args_list()
        argslist$x <- x[]

        # call data.frame method
        x[] <- do.call(rescale, args = argslist)
        x
    }
)

#' @rdname rescale
#' @param geom character. Named vector of colnames of x, y, (z) coordinate
#' columns. Default is `c("sdimx", "sdimy", "sdimz")`
setMethod(
    "rescale", signature("data.frame"),
    function(x, fx = 1, fy = fx, fz = fx, x0, y0, z0,
    geom = c("sdimx", "sdimy", "sdimz")) {
        x <- data.table::as.data.table(x)

        # find center
        if (missing(x0)) x0 <- x[, mean(range(get(geom[1L])))]
        if (missing(y0)) y0 <- x[, mean(range(get(geom[2L])))]
        if ("sdimz" %in% names(x) & missing(z0)) {
            z0 <- x[, mean(range(get(geom[3L])))]
        } else {
            z0 <- 0
        }

        x <- .scale_spatial_locations(
            spatlocs = x,
            scale_factor = c(x = fx, y = fy, z = fz),
            scenter = c(x = x0, y = y0, z = z0),
            geom = c("sdimx", "sdimy", "sdimz")
        )
        x
    }
)


#' @rdname rescale
#' @export
setMethod(
    "rescale", signature("giottoPolygon"),
    function(x, fx = 1, fy = fx, x0, y0) {
        a <- list(fx = fx, fy = fy)
        if (!missing(x0)) a$x0 <- x0
        if (!missing(y0)) a$y0 <- y0

        .do_gpoly(x, what = terra::rescale, args = a)
    }
)

#' @rdname rescale
#' @export
setMethod(
    "rescale", signature("giottoPoints"),
    function(x, fx = 1, fy = fx, x0, y0) {
        a <- list(x = x[], fx = fx, fy = fy)
        if (!missing(x0)) a$x0 <- x0
        if (!missing(y0)) a$y0 <- y0

        x[] <- do.call("rescale", args = a)
        return(x)
    }
)

#' @rdname rescale
#' @export
setMethod("rescale", signature("giottoLargeImage"), function(x, fx = 1, fy = fx, x0, y0) {
    a <- list(x = x@raster_object, fx = fx, fy = fy)
    if (!missing(x0)) a$x0 <- x0
    if (!missing(y0)) a$y0 <- y0

    x@raster_object <- do.call("rescale", args = a)
    return(x)
})



# TODO more methods for other objects






# internals ####

#' @title Scale spatial locations
#' @name .scale_spatial_locations
#' @description Simple scaling of spatial locations by
#' given \code{scale_factor}. Values will be scaled from the coordinate origin
#' or coordinates provided through \code{scenter} param. Default values supply
#' values for z axis, but these values will only be applied if
#' input `data.table` has z information as detected by the third item in
#' the `geom` param.
#' @param spatlocs data.table. spatial locations information to scale
#' @param scale_factor scaling factor to apply to coordinates. Default is
#' `c(1, 1, 1)`
#' @param scenter center from which to scale spatial coordinates. Given as
#' vector of xy(z) coordinates. Default is `c(0, 0, 0)`
#' @param geom character. Named vector of colnames of x, y, (z) coordinate
#' columns. Default is `c("sdimx", "sdimy", "sdimz")`
#' @returns spatial locations
#' @details \code{scale_factor} either given as a single value where it will
#' be applied to x, y, and z (if available) dimensions or as a vector of named
#' values for 'x', y', (and 'z').
#' @keywords internal
.scale_spatial_locations <- function(spatlocs,
    scale_factor = c(1, 1, 1),
    scenter = c(0, 0, 0),
    geom = c("sdimx", "sdimy", "sdimz")) {
    checkmate::assert_data_table(spatlocs)

    xyz <- c("x", "y", "z")
    if (is.null(names(scale_factor))) names(scale_factor) <- xyz
    if (is.null(names(scenter))) names(scenter) <- xyz
    if (is.null(names(geom))) names(geom) <- xyz

    hasZ <- geom[["z"]] %in% colnames(spatlocs)

    if (length(scale_factor) == 1) {
        scale_factor <- c(x = scale_factor, y = scale_factor, z = scale_factor)
    }
    if (!all(names(scenter) %in% xyz)) {
        stop("scenter value names not recognized")
    }
    if (!all(names(scale_factor) %in% xyz)) {
        stop("scale_factor value names not recognized")
    }
    if (!all(names(geom) %in% xyz)) stop("geom value names not recognized")

    # Adjust for scaling center
    spatlocs[, (geom[["x"]]) := get(geom[["x"]]) - scenter[["x"]]]
    spatlocs[, (geom[["y"]]) := get(geom[["y"]]) - scenter[["y"]]]

    # Perform scale
    spatlocs[, (geom[["x"]]) := get(geom[["x"]]) * scale_factor[["x"]]]
    spatlocs[, (geom[["y"]]) := get(geom[["y"]]) * scale_factor[["y"]]]

    if (isTRUE(hasZ)) {
        # Adjust for scaling z center
        spatlocs[, (geom[["z"]]) := get(geom[["z"]]) - scenter[["z"]]]

        # Perform z scale
        spatlocs[, (geom[["z"]]) := get(geom[["z"]]) * scale_factor[["z"]]]

        # Revert z scaling center adjustments
        spatlocs[, (geom[["z"]]) := get(geom[["z"]]) + scenter[["z"]]]
    }

    # Revert scaling center adjustments
    spatlocs[, (geom[["x"]]) := get(geom[["x"]]) + scenter[["x"]]]
    spatlocs[, (geom[["y"]]) := get(geom[["y"]]) + scenter[["y"]]]

    return(spatlocs)
}







#' @title Rescale polygons
#' @name .rescale_polygons
#' @returns polygons
#' @description  rescale individual polygons by a factor x and y
#' @keywords internal
.rescale_polygons <- function(spatVector,
    spatVectorCentroids,
    fx = 0.5, fy = 0.5) {
    # DT vars
    poly_ID <- NULL

    spatVectorCentroidsDT <- .spatvector_to_dt(spatVectorCentroids)

    cell_ids <- spatVector$poly_ID

    l <- lapply(cell_ids, FUN = function(id) {
        single_polygon <- spatVector[spatVector$poly_ID == id]
        single_centroid <- spatVectorCentroidsDT[poly_ID == id]

        single_polygon_resc <- terra::rescale(
            x = single_polygon,
            fx = fx, fy = fy,
            x0 = single_centroid$x,
            y0 = single_centroid$y
        )
    })

    new_polygons <- do.call("rbind", l)
    return(new_polygons)
}



#' @title rescalePolygons
#' @name rescalePolygons
#' @description Rescale individual polygons by a x and y factor
#' @param gobject giotto object
#' @param poly_info polygon information name
#' @param name name of new polygon layer
#' @param fx x-scaling factor
#' @param fy y-scaling factor
#' @param calculate_centroids calculate centroids
#' @param return_gobject return giotto object
#' @return giotto object
#' @concept polygon scaling
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' rescalePolygons(g, poly_info = "aggregate")
#' @export
rescalePolygons <- function(gobject,
    poly_info = "cell",
    name = "rescaled_cell",
    fx = 0.5,
    fy = 0.5,
    calculate_centroids = TRUE,
    return_gobject = TRUE) {
    # 1. get polygon information
    original <- get_polygon_info(
        gobject = gobject,
        polygon_name = poly_info,
        return_giottoPolygon = TRUE
    )

    original_vector <- slot(original, "spatVector")
    original_centroids <- slot(original, "spatVectorCentroids")

    if (is.null(original_centroids)) {
        stop("Selected polygons don't have associated centroid,
         use addSpatialCentroidLocations() ")
    }


    # 2. rescale polygon
    rescaled_original <- .rescale_polygons(original_vector,
        original_centroids,
        fx = fx, fy = fy
    )

    # 3. create new Giotto polygon and calculate centroids
    S4_polygon <- create_giotto_polygon_object(
        name = name,
        spatVector = rescaled_original
    )
    if (calculate_centroids) {
        S4_polygon <- .calculate_centroids_polygons(
            gpolygon = S4_polygon,
            append_gpolygon = TRUE
        )
    }


    # 4. return object or S4 polygon
    if (return_gobject) {
        # TODO: update parameters
        gobject <- setPolygonInfo(gobject = gobject, x = S4_polygon)
        return(gobject)
    } else {
        return(S4_polygon)
    }
}
