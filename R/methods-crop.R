# documentation ####

#' @name crop
#' @title Crop to a spatial subset
#' @description Spatially subset an object x using object y. Giotto spatial
#' subobjects respond to [terra::crop]. For `giottoPoints` and `giottoPolygon`,
#' an alternative faster crop operation is implemented through `data.table`
#' manipulation of the geometry information and are used by default. This mode
#' also only allows rectangular subsetting. Additionally, `giottoPolygons` will
#' be cropped using their centroids so that the entire polygon is either present
#' or not instead of the default `crop` behavior that would keep the portion of
#' the polygon that does fall within the selected crop region.
#' Set `DT = FALSE` in order to use the default style of terra::crop behavior
#' that also allows usage of additional params through ...
#' @param x object
#' @param y any object that has a SpatExtent or returns a SpatExtent
#' @param \dots additional params to pass to terra::crop
#' @returns SpatRaster
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' g_image <- getGiottoImage(g, image_type = "largeImage")
#' crop(g_image, g_image)
NULL




# methods ####


# * giottoLargeImage ####
#' @rdname crop
#' @export
setMethod("crop", signature("giottoLargeImage"), function(x, y, ...) {
    do_crop <- .crop_check(x, y)
    if (!do_crop) return(initialize(x))
    x@raster_object <- terra::crop(x@raster_object, y, ...)
    return(initialize(x))
})

# * giottoAffineImage ####
#' @rdname crop
#' @export
setMethod("crop", signature("giottoAffineImage"), function(x, y, ...) {
    crop_ext <- ext(y)
    d <- .bound_poly(crop_ext)
    aff <- x@affine
    img_crop_ext <- ext(affine(d, aff, inv = TRUE)) # find extent in img space
    
    do_crop <- .crop_check(x@raster_object, img_crop_ext)
    if (!do_crop) return(initialize(x))
    x@raster_object <- terra::crop(x@raster_object, img_crop_ext)
    return(initialize(x))
})

# * spatLocsObj ####
#' @rdname crop
#' @export
setMethod("crop", signature("spatLocsObj"), function(x, y, ...) {
    # NSE vars
    sdimx <- sdimy <- NULL
    e <- ext(y)
    
    do_crop <- .crop_check(x, y)
    if (!do_crop) return(x)
    
    b <- .ext_to_num_vec(e) # bounds as a numerical vector
    x[] <- x[][sdimx >= b[1] & sdimx <= b[2] & sdimy >= b[3] & sdimy <= b[4]]
    return(x)
})

# should only be used with spatial networks that contain spatial information
# * spatialNetworkObj ####
#' @rdname crop
#' @export
setMethod("crop", signature("spatialNetworkObj"), function(x, y, ...) {
    # NSE vars
    sdimx_begin <- sdimy_begin <- sdimx_end <- sdimy_end <- NULL
    e <- ext(y)
    
    do_crop <- .crop_check(x, y)
    if (!do_crop) return(x)
    
    b <- .ext_to_num_vec(e) # bounds as a numerical vector
    x[] <- x[][sdimx_begin >= b[1] & sdimx_begin <= b[2] &
               sdimy_begin >= b[3] & sdimy_begin <= b[4]]
    x[] <- x[][sdimx_end >= b[1] & sdimx_end <= b[2] &
               sdimy_end >= b[3] & sdimy_end <= b[4]]
    return(x)
})

# * giottoPoints ####
#' @rdname crop
#' @param DT logical. Use alternative DT subsetting for crop operation
#' @param xmin,xmax,ymin,ymax only used if DT = TRUE. Set extent bounds
#' independently
#' @export
setMethod(
    "crop", signature("giottoPoints"),
    function(
        x, y, DT = TRUE, xmin = NULL, xmax = NULL,
        ymin = NULL, ymax = NULL, ...) {
        checkmate::assert_logical(DT)
        if (DT) {
            # converting to DT, subsetting, then regeneration of SpatVector with vect()
            # is currently faster than using terra::crop() as of 9/21/23
            missing_y <- missing(y)
            if (missing_y) y <- NULL # make easier to pass as a param downstream
            n_single_bounds <- 4 - sum(
                vapply(list(xmin, xmax, ymin, ymax), is.null, logical(1L))
            )

            # 1. get final crop bounds (numeric vector of xmin, xmax, ymin, ymax)
            b <- .determine_crop_bounds(
                x, y, missing_y, n_single_bounds, xmin, xmax, ymin, ymax
            )
            
            do_crop <- .crop_check(x, b)
            if (!do_crop) return(x)

            # 2. convert to DT
            sv <- x@spatVector
            spatDT <- data.table::as.data.table(sv, geom = "XY")

            # 3. spatial subset then vect() to SpatVector again
            sub_idx <- spatDT[
                ,
                which(x >= b[[1]] & x <= b[2] & y >= b[3] & y <= b[4])
            ]

            # 4. update x
            x@spatVector <- sv[sub_idx]
        } else {
            # non-DT method. terra default.

            do_crop <- .crop_check(x, y)
            if (!do_crop) return(x)

            x@spatVector <- terra::crop(x@spatVector, y, ...)
        }

        # update ID cache and return
        x@unique_ID_cache <- unique(terra::values(x@spatVector)$feat_ID)
        x
    }
)


# * giottoPolygon ####
#' @rdname crop
#' @param DT logical. Use alternative DT subsetting for crop operation
#' @param xmin,xmax,ymin,ymax only used if DT = TRUE. Set extent bounds
#' independently
#' @export
setMethod(
    "crop", signature("giottoPolygon"),
    function(
        x, y, DT = TRUE, xmin = NULL, xmax = NULL, ymin = NULL,
        ymax = NULL, ...) {
        # A. spatVector cropping
        checkmate::assert_logical(DT)
        if (DT) {
            # converting to DT, subsetting, then regeneration of SpatVector with
            # vect() is currently faster than using terra::crop() as of 9/21/23
            missing_y <- missing(y)
            if (missing_y) y <- NULL # make easier to pass as a param downstream
            n_single_bounds <- 4 - sum(
                vapply(list(xmin, xmax, ymin, ymax),
                       is.null, FUN.VALUE = logical(1L))
            )

            # 1. get final crop bounds (numeric vector of xmin, xmax, ymin, ymax)
            b <- .determine_crop_bounds(
                x, y, missing_y, n_single_bounds, xmin, xmax, ymin, ymax
            )
            
            do_crop <- .crop_check(x, b)
            if (!do_crop) return(x)

            # 2. convert to DT
            sv <- x@spatVectorCentroids
            if (is.null(sv)) {
                # generate centroids if missing
                sv <- terra::centroids(x@spatVector)
            }
            spatDT <- as.data.table(sv, geom = "XY")

            # 3. get subset indices
            sub_idx <- spatDT[
                ,
                which(x >= b[[1]] & x <= b[2] & y >= b[3] & y <= b[4])
            ]

            # 4. update x
            x@spatVector <- x@spatVector[sub_idx]
            x@spatVectorCentroids <- sv[sub_idx]
            # update ID cache (use DT for more efficiency)
            x@unique_ID_cache <- spatDT[sub_idx, get("poly_ID")]
        } else {
            # non-DT method. terra default.
            
            do_crop <- .crop_check(x, y)
            if (!do_crop) return(x)

            args <- list(y = y, ...)
            x <- .do_gpoly(x, what = terra::crop, args = args)
            # update ID cache
            x@unique_ID_cache <- unique(terra::values(x@spatVector)$poly_ID)
        }

        # B. overlaps subsetting
        if (is.null(x@overlaps)) {
            return(x)
        } # return if none existing

        # iterate through all overlaps, removing cell_ids that were removed in the
        # crop.
        for (feat in names(x@overlaps)) {
            cell_id_bool <- terra::as.list(
                x@overlaps[[feat]]
            )$poly_ID %in% x@unique_ID_cache
            x@overlaps[[feat]] <- x@overlaps[[feat]][cell_id_bool]
        }

        x
    }
)





# helpers ####





# internal helper function to get a final set of crop bounds from a possible
# combination of the extent of x and the single spatial bound parameters or y
#
# returns a numeric vector of the 4 bounds in the order of:
#   xmin, xmax, ymin, ymax
.determine_crop_bounds <- function(
        x, y, missing_y, n_single_bounds,
        xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL,
        output = c("numeric", "extent")) {
    # check cropping params
    output <- match.arg(tolower(output), choices = c("numeric", "extent"))

    # ONLY y OR the single spat bounds can be used at any one time
    if ((missing_y && n_single_bounds == 0) ||
        (!missing_y && n_single_bounds > 0)) {
        stop(wrap_txt("Crop bounds must be supplied through either a
                    SpatExtent passed to 'y'
                    or single numerical bounds passed to one or more of
                    'xmin','xmax', 'ymin', 'ymax'"))
    }

    # Get full set of cropping bounds
    if (!missing_y) {
        # if y is provided, use y extent directly.
        e <- ext(y)
    } else {
        # otherwise, replace values in x extent with any provided values
        e <- ext(x)
        if (!is.null(xmin)) e$xmin <- xmin
        if (!is.null(xmax)) e$xmax <- xmax
        if (!is.null(ymin)) e$ymin <- ymin
        if (!is.null(ymax)) e$ymax <- ymax
    }

    # return bounds
    switch(output,
        "numeric" = .ext_to_num_vec(e),
        "extent" = e
    )
}

# warning if crop area is empty
# TRUE if crop is needed
# FALSE if crop extent is larger than all available data
# when FALSE, original data can be returned directly without modification
.crop_check <- function(x, y) {
    ex <- ext(x)
    ey <- ext(y)
    exv <- .ext_to_num_vec(ex)
    eyv <- .ext_to_num_vec(ey)
    
    # no overlap in extents
    if (is.null(terra::intersect(ex, ey))) {
        warning("crop region is empty", call. = FALSE)
        return(TRUE) # this will likely be an empty object though
    }
    
    # if crop ext (y) fully encapsulates object ext (x):
    # yes, return FALSE, meaning no crop is needed
    # no, return TRUE, meaning crop is needed
    if (eyv[[1]] <= exv[[1]] &&
        eyv[[2]] >= exv[[2]] &&
        eyv[[3]] <= exv[[3]] &&
        eyv[[4]] >= exv[[4]]) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}
