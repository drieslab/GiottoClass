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
NULL






# methods ####



#' @describeIn crop Crop a giottoLargeImage
#' @export
setMethod("crop", signature("giottoLargeImage"), function(x, y, ...) {
    if (is.null(terra::intersect(terra::ext(x), terra::ext(y)))) {
        warning("crop region is empty", call. = FALSE)
    }
    x@raster_object <- terra::crop(x@raster_object, y, ...)
    x@extent <- ext(x@raster_object)
    intensity_range <- .spatraster_intensity_range(x@raster_object)
    x@min_intensity <- intensity_range[["min"]]
    x@max_intensity <- intensity_range[["max"]]

    x
})


#' @describeIn crop Crop a giottoPoints
#' @param DT logical. Use alternative DT subsetting for crop operation
#' @param xmin,xmax,ymin,ymax only used if DT = TRUE. Set extent bounds
#' independently
#' @export
setMethod("crop", signature("giottoPoints"), 
        function(x, y, DT = TRUE, xmin = NULL, xmax = NULL, 
                ymin = NULL, ymax = NULL, ...) {
    checkmate::assert_logical(DT)
    if (DT) {
        # converting to DT, subsetting, then regeneration of SpatVector with vect()
        # is currently faster than using terra::crop() as of 9/21/23
        missing_y <- missing(y)
        if (missing_y) y <- NULL # make easier to pass as a param downstream
        n_single_bounds <- 4 - sum(
            sapply(list(xmin, xmax, ymin, ymax), is.null))

        # 1. get final crop bounds (numeric vector of xmin, xmax, ymin, ymax)
        b <- .determine_crop_bounds(
            x, y, missing_y, n_single_bounds, xmin, xmax, ymin, ymax)

        # 2. convert to DT
        sv <- x@spatVector
        spatDT <- as.data.table(sv, geom = "XY")

        # 3. spatial subset then vect() to SpatVector again
        sub_idx <- spatDT[, 
                        which(x >= b[[1]] & x <= b[2] & y >= b[3] & y <= b[4])]
        if (length(sub_idx) == 0L) 
            warning("crop region is empty", call. = FALSE)

        # 4. update x
        x@spatVector <- sv[sub_idx]
    } else {
        # non-DT method. terra default.

        if (is.null(terra::intersect(terra::ext(x), terra::ext(y)))) {
            warning("crop region is empty", call. = FALSE)
        }

        x@spatVector <- terra::crop(x@spatVector, y, ...)
    }

    # update ID cache and return
    x@unique_ID_cache <- unique(terra::values(x@spatVector)$feat_ID)
    x
})



#' @describeIn crop Crop a giottoPoints
#' @param DT logical. Use alternative DT subsetting for crop operation
#' @param xmin,xmax,ymin,ymax only used if DT = TRUE. Set extent bounds
#' independently
#' @export
setMethod("crop", signature("giottoPolygon"), 
        function(x, y, DT = TRUE, xmin = NULL, xmax = NULL, ymin = NULL, 
                ymax = NULL, ...) {
    # A. spatVector cropping
    checkmate::assert_logical(DT)
    if (DT) {
        # converting to DT, subsetting, then regeneration of SpatVector with 
        # vect() is currently faster than using terra::crop() as of 9/21/23
        missing_y <- missing(y)
        if (missing_y) y <- NULL # make easier to pass as a param downstream
        n_single_bounds <- 4 - sum(
            sapply(list(xmin, xmax, ymin, ymax), is.null))

        # 1. get final crop bounds (numeric vector of xmin, xmax, ymin, ymax)
        b <- .determine_crop_bounds(
            x, y, missing_y, n_single_bounds, xmin, xmax, ymin, ymax)

        # 2. convert to DT
        sv <- x@spatVectorCentroids
        if (is.null(sv)) {
            # generate centroids if missing
            sv <- terra::centroids(x@spatVector)
        }
        spatDT <- as.data.table(sv, geom = "XY")

        # 3. get subset indices
        sub_idx <- spatDT[, 
                        which(x >= b[[1]] & x <= b[2] & y >= b[3] & y <= b[4])]
        if (length(sub_idx) == 0L) 
            warning("crop region is empty", call. = FALSE)

        # 4. update x
        x@spatVector <- x@spatVector[sub_idx]
        x@spatVectorCentroids <- sv[sub_idx]
        # update ID cache (use DT for more efficiency)
        x@unique_ID_cache <- spatDT[sub_idx, get("poly_ID")]
    } else {
        # non-DT method. terra default.

        if (is.null(terra::intersect(terra::ext(x), terra::ext(y)))) {
            warning("crop region is empty", call. = FALSE)
        }

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
            x@overlaps[[feat]])$poly_ID %in% x@unique_ID_cache
        x@overlaps[[feat]] <- x@overlaps[[feat]][cell_id_bool]
    }

    x
})



# helpers ####





# internal helper function to get a final set of crop bounds from a possible
# combination of the extent of x and the single spatial bound parameters or y
#
# returns a numeric vector of the 4 bounds in the order of:
#   xmin, xmax, ymin, ymax
.determine_crop_bounds <- function(x, y, missing_y, n_single_bounds, 
                                xmin, xmax, ymin, ymax) {
    # check cropping params
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
        # if y is available, use y values directly.
        # only the extent of y is usable for the DT method
        if (!inherits(y, "SpatExtent")) {
            warning(wrap_txt("Only the extent of y is used when cropping with 
                            DT = TRUE"))
            y <- ext(y)
        }

        xmin <- terra::xmin(y)
        xmax <- terra::xmax(y)
        ymin <- terra::ymin(y)
        ymax <- terra::ymax(y)
    } else {
        # otherwise, fill in any spatial subset bounds that may not have been
        # supplied with the current extent value(s)
        current_ext <- ext(x)
        if (is.null(xmin)) xmin <- terra::xmin(current_ext)
        if (is.null(xmax)) xmax <- terra::xmax(current_ext)
        if (is.null(ymin)) ymin <- terra::ymin(current_ext)
        if (is.null(ymax)) ymax <- terra::ymax(current_ext)
    }

    # return crop bounds as numeric vector
    c(xmin, xmax, ymin, ymax)
}
