# documentation ####

#' @name crop
#' @title Crop to a spatial subset
#' @description
#' Spatially subset an object x using object y. `crop()` will only return a 
#' modified object if the `y` extent is smaller than the original in `x`.
#' For object type specifics, see below.
#' 
#' ## `giottoLargeImage` and `giottoAffineImage`
#' 
#' When `write = FALSE` and a `filename` is not specified, [terra::window()]
#' will instead be used to create a lazy subset of the image. An independent
#' version of the cropped subset will only be created when either of the
#' above are provided, at which point it is handled through [terra::crop()].
#' `...` params are only used when materializing the subset.
#' 
#' ## `giottoPoints` and `giottoPolygon`
#' 
#' An alternative faster crop operation is implemented through `data.table`
#' manipulation of the geometry information and are used by default. This mode
#' also only allows rectangular subsetting. Additionally, `giottoPolygons` will
#' be cropped using their centroids so that the entire polygon is either present
#' or not instead of the default `crop` behavior that would keep the portion of
#' the polygon that does fall within the selected crop region.
#' Set `DT = FALSE` in order to use the default style of terra::crop behavior
#' that also allows usage of additional params through ...
#' @param x object
#' @param y any object that has a SpatExtent or returns a SpatExtent
#' @param write `logical` <images only> (default = FALSE). Whether to write and
#'   materialize the cropped subset to disk. This is overridden when a `filename` 
#'   is specifically provided.
#' @param filename `character` <images only> (default is a .tif tempfile). 
#'   This file is not actually written unless the user specifically provides a 
#'   path or `write = TRUE`.
#' @param \dots additional params to pass to terra::crop
#' @returns object of same class as `x`, spatially subsetted  
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' img1 <- getGiottoImage(g, name = "image")
#' plot(img1)
#' 
#' # modify extent
#' e <- ext(img1)
#' e <- e - 1000
#' 
#' img2 <- crop(img1, e)
#' plot(img2)
NULL




# methods ####


# * giottoLargeImage ####
#' @rdname crop
#' @export
setMethod("crop", signature("giottoLargeImage"), function(x, y,
    write = FALSE,
    filename = tempfile(fileext = ".tif"),
    ...) {
    do_crop <- .crop_check(x, y)
    if (!do_crop) return(x)
  
    if (!isTRUE(write) && !hasArg(filename)) {
        terra::window(x[]) <- ext(y)
        return(initialize(x))
    }
    
    x[] <- terra::crop(x[], y, filename = filename, ...)
    return(initialize(x))
})

# * giottoAffineImage ####
#' @rdname crop
#' @export
setMethod("crop", signature("giottoAffineImage"), function(x, y, 
    write = FALSE,
    filename = tempfile(fileext = ".tif"),
    ...) {
    crop_ext <- ext(y)
    d <- .bound_poly(crop_ext)
    aff <- x@affine
    img_crop_ext <- ext(affine(d, aff, inv = TRUE)) # find extent in img space

    do_crop <- .crop_check(x@raster_object, img_crop_ext)
    if (!do_crop) return(x)

    if (!isTRUE(write) && !hasArg(filename)) {
        terra::window(x[]) <- img_crop_ext
        return(initialize(x))
    }

    x[] <- terra::crop(x[], img_crop_ext, filename = filename, ...)
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
    if (!do_crop) {
        return(x)
    }

    b <- .ext_to_num_vec(e) # bounds as a numerical vector
    x[] <- x[][sdimx >= b[1] & sdimx <= b[2] & sdimy >= b[3] & sdimy <= b[4]]
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
            if (!do_crop) {
                return(x)
            }

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
            if (!do_crop) {
                return(x)
            }

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
                    is.null,
                    FUN.VALUE = logical(1L)
                )
            )

            # 1. get final crop bounds (numeric vector of xmin, xmax, ymin, ymax)
            b <- .determine_crop_bounds(
                x, y, missing_y, n_single_bounds, xmin, xmax, ymin, ymax
            )

            do_crop <- .crop_check(x, b)
            if (!do_crop) {
                return(x)
            }

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
            if (!do_crop) {
                return(x)
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
        x <- .subset_overlaps_poly(x, x@unique_ID_cache)
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


# * giotto (indirect-only via view=) ####
# Eager crop at the gobject level is not yet implemented (it would require
# coordinated narrowing of every spatial subobject, images, expression and
# metadata). Until then this method only accepts `view = `, recording a
# crop step onto the named slotted view. Eager calls without `view` error
# with a clear pointer.
#
# A crop narrows the CELL SET; it is relate-based membership, not geometric
# clipping. Surviving subobjects keep their geometry.

#' @rdname crop
#' @param relation `character(1)`. Spatial predicate. A crop narrows the
#'   **cell set**, so each cell is reduced to a geometry (see `geom`) and
#'   tested against the region. One of `"intersects"` (default),
#'   `"disjoint"`, `"within"`, `"touches"`, `"contains"`, `"covers"`,
#'   `"overlaps"`, `"crosses"`. The last four are always `FALSE` against a
#'   centroid, so requesting one promotes `geom` to `"poly"` with a warning.
#'   (`"covered_by"` is not a terra predicate and is rejected.)
#' @param geom `character(1)`. What represents a cell when the predicate is
#'   evaluated: `"centroid"` (default) uses the cell's `spatial_locs` row —
#'   cheap, and the conventional choice, but a cell whose polygon straddles
#'   the region boundary with its centroid outside is dropped. `"poly"` uses
#'   the cell's actual polygon — exact, and requires a polygon source on the
#'   object. The choice is recorded on the step, so a saved recipe states
#'   which question it asks.
#' @param view `NULL` or `character(1)`. When supplied, records the crop as
#'   a step on the named view, creating it if it does not exist yet, instead
#'   of executing eagerly. Eager `crop()` on a `giotto` is not yet
#'   implemented.
#' @param space `NULL` or `character(1)`. Name of a slotted space on `x`.
#'   Sets the view's `space` reference — the coordinate frame in which the
#'   crop region is interpreted at resolution time. First call sets it;
#'   subsequent calls that try to rebind to a different name error. `NULL`
#'   leaves the view in whatever frame it was already bound to (or the
#'   gobject's native frame if unbound).
#' @export
setMethod("crop", signature(x = "gAny", y = "ANY"),
    function(x, y, relation = "intersects", geom = c("centroid", "poly"),
             ..., view = NULL, space = NULL) {
        if (is.null(view)) {
            stop("`crop()` on a giotto / giottoMulti requires `view = `. ",
                "Eager gobject-level crop is not implemented. Pass ",
                "`view = \"<name>\"` to record the step onto a view, then ",
                "apply it with `materialize(x, \"<name>\")`.",
                call. = FALSE)
        }
        checkmate::assert_character(relation, len = 1L, any.missing = FALSE)
        geom <- match.arg(geom)
        region <- .normalize_crop_region(y)
        # vocabulary checks and the poly-only promotion live in the step
        # constructor, so they fire before anything is recorded on x
        step <- .view_step_crop(region, relation, geom)
        .record_view_on_gobject(x, view, step, space = space)
    }
)
