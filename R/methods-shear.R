







setMethod(
    "shear", signature("giottoLargeImage"),
    function(
        x,
        delayed = TRUE,
        ...
    ) {

    }
)

setMethod(
    "shear", signature("SpatVector"),
    function(
        x,
        ...
    ) {

    }
)


setMethod(
    "shear", signature("giottoPolygon"),
    function(
        x,
        ...
    ) {

    }
)



#' @name .shear_image
#' @title Shear an image
#' @description
#' Accepts an image object, samples to `magick-image` if needed and then
#' performs an image shear defined by a `shear_params` object
#' @param x image object (`giottoLargeImage`, `SpatRaster`, `magick-image`)
#' @param extent current extent (different from that passed through `shear_params`)
#' This extent will be updated after the transform
#' @keywords internal
.shear_image <- function(x, shear_params, extent, maxcell = 5e5, output = c("spatraster", "magick")) {

    checkmate::assert_class(shear_params, "shear_params")
    maxcell <- as.integer(maxcell)

    # original extent
    oe <- terra::ext(extent)[]

    # convert to sampled magick-image
    if (inherits(x, c("SpatRaster", "giottoLargeImage"))) {
        x <- .spatraster_sample_values(x, size = maxcell, output = "magick")
    }

    if (!inherits(x, "magick-image")) {
        .gstop(class(x), "is not supported in .shear_image()")
    }

    sgeom <- paste0(shear_params$deg, collapse = "x")
    simg <- magick::image_shear(x, geometry = sgeom)

    # TODO
}





#' @title Shear params
#' @name .shear_image_params
#' @description
#' Assemble params for x and y shear on an image object.
#' @details The generated params can be appended to the `giottoLargeImage` in
#' the `lazy_tfs` slot so that the shear can be performed when needed.
#' @param x `giottoLargeImage`
#' @param rad numeric vector. Clockwise shear angle in radians, respectively
#' measured from the y and x axes.
#' @param deg numeric vector. Clockwise shear angle in degrees, respectively
#' measured from the y and x axes.
#' @param lin numeric vector. Linear shearing of the image along the
#' x and y axes respectively. Applied relative to the `extent` param.
#' @param extent `SpatExtent` or object coercible to it. Shear will be applied
#' across this extent. (defaults to extent of `x`)
#' @param x0 numeric. x-coordinate min of shear. Defaults to center x val if not given.
#' @param y0 numeric. y-coordinate min of shear. Defaults to center y val if not given.
#' @param append_gimage whether to append params to `giottoLargeImage` (default)
#' or return as `list`
#' @keywords internal
.shear_image_params <- function(
        x,
        deg = NULL,
        rad = NULL,
        lin = NULL,
        extent = x,
        x0 = NULL,
        y0 = NULL,
        append_gimage = TRUE
) {

    checkmate::assert_numeric(deg, null.ok = TRUE, len = 2L)
    checkmate::assert_numeric(rad, null.ok = TRUE, len = 2L)
    checkmate::assert_numeric(lin, null.ok = TRUE, len = 2L)
    checkmate::assert_numeric(x0, null.ok = TRUE, len = 1L)
    checkmate::assert_numeric(y0, null.ok = TRUE, len = 1L)

    # 1. ------------- Get image bounds -------------- #
    # used for determining total xy displacement needed after shear
    # (due to image bounds)
    # This value from the whole image should be used even for crops
    e <- terra::ext(extent)[]
    # used for finding shear angle when linear shear is provided
    ext_dims <- c(
        "x" = abs(diff(c(e[["xmax"]], e[["xmin"]]))),
        "y" = abs(diff(c(e[["ymax"]], e[["ymin"]])))
    )


    # 1.1 ----------- Get shear as degrees ------------ #
    # magick::image_shear() is used to perform the shear operation.
    # The function's `geom` param accepts a shear along the x axis and y axis.
    # (respectively defined as clockwise angle from the y axis and the x axis)
    # This angle should be provided as *degrees*

    if (!is.null(lin)) { #                           (lin overrides rad input)
        rad <- c(atan(lin[1L], ext_dims[["y"]]),
                 atan(lin[2L], ext_dims[["x"]]))
    }
    if (!is.null(rad)) deg <- degrees(rad) #         (rad overrides deg input)


    # 1.1 ------------ Check shear values ------------- #
    # error if no or wrong shear input
    if (!checkmate::test_numeric(deg, len = 2L)) {
        .gstop("A numeric vector of length 2 must be supplied to one of 'deg', 'rad', or 'lin' params")
    }

    # use remainder. shear angles are defined up to 180 degrees.
    deg <- deg %% 180

    # error if shear is 90 degrees
    if (any(deg == 90)) {
        .gstop("Shear angles of 90 degrees not permitted")
    }


    # 2 ----- store shear info ------ #
    # calculate linear shear values if needed to determine extent increase
    if (is.null(lin)) {
        rad <- radians(deg)
        lin <- c(ext_dims[["y"]] * tan(rad),
                 ext_dims[["x"]] * tan(rad))
    }

    if (is.null(x0)) x0 <- mean(e[["xmax"]], e[["xmin"]])
    if (is.null(y0)) y0 <- mean(e[["ymax"]], e[["ymin"]])


    sparams <- list(
        extent = e,
        deg = deg,
        lin = lin,
        x0 = x0,
        y0 = y0
    )
    class(sparams) <- "shear_params"

    if (isTRUE(append_gimage)) {
        tf_idx <- length(x@lazy_tfs) + 1L
        x@lazy_tfs[[tf_idx]] <- sparams
        return(x)
    } else {
        return(sparams)
    }

}



