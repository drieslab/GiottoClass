#' @include generics.R
NULL

# docs ----------------------------------------------------------- #
#' @title Zoom a Giotto spatial object
#' @name zoom
#' @description Works like [terra::zoom()]. Zoom in on a plot by providing
#' a new extent, by default this is done by clicking twice on the existing
#' plot. When passing a `SpatExtent` without drawing, no pre-existing plot
#' is needed.
#' @param x giotto image, giottoPolygon, or giottoPoints object
#' @param e SpatExtent
#' @param \dots additional parameters to pass to `plot()`
#' @returns SpatExtent (invisibly)
#' @examples
#' gimg <- GiottoData::loadSubObjectMini("giottoLargeImage")
#' gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")
#' gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
#' e <- ext(6400, 6800, -4860, -4750) # arbitrary
#'
#' # With extent passed
#' zoom(gimg, e)
#' zoom(gpoly, e)
#' zoom(gpoints, e)
#'
#' \dontrun{
#' # Examples that use manual drawing - can't be automatically run
#' plot(gimg)
#' zoom(gimg)
#'
#' plot(gpoly)
#' zoom(gpoly)
#'
#' # starting plot for points currently must be raster = FALSE for
#' # drawn extent to be accurate.
#' plot(gpoints, raster = FALSE)
#' # raster param passed to plot through zoom() CAN be TRUE or FALSE.
#' zoom(gpoints, raster = FALSE)
#' }
NULL
# ---------------------------------------------------------------- #


#' @rdname zoom
#' @export
setMethod(
    "zoom", signature("giottoLargeImage"),
    function(x, e = terra::draw(), ...) {
        plot(x, ext = e, ...)
        return(invisible(e))
    })

#' @rdname zoom
#' @export
setMethod(
    "zoom", signature("giottoPolygon"),
    function(x, e = terra::draw(), ...) {
        plot(x, ext = e, ...)
        return(invisible(e))
    })

# TODO does not work properly when rasterized
#' @rdname zoom
#' @export
setMethod(
    "zoom", signature("giottoPoints"),
    function(x, e = terra::draw(), ...) {
        plot(x, ext = e, ...)
        return(invisible(e))
    })
