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
#' g <- GiottoData::loadGiottoMini("vizgen")
#' gimg <- getGiottoImage(g, image_type = "largeImage")
#' gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")
#' gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
#' e <- ext(6400, 6800, -4860, -4750) # arbitrary
#'
#' # With extent passed
#' zoom(gimg, e)
#' zoom(gpoly, e)
#' zoom(gpoints, e)
#'
NULL
# ---------------------------------------------------------------- #


#' @rdname zoom
#' @export
setMethod(
    "zoom", signature("giottoLargeImage"),
    function(x, e = terra::draw(), ...) {
        plot(x, ext = e, ...)
        return(invisible(e))
    }
)

#' @rdname zoom
#' @export
setMethod(
    "zoom", signature("giottoPolygon"),
    function(x, e = terra::draw(), ...) {
        plot(x, ext = e, ...)
        return(invisible(e))
    }
)

# TODO does not work properly when rasterized
#' @rdname zoom
#' @export
setMethod(
    "zoom", signature("giottoPoints"),
    function(x, e = terra::draw(), ...) {
        plot(x, ext = e, ...)
        return(invisible(e))
    }
)
