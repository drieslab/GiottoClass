## ext ####

#' @name ext
#' @aliases ext<-
#' @title Get a SpatExtent
#' @description Get a SpatExtent of an object. This is the spatial minmax x 
#' and y that the object is mapped to.
#' @param x spatial object
#' @param value value to set. Accepts any object that `ext()` will work on
#' @param ... additional params to pass
#' @returns SpatExtent
NULL

#' @rdname ext
#' @export
setMethod("ext", signature("spatLocsObj"), function(x, ...) {
    sdimx <- sdimy <- NULL # dt vars
    terra::ext(c(range(x[][, sdimx]), range(x[][, sdimy])))
})

#' @rdname ext
#' @export
setMethod("ext", signature("giottoPolygon"), function(x, ...) {
    terra::ext(x@spatVector, ...)
})

#' @rdname ext
#' @export
setMethod("ext", signature("giottoPoints"), function(x, ...) {
    terra::ext(x@spatVector, ...)
})

#' @rdname ext
#' @export
setMethod("ext", signature("spatialNetworkObj"), function(x, ...) {
    sdimx_begin <- sdimx_end <- sdimy_begin <- sdimy_end <- NULL # dt vars
    terra::ext(c(x[][, range(c(sdimx_begin, sdimx_end))], 
                x[][, range(c(sdimy_begin, sdimy_end))]))
})

#' @rdname ext
#' @export
setMethod("ext", signature("giottoLargeImage"), function(x, ...) {
    terra::ext(x@raster_object)
})

#' @rdname ext
#' @export
setMethod("ext", signature("giottoImage"), function(x, ...) {
    terra::ext((x@boundaries + x@minmax)[c(2, 1, 4, 3)])
})

#' @rdname ext
#' @export
setMethod("ext<-", signature(x = "giottoPoints", value = "SpatExtent"), 
        function(x, value) {
    old_ext <- .ext_to_num_vec(ext(x))
    new_ext <- .ext_to_num_vec(value)
    xy_scale <- c(
        diff(new_ext[c(2, 1)]) / diff(old_ext[c(2, 1)]),
        diff(new_ext[c(4, 3)]) / diff(old_ext[c(4, 3)])
    )
    x@spatVector <- terra::rescale(x@spatVector, fx = xy_scale[1], 
                                fy = xy_scale[2], x0 = old_ext[1L], 
                                y0 = old_ext[3L])
    x <- spatShift(x, dx = new_ext[1L] - old_ext[1L], 
                dy = new_ext[3L] - old_ext[3L])
    x
})

#' @rdname ext
#' @export
setMethod("ext<-", signature(x = "giottoPolygon", value = "SpatExtent"), 
        function(x, value) {
    old_ext <- .ext_to_num_vec(ext(x))
    new_ext <- .ext_to_num_vec(value)
    xy_scale <- c(
        diff(new_ext[c(2, 1)]) / diff(old_ext[c(2, 1)]),
        diff(new_ext[c(4, 3)]) / diff(old_ext[c(4, 3)])
    )
    x <- .do_gpoly(x, terra::rescale, 
                args = list(fx = xy_scale[1], fy = xy_scale[2], 
                            x0 = old_ext[1L], y0 = old_ext[3L]))
    x <- spatShift(x, 
                dx = new_ext[1L] - old_ext[1L], dy = new_ext[3L] - old_ext[3L])
    x
})

#' @rdname ext
#' @export
setMethod("ext<-", signature(x = "giottoLargeImage", 
                            value = "SpatExtent"), function(x, value) {
    terra::ext(x@raster_object) <- value
    x@extent <- value
    x
})

# Convert numeric inputs to SpatExtent and have terra deal with inconsistencies
#' @rdname ext
#' @export
setMethod("ext<-", signature(x = "ANY", value = "ANY"), function(x, value) {
    value <- terra::ext(value)
    methods::callGeneric(x, value)
})

# Helper function to convert a SpatExtent object to a simple numeric vector
# and strip the names
.ext_to_num_vec <- function(x) {
    out <- x[]
    names(out) <- NULL
    out
}
