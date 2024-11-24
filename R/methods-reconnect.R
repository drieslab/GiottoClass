#' @name reconnect
#' @title Reconnect a GiottoClass object
#' @param x object to reconnect
#' @param path character. (optional) New filepath to associate with the object.
#' Only needed if the filepath is not normally carried by the object or the path
#' has changed.
#' @param ... additional params to pass
#' @returns GiottoClass object
#' @examples
#' f <- tempfile()
#' a <- GiottoData::loadSubObjectMini("giottoLargeImage")
#' saveRDS(a, f)
#'
#' b <- readRDS(f) # expected to be null pointer
#' b <- reconnect(b) # reconnected to source image
NULL



#' @rdname reconnect
#' @export
setMethod("reconnect", signature("giottoAffineImage"), function(x, path = NULL, ...) {
    path <- path %null% slot(x, "file_path")
    .image_path_checks(path)

    # replace old raster objects
    raster_object <- .create_terra_spatraster(image_path = path)
    slot(x, "raster_object") <- raster_object

    # inherit tracked extents (image extent and user-facing extent)
    img_ext <- x@affine@anchor
    user_ext <- x@extent

    # this method affects multiple slots, including the extent of the image, so do first
    ext(x) <- user_ext
    # this only affects the image extent, so do second
    ext(x@raster_object) <- img_ext

    return(initialize(x))
})


#' @rdname reconnect
#' @export
setMethod("reconnect", signature("giottoLargeImage"), function(x, path = NULL, ...) {
    path <- path %null% slot(x, "file_path")
    .image_path_checks(path)

    # replace old raster objects and inherit tracked extents
    raster_object <- .create_terra_spatraster(image_path = path)
    slot(x, "raster_object") <- raster_object
    ext(slot(x, "raster_object")) <- slot(x, "extent")

    return(initialize(x))
})


#' @rdname reconnect
#' @export
setMethod("reconnect", signature("giottoImage"), function(x, path = NULL, ...) {
    path <- path %null% slot(x, "file_path")
    .image_path_checks(path)

    # replace old magick object
    mg_object <- magick::image_read(path)
    slot(x, "mg_object") <- mg_object

    return(x)
})



# internals ####

.image_path_checks <- function(path) {
    if (is.null(path)) {
        stop(wrap_txt(
            "No filepath recorded in image object and no 'path' value provided."
        ))
    }
    if (!file.exists(path)) {
        stop(wrap_txt(sprintf(
            "image reconnection attempted, but %s does not exist", path
        )))
    }
    invisible(NULL)
}
