#' @name reconnect
#' @title Reconnect a GiottoClass object
#' @param x object to reconnect
#' @param path character. (optional) New filepath to associate with the object.
#' Only needed if the filepath is not normally carried by the object or the path
#' has changed.
#' @param ... additional params to pass
#' @returns GiottoClass object
NULL





#' @rdname reconnect
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' g_image <- getGiottoImage(g, image_type = "largeImage")
#'
#' reconnect(g_image)
#' @export
setMethod("reconnect", signature("giottoLargeImage"), function(x, path = NULL, ...) {
    path <- path %null% slot(x, "file_path")
    .image_path_checks(path)

    # replace old raster objects and inherit tracked extents
    raster_object <- .create_terra_spatraster(image_path = path)
    slot(x, "raster_object") <- raster_object
    terra::ext(slot(x, "raster_object")) <- slot(x, "extent")

    return(x)
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
