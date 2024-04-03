#' @title Set giotto subobjects into giotto object
#' @name setGiotto
#' @param gobject giotto object
#' @param x giottoSubobject to set
#' @param \dots additional params to pass to specific Giotto setter functions
#' @returns giottoSubobject
NULL



# methods ####

#' @rdname setGiotto
#' @export
setMethod("setGiotto", signature("giotto", "cellMetaObj"), 
        function(gobject, x, ...) {
    gobject <- setCellMetadata(gobject = gobject, x = x, ...)
    gobject
})

#' @rdname setGiotto
#' @export
setMethod("setGiotto", signature("giotto", "featMetaObj"), 
        function(gobject, x, ...) {
    gobject <- setFeatureMetadata(gobject = gobject, x = x, ...)
    gobject
})

#' @rdname setGiotto
#' @export
setMethod("setGiotto", signature("giotto", "exprObj"), 
        function(gobject, x, ...) {
    gobject <- setExpression(gobject = gobject, x = x, ...)
    gobject
})

#' @rdname setGiotto
#' @export
setMethod("setGiotto", signature("giotto", "giottoPoints"), 
        function(gobject, x, ...) {
    gobject <- setFeatureInfo(gobject = gobject, x = x, ...)
    gobject
})

#' @rdname setGiotto
#' @export
setMethod("setGiotto", signature("giotto", "giottoPolygon"), 
        function(gobject, x, ...) {
    gobject <- setPolygonInfo(gobject = gobject, x = x, ...)
    gobject
})

#' @rdname setGiotto
#' @export
setMethod("setGiotto", signature("giotto", "dimObj"), 
        function(gobject, x, ...) {
    gobject <- setDimReduction(gobject = gobject, x = x, ...)
    gobject
})

#' @rdname setGiotto
#' @export
setMethod("setGiotto", signature("giotto", "spatLocsObj"), 
        function(gobject, x, ...) {
    gobject <- setSpatialLocations(gobject = gobject, x = x, ...)
    gobject
})

#' @rdname setGiotto
#' @export
setMethod("setGiotto", signature("giotto", "spatEnrObj"), 
        function(gobject, x, ...) {
    gobject <- setSpatialEnrichment(gobject = gobject, x = x, ...)
    gobject
})

#' @rdname setGiotto
#' @export
setMethod("setGiotto", signature("giotto", "nnNetObj"), 
        function(gobject, x, ...) {
    gobject <- setNearestNetwork(gobject = gobject, x = x, ...)
    gobject
})

#' @rdname setGiotto
#' @export
setMethod("setGiotto", signature("giotto", "spatialNetworkObj"), 
        function(gobject, x, ...) {
    gobject <- setSpatialNetwork(gobject = gobject, x = x, ...)
    gobject
})

# TODO update this
#' @rdname setGiotto
#' @export
setMethod("setGiotto", signature("giotto", "giottoLargeImage"), 
        function(gobject, x, ...) {
    gobject <- setGiottoImage(gobject = gobject, image = x, 
                            image_type = "largeImage", name = x@name, ...)
    gobject
})

#' @rdname setGiotto
#' @export
setMethod("setGiotto", signature("giotto", "giottoImage"), 
        function(gobject, x, ...) {
    gobject <- setGiottoImage(gobject = gobject, image = x, 
                            image_type = "image", name = x@name, ...)
    gobject
})
