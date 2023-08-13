## ext ####

#' @name ext-generic
#' @title Get a SpatExtent
#' @description Get a SpatExtent of an object. This is the spatial minmax x and y
#' that the object is mapped to.
#' @param x spatial object
#' @param ... additional params to pass
NULL

#' @describeIn ext-generic Get extent of spatLocsObj
#' @export
setMethod('ext', signature('spatLocsObj'), function(x, ...) {
  sdimx = sdimy = NULL # dt vars
  terra::ext(c(range(x[][, sdimx]), range(x[][, sdimy])))
})

#' @describeIn ext-generic Get extent of giottoPolygon
#' @export
setMethod('ext', signature('giottoPolygon'), function(x, ...) {
  terra::ext(x@spatVector, ...)
})

#' @describeIn ext-generic Get extent of giottoPoints
#' @export
setMethod('ext', signature('giottoPoints'), function(x, ...) {
  terra::ext(x@spatVector, ...)
})

#' @describeIn ext-generic Get extent of spatialNetworkObj
#' @export
setMethod('ext', signature('spatialNetworkObj'), function(x, ...) {
  sdimx_begin = sdimx_end = sdimy_begin = sdimy_end = NULL # dt vars
  terra::ext(c(x[][, range(c(sdimx_begin, sdimx_end))], x[][, range(c(sdimy_begin, sdimy_end))]))
})

#' @rdname ext-generic
#' @export
setMethod('ext', signature('giottoLargeImage'), function(x, ...) {
  terra::ext(x@raster_object)
})

#' @rdname ext-generic
#' @export
setMethod('ext<-', signature(x = 'giottoLargeImage', value = 'SpatExtent'), function(x, value) {
  terra::ext(x@raster_object) = value
  x
})

# Convert numeric inputs to SpatExtent and have terra deal with inconsistencies
#' @rdname ext-generic
#' @export
setMethod('ext<-', signature(x = 'ANY', value = 'ANY'), function(x, value) {
  value = terra::ext(value)
  methods::callGeneric(x, value)
})
