

#' @name crop-generic
#' @title Crop to a spatial subset
#' @description see [terra::crop]. Object x will be cropped using object y.
#' @param x object
#' @param y any object that has a SpatExtent or returns a SpatExtent
#' @param ... additional params to pass to terra::crop
NULL



#' @describeIn crop-generic Crop a giottoLargeImage
#' @export
setMethod('crop', signature('giottoLargeImage'), function(x, y, ...) {
  x@raster_object = terra::crop(x@raster_object, y, ...)
  x@extent = ext(x@raster_object)
  intensity_range = spatraster_intensity_range(x@raster_object)
  x@min_intensity = intensity_range[['min']]
  x@max_intensity = intensity_range[['max']]

  x
})
