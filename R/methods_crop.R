
# documentation ####

#' @name crop-generic
#' @title Crop to a spatial subset
#' @description see [terra::crop]. Object x will be cropped using object y.
#' @param x object
#' @param y any object that has a SpatExtent or returns a SpatExtent
#' @param \dots additional params to pass to terra::crop
NULL






# methods ####



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


#' @describeIn crop-generic Crop a giottoPoints
#' @param DT logical. Use alternative DT subsetting for crop operation
#' @param xmin,xmax,ymin,ymax only used if DT = TRUE. Set extent bounds
#' independently
#' @export
setMethod('crop', signature('giottoPoints'), function(
    x, y, DT = TRUE, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, ...
) {
  checkmate::assert_logical(DT)
  if(DT) {
    # converting to DT, subsetting, then regeneration of SpatVector with vect()
    # is currently faster than using terra::crop() as of 9/21/23
    missing_y = missing(y)
    n_single_bounds = 4 - sum(sapply(list(xmin, xmax, ymin, ymax), is.null))

    # check cropping params
    # ONLY y OR the single spat bounds can be used at any one time
    if((missing_y && n_single_bounds == 0) ||
       (!missing_y && n_single_bounds > 0)) {
      stop(wrap_txt('Crop bounds must be supplied through either a SpatExtent passed to \'y\'
                    or single numerical bounds passed to one or more of \'xmin\',\'xmax\', \'ymin\', \'ymax\''))
    }

    # 1. Get full set of cropping bounds
    if(!missing_y) {
      # if y is available, use y values directly.
      # only the extent of y is usable for the DT method
      if(!inherits(y, 'SpatExtent')) {
        warning(wrap_txt('Only the extent of y is used when cropping with DT = TRUE'))
        y = ext(y)
      }

      xmin = terra::xmin(y)
      xmax = terra::xmax(y)
      ymin = terra::ymin(y)
      ymax = terra::ymax(y)

    } else {
      # otherwise, fill in any spatial subset bounds that may not have been
      # supplied with the current extent value(s)
      current_ext = ext(x)
      if(is.null(xmin)) xmin = terra::xmin(current_ext)
      if(is.null(xmax)) xmax = terra::xmax(current_ext)
      if(is.null(ymin)) ymin = terra::ymin(current_ext)
      if(is.null(ymax)) ymax = terra::ymax(current_ext)
    }

    # 2. convert to DT
    sv = x@spatVector
    spatDT = as.data.table(sv, geom = 'XY')

    # 3. spatial subset then vect() to SpatVector again
    spatDT_subset = spatDT[x >= xmin & x <= xmax & y >= ymin & y <= ymax]
    sv_subset = terra::vect(spatDT_subset, c('x', 'y'))

    # 4. update x
    x@spatVector = sv_subset

  } else {
    x@spatVector = terra::crop(x@spatVector, y, ...)
  }

  # update ID cache and return
  x@unique_ID_cache = unique(terra::values(x@spatVector)$feat_ID)
  x
})



