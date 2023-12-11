#' @name spatShift
#' @title Spatially shift an object
#' @param x object
#' @param dx numeric. The shift on the x axis
#' @param dy numeric. The shift on the y axis
#' @param dz numeric. The shift on the z axis
#' @param copy_obj Default = TRUE
#' @param ... additional params to pass to methods
#' @description Shift the spatial locations of an object
NULL




## spatShift ####


#' @describeIn spatShift Shift the locations of a spatLocsObj
#' @export
setMethod("spatShift", signature("spatLocsObj"), function(x, dx = 0, dy = 0, dz = 0,
                                                          copy_obj = TRUE, ...) {
  x[] <- shift_spatial_locations(spatlocs = x[], dx = dx, dy = dy, dz = dz, ...)
  return(x)
})
#' @describeIn spatShift Shift the locations of a spatialNetworkObj
#' @export
setMethod("spatShift", signature("spatialNetworkObj"), function(x, dx = 0, dy = 0, dz = 0,
                                                                copy_obj = TRUE, ...) {
  x@networkDT <- shift_spatial_network(
    spatnet = x@networkDT,
    dx = dx, dy = dy, dz = dz, ...
  )
  if (!is.null(x@networkDT_before_filter)) {
    x@networkDT_before_filter <- shift_spatial_network(
      spatnet = x@networkDT_before_filter,
      dx = dx, dy = dy, dz = dz, ...
    )
  }
  return(x)
})

#' @rdname spatShift
#' @export
setMethod("spatShift", signature("giottoPolygon"), function(x, dx = 0, dy = 0, copy_obj = FALSE, ...) {
  shift_gpoly(gpoly = x, dx = dx, dy = dy, copy_obj = copy_obj, ...)
})

#' @rdname spatShift
#' @export
setMethod("spatShift", signature("giottoPoints"), function(x, dx = 0, dy = 0, copy_obj = FALSE, ...) {
  shift_gpoints(gpoints = x, dx = dx, dy = dy, copy_obj = copy_obj, ...)
})

#' @rdname spatShift
#' @export
setMethod("spatShift", signature("giottoLargeImage"), function(x, dx = 0, dy = 0, copy_obj = FALSE, ...) {
  shift_large_image(image = x, dx = dx, dy = dy, copy_obj = copy_obj, ...)
})






# internals ####

# internal for deprecation
xy_translate_spatial_locations <- function(...) {
  .Deprecated(new = "shift_spatial_locations")

  shift_spatial_locations(...)
}


#' @title Shift spatial locations
#' @name shift_spatial_locations
#' @description Shift given coordinates by given translation values
#' @param spatlocs spatial locations to use
#' @param dx value to shift coordinates in the positive x direction
#' @param dy value to shift coordinates in the positive y direction
#' @param dz value to shift coordinates in the positive z direction
#' @param xtranslate deprecated. use dx
#' @param ytranslate deprecated. use dy
#' @param ztranslate deprecated. use dz
#' @param copy_obj copy/duplicate object (default = TRUE)
#' @keywords internal
shift_spatial_locations <- function(spatlocs,
                                    dx = 0,
                                    dy = 0,
                                    dz = 0,
                                    xtranslate = NULL,
                                    ytranslate = NULL,
                                    ztranslate = NULL,
                                    copy_obj = TRUE) {
  sdimx <- sdimy <- sdimz <- NULL

  if (!is.null(xtranslate)) {
    warning(wrap_txt("xtranslate is deprecated. use dx"))
    dx <- xtranslate
  }
  if (!is.null(ytranslate)) {
    warning(wrap_txt("ytranslate is deprecated. use dy"))
    dy <- ytranslate
  }
  if (!is.null(ztranslate)) {
    warning(wrap_txt("ztranslate is deprecated. use dz"))
    dz <- ztranslate
  }

  spatlocs[, sdimx := sdimx + dx]
  spatlocs[, sdimy := sdimy + dy]
  if ("sdimz" %in% names(spatlocs)) spatlocs[, sdimz := sdimz + dz]

  return(spatlocs)
}









# See function spatShift in generics.R
#' @name shift_spatial_network
#' @title Shift spatial network
#' @description Shift spatial network coordinates
#' @param spatnet spatial network data.table
#' @param dx distance to shift on x axis
#' @param dy distance to shift on y axis
#' @param dz distance to shift on z axis
#' @param copy_obj copy/duplicate object (default = TRUE)
#' @keywords internal
shift_spatial_network <- function(spatnet, dx = 0, dy = 0, dz = 0, copy_obj = TRUE) {
  sdimx_begin <- sdimx_end <- sdimy_begin <- sdimy_end <- sdimz_begin <- sdimz_end <- NULL

  # if 3D info present
  is3D <- FALSE
  if (all(c("sdimz_begin", "sdimz_end") %in% colnames(spatnet))) is3D <- TRUE

  if (copy_obj) spatnet <- data.table::copy(spatnet)

  spatnet[, `:=`(
    sdimx_begin = sdimx_begin + dx,
    sdimx_end = sdimx_end + dx,
    sdimy_begin = sdimy_begin + dy,
    sdimy_end = sdimy_end + dy
  )]
  if (is3D) {
    spatnet[, `:=`(
      sdimz_begin = sdimz_begin + dz,
      sdimz_end = sdimz_end + dz
    )]
  }
  return(spatnet)
}





#' @rdname spatShift
#' @param ... additional params to pass
#' @keywords internal
#' @noRd
shift_large_image <- function(image,
                              dx = 0,
                              dy = 0,
                              copy_obj = FALSE,
                              ...) {
  if (copy_obj) image@raster_object <- terra::deepcopy(image@raster_object)

  if (!all(dx == 0, dy == 0)) {
    image@raster_object <- terra::shift(image@raster_object, dx = dx, dy = dy, ...)
  }
  image
}

#' @rdname spatShift
#' @keywords internal
#' @noRd
shift_gpoints <- function(gpoints,
                          dx = 0,
                          dy = 0,
                          copy_obj = FALSE,
                          ...) {
  if (copy_obj) gpoints@spatVector <- terra::deepcopy(gpoints@spatVector)

  if (!all(dx == 0, dy == 0)) {
    gpoints@spatVector <- terra::shift(gpoints@spatVector, dx = dx, dy = dy, ...)
  }
  gpoints
}

#' @rdname spatShift
#' @keywords internal
#' @noRd
shift_gpoly <- function(gpoly,
                        dx = 0,
                        dy = 0,
                        copy_obj = FALSE,
                        ...) {
  if (copy_obj) gpoly@spatVector <- terra::deepcopy(gpoly@spatVector)

  if (!all(dx == 0, dy == 0)) {
    gpoly <- .do_gpoly(gpoly,
      what = terra::shift,
      args = list(
        dx = dx,
        dy = dy,
        ...
      )
    )
  }
  gpoly
}
