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
setMethod(
  "spatShift", signature("spatLocsObj"),
  function(x, dx = 0, dy = 0, dz = 0, copy_obj = TRUE, ...)
  {
    argslist <- get_args_list()
    argslist$x <- x[]

    # pass to data.frame method
    x[] <- do.call(spatShift, argslist)

    return(x)
  }
)

#' @rdname spatShift
#' @param geom character. Named vector of colnames of x, y, (z) coordinate columns.
#' Default is `c("sdimx", "sdimy", "sdimz")`
#' @export
setMethod(
  "spatShift", signature("data.frame"),
  function(
    x, dx = 0, dy = 0, dz = 0, copy_obj = TRUE,
    geom = c("sdimx", "sdimy", "sdimz"), ...
  ) {

    x <- data.table::as.data.table(x)
    x <- .shift_spatial_locations(
      spatlocs = x,
      dx = dx, dy = dy, dz = dz,
      copy_obj = copy_obj,
      geom = geom, ...
    )

    return(x)
  }
)


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


#' @title Shift spatial locations
#' @name .shift_spatial_locations
#' @description Shift given coordinates by given translation values
#' @param spatlocs spatial locations to use
#' @param dx value to shift coordinates in the positive x direction
#' @param dy value to shift coordinates in the positive y direction
#' @param dz value to shift coordinates in the positive z direction
#' @param copy_obj copy/duplicate object (default = TRUE)
#' @keywords internal
.shift_spatial_locations <- function(spatlocs,
                                    dx = 0,
                                    dy = 0,
                                    dz = 0,
                                    geom = c("sdimx", "sdimy", "sdimz"),
                                    copy_obj = TRUE) {

  if(copy_obj) spatlocs <- data.table::copy(spatlocs)

  xyz <- c("x", "y", "z")

  if (is.null(names(geom))) names(geom) <- xyz
  if (!all(names(geom) %in% xyz)) stop("geom value names not recognized")

  spatlocs[, (geom[["x"]]) := get(geom[["x"]]) + dx]
  spatlocs[, (geom[["y"]]) := get(geom[["y"]]) + dy]
  if (geom[["z"]] %in% colnames(spatlocs)) {
    spatlocs[, (geom[["z"]]) := get(geom[["z"]]) + dz]
  }

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
