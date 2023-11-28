## spin ####

#' @title Spin an object
#' @name spin-generic
#' @description Spin (rotate) an object spatially (limited to xy rotations)
#' @param x object
#' @param angle numeric. Angle of rotation in degrees
#' @param x0 numeric. x-coordinate of the center of rotation. Defaults to center x val if not given.
#' @param y0 numeric. y-coordinate of the center of rotation. Defaults to center y val if not given.
NULL

#' @describeIn spin-generic Spin a giottoPolygon object
#' @export
setMethod(
  "spin", signature(x = "giottoPolygon"),
  function(x, angle, x0 = NULL, y0 = NULL) {
    if (is.null(x0)) x0 <- terra::mean(terra::ext(x@spatVector))[1]
    if (is.null(y0)) y0 <- terra::mean(terra::ext(x@spatVector))[2]
    return(do_gpoly(x = x, what = "terra"::"spin", args = list(angle = angle, x0 = x0, y0 = y0)))
  }
)

#' @describeIn spin-generic Spin a giottoPoints object
#' @export
setMethod(
  "spin", signature(x = "giottoPoints"),
  function(x, angle, x0 = NULL, y0 = NULL) {
    if (is.null(x0)) x0 <- terra::mean(terra::ext(x@spatVector))[1]
    if (is.null(y0)) y0 <- terra::mean(terra::ext(x@spatVector))[2]
    x@spatVector <- terra::spin(x@spatVector,
      angle = angle,
      x0 = x0,
      y0 = y0
    )
    return(x)
  }
)

#' @describeIn spin-generic Spin a spatLocsObj
#' @param z0 spatLocsObj specific. Numeric. z-coordinate of the center of rotation.
#' Depending on if z data is present, defaults to either 0 or center z val if not given.
#' @param xy_angle spatLocsObj specific. xy plane rotation in degrees.
#' Overrides angle param
#' @param zy_angle spatLocsObj specific. zy plane rotation
#' @param xz_angle spatLocsObj specific. xz plane rotation
#' @export
setMethod(
  "spin", signature(x = "spatLocsObj"),
  function(x, angle = NULL, x0 = NULL, y0 = NULL, z0 = NULL,
           xy_angle = NULL, zy_angle = NULL, xz_angle = NULL) {
    if (!is.null(angle)) xy_angle <- angle
    if (is.null(xy_angle)) xy_angle <- 0
    if (is.null(zy_angle)) zy_angle <- 0
    if (is.null(xz_angle)) xz_angle <- 0
    angles <- c(xy = xy_angle, zy = zy_angle, xz = xz_angle)
    angles <- radians(angles)

    if (is.null(x0)) x0 <- mean(c(min(x[]$sdimx), max(x[]$sdimx)))
    if (is.null(y0)) y0 <- mean(c(min(x[]$sdimy), max(x[]$sdimy)))
    if ("sdimz" %in% colnames(x[])) {
      if (is.null(z0)) {
        z0 <- mean(c(min(x[]$sdimz), max(x[]$sdimz)))
      } else {
        z0 <- 0
      }
    }
    x[] <- rotate_spatial_locations(
      spatlocs = x[],
      rotateradians = angles,
      rcenter = c(x = x0, y = y0, z = z0)
    )
    return(x)
  }
)





# internals ####

#' @param DT data.table with xy values
#' @param xy character vector of the columns that contain respectively x and y
#' info
#' @noRd
rotate2D <- function(DT, xy = c("x", "y"), rotate_rad = NULL, rotate_deg = NULL) {
  if (is.null(rotate_rad) && is.null(rotate_deg) ||
    !is.null(rotate_rad) && !is.null(rotate_deg)) {
    stop(wrap_txt(
      "GiottoClass: rotate2D:
      rotation must be supplied through one of 'rotate_rad' or 'rotate_deg'"
    ))
  }

  if (!is.null(rotate_deg)) rotate_rad <- radians(deg = rotate_deg)

  xvals <- DT[, xy[[1L]], with = FALSE]
  yvals <- DT[, xy[[2L]], with = FALSE]
  DT[, (xy[[1L]]) := xvals * cos(rotate_rad) + yvals * sin(rotate_rad)]
  DT[, (xy[[2L]]) := -xvals * sin(rotate_rad) + yvals * cos(rotate_rad)]
  return(DT)
}

# TODO cleanup rotate_spatial_locations code using rotate2D

#' @title Rotate spatial locations
#' @name rotate_spatial_locations
#' @description Rotate given spatlocs by given radians
#' @param spatlocs spatial locations to use
#' @param rotateradians Named vector of radians for rotation along each of the 3 coordinate
#' axes. If only a single value is provided, it will be treated as xy rotation.
#' @param rcenter center of rotation given as vector xy(z) coordinates (defaults to coordinate center)
#' @details Radians are provided through \code{rotateradians} param as a named vector
#' with values for \code{xy} (yaw), \code{zy} (pitch), \code{xz} (roll)
#' @keywords internal
rotate_spatial_locations <- function(spatlocs,
                                     rotateradians = c(xy = 0, zy = 0, xz = 0),
                                     rcenter = c(x = 0, y = 0, z = 0)) {
  if (length(rotateradians) == 1) rotateradians <- c(xy = rotateradians, zy = 0, xz = 0)
  if (!all(names(rotateradians) %in% c("xy", "zy", "xz"))) stop("rotateradians value names not recognized")
  if (!all(names(rcenter) %in% c("x", "y", "z"))) stop("rcenter value names not recognized")
  hasZ <- "sdimz" %in% names(spatlocs)

  # xy center of rotation adjustment
  spatlocs$sdimx <- spatlocs$sdimx - rcenter[["x"]]
  spatlocs$sdimy <- spatlocs$sdimy - rcenter[["y"]]

  xvals <- spatlocs$sdimx
  yvals <- spatlocs$sdimy

  # Perform rotation XY
  if (rotateradians[["xy"]] != 0) {
    spatlocs$sdimx <- xvals * cos(rotateradians[["xy"]]) + yvals * sin(rotateradians[["xy"]])
    spatlocs$sdimy <- -xvals * sin(rotateradians[["xy"]]) + yvals * cos(rotateradians[["xy"]])
  }

  # if z values are available
  if (isTRUE(hasZ)) {
    # z center of rotation adjustment
    spatlocs$sdimz <- spatlocs$sdimz - rcenter[["z"]]

    zvals <- spatlocs$sdimz

    # Perform rotations
    if (rotateradians[["zy"]] != 0) {
      spatlocs$sdimz <- zvals * cos(rotateradians[["zy"]]) + yvals * sin(rotateradians[["zy"]])
      spatlocs$sdimy <- -zvals * sin(rotateradians[["zy"]]) + yvals * cos(rotateradians[["zy"]])
    }

    if (rotateradians[["xz"]] != 0) {
      spatlocs$sdimx <- xvals * cos(rotateradians[["xz"]]) + zvals * sin(rotateradians[["xz"]])
      spatlocs$sdimz <- -xvals * sin(rotateradians[["xz"]]) + zvals * cos(rotateradians[["xz"]])
    }

    # Revert z center of rotation adjustment
    spatlocs$sdimz <- spatlocs$sdimz + rcenter[["z"]]
  }

  # Revert xy center of rotation adjustment
  spatlocs$sdimx <- spatlocs$sdimx + rcenter[["x"]]
  spatlocs$sdimy <- spatlocs$sdimy + rcenter[["y"]]

  return(spatlocs)
}
