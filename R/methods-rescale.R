# docs -------------------------------------------------------------- #
#' @title Rescale an object
#' @name rescale-generic
#' @description Rescale an object spatially. Z dimension scaling is supported for
#' some types of subobjects.
#' @param x object
#' @param fx numeric > 0. The horizontal scaling factor
#' @param fy numeric > 0. The vertical scaling factor
#' @param fz numeric > 0. The z scaling factor (only for supported objects)
#' @param x0 numeric. x-coordinate of the center of rescaling. If missing, the center of the extent of x is used
#' @param y0 numeric. y-coordinate of the center of rescaling. If missing, the center of the extent of x is used
#' @param z0 numeric. z-coordinate of the center of rescaling. If missing, the center of the extent of x is used
#' (only for supported objects)
NULL
# ------------------------------------------------------------------- #



#' @describeIn rescale-generic Rescale spatLocsObj
#' @export
setMethod(
  "rescale", signature("spatLocsObj"),
  function(x, fx = 1, fy = fx, fz = fx, x0, y0, z0) {
    # data.table vars
    sdimx <- sdimy <- sdimz <- NULL

    # find center
    if (missing(x0)) x0 <- x[][, mean(range(sdimx))]
    if (missing(y0)) y0 <- x[][, mean(range(sdimy))]
    if ("sdimz" %in% names(x[]) & missing(z0)) {
      z0 <- x[][, mean(range(sdimz))]
    } else {
      z0 <- 0
    }

    # perform scaling
    x[] <- scale_spatial_locations(
      spatlocs = x[],
      scale_factor = c(x = fx, y = fy, z = fz),
      scenter = c(x = x0, y = y0, z = z0)
    )
    x
  }
)



# TODO more methods for other objects






# internals ####

#' @title Scale spatial locations
#' @name scale_spatial_locations
#' @description Simple scaling of spatial locations by given \code{scale_factor}.
#' Values will be scaled from the coordinate origin or coordinates provided through
#' \code{scenter} param.
#' @param spatlocs spatial locations information to scale
#' @param scale_factor scaling factor to apply to coordinates.
#' @param scenter center from which to scale spatial coordinates. Given as vector
#' of xy(z) coordinates.
#' @details \code{scale_factor} either given as a single value where it will be applied to
#' x, y, and z (if available) dimensions or as a vector of named values for 'x',
#' 'y', (and 'z').
#' @keywords internal
scale_spatial_locations <- function(spatlocs,
                                    scale_factor = c(x = 1, y = 1, z = 1),
                                    scenter = c(x = 0, y = 0, z = 0)) {
  # data.table vars
  sdimx <- sdimy <- sdimz <- NULL

  hasZ <- "sdimz" %in% names(spatlocs)

  if (length(scale_factor) == 1) scale_factor <- c(x = scale_factor, y = scale_factor, z = scale_factor)
  if (!all(names(scenter) %in% c("x", "y", "z"))) stop("scenter value names not recognized")
  if (!all(names(scale_factor) %in% c("x", "y", "z"))) stop("scale_factor value names not recognized")

  # Adjust for scaling center
  spatlocs[, sdimx := sdimx - scenter[["x"]]]
  spatlocs[, sdimy := sdimy - scenter[["y"]]]

  # Perform scale
  spatlocs[, sdimx := sdimx * scale_factor[["x"]]]
  spatlocs[, sdimy := sdimy * scale_factor[["y"]]]

  if (isTRUE(hasZ)) {
    # Adjust for scaling z center
    spatlocs[, sdimz := sdimz - scenter[["z"]]]

    # Perform z scale
    spatlocs[, sdimz := sdimz * scale_factor[["z"]]]

    # Revert z scaling center adjustments
    spatlocs[, sdimz := sdimz + scenter[["z"]]]
  }

  # Revert scaling center adjustments
  spatlocs[, sdimx := sdimx + scenter[["x"]]]
  spatlocs[, sdimy := sdimy + scenter[["y"]]]

  return(spatlocs)
}







#' @title rescale polygons
#' @name rescale_polygons
#' @description  rescale individual polygons by a factor x and y
#' @keywords internal
rescale_polygons <- function(spatVector,
                             spatVectorCentroids,
                             fx = 0.5, fy = 0.5) {
  # DT vars
  poly_ID <- NULL

  spatVectorCentroidsDT <- spatVector_to_dt(spatVectorCentroids)

  cell_ids <- spatVector$poly_ID

  l <- lapply(cell_ids, FUN = function(id) {
    single_polygon <- spatVector[spatVector$poly_ID == id]
    single_centroid <- spatVectorCentroidsDT[poly_ID == id]

    single_polygon_resc <- terra::rescale(
      x = single_polygon,
      fx = fx, fy = fy,
      x0 = single_centroid$x,
      y0 = single_centroid$y
    )
  })

  new_polygons <- do.call("rbind", l)
  return(new_polygons)
}



#' @title rescalePolygons
#' @name rescalePolygons
#' @description Rescale individual polygons by a x and y factor
#' @param gobject giotto object
#' @param poly_info polygon information name
#' @param name name of new polygon layer
#' @param fx x-scaling factor
#' @param fy y-scaling factor
#' @param calculate_centroids calculate centroids
#' @param return_gobject return giotto object
#' @return giotto object
#' @concept polygon scaling
#' @export
rescalePolygons <- function(gobject,
                            poly_info = "cell",
                            name = "rescaled_cell",
                            fx = 0.5,
                            fy = 0.5,
                            calculate_centroids = TRUE,
                            return_gobject = TRUE) {
  # 1. get polygon information
  original <- get_polygon_info(
    gobject = gobject,
    polygon_name = poly_info,
    return_giottoPolygon = TRUE
  )

  original_vector <- slot(original, "spatVector")
  original_centroids <- slot(original, "spatVectorCentroids")

  if (is.null(original_centroids)) {
    stop("Selected polygons don't have associated centroid,
         use addSpatialCentroidLocations() ")
  }


  # 2. rescale polygon
  rescaled_original <- rescale_polygons(original_vector,
    original_centroids,
    fx = fx, fy = fy
  )

  # 3. create new Giotto polygon and calculate centroids
  S4_polygon <- create_giotto_polygon_object(
    name = name,
    spatVector = rescaled_original
  )
  if (calculate_centroids) {
    S4_polygon <- calculate_centroids_polygons(
      gpolygon = S4_polygon,
      append_gpolygon = TRUE
    )
  }


  # 4. return object or S4 polygon
  if (return_gobject) {
    # TODO: update parameters
    gobject <- setPolygonInfo(gobject = gobject, x = S4_polygon)
    return(gobject)
  } else {
    return(S4_polygon)
  }
}
