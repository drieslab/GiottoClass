#' @include generics.R
NULL

# docs ----------------------------------------------------------- #
#' @title Preview a Giotto spatial object
#' @name plot-generic
#' @description S4 generic for previewing Giotto's image and subcellular objects.
#' @param x giotto image, giottoPolygon, or giottoPoints object
#' @param y Not used.
#' @param \dots additional parameters to pass
#' @aliases plot
#' @family plot
NULL
# ---------------------------------------------------------------- #






#' @describeIn plot-generic Plot \emph{magick}-based giottoImage object. ... param passes to \code{\link{.plot_giottoimage_mg}}
#' @export
setMethod("plot", signature(x = "giottoImage", y = "missing"), function(x, y, ...) .plot_giottoimage_mg(giottoImage = x, ...))

#' @describeIn plot-generic Plot \emph{terra}-based giottoLargeImage object. ... param passes to \code{\link{.plot_giottolargeimage}}
#' @export
setMethod("plot", signature(x = "giottoLargeImage", y = "missing"), function(x, y, ...) .plot_giottolargeimage(giottoLargeImage = x, ...))

#' @describeIn plot-generic Plot \emph{terra}-based giottoPolygon object. ... param passes to \code{\link[terra]{plot}}
#' @param point_size size of points when plotting giottoPolygon object centroids
#' @param type what to plot: either 'poly' (default) or polygon 'centroid'
#' @param max_poly numeric. If `type` is not specified, maximum number of
#' polygons to plot before automatically switching to centroids plotting.
#' Default is 1e4. This value is settable using options("giotto.plot_max_poly")
#' @export
setMethod(
  "plot", signature(x = "giottoPolygon", y = "missing"),
  function(
    x,
    point_size = 0.6,
    type = c("poly", "centroid"),
    max_poly = getOption("giotto.plot_max_poly", 1e4),
    ...
  ) {
    if (length(x@unique_ID_cache) == 0) {
      stop(wrap_txt("No geometries to plot"), call. = FALSE)
    }

    # if greater than max_poly, simplify to centroid
    if (nrow(x) > max_poly &&
        length(type) == 2L) {
      type <- "centroid"
    }

    .plot_giotto_polygon(x = x, point_size = point_size, type = type, ...)
  }
)

#' @describeIn plot-generic \emph{terra}-based giottoPoint object. ... param passes to \code{\link[terra]{plot}}
#' @param point_size size of points when plotting giottoPoints
#' @param feats specific features to plot within giottoPoints object (defaults to NULL, meaning all available features)
#' @param raster default = TRUE, whether to plot points as rasterized plot with
#' size based on \code{size} param
#' @param raster_size Default is 600. Only used when \code{raster} is TRUE
#' @export
setMethod(
  "plot", signature(x = "giottoPoints", y = "missing"),
  function(x, point_size = 0, feats = NULL, raster = TRUE, raster_size = 600, ...) {
    if (length(x@unique_ID_cache) == 0) {
      stop(wrap_txt("No geometries to plot"), call. = FALSE)
    }
    .plot_giotto_points(
      x = x, point_size = point_size, feats = feats,
      raster = raster, raster_size = raster_size, ...
    )
  }
)


#' @describeIn plot-generic Plot a spatLocsObj
#' @export
setMethod("plot", signature(x = "spatLocsObj", y = "missing"), function(x, ...) {
  l <- list(...)
  if (is.null(l$asp)) l$asp <- 1
  if (is.null(l$xlab)) l$xlab <- "x coordinates"
  if (is.null(l$ylab)) l$ylab <- "y coordinates"
  if (is.null(l$cex)) l$cex <- 0.5
  if (nrow(x) > 10000L) {
    if (is.null(l$pch)) l$pch <- "."
  }

  do.call("plot", append(l, list(x = x[]$sdimx, y = x[]$sdimy)))
})

#' @describeIn plot-generic Plot a dimObj
#' @param dims dimensions to plot
#' @export
setMethod(
  "plot", signature(x = "dimObj", y = "missing"),
  function(
    x, dims = c(1,2), ...
  ) {
    plot_vals <- x[][, dims]

    l <- list(...)
    if (is.null(l$asp)) l$asp <- 1
    if (is.null(l$xlab)) l$xlab <- colnames(plot_vals)[1L]
    if (is.null(l$ylab)) l$ylab <- colnames(plot_vals)[2L]
    if (is.null(l$cex)) l$cex <- 0.5
    if (nrow(x) > 10000L) {
      if (is.null(l$pch)) l$pch <- "."
    } else {
      if (is.null(l$pch)) l$pch <- 20
    }

    do.call("plot", append(l, list(x = plot_vals[,1], y = plot_vals[,2])))
  }
)


#' @describeIn plot-generic Plot a spatialNetworkObj
#' @export
setMethod("plot", signature(x = "spatialNetworkObj", y = "missing"), function(x, ...) {
  l <- list(...)
  if (is.null(l$asp)) l$asp <- 1
  if (is.null(l$xlab)) l$xlab <- ""
  if (is.null(l$ylab)) l$ylab <- ""
  if (is.null(l$cex)) l$cex <- 0.5
  if (is.null(l$col)) {
    line_col <- "red"
  } else {
    line_col <- l$col
    l$col <- NULL
  }
  if (is.null(l$lwd)) {
    line_width <- 1L
  } else {
    line_width <- l$lwd
    l$lwd <- NULL
  }
  if (is.null(l$lty)) {
    line_type <- 1L
  } else {
    line_type <- l$lty
    l$lty <- NULL
  }
  # find nodes
  nodes <- unique(rbind(x[][, c("sdimx_begin", "sdimy_begin")],
    x[][, c("sdimx_end", "sdimy_end")],
    use.names = FALSE
  ))
  if (nrow(nodes) > 10000L) {
    if (is.null(l$pch)) l$pch <- "."
  }
  do.call("plot", append(l, list(x = nodes$sdimx_begin, y = nodes$sdimy_begin)))
  segments(
    x0 = x[]$sdimx_begin, y0 = x[]$sdimy_begin,
    x1 = x[]$sdimx_end, y1 = x[]$sdimy_end,
    col = line_col, lty = line_type, lwd = line_width
  )
})






# internals ####

#' @title .plot_giottoimage_mg
#' @name .plot_giottoimage_mg
#' @description get and plot a giottoImage either directly or from a giotto object
#' @param gobject giotto object
#' @param image_name name of giotto image \code{\link{showGiottoImageNames}}
#' @param giottoImage giottoImage object
#' @return plot
#' @keywords internal
.plot_giottoimage_mg <- function(gobject = NULL,
                                image_name = NULL,
                                giottoImage = NULL) {
  if (!is.null(giottoImage)) {
    graphics::plot(giottoImage@mg_object)
  } else {
    if (is.null(gobject)) stop("The giotto object that will be updated needs to be provided \n")
    if (is.null(image_name)) stop("The name of the giotto image that will be updated needs to be provided \n")

    g_image_names <- names(gobject@images)
    if (!image_name %in% g_image_names) stop(image_name, " was not found among the image names, see showImageNames()")

    graphics::plot(gobject@images[[image_name]]@mg_object)
  }
}





# TODO link this up to plot_auto_largeImage_resample() ?

#' @title .plot_giottolargeimage
#' @name .plot_giottolargeimage
#' @description Plot a \emph{downsampled} version of giottoLargeImage. Cropping can increase plot resolution of region of interest.
#' @param gobject giotto object
#' @param largeImage_name name of giottoLargeImage
#' @param giottoLargeImage giottoLargeImage object
#' @param crop_extent (optional) extent object to focus on specific region of image
#' @param xmax_crop,xmin_crop,ymax_crop,ymin_crop (optional) crop min/max x and y bounds
#' @param max_intensity (optional) value to treat as maximum intensity in color scale
#' @param asRGB (optional) boolean. Force RGB plotting if not automatically detected
#' @param stretch character. Option to stretch the values to increase contrast: "lin"
#' linear or "hist" (histogram)
#' @param axes boolean. Default = TRUE. Whether to draw axes
#' @param smooth boolean. default = TRUE. whether to apply smoothing on the image
#' @param mar plot margins default = c(3,5,1.5,1)
#' @param legend whether to plot legend of color scale (grayscale only).
#' default = FALSE
#' @param maxcell positive integer. Maximum number of image cells to use for the plot
#' @param col character. Colors for single channel images. The default is
#' grDevices::grey.colors(n = 256, start = 0, end = 1, gamma = 1). It can also be a
#' data.frame with two columns (value, color) to get a "classes" type legend or with
#' three columns (from, to, color) to get an "interval" type legend
#' @param asp numeric. (default = 1) specific aspect ratio to use
#' @param ... additional params to pass to terra::plot or terra::plotRGB depending
#' depending on image type
#' @return plot
#' @keywords internal
.plot_giottolargeimage <- function(gobject = NULL,
                                  largeImage_name = NULL,
                                  giottoLargeImage = NULL,
                                  crop_extent = NULL,
                                  xmax_crop = NULL,
                                  xmin_crop = NULL,
                                  ymax_crop = NULL,
                                  ymin_crop = NULL,
                                  max_intensity = NULL,
                                  asRGB = FALSE,
                                  stretch = NULL,
                                  axes = TRUE,
                                  smooth = TRUE,
                                  mar = c(3, 5, 1.5, 1),
                                  legend = FALSE,
                                  maxcell = 5e5,
                                  col = grDevices::grey.colors(n = 256, start = 0, end = 1, gamma = 1),
                                  asp = 1,
                                  ...) {
  # Get giottoLargeImage and check and perform crop if needed
  giottoLargeImage <- cropGiottoLargeImage(
    gobject = gobject,
    largeImage_name = largeImage_name,
    giottoLargeImage = giottoLargeImage,
    crop_extent = crop_extent,
    xmax_crop = xmax_crop,
    xmin_crop = xmin_crop,
    ymax_crop = ymax_crop,
    ymin_crop = ymin_crop
  )

  raster_object <- giottoLargeImage@raster_object

  # Determine likely image bitdepth
  if (is.null(max_intensity)) {
    bitDepth <- ceiling(log(x = giottoLargeImage@max_intensity, base = 2))
    # Assign discovered bitdepth as max_intensity
    max_intensity <- 2^bitDepth - 1

    # account for situations where values are scaled from 0 to 1
    if (max_intensity == 0) {
      max_intensity <- 1
    }
  }

  # plot
  if (isTRUE(asRGB) ||
    terra::has.RGB(raster_object) ||
    terra::nlyr(raster_object) >= 3) {
    terra::plotRGB(raster_object,
      axes = axes,
      r = 1, g = 2, b = 3,
      scale = max_intensity,
      stretch = stretch,
      smooth = smooth,
      mar = mar,
      maxcell = maxcell,
      asp = asp,
      ...
    )
  } else {
    if (is.null(stretch)) stretch <- "lin"
    terra::plot(raster_object,
      col = col,
      axes = axes,
      range = c(0, max_intensity),
      stretch = stretch,
      smooth = smooth,
      mar = mar,
      maxcell = maxcell,
      legend = legend,
      asp = asp,
      ...
    )
  }
}







#' @name .plot_giotto_points
#' @title Plot a giotto points object
#' @param x giottoPoints object
#' @param point_size (default = 0.1) size of plotted points
#' @param feats (default is all) which features to plot
#' @param raster whether to plot using rasterized method (default = TRUE)
#' @param raster_size size of rasterized plot to generate
#' @param ... additional params to pass to plot functions
#' @keywords internal
#' @noRd
.plot_giotto_points <- function(x,
                               point_size = 0,
                               feats = NULL,
                               raster = TRUE,
                               raster_size = 600L,
                               ...) {
  args_list <- list(feats, asp = 1L, ...)

  # point size
  if (is.null(args_list$cex)) args_list$cex <- point_size

  # get values to plot
  args_list$data <- x[]


  # plot
  if (raster) {
    package_check(
      "scattermore",
      repository = "CRAN",
      custom_msg = "scattermore must be installed for plotting mode 'raster' = TRUE
      To install:
      install.packages('scattermore')"
    )
    args_list$size <- raster_size
    do.call(".plot_giotto_points_raster", args_list)
  } else {
    do.call(".plot_giotto_points_vector", args_list)
  }
}



#' @description plot giotto points on a raster
#' @param data points SpatVector
#' @param feats feature(s) to plot. Leaving NULL plots all points
#' Rasterized plotting workflow for giottoPoints via scattermore
#' @param ... additional params to pass
#' @noRd
.plot_giotto_points_raster <- function(data, feats = NULL, ...) {
  args_list <- list(...)

  opar <- par(no.readonly = TRUE)
  on.exit(par(opar), add = TRUE)


  # raster size
  if (is.null(args_list$size)) {
    args_list$size <- c(600, 600)
  } else if (length(args_list$size) == 1L) {
    # if size provided as single value, replicate to give a square window
    args_list$size <- rep(args_list$size, 2L)
  }

  # axis font size
  if (is.null(args_list$cex.axis)) args_list$cex.axis <- 0.7

  args_list$ann <- FALSE

  if (is.null(feats)) {
    include_values = FALSE
  } else {
    include_values = TRUE
  }

  dataDT <- data.table::as.data.table(
    x = data,
    geom = "XY",
    include_values = include_values
  )


  if (length(feats) == 0L) {

    .plot_giotto_points_all(
      dataDT = dataDT,
      args_list = args_list
    )

  } else if (length(feats) == 1L) {

    .plot_giotto_points_one(
      dataDT = dataDT,
      feats = feats,
      args_list = args_list
    )

  } else {

    .plot_giotto_points_several(
      dataDT = dataDT,
      feats = feats,
      args_list = args_list
    )

  }
}



.plot_giotto_points_all <- function(dataDT, args_list) {
  par(mar = c(2.7, 3.5, 2, 2))

  args_list$x <- dataDT$x
  args_list$y <- dataDT$y
  args_list$col <- "white"

  plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
  u <- par("usr") # coordinates of the plot area
  rect(u[1], u[3], u[2], u[4], col = "black", border = NA)
  par(new = TRUE)

  do.call(scattermore::scattermoreplot, args_list)
}



.plot_giotto_points_one <- function(dataDT, feats, args_list) {

  # NSE vars
  feat_ID <- NULL

  if (!feats %in% dataDT[, feat_ID]) {
    .gstop(vector_to_string(feats), "not found in giottoPoints", .n = 6L)
  }

  par(mar = c(2.7, 3.5, 2, 2))

  dataDT <- dataDT[feat_ID == feats] # select single feats's data
  args_list$x <- dataDT$x
  args_list$y <- dataDT$y
  args_list$col <- "white"

  plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
  u <- par("usr") # coordinates of the plot area
  rect(u[1], u[3], u[2], u[4], col = "black", border = NA)
  par(new = TRUE)

  do.call(scattermore::scattermoreplot, args_list)
}



.plot_giotto_points_several <- function(dataDT, feats, args_list) {
  # NSE vars
  feat_color_idx <- feat_ID <- NULL

  missing_feats <- feats[!feats %in% dataDT[, feat_ID]]
  if (length(missing_feats) > 0L) {
    .gstop(vector_to_string(missing_feats), "not found in giottoPoints", .n = 6L)
  }

  par(mar = c(2.7, 3.5, 2, 4))
  feat_colors <- getRainbowColors(length(feats))

  data.table::setkey(dataDT, "feat_ID")
  dataDT <- dataDT[feat_ID %in% feats]
  dataDT[, feat_color_idx :=
           sapply(feat_ID, function(feat_i) which(feats == feat_i))]

  args_list$x <- dataDT$x
  args_list$y <- dataDT$y
  args_list$col <- feat_colors[dataDT$feat_color_idx]

  plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
  u <- par("usr") # coordinates of the plot area
  rect(u[1], u[3], u[2], u[4], col = "black", border = NA)
  par(new = TRUE)

  do.call(scattermore::scattermoreplot, args_list)
  legend(
    x = "topright",
    inset = c(-1.3 / dev.size()[1], 0),
    legend = feats,
    col = feat_colors,
    bty = "n",
    pch = 20,
    cex = 0.6,
    title = "feat_ID",
    xpd = TRUE
  )
}







#' @description plot giotto points with a base plot
#' @param data points SpatVector
#' @param feats feature(s) to plot. Leaving NULL plots all points
#' @param ... additional params to pass
#' Vectorized plotting workflow for giottoPoints via base plot()
#' @noRd
.plot_giotto_points_vector <- function(data, feats = NULL, ...) {
  args_list <- list(...)

  # base plot does not understand cex of 0
  if (args_list$cex == 0) args_list$cex <- 0.1

  args_list$background <- "black"

  if (is.null(feats)) {
    args_list$x <- data
    args_list$col <- "white"
    do.call(terra::plot, args_list)
  } else {
    args_list$x <- terra::subset(data, terra::values(data)$feat_ID %in% feats)
    if (length(feats) == 1L) {
      args_list$col <- "white"
    }
    if (length(feats) > 1L) {
      args_list$y <- "feat_ID"
    }
    do.call(terra::plot, args_list)
  }
}





#' @name .plot_giotto_polygon
#' @title Plot a giotto polygon object
#' @param x giottoPolygon object
#' @param point_size (default = 0.6) size of plotted points when plotting centroids
#' @param type (default is poly) plot the 'polygon' or its 'centroid'
#' @param ... additional params to pass to plot function
#' @keywords internal
#' @noRd
.plot_giotto_polygon <- function(x, point_size = 0.6,
                                type = c("poly", "centroid"), ...) {
  type <- match.arg(type, choices = c("poly", "centroid"))
  if (type == "poly") {
    terra::plot(x = x@spatVector, ...)
  }
  if (type == "centroid") {
    if (!is.null(x@spatVectorCentroids)) {
      terra::plot(x = x@spatVectorCentroids, cex = point_size, ...)
    } else {
      cat("no centroids calculated\n")
    }
  }
}
