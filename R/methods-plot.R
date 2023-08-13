
#' @include classes.R
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






#' @describeIn plot-generic Plot \emph{magick}-based giottoImage object. ... param passes to \code{\link{plot_giottoImage_MG}}
#' @export
setMethod('plot', signature(x = 'giottoImage', y = 'missing'), function(x,y,...) plot_giottoImage_MG(giottoImage = x,...))

#' @describeIn plot-generic Plot \emph{terra}-based giottoLargeImage object. ... param passes to \code{\link{plot_giottoLargeImage}}
#' @importMethodsFrom terra plot
#' @export
setMethod('plot', signature(x = 'giottoLargeImage', y = 'missing'), function(x,y,...) plot_giottoLargeImage(giottoLargeImage = x,...))

#' @describeIn plot-generic Plot \emph{terra}-based giottoPolygon object. ... param passes to \code{\link[terra]{plot}}
#' @importMethodsFrom terra plot
#' @param point_size size of points when plotting giottoPolygon object centroids
#' @param type what to plot: either 'poly' (default) or polygon 'centroid'
#' @export
setMethod('plot', signature(x = 'giottoPolygon', y = 'missing'),
          function(x, point_size = 0.1, type = c('poly', 'centroid'), ...) {
            plot_giotto_polygon(x = x, point_size = point_size, type = type, ...)
          })

#' @describeIn plot-generic \emph{terra}-based giottoPoint object. ... param passes to \code{\link[terra]{plot}}
#' @param point_size size of points when plotting giottoPoints
#' @param feats specific features to plot within giottoPoints object (defaults to NULL, meaning all available features)
#' @param raster default = TRUE, whether to plot points as rasterized plot with
#' size based on \code{size} param
#' @param raster_size Default is 600. Only used when \code{raster} is TRUE
#' @export
setMethod('plot', signature(x = 'giottoPoints', y = 'missing'),
          function(x, point_size = 0, feats = NULL, raster = TRUE, raster_size = 600, ...) {
            plot_giotto_points(x = x, point_size = point_size, feats = feats,
                               raster = raster, raster_size = raster_size, ...)
          })


#' @describeIn plot-generic Plot a spatLocsObj
#' @export
setMethod('plot', signature(x = 'spatLocsObj', y = 'missing'), function(x, ...) {
  l = list(...)
  if(is.null(l$asp)) l$asp = 1
  if(is.null(l$xlab)) l$xlab = ''
  if(is.null(l$ylab)) l$ylab = ''
  if(is.null(l$cex)) l$cex = 0.5
  if(nrow(x) > 10000L) {
    if(is.null(l$pch)) l$pch = '.'
  }

  do.call('plot', append(l, list(x = x[]$sdimx, y = x[]$sdimy)))
})


#' @describeIn plot-generic Plot a spatialNetworkObj
#' @export
setMethod('plot', signature(x = 'spatialNetworkObj', y = 'missing'), function(x, ...) {
  l = list(...)
  if(is.null(l$asp)) l$asp = 1
  if(is.null(l$xlab)) l$xlab = ''
  if(is.null(l$ylab)) l$ylab = ''
  if(is.null(l$cex)) l$cex = 0.5
  if(is.null(l$col)) {
    line_col = 'red'
  } else {
    line_col = l$col
    l$col = NULL
  }
  if(is.null(l$lwd)) {
    line_width = 1L
  } else {
    line_width = l$lwd
    l$lwd = NULL
  }
  if(is.null(l$lty)) {
    line_type = 1L
  } else {
    line_type = l$lty
    l$lty = NULL
  }
  # find nodes
  nodes = unique(rbind(x[][, c('sdimx_begin', 'sdimy_begin')],
                       x[][, c('sdimx_end', 'sdimy_end')],
                       use.names = FALSE))
  if(nrow(nodes) > 10000L) {
    if(is.null(l$pch)) l$pch = '.'
  }
  do.call('plot', append(l, list(x = nodes$sdimx_begin, y = nodes$sdimy_begin)))
  segments(x0 = x[]$sdimx_begin, y0 = x[]$sdimy_begin,
           x1 = x[]$sdimx_end, y1 = x[]$sdimy_end,
           col = line_col, lty = line_type, lwd = line_width)
})







