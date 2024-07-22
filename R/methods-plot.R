#' @include generics.R
NULL

# docs ----------------------------------------------------------- #
#' @title Preview a Giotto spatial object
#' @name plot-generic
#' @description S4 generic for previewing Giotto's image and subcellular
#' objects.
#' @param x giotto image, giottoPolygon, or giottoPoints object
#' @param y Not used.
#' @param \dots additional parameters to pass
#' @returns plot
#' @aliases plot
#' @family plot
NULL
# ---------------------------------------------------------------- #






#' @describeIn plot-generic Plot \emph{magick}-based giottoImage object. ... param passes to \code{\link{.plot_giottoimage_mg}}
#' @export
setMethod("plot", signature(x = "giottoImage", y = "missing"), function(x, y, ...) .plot_giottoimage_mg(giottoImage = x, ...))

#' @describeIn plot-generic Plot \emph{terra}-based giottoLargeImage object. ... param passes to \code{\link{.plot_giottolargeimage}}
#' @param col character. Colors. The default is grDevices::grey.colors(n = 256, start = 0, end = 1, gamma = 1)
#' @param max_intensity (optional) value to treat as maximum intensity in color scale
#' @param mar numeric vector of length 4 to set the margins of the plot (to make space for the legend). The default is (3, 5, 1.5, 1)
#' @param asRGB (optional) logical. Force RGB plotting if not automatically detected
#' @param legend logical or character. If not FALSE a legend is drawn. The character value can be used to indicate where the legend is to be drawn. For example "topright" or "bottomleft"
#' @param axes logical. Draw axes?
#' @param maxcell positive integer. Maximum number of cells to use for the plot
#' @param smooth logical. If TRUE the cell values are smoothed
#' @examples
#' ######### giottoLargeImage plotting #########
#' \dontrun{
#' gimg <- GiottoData::loadSubObjectMini("giottoLargeImage")
#' gimg <- GiottoClass:::.update_giotto_image(gimg) # only needed if out of date
#' plot(gimg)
#' plot(gimg, col = grDevices::hcl.colors(256))
#' plot(gimg, max_intensity = 100)
#' }
#'
#' @export
setMethod(
    "plot",
    signature(x = "giottoLargeImage", y = "missing"),
    function(x, y, col, max_intensity, mar, asRGB = FALSE, legend = FALSE, axes = TRUE,
    maxcell = 5e5, smooth = TRUE, ...) {
        arglist <- list(
            giottoLargeImage = x,
            asRGB = asRGB,
            legend = legend,
            axes = axes,
            maxcell = maxcell,
            smooth = smooth,
            ...
        )

        # check for pre-0.1.2 class
        if (is.null(attr(x, "colors"))) {
            .gstop("This image object is out of date
             Please run `GiottoClass:::.update_giotto_image()` on this object.",
                .n = 2
            )
        }

        # If no 'col' param, pull from `colors` slot
        if (missing("col")) {
            arglist$col <- x@colors
        } else {
            arglist$col <- col
        }
        # if no 'max_intensity' param, pull from `max_window` slot
        if (missing("max_intensity")) {
            arglist$max_intensity <- x@max_window
        } else {
            arglist$max_intensity <- max_intensity
        }
        # if mar param provided, use it
        if (!missing("mar")) arglist$mar <- mar

        do.call(.plot_giottolargeimage, args = arglist)
    }
)

#' @rdname plot-generic
#' @export
setMethod("plot", signature(x = "giottoAffineImage", y = "missing"),
          function(x, ...) {
              .plot_giottoaffineimage(x, ...)
          })

#' @describeIn plot-generic Plot \emph{terra}-based giottoPolygon object. ... param passes to \code{\link[terra]{plot}}
#' @param point_size size of points when plotting giottoPolygon object centroids
#' @param type what to plot: either 'poly' (default) or polygon 'centroid'
#' @param max_poly numeric. If `type` is not specified, maximum number of
#' polygons to plot before automatically switching to centroids plotting.
#' Default is 1e4. This value is settable using options("giotto.plot_max_poly")
#' @examples
#' ######### giottoPolygon plotting #########
#' gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")
#' plot(gpoly)
#' plot(gpoly, type = "centroid")
#'
#' @export
setMethod(
    "plot", signature(x = "giottoPolygon", y = "missing"),
    function(
        x,
        point_size = 0.6,
        type = c("poly", "centroid"),
        max_poly = getOption("giotto.plot_max_poly", 1e4),
        ...) {
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
#' @param feats specific features to plot within giottoPoints object
#' (defaults to NULL, meaning all available features)
#' @param raster default = TRUE, whether to plot points as rasterized plot with
#' size based on \code{raster_size} param. See details. When `FALSE`, plots via
#' [terra::plot()]
#' @param raster_size Default is 600. Only used when \code{raster} is TRUE
#' @details
#' *\[giottoPoints raster plotting\]*
#' Fast plotting of points information by rasterizing the information using
#' [terra::rasterize()]. For \pkg{terra} `SpatVectors`, this is faster than
#' \pkg{scattermore} plotting. When plotting as a raster, `col` colors map on
#' whole image level, as opposed to mapping to individual points, as it does
#' when `raster = FALSE`
#' Allows the following additional params when
#' plotting with no specific `feats` input:
#'   * **force_size** logical. `raster_size` param caps at 1:1 with the
#'   spatial extent, but also with a minimum resulting px dim of 100. To ignore
#'   these constraints, set `force_size = FALSE`
#'   * **dens** logical. Show point density using `count` statistic per
#'   rasterized cell. (Default = FALSE). This param affects `col` param is
#'   defaults. When TRUE, `col` is `grDevices::hcl.colors(256)`. When `FALSE`,
#'   "black" and "white" are used.
#'   * **background** (optional) background color. Usually not used when a
#'   `col` color mapping is sufficient.
#' 
#' Note that `col` param and other [base::plot()] graphical params are available
#' through `...`
#' @examples
#' ######### giottoPoints plotting #########
#' gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
#'
#' # ----- rasterized plotting ----- #
#' # plot points binary
#' plot(gpoints)
#' # plotting all features maps colors on an image level
#' plot(gpoints, col = grDevices::hcl.colors(n = 256)) # only 2 colors are used
#' plot(gpoints, col = "green", background = "purple")
#'
#' # plot points density (by count)
#' plot(gpoints, dens = TRUE, raster_size = 300)
#'
#' # force_size = TRUE to ignore default constraints on too big or too small
#' # (see details)
#' plot(gpoints, dens = TRUE, raster_size = 80, force_size = TRUE)
#'
#' # plot specific feature(s)
#' plot(gpoints, feats = featIDs(gpoints)[seq_len(4)])
#'
#' # ----- vector plotting ----- #
#' # non-rasterized plotting (slower, but higher quality)
#' plot(gpoints, raster = FALSE)
#'
#' # vector plotting maps colors to transcripts
#' plot(gpoints, raster = FALSE, col = grDevices::rainbow(nrow(gpoints)))
#'
#' # plot specific feature(s)
#' plot(gpoints, feats = featIDs(gpoints)[seq_len(4)], raster = FALSE)
#'
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
#' @examples
#' ######### spatLocsObj plotting #########
#' sl <- GiottoData::loadSubObjectMini("spatLocsObj")
#' plot(sl)
#'
#' @export
setMethod("plot", signature(x = "spatLocsObj", y = "missing"), function(x, ...) {
    if (nrow(x) == 0L) {
        message("No locations to plot")
        return(invisible(NULL))
    }

    if ("sdimz" %in% colnames(x)) {
        .plot_spatlocs_3d(x, ...)
    } else {
        # 2d plotting
        .plot_spatlocs_2d(x, ...)
    }
})


#' @describeIn plot-generic Plot a dimObj
#' @param dims dimensions to plot
#' @examples
#' ######### dimObj plotting #########
#' d <- GiottoData::loadSubObjectMini("dimObj")
#' plot(d)
#' plot(d, dims = c(3, 5))
#'
#' @export
setMethod(
    "plot", signature(x = "dimObj", y = "missing"),
    function(x, dims = c(1, 2), ...) {
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

        do.call("plot", append(l, list(x = plot_vals[, 1], y = plot_vals[, 2])))
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


#' @describeIn plot-generic Plot a affine2d. blue is start, red is end
#' @export
setMethod("plot", signature(x = "affine2d", y = "missing"), function(x, ...) {
    a <- as.polygons(ext(x@anchor))
    a$id <- "start"
    b <- affine(a, x)
    b$id <- "end"
    res <- rbind(a, b)
    terra::plot(res, border = c("blue", "red"), alpha = 0.5)
})

# internals ####

.plot_spatlocs_2d <- function(x, ...) {
    l <- list(...)
    if (is.null(l$asp)) l$asp <- 1
    if (is.null(l$xlab)) l$xlab <- "x coordinates"
    if (is.null(l$ylab)) l$ylab <- "y coordinates"
    if (is.null(l$cex)) l$cex <- 0.5
    if (nrow(x) > 10000L) {
        if (is.null(l$pch)) l$pch <- "."
    }

    do.call("plot", append(l, list(x = x[]$sdimx, y = x[]$sdimy)))
}

.plot_spatlocs_3d <- function(x, ...) {
    engine <- (getOption("giotto.plotengine3d", "rgl"))

    switch(engine,
        "rgl" = .plot_spatlocs_3d_rgl(x, ...),
        "plotly" = .plot_spatlocs_3d_plotly(x, ...)
    )
}

.plot_spatlocs_3d_rgl <- function(x, ...) {
    package_check("rgl", repository = "CRAN")

    l <- list(...)
    # params changes
    l$x <- x$sdimx
    l$y <- x$sdimy
    l$z <- x$sdimz
    if (is.null(l$xlab)) l$xlab <- "x coordinates"
    if (is.null(l$ylab)) l$ylab <- "y coordinates"
    if (is.null(l$zlab)) l$zlab <- "z coordinates"
    if (!is.null(l$cex)) {
        l$size <- l$cex
    }
    if (is.null(l$type)) l$type <- "p"

    do.call(rgl::plot3d, args = l)
}

.plot_spatlocs_3d_plotly <- function(x, asp, ...) {
    package_check("plotly", repository = "CRAN")

    l <- list(...)

    l$data <- x[]
    l$x <- ~sdimx
    l$y <- ~sdimy
    l$z <- ~sdimz
    l$type <- "scatter3d"
    l$mode <- "markers"
    if (!is.null(l$cex)) {
        l$marker <- list(size = l$cex)
        l$cex <- NULL
    } else {
        l$marker <- list(size = l$size)
        l$size <- NULL
    }
    if (is.null(l$marker$size)) l$marker$size <- 1

    pl <- do.call(plotly::plot_ly, args = l)

    if (missing(asp)) {
        asp <- rep(1L, 3L)
    } else if (length(asp) == 1L) asp <- rep(asp, 3L)

    # layout
    pl <- pl %>% plotly::layout(
        scene = list(
            xaxis = list(title = "x coordinates"),
            yaxis = list(title = "y coordinates"),
            zaxis = list(title = "z coordinates"),
            aspectmode = "manual",
            aspectratio = list(
                x = asp[[1]],
                y = asp[[2]],
                z = asp[[3]]
            )
        ),
        legend = list(
            x = 100,
            y = 0.5,
            font = list(
                family = "sans-serif",
                size = 12
            )
        )
    )

    return(pl)
}





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






#' @title .plot_giottolargeimage
#' @name .plot_giottolargeimage
#' @description Plot a \emph{downsampled} version of giottoLargeImage. Cropping can increase plot resolution of region of interest.
#' @param gobject giotto object
#' @param largeImage_name name of giottoLargeImage
#' @param giottoLargeImage giottoLargeImage object
#' @param crop_extent (optional) extent object to focus on specific region of image
#' @param xmax_crop,xmin_crop,ymax_crop,ymin_crop (optional) crop min/max x and y bounds
#' @param max_intensity (optional) value to treat as maximum intensity in color scale
#' @param asRGB (optional) logical. Force RGB plotting if not automatically detected
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
    a <- c(get_args_list(), list(...))

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

    a <- a[!c(names(a) %in% c(
        "gobject", "largeImage_name", "giottoLargeImage", "crop_extent",
        "xmax_crop", "xmin_crop", "ymax_crop", "ymin_crop", "asRGB",
        "max_intensity"
    ))]
    a$x <- giottoLargeImage@raster_object


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
        terra::has.RGB(a$x) ||
        terra::nlyr(a$x) >= 3) {
        a$scale <- max_intensity
        a$r <- 1
        a$g <- 2
        a$b <- 3
        a$legend <- NULL
        a$col <- NULL

        do.call(terra::plotRGB, args = a)
    } else {
        if (is.null(a$stretch)) a$stretch <- "lin"
        if (!"range" %in% names(a)) a$range <- c(0, max_intensity)

        do.call(terra::plot, args = a)
    }
}

.plot_giottoaffineimage <- function(x, maxcell = 5e5, ...) {
    pargs <- get_args_list(...)
    gimg <- x@funs$realize_magick()
    pargs$x <- gimg
    do.call(plot, args = pargs)
    # TODO things to be implemented for this pipeline:
    # col (the trip the magick-image flattened the image without applying col)
    # max_intensity same as above
    # the above options are also stripped when the fresh largeImage is created
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
        include_values <- FALSE
    } else {
        include_values <- TRUE
    }

    dataDT <- data.table::as.data.table(
        x = data,
        geom = "XY",
        include_values = include_values
    )


    if (length(feats) == 0L) {
        do.call(.plot_giotto_points_all, args = c(list(x = data), args_list))
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


#' @description
#' Quick plotting of SpatVector points information via terra::rasterize(). For
#' terra `SpatVectors`, this is faster than scattermore plotting.
#' @param x input `SpatVector` or `giottoPoints`
#' @param size numeric. Rasterization major axis pixel length. Automatically
#' caps at the original extent size AKA full res, but with a minimum px dim
#' of 100. To ignore these constraints, use `force_size = TRUE`
#' @param force_size logical. Whether to ignore constrains on `size` param
#' @param col character vector. Colors to map. Default is
#' `grDevices::hcl.colors(256)` for `dens = TRUE`, and black and white when
#' `dens = FALSE`
#' @param background (optional) background color. Usually not used when a `col`
#' color mapping is sufficient.
#' @param dens logical. Show point density using `count` statistic per
#' rasterized cell. (Default = FALSE)
#' @param ... additonal params to pass to terra::plot()
#' @keywords internal
#' @noRd
.plot_giotto_points_all <- function(x, size = 600, force_size = FALSE, dens = FALSE, col = NULL, background, ...) {
    pargs <- list(...)
    rargs <- list()
    if (!is.null(pargs$ext)) {
        e <- ext(pargs$ext)
    } else {
        e <- ext(x)
    }
    e_r <- range(e)

    # decide rasterization resolution
    # Select res that results in a major axis with length equal to size param,
    # up to a maximum resolution of 1 (1:1 with extent dims),
    # but with a min dim px of 100 (to help with cases where extent is small)
    res <- max(e_r / size[1L])
    if (!isTRUE(force_size)) {
        res <- max(res, 1)
        res <- min(res, c(e_r / 100))
    }

    # rasterization
    r <- terra::rast(e, res = res)
    if (isTRUE(dens)) rargs$fun <- "count"
    rargs$y <- r
    rargs$x <- x[]
    r2 <- do.call(terra::rasterize, args = rargs)

    # plotting
    pargs$x <- r2
    pargs$legend <- pargs$legend %null% FALSE
    if (is.null(col)) {
        if (isTRUE(dens)) {
            pal <- grDevices::hcl.colors(n = 256)
        } else {
            pal <- c("black", "white")
        }
        pargs$col <- pal[2L:length(pal)]
        pargs$background <- pal[1L]
        # replace background col if specifically provided
        if (!missing(background)) pargs$background <- background
    } else {
        if (missing(background)) {
            pargs$col <- col[2L:length(col)]
            pargs$background <- col[1L]
        } else {
            pargs$col <- col
            pargs$background <- background
        }
    }

    do.call(terra::plot, args = pargs)
}



.plot_giotto_points_one <- function(dataDT, feats, args_list) {
    # NSE vars
    feat_ID <- NULL

    if (!feats %in% dataDT[, feat_ID]) {
        .gstop(str_vector(feats), "not found in giottoPoints", .n = 6L)
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
        .gstop(str_vector(missing_feats), "not found in giottoPoints", .n = 6L)
    }

    par(mar = c(2.7, 3.5, 2, 4))
    feat_colors <- getRainbowColors(length(feats))

    data.table::setkey(dataDT, "feat_ID")
    dataDT <- dataDT[feat_ID %in% feats]
    dataDT[, feat_color_idx :=
        vapply(
            feat_ID,
            function(feat_i) which(feats == feat_i),
            FUN.VALUE = integer(1L)
        )
    ]

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
        args_list$col <- args_list$col %null% "white"
        do.call(terra::plot, args_list)
    } else {
        args_list$x <- terra::subset(data, terra::values(data)$feat_ID %in% feats)
        if (length(feats) == 1L) {
            args_list$col <- args_list$col %null% "white"
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
#' @param type (default is poly) plot the 'poly' or its 'centroid'
#' @param ... additional params to pass to plot function
#' @keywords internal
#' @noRd
.plot_giotto_polygon <- function(
        x, point_size = 0.6,
        type = c("poly", "centroid"), ...) {
    a <- list(...)

    type <- match.arg(type, choices = c("poly", "centroid"))

    switch(type,
        "poly" = do.call(terra::plot, args = c(list(x = x@spatVector), a)),
        "centroid" = {
            a$cex <- point_size
            a$x <- centroids(x)
            do.call(terra::plot, args = a)
        }
    )
}
