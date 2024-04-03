# giottoImage or magick tools ####

#' @title convert_mgImage_to_array_DT
#' @name convert_mgImage_to_array_DT
#' @description converts a magick image object to a data.table
#' @param mg_object magick image or Giotto image object
#' @keywords internal
#' @returns data.table with image pixel information
#' @export
convert_mgImage_to_array_DT <- function(mg_object) {
    if (inherits(mg_object, "giottoImage")) {
        mg_object <- mg_object@mg_object
    }

    # data.table variables
    RGB <- c.1 <- c.2 <- c.3 <- NULL

    # convert magick object to an array
    num_res <- as.numeric(mg_object[[1]])
    num_res_m <- data.table::as.data.table(reshape2::melt(num_res))
    colnames(num_res_m) <- c("x", "y", "c", "color")
    array_dt <- data.table::dcast.data.table(
        num_res_m, value.var = "color", formula = "x+y~c")
    colnames(array_dt) <- c("x", "y", "c.1", "c.2", "c.3")
    array_dt[, RGB := grDevices::rgb(c.1, c.2, c.3)]

    return(array_dt)
}


#' @title estimateImageBg
#' @name estimateImageBg
#' @description helps to estimate which color is the background color of your
#' plot
#' @param mg_object magick image or Giotto image object
#' @param top_color_range top possible background colors to return
#' @returns vector of pixel color frequencies and an associated barplot
#' @export
estimateImageBg <- function(mg_object, top_color_range = seq_len(50)) {
    if (inherits(mg_object, "giottoImage")) {
        mg_object <- mg_object@mg_object
    }

    arrayDT <- convert_mgImage_to_array_DT(mg_object = mg_object)
    sort_table <- sort(table(arrayDT$RGB), decreasing = TRUE)
    graphics::barplot(sort_table[top_color_range],
                    col = names(sort_table[top_color_range]))

    cat("Most abundant pixel colors: \n")
    print(sort_table[top_color_range])
}


#' @title changeImageBg
#' @name changeImageBg
#' @description Function to change the background color of a magick image
#' plot to another color
#' @param mg_object magick image or giotto image object
#' @param bg_color estimated current background color
#' @param perc_range range around estimated background color to
#' include (percentage)
#' @param new_color new background color
#' @param new_name change name of Giotto image
#' @import magick
#' @returns magick image or giotto image object with updated background color
#' @export
changeImageBg <- function(mg_object,
    bg_color,
    perc_range = 10,
    new_color = "#FFFFFF",
    new_name = NULL) {
    if (inherits(mg_object, "giottoImage")) {
        is_g_image <- TRUE
        g_image <- mg_object
        mg_object <- mg_object@mg_object
    } else {
        is_g_image <- FALSE
    }

    if (!inherits(mg_object, "magick-image")) {
        stop("mg_object needs to be a giottImage or a 'magick-image' object
            from the magick package")
    }

    # new background color
    new_rbg_color <- grDevices::col2rgb(new_color) / 255

    # current background limits
    rbgcolors <- grDevices::col2rgb(bg_color) / 255
    perc_range_min <- rbgcolors - (rbgcolors / 100) * perc_range
    perc_range_max <- rbgcolors + (rbgcolors / 100) * perc_range

    # convert magick image to array data.table
    arrayDT <- convert_mgImage_to_array_DT(mg_object = mg_object)

    # create new background
    c1_min <- perc_range_min[1, 1]
    c2_min <- perc_range_min[2, 1]
    c3_min <- perc_range_min[3, 1]
    c1_max <- perc_range_max[1, 1]
    c2_max <- perc_range_max[2, 1]
    c3_max <- perc_range_max[3, 1]

    c1_new <- new_rbg_color[1, 1]
    c2_new <- new_rbg_color[2, 1]
    c3_new <- new_rbg_color[3, 1]

    # find background color pixels

    # data.table variables
    c.1 <- c.2 <- c.3 <- NULL

    c1_ind <- arrayDT[["c.1"]] > c1_min & arrayDT[["c.1"]] < c1_max
    c2_ind <- arrayDT[["c.2"]] > c2_min & arrayDT[["c.2"]] < c2_max
    c3_ind <- arrayDT[["c.3"]] > c3_min & arrayDT[["c.3"]] < c3_max
    c_ind <- c1_ind * c2_ind * c3_ind

    # data.table variables
    c1 <- c2 <- c3 <- NULL

    # replace old background with new background
    arrayDT[, "c1" := ifelse(c_ind == TRUE, c1_new, c.1)]
    arrayDT[, "c2" := ifelse(c_ind == TRUE, c2_new, c.2)]
    arrayDT[, "c3" := ifelse(c_ind == TRUE, c3_new, c.3)]


    # data.table variables
    x <- y <- NULL

    # setorder for x and y coordinates
    data.table::setorder(arrayDT, y, x)

    # convert array_dt to array and then to magick image object
    original_width <- magick::image_info(mg_object)[2]
    original_heigth <- magick::image_info(mg_object)[3]
    myarray <- array(as.vector(as.matrix(arrayDT[, .(c1, c2, c3)])),
                    dim = c(original_width, original_heigth, 3))
    new_mg_object <- magick::image_read(myarray)

    # return magick or giotto image object
    if (is_g_image == TRUE) {
        if (!is.null(new_name)) g_image$name <- new_name
        g_image@mg_object <- new_mg_object
        return(g_image)
    } else {
        return(new_mg_object)
    }
}






# TODO Check if this is still the best way to do things
#' @name get_img_minmax
#' @title get_img_minmax
#' @param mg_img magick object
#' @param negative_y Map image to negative y spatial values if TRUE during
#' automatic alignment. Meaning that origin is in upper left instead of lower
#' left.
#' @keywords internal
#' @returns numeric
#' @export
get_img_minmax <- function(mg_img,
    negative_y = TRUE) {
    # Get magick object dimensions. xmin and ymax assumed to be 0.
    info <- magick::image_info(mg_img)
    img_xmax <- info$width # width
    img_xmin <- 0 # x origin
    if (negative_y == TRUE) {
        img_ymax <- 0 # y origin
        img_ymin <- -(info$height) # height
    } else if (negative_y == FALSE) {
        img_ymax <- info$height
        img_ymin <- 0
    }

    return(list(
        "img_xmax" = img_xmax,
        "img_xmin" = img_xmin,
        "img_ymax" = img_ymax,
        "img_ymin" = img_ymin
    ))
}



# TODO Check if this is still the best way to do things
#' @name get_adj_rescale_img
#' @title get_adj_rescale_img
#' @keywords internal
#' @returns numeric
#' @export
get_adj_rescale_img <- function(img_minmax,
    spatial_locs,
    scale_factor = 1) {
    # Expand scale_factor if needed
    if (length(scale_factor) == 1) {
        scale_factor <- c(x = scale_factor, y = scale_factor)
    }

    # Spatial minmax
    my_xmin <- min(spatial_locs$sdimx)
    my_xmax <- max(spatial_locs$sdimx)
    my_ymin <- min(spatial_locs$sdimy)
    my_ymax <- max(spatial_locs$sdimy)

    # Find scaled image adjustments based on scaled spatlocs
    xmin_adj_scaled <- (my_xmin * scale_factor[["x"]]) - (img_minmax$img_xmin)
    xmin_adj_orig <- xmin_adj_scaled / scale_factor[["x"]]

    xmax_adj_scaled <- (img_minmax$img_xmax) - (my_xmax * scale_factor[["x"]])
    xmax_adj_orig <- xmax_adj_scaled / scale_factor[["x"]]

    ymin_adj_scaled <- (my_ymin * scale_factor[["y"]]) - (img_minmax$img_ymin)
    ymin_adj_orig <- ymin_adj_scaled / scale_factor[["y"]]

    ymax_adj_scaled <- (img_minmax$img_ymax) - (my_ymax * scale_factor[["y"]])
    ymax_adj_orig <- ymax_adj_scaled / scale_factor[["y"]]

    # return scaled adjustments
    return(c(
        "xmin_adj_orig" = xmin_adj_orig,
        "xmax_adj_orig" = xmax_adj_orig,
        "ymin_adj_orig" = ymin_adj_orig,
        "ymax_adj_orig" = ymax_adj_orig
    ))
}



#' @title addGiottoImageMG
#' @name addGiottoImageMG
#' @description Adds giotto image objects to your giotto object
#' @param gobject giotto object
#' @param images list of giotto image objects,
#' see \code{\link{createGiottoImage}}
#' @param spat_unit spatial unit
#' @param spat_loc_name provide spatial location slot in Giotto to align
#' images. Defaults to first one
#' @param scale_factor provide scale of image pixel dimensions relative to
#' spatial coordinates.
#' @param negative_y Map image to negative y spatial values if TRUE during
#' automatic alignment. Meaning that origin is in upper left instead of lower
#' left.
#' @returns an updated Giotto object with access to the list of images
#' @export
addGiottoImageMG <- function(gobject,
    images,
    spat_unit = NULL,
    spat_loc_name = NULL,
    scale_factor = NULL,
    negative_y = TRUE) {
    # 0. check params
    if (is.null(gobject))
        stop("The giotto object that will be updated needs to be provided")

    if (is.null(images))
        stop("The giotto image(s) that will be added needs to be provided")

    if (is.null(spat_loc_name)) {
        if (!is.null(slot(gobject, "spatial_locs"))) {
            spat_loc_name <- list_spatial_locations(
                gobject = gobject, spat_unit = spat_unit)[1, ]
        } else {
            spat_loc_name <- NULL
            cat("No spatial locations have been found \n")
        }
    }

    ext_scale_factor <- FALSE
    if (!is.null(scale_factor)) {
        if (!is.numeric(scale_factor))
            stop("Given scale_factor(s) must be numeric")

        if ((length(scale_factor) == length(images)) ||
            length(scale_factor) == 1) {
            # cat('scale_factor(s) external to giottoImage have been given and
            # will be used')
            ext_scale_factor <- TRUE
        } else {
            stop("if scale_factor is given, it must be a numeric with either
                a single value or as many values as there are images are
                provided")
        }
    }

    # 1. expand scale_factors
    if (ext_scale_factor == TRUE) {
        if (length(scale_factor == 1)) {
            scale_factor <- rep(scale_factor, length(images))
        }
    }


    # 2. Add image with for loop
    for (image_i in seq_len(length(images))) {
        im <- images[[image_i]]

        if (inherits(im, "giottoImage")) {
            im_name <- im@name

            all_im_names <- names(gobject@images)

            if (im_name %in% all_im_names) {
                cat("\n ", im_name, " has already been used, will be
                    overwritten \n")
            }

            # 3. Update boundaries if not already done during
            # createGiottoImage() due to lack of spatlocs and gobject
            if (sum(im@boundaries == c(0, 0, 0, 0)) == 4 &&
                sum(im@minmax == c(10, 0, 10, 0)) == 4) {
                if (!is.null(spat_loc_name)) { # A check for the first
                    # available spatloc was already done
                    spatlocs <- get_spatial_locations(
                        gobject = gobject,
                        spat_unit = spat_unit,
                        spat_loc_name = spat_loc_name
                    )

                    # Find spatial minmax values
                    xmin_sloc <- min(spatlocs$sdimx)
                    xmax_sloc <- max(spatlocs$sdimx)
                    ymin_sloc <- min(spatlocs$sdimy)
                    ymax_sloc <- max(spatlocs$sdimy)

                    # Find adjustment values
                    img_minmax <- get_img_minmax(
                        mg_img = im@mg_object,
                        negative_y = negative_y
                    )
                    if (ext_scale_factor == TRUE) {
                        adj_values <- get_adj_rescale_img(
                            img_minmax = img_minmax,
                            spatial_locs = spatlocs,
                            scale_factor = scale_factor[[image_i]]
                        )
                    } else if (ext_scale_factor == FALSE) {
                        adj_values <- get_adj_rescale_img(
                            img_minmax = img_minmax,
                            spatial_locs = spatlocs,
                            scale_factor = im@scale_factor[[spat_loc_name]]
                        )
                    }

                    # Add minmax values to giottoImage@minmax
                    im@minmax <- c(
                        "xmax_sloc" = xmax_sloc,
                        "xmin_sloc" = xmin_sloc,
                        "ymax_sloc" = ymax_sloc,
                        "ymin_sloc" = ymin_sloc
                    )

                    # Add adjustment values to giottoImage@boundaries
                    im@boundaries <- c(
                        "xmax_adj" = as.numeric(adj_values[["xmax_adj_orig"]]),
                        "xmin_adj" = as.numeric(adj_values[["xmin_adj_orig"]]),
                        "ymax_adj" = as.numeric(adj_values[["ymax_adj_orig"]]),
                        "ymin_adj" = as.numeric(adj_values[["ymin_adj_orig"]])
                    )

                    # Inherit external scaling factors if given
                    if (ext_scale_factor == TRUE) {
                        im@scale_factor[[spat_loc_name]] <-
                            scale_factor[[image_i]]
                        im@resolution[[spat_loc_name]] <-
                            1 / (scale_factor[[image_i]])
                    }
                    ## Externally given scale_factors will only be written
                    ## in/used if boundary adj values are not pre-existing
                }
            }

            # 4. Add giottoImage to gobject
            gobject@images[[im_name]] <- im
        } else {
            warning("image [", image_i, "] is not a giotto image object")
        }
    }

    return(gobject)
}









#' @title updateGiottoImageMG
#' @name updateGiottoImageMG
#' @description Updates the boundaries of a giotto \code{image} object attached
#' to a \code{giotto} object if both \code{gobject} and \code{image_name}
#' params are given. Alternatively can directly accept and return
#' as \code{image}
#' @inheritParams updateGiottoImage
#' @param gobject \code{giotto} object containing giotto \code{image} object
#' @param giottoImage \code{image} object to directly update
#' @param xmin_set set image xmin boundary. Applied before adjustments
#' @param xmax_set set image xmax boundary. Applied before adjustments
#' @param ymin_set set image ymin boundary. Applied before adjustments
#' @param ymax_set set image ymax boundary. Applied before adjustments
#' @param return_gobject return a \code{giotto} object if \code{TRUE}, a
#' giotto \code{image} object if \code{FALSE}
#' @returns a \code{giotto} object or an updated giotto \code{image} object
#' if \code{return_gobject = FALSe}
#' @export
updateGiottoImageMG <- function(gobject = NULL,
    image_name = NULL,
    giottoImage = NULL,
    xmax_adj = 0,
    xmin_adj = 0,
    ymax_adj = 0,
    ymin_adj = 0,
    x_shift = 0,
    y_shift = 0,
    scale_factor = NULL,
    scale_x = 1,
    scale_y = 1,
    order = c("first_adj", "first_scale"),
    xmin_set = NULL,
    xmax_set = NULL,
    ymin_set = NULL,
    ymax_set = NULL,
    return_gobject = TRUE,
    verbose = TRUE) {
    # 0. Check params
    # Check input image
    if (is.null(gobject)) {
        if (is.null(giottoImage))
            stop("Image to be updated must be given as gobject AND image_name
                OR giottoImage argument(s) \n")
        if (verbose == TRUE)
            cat("gobject argument not given \n return_gobject set to FALSE \n")
        return_gobject <- FALSE
    }
    if (is.null(giottoImage) && is.null(image_name))
        stop("The name of the giotto image that will be updated needs to be
            provided \n")

    if (!is.null(giottoImage)) {
        if (!inherits(giottoImage, "giottoImage"))
            stop("giottoImage argument only accepts giottoImage objects \n")
        if (verbose == TRUE && !is.null(gobject))
            cat("giottoImage argument is given and will take priority \n
                return_gobject set to FALSE \n")
        return_gobject <- FALSE
    }

    # Check scalefactors
    if (!is.null(scale_factor)) scale_x <- scale_y <- scale_factor

    # Check spatial anchor values
    spatAnchor <- c(
        "xmax_sloc" = xmax_set,
        "xmin_sloc" = xmin_set,
        "ymax_sloc" = ymax_set,
        "ymin_sloc" = ymin_set
    )
    if (length(spatAnchor) < 4 && length(spatAnchor) > 0)
        stop("If set arguments are being used, all four must be given \n")
    if (!is.null(spatAnchor)) {
        if (xmax_set < xmin_set)
            stop("xmax_set must be greater than xmin_set \n")
        if (ymax_set < ymin_set)
            stop("ymax_set must be greater than ymin_set \n")
    }

    # Find order of adjust and scaling
    order <- match.arg(order, choices = c("first_adj", "first_scale"))


    # 1. get giottoImage if necessary
    if (is.null(giottoImage)) {
        if (!is.null(gobject) && !is.null(image_name)) {
            g_image <- get_giottoImage_MG(
                gobject = gobject,
                name = image_name
            )
        } else {
            stop("either a giottoImage or both the gobject and name of the
                giottoImage must be given. \n")
        }
    }


    # 2. Find minmax spatial anchor values
    if (is.null(spatAnchor)) {
        spatAnchor <- g_image@minmax
    }

    # Perform scale if first_scale
    if (order == "first_scale") {
        spatAnchor <- spatAnchor * c(scale_x, scale_x, scale_y, scale_y)
    }

    # 3. Prepare adjustment values
    # Apply shifts
    xmin_adj <- xmin_adj - x_shift
    xmax_adj <- xmax_adj + x_shift
    ymin_adj <- ymin_adj - y_shift
    ymax_adj <- ymax_adj + y_shift

    # Find final bounds
    xmax_final <- spatAnchor[["xmax_sloc"]] + xmax_adj
    xmin_final <- spatAnchor[["xmin_sloc"]] - xmin_adj
    ymax_final <- spatAnchor[["ymax_sloc"]] + ymax_adj
    ymin_final <- spatAnchor[["ymin_sloc"]] - ymin_adj

    # Perform scale if first_adj
    if (order == "first_adj") {
        xmax_final <- xmax_final * scale_x
        xmin_final <- xmin_final * scale_x
        ymax_final <- ymax_final * scale_y
        ymin_final <- ymin_final * scale_y
    }

    # Find final adj values
    xmax_adj <- xmax_final - g_image@minmax[["xmax_sloc"]]
    xmin_adj <- g_image@minmax[["xmin_sloc"]] - xmin_final
    ymax_adj <- ymax_final - g_image@minmax[["ymax_sloc"]]
    ymin_adj <- g_image@minmax[["ymin_sloc"]] - ymin_final


    # 4. Update the boundaries
    g_image@boundaries <- c(
        "xmax_adj" = xmax_adj,
        "xmin_adj" = xmin_adj,
        "ymax_adj" = ymax_adj,
        "ymin_adj" = ymin_adj
    )

    # 5. Update the scalefactors for x and y
    x_range <- xmax_final - xmin_final
    y_range <- ymax_final - ymin_final
    im_dims <- magick::image_info(g_image@mg_object)
    x_scalefactor <- im_dims[["width"]] / x_range
    y_scalefactor <- im_dims[["height"]] / y_range

    g_image@scale_factor <- c("x" = x_scalefactor, "y" = y_scalefactor)
    g_image@resolution <- (1 / g_image@scale_factor)

    if (return_gobject == TRUE) {
        gobject@images[[image_name]] <- g_image
        return(gobject)
    } else {
        return(g_image)
    }
}






#' @title reconnect_giottoImage_MG
#' @name reconnect_giottoImage_MG
#' @description reconnect giottoImage if image pointer is broken
#' @param giottoImage giottoImage to reconnect
#' @param image_path path to source file of giottoImage
#' @returns reconnected giottoImage
#' @keywords internal
#' @export
reconnect_giottoImage_MG <- function(
        giottoImage,
        image_path) {

    deprecate_soft("0.2.4", "reconnect_giottoImage_MG()", "reconnect()")

    # load in new magick object
    mg_object <- magick::image_read(image_path)

    # replace old magick object
    giottoImage@mg_object <- mg_object

    # return reconnected giottoImage
    return(giottoImage)
}



# giottoLargeImage or terra tools ####


#' @title Load image as a terra spatRaster object
#' @name .create_terra_spatraster
#' @param image_path existing full filepath to image to be loaded as a terra
#' spatRaster
#' @keywords internal
#' @returns spatRaster object
.create_terra_spatraster <- function(image_path) {
    raster_object <- try(suppressWarnings(terra::rast(x = image_path)))
    if (inherits(raster_object, "try-error")) {
        stop(raster_object, " can not be read by terra::rast() \n")
    }
    return(raster_object)
}






#' @title Optimized largeImage resampling
#' @name plot_auto_largeImage_resample
#' @description Downsample \code{largeImage} for plotting.
#' Uses \code{\link[terra]{spatSample}} to load only a portion of the original
#' image, speeding up plotting and lowering memory footprint.
#'
#' Default behavior of \code{spatSample} is to crop if only a smaller ROI is
#' needed for plotting followed by the sampling process in order to reduce
#' wasted sampling by focusing the sample space. For very large ROIs, this
#' crop can be time intensive and require writing to disk.
#'
#' This function examines the ROI dimensions as defined through the limits of
#' the spatial locations to be plotted, and decides between the following two
#' methods in order to avoid this issue:
#' \itemize{
#'     \item{\strong{Method A.} First crop original image and then sample n
#'     values where n = 500,000 to generate final image}
#'     \item{\strong{Method B.} First oversample n values and then crop, where
#'     n = 500,000 scaled by a value >1. Scaling factor increases the smaller
#'     the ROI is and is defined by: original dimensions/crop dimensions where
#'     the larger ratio between x and y dims is chosen. Scale factor is capped
#'     by \code{max_resample_scale}}
#' }
#' Control points for this function are set by \code{max_crop} which decides
#' the max ROI area after which switchover to method B happens in order to
#' avoid laborious crops and \code{max_resample_scale} which determines the
#' maximum scale factor for number of values to sample. Both values can be
#' adjusted depending on system resources. Additionally, \code{flex_resample}
#' determines if this switching behavior happens.
#' When set to \code{FALSE}, only method A is used.
#' @param gobject \code{gobject} containing \code{largeImage} object
#' @param giottoLargeImage \code{largeImage} object to resample if not provided
#' through \code{gobject} and \code{largeImage_name}
#' @param largeImage_name name of \code{largeImage} in \code{gobject}
#' @param spat_unit spatial unit
#' @param spat_loc_name name of spatial locations to plot
#' @param polygon_feat_type name of polygon/spatial_info to plot
#' @param include_image_in_border logical. expand the extent sampled to also
#' show image in border regions not included in spatlocs. This prevents images
#' in plots from being sharply cut off around the furthest spatial
#' locations. (default is \code{TRUE})
#' @param flex_resample logical. Whether to allow automatic selection of
#' sampling workflow as defined in details sections. (default is \code{TRUE})
#' @param max_crop maximum crop size allowed for \strong{method A} before
#' switching to \strong{method B} (see description)
#' @param max_resample_scale maximum cells allowed to resample to compensate for
#' decreased resolution when cropping after sampling
#' @returns a \code{giottoLargeImage} cropped and resampled properly for plotting
#' @seealso \code{\link[terra]{spatSample}}
#' @export
plot_auto_largeImage_resample <- function(gobject,
    giottoLargeImage = NULL,
    largeImage_name = NULL,
    spat_unit = NULL,
    spat_loc_name = NULL,
    polygon_feat_type = NULL,
    include_image_in_border = TRUE,
    flex_resample = TRUE,
    max_crop = 1e+08,
    max_resample_scale = 100) {
    # If no giottoLargeImage, select specified giottoLargeImage. If none
    # specified, select first one.
    if (is.null(giottoLargeImage)) {
        giottoLargeImage <- get_giottoLargeImage(
            gobject = gobject,
            name = largeImage_name
        )
    }

    # Set spat_unit
    spat_unit <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = spat_unit
    )

    # Get spatial locations
    cell_locations <- get_spatial_locations(
        gobject = gobject,
        spat_unit = spat_unit,
        spat_loc_name = spat_loc_name,
        output = "data.table",
        copy_obj = TRUE
    )

    # If no spatial locations are available, rely on first existing polygon
    # extent
    if (is.null(cell_locations)) {
        sub_obj <- get_polygon_info(
            gobject = gobject,
            polygon_name = polygon_feat_type,
            return_giottoPolygon = TRUE
        )

        # Find centroids then if there are more than 100, sample 30%
        sub_obj <- .calculate_centroids_polygons(sub_obj)
        sampleSize <- ifelse(nrow(sub_obj) > 100,
                            ceiling(0.3 * nrow(sub_obj)), nrow(sub_obj))

        centroid_sample_DT <- slot(sub_obj, "spatVectorCentroids") %>%
            sample(., size = sampleSize) %>%
            terra::geom() %>%
            as.data.table()

        cell_locations <- data.table::data.table(
            sdimx = c(centroid_sample_DT$x),
            sdimy = c(centroid_sample_DT$y)
        )

        # sub_ext = terra::ext(sub_obj)[seq_len(4)]
        # cell_locations = data.table::data.table(
        # sdimx = c(sub_ext[['xmin']], sub_ext[['xmax']]),
        # sdimy = c(sub_ext[['ymin']], sub_ext[['ymax']]))
    }

    if (is.null(cell_locations))
        stop("No spatial locations or polygons discovered.\n Cannot determine
            largeImage resample extent\n")

    # Get image extent minmax
    im_minmax <- terra::ext(giottoLargeImage@raster_object)[seq_len(4)]
    # Determine crop
    if (isTRUE(include_image_in_border)) {
        # with crop padding
        x_range <- range(cell_locations$sdimx)
        y_range <- range(cell_locations$sdimy)
        x_halfPaddedRange <- diff(x_range) * 0.625
        y_halfPaddedRange <- diff(y_range) * 0.625
        x_midpt <- mean(x_range)
        y_midpt <- mean(y_range)

        xmax_crop <- x_midpt + x_halfPaddedRange
        xmin_crop <- x_midpt - x_halfPaddedRange
        ymax_crop <- y_midpt + y_halfPaddedRange
        ymin_crop <- y_midpt - y_halfPaddedRange

        if (xmin_crop < im_minmax[["xmin"]]) xmin_crop <- im_minmax[["xmin"]]
        if (xmax_crop > im_minmax[["xmax"]]) xmax_crop <- im_minmax[["xmax"]]
        if (ymin_crop < im_minmax[["ymin"]]) ymin_crop <- im_minmax[["ymin"]]
        if (ymax_crop > im_minmax[["ymax"]]) ymax_crop <- im_minmax[["ymax"]]
    } else {
        # no crop padding
        x_range <- range(cell_locations$sdimx)
        y_range <- range(cell_locations$sdimy)
        xmin_crop <- x_range[1]
        xmax_crop <- x_range[2]
        ymin_crop <- y_range[1]
        ymax_crop <- y_range[2]
    }

    # setup crop extent object
    crop_ext <- terra::ext(
        xmin_crop, xmax_crop,
        ymin_crop, ymax_crop
    )

    # zoom and resample giottoLargeImage
    crop_xdim <- abs(xmax_crop - xmin_crop)
    crop_ydim <- abs(ymax_crop - ymin_crop)
    crop_area_px <- crop_xdim * giottoLargeImage@scale_factor[["x"]] *
        giottoLargeImage@scale_factor[["y"]] * crop_ydim
    im_xdim <- abs(im_minmax[2] - im_minmax[1])
    im_ydim <- abs(im_minmax[2] - im_minmax[1])
    crop_relative_scale <- max(im_xdim / crop_xdim, im_ydim / crop_ydim)

    if (!isTRUE(flex_resample) | crop_area_px <= max_crop) {
        # METHOD A: Crop if needed then resample to final image
        if (!isTRUE(flex_resample) & crop_area_px > max_crop) {
            warning("Plotting large regions with flex_resample == FALSE will
                    be costly in time and drive space.")
        }
        # For ROIs with area smaller than max_crop OR if flex_resample is FALSE
        crop_image <- terra::crop(
            x = giottoLargeImage@raster_object,
            y = crop_ext
        )
        giottoLargeImage@raster_object <- terra::spatSample(crop_image,
            size = 500000,
            method = "regular",
            as.raster = TRUE
        )
    } else {
        # METHOD B: Resample then crop to final image
        # Sample n values where n = default val scaled by a value >1
        if (crop_relative_scale <= max_resample_scale) {
            # Scale factor is fullsize image dim/crop dim. Larger of the two
            # ratios is chosen
            resample_image <- terra::spatSample(giottoLargeImage@raster_object,
                size = round(500000 * crop_relative_scale),
                method = "regular",
                as.raster = TRUE
            )
        } else {
            # For scale factors that are too large, scaling is capped by
            # max_resample_scale
            resample_image <- terra::spatSample(giottoLargeImage@raster_object,
                size = 500000 * max_resample_scale,
                method = "regular",
                as.raster = TRUE
            )
        }
        giottoLargeImage@raster_object <- terra::crop(
            x = resample_image,
            y = crop_ext
        )
    }

    return(giottoLargeImage)
}




#' @title Sample values from SpatRaster
#' @name .spatraster_sample_values
#' @description
#' Sample numerical values from a `SpatRaster`. The output format depends on the
#' value of the `output` param.
#' @param raster_object terra `SpatRaster` to sample from
#' @param size rough maximum of pixels allowed when resampling
#' @param output what output to return as. Defaults to "data.frame"
#' @param verbose be verbose
#' @param \dots additional params to pass to `terra::spatSample`
#' @returns magick or EBImage image
#' @keywords internal
.spatraster_sample_values <- function(
        raster_object,
        size = 5000,
        output = c('data.frame', 'array', 'magick', 'EBImage'),
        verbose = NULL,
        ...
) {
    output <- match.arg(
        arg = output,
        choices = c("data.frame", "array", "magick", "EBImage")
    )

    # account for possible giottoLargeImage input
    if (inherits(raster_object, "giottoLargeImage")) {
        raster_object <- raster_object@raster_object
    }

    # assemble argslist for terra::spatSample()
    argslist <- list(
        x = raster_object,
        size = size,
        as.df = TRUE, # default behavior
        method = "regular",
        value = TRUE,
        ...
    )

    if (output != "data.frame") {
        # if desired output is not data.frame, all other outputs require raster
        argslist$as.raster <- TRUE
        argslist$as.df <- FALSE
    }

    res <- do.call(terra::spatSample, args = argslist)

    # convert and handle NA values
    if (isTRUE(argslist$as.df)) {
        res <- stats::na.omit(res) # data.frame remove NAs
    } else {
        # all others
        res <- terra::as.array(res)
        na_bool <- is.na(res)
        res[na_bool] <- 0L # set NA values to 0
    }

    # throw error when there are no values discovered.
    if (nrow(res) == 0) {
        vmsg(.v = verbose,
            "No values discovered when sampling for image characteristics")
    }

    # convert to specified image type if desired.
    # Note that there is a conversion of image values to range of 0-1
    if (output %in% c("magick", "EBImage")) {
        res <- magick::image_read(res / max(res))
    }
    if (output == "EBImage") {
        GiottoUtils::package_check("EBImage", repository = "Bioc")
        res <- magick::as_EBImage(res)
    }

    return(res)
}




#' @title Find SpatRaster intensity range
#' @name .spatraster_intensity_range
#' @keywords internal
#' @noRd
#' @return named numeric vector of min then max detected values
.spatraster_intensity_range <- function(
        raster_object,
        sample_values = .spatraster_sample_values(raster_object)) {
    # get intensity range
    srMinmax <- suppressWarnings(terra::minmax(raster_object))
    if (sum(is.infinite(srMinmax)) == 0) { # pull minmax values from terra
        #spatRaster obj if img was small enough for them to be calculated
        res <- c(srMinmax[1], srMinmax[2])
    } else { # pull minmax values from sampled subset if img was too large
        intensityRange <- range(sample_values)
        res <- c(intensityRange[1], intensityRange[2])
    }

    names(res) <- c("min", "max")
    return(res)
}




#' @title Find SpatRaster int or floating point
#' @name .spatraster_is_int
#' @keywords internal
#' @noRd
#' @return logical
.spatraster_is_int <- function(
        raster_object,
        sample_values = .spatraster_sample_values(raster_object)) {
    # find out if image is int or floating point
    identical(sample_values, round(sample_values))
}






#' @title Plot smoothed curve of giotto largeImage intensity values
#' @name density_giottoLargeImage
#' @param gobject giotto object
#' @param image_type image object type (only supports largeImage and is set as
#' default)
#' @param giottoLargeImage giotto large image object
#' @param method method of plotting image distribution
#' @param ... additional params to pass
#' @returns density or histogram plot
#' @keywords internal
.dist_giottolargeimage <- function(
        gobject = NULL,
        image_name = NULL,
        giottoLargeImage = NULL,
        method = "dens",
        ...) {
    # get image object
    if (!is.null(gobject) & !is.null(image_name)) {
        img_obj <- getGiottoImage(
            gobject = gobject,
            image_type = "largeImage",
            name = image_name
        )
    } else if (!is.null(giottoLargeImage)) {
        img_obj <- giottoLargeImage
    } else {
        stop("No giottoLargeImage given \n")
    }

    a = list(x = img_obj, ...)

    # plot
    switch(method,
        "dens" = do.call(density, args = a),
        "hist" = do.call(hist, args = a)
    )
}



#' @title Stitch multiple giottoLargeImage objects into a single
#' giottoLargeImage object
#' @name stitchGiottoLargeImage
#' @description Function to stitch together multiple field of view (FOV) images
#' into a single final image. Images are loaded into Giotto
#' as \code{giottoLargeImage} and stitched based on a set of FOV positions
#' into a single final \code{giottoLargeImage}.
#' @details This function is time consuming. Setting a save location through the
#' \code{filename} parameter is also highly recommended as file size will
#' likely be large. This function creates a single stitched image from multiple
#' FOV tiles and saves that image to disk as it works. When finished, the
#' pointer to that new image is loaded in as a \code{giottoLargeImage}
#' object. \cr \strong{Note:} Dry runs are on by default and \code{dryRun}
#' param must be set to FALSE to proceed with the final stitching operation.
#' @section Dry runs are default:
#'     To ensure that disk space and time is not wasted, this function defaults
#'     to previewing the stitching operation.
#' @section FOV positions:
#'     The final image is stitched together from multiple FOV tiles.
#'     The \code{FOV_positions} parameter accepts a table of x and y values for
#'     where each FOV tile should be placed when performing the stitch. Which
#'     columns are the x and y values are determined by the \code{FOV_xcol}
#'     and \code{FOV_ycol} params respectively. FOV tiles are at full resolution
#'     with a starting position where either the lower left or upper left of
#'     the image touch the origin depending on the value of \code{FOV_inverty}
#'     param. The FOV image is then translated according to the x and y shift
#'     values.
#' @section FOV invert y:
#'     Many imaging systems may treat the origin according to image convention
#'     where (0,0) is at the upper left of an image. This is at odds with
#'     coordinate convention and what Giotto uses internally where the
#'     coordinate (0,0) is at the lower left. The \code{FOV_inverty} defaults
#'     to FALSE, but if set to TRUE, then FOV tile images will start with the
#'     upper left touching (0,0) and all y values given
#'     through \code{FOV_positions} and \code{FOV_ycol} will be treated as
#'     negative y shift values.
#' @section dataType:
#'     There are multiple datatypes defining the range of intensity values that
#'     images can be saved with. Setting a value with the \code{dataType} para
#'     is optional and Giotto attempts to determine compatible data type to
#'     save the image as automatically.
#' @param largeImage_list list of \code{giottoLargeImage} objects
#' @param gobject_list list of \code{gobjects}
#' containing \code{giottoLargeImages}
#' @param largeImage_nameList list of names of \code{giottoLargeImages}
#' within \code{gobjects}
#' @param FOV_positions dataframe of FOV positions. Values (if any) are
#' directly added to current image mapping
#' @param FOV_xcol column name for FOV position x values
#' @param FOV_ycol column name for FOV position y values
#' @param FOV_inverty make FOV y position values negative
#' @param method method of stitching images (\strong{mosaic}: average
#' overlapping area values, \strong{merge}:values get priority by order of
#' images given)
#' @param round_positions boolean. Round image positions. May be necessary
#' to run.
#' @param filename file name to write the stitched image to. Defaults
#' to \code{"save_dir/stitch.tif"} if \code{save_dir} param is found in the
#' first \code{gobject}'s Giotto instructions
#' @param dataType (optional) values for \code{dataType} are "INT1U", "INT2U",
#' "INT2S", "INT4U", "INT4S", "FLT4S", "FLT8S". The first three letters
#' indicate whether the \code{dataType} is integer (whole numbers) of a real
#' number (decimal numbers), the fourth character indicates the number of
#' bytes used (allowing for large numbers and/or more precision), and the "S"
#' or "U" indicate whether the values are signed (both negative and positive)
#' or unsigned (positive values only).
#' @param fileType (optional) image format (e.g. .tif) If not given, defaults
#' to format given in the filename
#' @param dryRun boolean. Plot placeholder bounding rectangles where FOV images
#' will be stitched without actually proceeding with the full image stitching
#' and saving process.
#' @param overwrite boolean. Overwrite if filename to save image as already
#' exists. Defaults to TRUE
#' @param verbose boolean. Be verbose
#' @returns \code{largeGiottoImage} object with pointer to stitched image
#' @export
stitchGiottoLargeImage <- function(largeImage_list = NULL,
    gobject_list = NULL,
    largeImage_nameList = NULL,
    FOV_positions = NULL,
    FOV_xcol = NULL,
    FOV_ycol = NULL,
    FOV_inverty = FALSE,
    method = c("mosaic", "merge"),
    round_positions = FALSE,
    filename = NULL,
    dataType = NULL,
    fileType = NULL,
    dryRun = TRUE,
    overwrite = FALSE,
    verbose = TRUE) {
    ## 0. Check params
    if (!is.null(gobject_list)) {
        # Set default largeImage_nameList
        if (is.null(largeImage_nameList)) {
            largeImage_nameList <- rep("image", length(gobject_list))
        }
    }

    # Select method for stitching
    method <- match.arg(method, choices = c("mosaic", "merge"))

    # Check for filename, set default if not found
    if (is.null(filename)) {
        if (!is.null(gobject_list)) {
            save_dir <- readGiottoInstructions(gobject_list[[1]],
                                            param = "save_dir")
        } else {
            save_dir <- path.expand("~")
        }
        filename <- paste0(save_dir, "/stitch.tif")
    }

    # check filename
    if (file.exists(filename)) {
        if (verbose == TRUE) {
            if (overwrite == TRUE)
                cat("File at", filename,
                    "exists.\n (overwrite == TRUE) Image will be overwritten")
            if (overwrite == FALSE)
                cat("File at", filename,
                    "exists.\n (overwrite == FALSE) Image will not be
                    overwritten")
        }
    }

    # Match dataType input if given
    dataTypeChoices <- c("INT1U", "INT2U", "INT2S", "INT4U", "INT4S",
                        "FLT4S", "FLT8S")
    if (!is.null(dataType)) dataType <- match.arg(
        dataType, choices = dataTypeChoices)
    # Determine compatible dataType from first  giottoLargeImage


    ## 1. Get list of raster objects
    if (is.null(largeImage_list)) {
        if (!is.null(gobject_list)) {
            # For loop to grab giottoLargeImages
            largeImage_list <- list()
            for (gobj_i in seq_len(length(gobject_list))) {
                largeImage_list[[gobj_i]] <- get_giottoLargeImage(
                    gobject = gobject_list[[gobj_i]],
                    name = largeImage_nameList[[gobj_i]]
                )
            }
        } else {
            stop("giottoLargeImages must be given either as the
                giottoLargeImage itself or as a giotto object and the
                giottoLargeImage name")
        }
    }

    # Determine datatype from first giottoLargeImage
    if (is.null(dataType)) {
        dataType <- .terra_writeraster_datatype(
            giottoLargeImage = largeImage_list[[1]])
    }

    # For loop to extract raster_objects
    raster_list <- list()
    for (img_i in seq_len(length(largeImage_list))) {
        raster_list[[img_i]] <- largeImage_list[[img_i]]@raster_object
    }

    ## 2. Apply FOV shifts (if given)
    if (!is.null(FOV_positions)) {
        # Check if there is an FOV position for every raster object
        if (nrow(FOV_positions) != length(raster_list)) {
            stop("If FOV_positions are given then there must be one set of
                values for every image being stitched")
        }

        if (FOV_inverty == TRUE) {
            FOV_positions["FOV_ycol"] <- -FOV_positions["FOV_ycol"]
        }

        # Shift the image extents as specified by POV_positions
        for (rast_i in seq_len(length(raster_list))) {
            raster_list[[rast_i]] <- terra::shift(
                x = raster_list[[rast_i]],
                dx = FOV_positions[rast_i, FOV_xcol],
                dy = FOV_positions[rast_i, FOV_ycol]
            )
        }
    }

    ## 3. Perform stitch
    # # Round final extent values (merge and mosaic may only work with
    # integer extents)
    if (round_positions == TRUE) {
        if (verbose == TRUE)
            cat("round_positions == TRUE \n Image spatial positions will be
                rounded to integers. \n")
        for (rast_i in seq_len(length(raster_list))) {
            terra::ext(raster_list[[rast_i]]) <- round(
                terra::ext(raster_list[[rast_i]]))
        }
    }

    if (dryRun == TRUE) {
        # Collect SpatExtents then convert to polygons
        imgBounds_list <- list()
        for (rast_i in seq_len(length(raster_list))) {
            img_ext <- terra::ext(raster_list[[rast_i]])
            img_bound_poly <- terra::as.polygons(img_ext)
            img_bound_poly$FOV <- rast_i
            # Add to imgBounds list
            imgBounds_list[[rast_i]] <- img_bound_poly
        }
        imgBounds <- do.call(rbind, imgBounds_list)
        terra::plot(imgBounds, "FOV",
            type = "classes",
            legend = TRUE,
            mar = c(3, 3, 2, 2),
            plg = list(x = "topright")
        )
        return(NULL)
    } else if (dryRun == FALSE) {
        # Create SpatRasterCollection
        rasterSRC <- terra::sprc(raster_list)

        # stitch raster objects
        if (method == "merge") {
            stitchImg <- terra::merge(
                x = rasterSRC,
                filename = filename,
                overwrite = overwrite,
                wopt = list(datatype = dataType)
            )
        } else if (method == "mosaic") {
            stitchImg <- terra::mosaic(
                x = rasterSRC,
                filename = filename,
                overwrite = overwrite,
                wopt = list(
                    datatype = dataType,
                    filetype = fileType
                )
            )
        }
        stitch_gLargeImg <- createGiottoLargeImage(
            raster_object = stitchImg,
            use_rast_ext = TRUE
        )
        return(stitch_gLargeImg)
    }
}


# returns extent to use for x
.evaluate_extent <- function(x, extent, xmin, xmax, ymin, ymax) {
    if (!missing(extent)) {
        if (checkmate::test_numeric(extent, len = 4L)) {
            return(extent)
        }
        if (inherits(extent, "SpatExtent")) {
            return(extent)
        }

        stop("extent inputs must be either of class SpatExtent or numerical
            in the order of xmin, xmax, ymin, ymax")
    }

    e <- terra::ext(x)

    if (!missing(xmin)) e$xmin <- xmin
    if (!missing(xmax)) e$xmax <- xmax
    if (!missing(ymin)) e$ymin <- ymin
    if (!missing(ymax)) e$ymax <- ymax

    return(e)
}


#' @title Crop a giotto largeImage object
#' @name cropGiottoLargeImage
#' @description Crop a giottoLargeImage based on crop_extent argument or
#' given values
#' @param gobject gobject holding the giottoLargeImage
#' @param largeImage_name name of giottoLargeImage within gobject
#' @param giottoLargeImage alternative input param using giottoLargeImage object
#' instead of through \code{gobject} and \code{largeImage_name} params
#' @param crop_name arbitrary name for cropped giottoLargeImage
#' @param crop_extent terra extent object used to crop the giottoLargeImage
#' @param xmax_crop,xmin_crop,ymax_crop,ymin_crop crop min/max x and y bounds
#' @returns a giottoLargeImage object
#' @export
cropGiottoLargeImage <- function(gobject = NULL,
    largeImage_name = NULL,
    giottoLargeImage = NULL,
    crop_name = "image",
    crop_extent = NULL,
    xmax_crop = NULL,
    xmin_crop = NULL,
    ymax_crop = NULL,
    ymin_crop = NULL) {
    ## 0. Check inputs
    if (!is.null(crop_extent)) {
        if (!inherits(crop_extent, "SpatExtent"))
            stop("crop_extent argument only accepts terra extent objects. \n")
    }
    if (!is.null(giottoLargeImage)) {
        if (!inherits(giottoLargeImage, "giottoLargeImage"))
            stop("giottoLargeImage argument only accepts giottoLargeImage
                objects. \n")
    }

    ## 1. get giottoLargeImage if necessary
    if (is.null(giottoLargeImage)) {
        if (!is.null(gobject) && !is.null(largeImage_name)) {
            giottoLargeImage <- get_giottoLargeImage(
                gobject = gobject,
                name = largeImage_name
            )
        } else {
            stop("either a giottoLargeImage or both the gobject and name of
                the giottoLargeImage must be given. \n")
        }
    }

    raster_object <- giottoLargeImage@raster_object

    ## 2. Find crop extent
    crop_bounds <- c(xmin_crop, xmax_crop, ymin_crop, ymax_crop)

    if (!is.null(crop_extent)) {
        raster_object <- terra::crop(raster_object,
            crop_extent,
            snap = "near"
        )
    } else if (length(crop_bounds == 4)) {
        crop_extent <- terra::ext(crop_bounds)

        raster_object <- terra::crop(raster_object,
            crop_extent,
            snap = "near"
        )
    } else if (length(crop_bounds) > 1) {
        stop("All four crop bounds must be given.")
    }

    ## 3. Return a cropped giottoLargeImage
    giottoLargeImage@name <- crop_name
    giottoLargeImage@raster_object <- raster_object
    giottoLargeImage@extent <- as.vector(terra::ext(raster_object))
    # The only things updated are the raster object itself, the name,
    # and the extent tracking slot.
    # The overall_extent slot must NOT be updated since it records the
    # original extent

    return(giottoLargeImage)
}




#' @title convertGiottoLargeImageToMG
#' @name convertGiottoLargeImageToMG
#' @description convert a giottoLargeImage by downsampling into a normal
#' magick based giottoImage
#' @param gobject gobject containing giottoLargeImage
#' @param largeImage_name name of giottoLargeImage
#' @param giottoLargeImage alternative input param using giottoLargeImage object
#' instead of through \code{gobject} and \code{largeImage_name} params
#' @param mg_name name to assign converted magick image based giottoImage.
#' Defaults to name of giottoLargeImage
#' @param spat_unit spatial unit
#' @param spat_loc_name gobject spatial location name to map giottoImage
#' to (optional)
#' @param crop_extent extent object to focus on specific region of image
#' @param xmax_crop assign crop boundary
#' @param xmin_crop assign crop boundary
#' @param ymax_crop assign crop boundary
#' @param ymin_crop assign crop boundary
#' @param resample_size maximum number of pixels to use when resampling
#' @param max_intensity value to treat as maximum intensity in color scale
#' @param return_gobject return as giotto object
#' @param verbose be verbose
#' @returns a giotto object if \code{return_gobject = TRUE} or an updated giotto
#' image object if \code{return_gobject = FALSE}
#' @export
convertGiottoLargeImageToMG <- function(gobject = NULL,
    largeImage_name = NULL,
    giottoLargeImage = NULL,
    mg_name = NULL,
    spat_unit = NULL,
    spat_loc_name = NULL,
    crop_extent = NULL,
    xmax_crop = NULL,
    xmin_crop = NULL,
    ymax_crop = NULL,
    ymin_crop = NULL,
    resample_size = 500000,
    max_intensity = NULL,
    return_gobject = TRUE,
    verbose = TRUE) {
    # Check params
    if (is.null(gobject)) {
        if (return_gobject == TRUE)
            stop("gobject must be given if return_gobject == TRUE \n")
        if (!is.null(spat_loc_name))
            stop("if spatial location name is given then gobject containing
                it must also be given \n")
    }

    # Set spat_unit
    spat_unit <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = spat_unit
    )

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

    # Resample and then convert to Array
    rastSample <- terra::spatSample(raster_object,
        size = resample_size, # Defines the rough maximum of pixels allowed
        # when resampling
        method = "regular",
        as.raster = TRUE
    )

    imArray <- terra::as.array(rastSample)

    # Set max_intensity
    if (is.null(max_intensity)) {
        max_intensity <- max(imArray)
    }

    # Read in array as magick image
    mImg <- magick::image_read(imArray / max_intensity)

    # Set boundary adj values
    xmin_adj <- xmax_adj <- ymin_adj <- ymax_adj <- 0


    # magick object name
    if (is.null(mg_name)) {
        mg_name <- giottoLargeImage@name
    }

    # Create giottoImage
    g_image <- createGiottoImage(
        name = mg_name,
        mg_object = mImg,
        do_manual_adj = TRUE,
        xmax_adj = xmax_adj,
        xmin_adj = xmin_adj,
        ymax_adj = ymax_adj,
        ymin_adj = ymin_adj,
        verbose = FALSE
    )

    # Set minimax
    if (is.null(spat_loc_name)) {
        current_ext <- terra::ext(raster_object)
        g_image@minmax <- c(
            current_ext$xmax,
            current_ext$xmin,
            current_ext$ymax,
            current_ext$ymin
        )
    } else if (!is.null(spat_loc_name)) {
        spatial_locs <- get_spatial_locations(
            gobject = gobject,
            spat_unit = spat_unit,
            spat_loc_name = spat_loc_name
        )
        x_range <- range(spatial_locs$sdimx)
        y_range <- range(spatial_locs$sdimy)
        g_image@minmax <- c(
            x_range[2],
            x_range[1],
            y_range[2],
            y_range[1]
        )
    }

    names(g_image@minmax) <- c("xmax_sloc", "xmin_sloc",
                            "ymax_sloc", "ymin_sloc")

    # Set scalefactor
    im_dims <- magick::image_info(g_image@mg_object)
    x_scalefactor <- im_dims[["width"]] / dim(raster_object)[2]
    y_scalefactor <- im_dims[["height"]] / dim(raster_object)[1]


    g_image@scale_factor <- c("x" = x_scalefactor, "y" = y_scalefactor)
    g_image@resolution <- 1 / g_image@scale_factor

    if (return_gobject == TRUE) {
        if (verbose == TRUE) {
            if (mg_name %in% names(gobject@images))
                cat("\n ", mg_name, " has already been used, will be
                    overwritten \n")
        }
        gobject@images[[mg_name]] <- g_image
        return(gobject)
    } else if (return_gobject == FALSE) {
        return(g_image)
    }
}



#' @name .bitdepth
#' @title Guess likely bitdepth from value(s)
#' @param x numeric vector. Values representative of the data to be assessed for
#' bitdepth. This is usually a sampled subset of values from the raster.
#' @param return_max logical. default is `FALSE`. Whether to return the maximum
#' possible value for the detected bitdepth instead of the bitdepth itself
#' @keywords internal
#' @returns numeric
.bitdepth <- function(x, return_max = FALSE) {
    res <- ceiling(log(max(x), base = 2L)) # power of 2 needed to represent
    # value(s)
    res <- 2^ceiling(log(res, base = 2L)) # actual bitdepth

    if (isTRUE(return_max)) {
        res <- 2^res - 1
    }

    return(res)
}






#' @title .terra_writeraster_datatype
#' @name .terra_writeraster_datatype
#' @description find likely compatible datatype for given image characteristics.
#' Values given in arguments take priority over those found from
#' giottoLargeImage metadata
#' @param giottoLargeImage giottoLargeImage object to determine max_intensity,
#' min_intensity, is_int settings from
#' @param quick_INTU_maxval Treat as maximum intensity to find compatible
#' unsigned integer settings
#' @param max_intensity,min_intensity value given as image maximum/minimum
#' intensity
#' @param is_int if image is integer (TRUE) or floating point (FALSE)
#' @param signed if image is signed (TRUE) or unsigned (TRUE)
#' @param bitDepth image bitDepth
#' @param verbose be verbose
#' @keywords internal
#' @returns datatype for terra writeRaster function
.terra_writeraster_datatype <- function(giottoLargeImage = NULL,
    quick_INTS_maxval = NULL,
    max_intensity = NULL,
    min_intensity = NULL,
    is_int = NULL,
    signed = NULL,
    bitDepth = NULL,
    verbose = TRUE) {
    # 1. Get any missing metadata from giottoLargeImage object if given
    if (!is.null(giottoLargeImage)) {
        if (is.null(max_intensity))
            max_intensity <- giottoLargeImage@max_intensity
        if (is.null(min_intensity))
            min_intensity <- giottoLargeImage@min_intensity
        if (is.null(is_int)) is_int <- giottoLargeImage@is_int
    }


    if (is.null(quick_INTS_maxval)) {
        if (length(c(max_intensity, min_intensity)) < 2)
            stop("Not enough metadata is given")

        # Determine if negative values are present

        if (min_intensity < 0) {
            signed <- TRUE
        }
    }

    # Set defaults if data still missing
    if (is.null(is_int)) is_int <- TRUE
    if (is.null(signed)) signed <- FALSE

    ## 2. Determine likely compatible datatype
    dataTypeVerbose <- data.frame(
        bitDepth = c(8, 16, 16, 32, 32, 32, 64),
        signed = c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE),
        integer = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
        dataTypeChoices = c("INT1U", "INT2U", "INT2S", "INT4U", "INT4S",
                            "FLT4S", "FLT8S"),
        dataTypeVerbose = c(
            "8bit unsigned integer", "16bit unsigned integer",
            "16bit signed integer",
            "32bit unsigned integer", "32bit signed integer",
            "32bit signed floating point",
            "64bit signed floating point"
        )
    )


    ## Find Compatible Bitdepth
    # If quick_INTS_maxval argument is set, will be treated as the highest
    # needed (unsigned preferred) bitdepth datatype
    if (is.null(quick_INTS_maxval)) {
        if (max_intensity > 0) {
            max_intensity <- max_intensity + 1 # Accounts for 0 occupying 1
            # of the available values.

            if (signed == FALSE) {
                bitDepth <- ceiling(log(x = max_intensity, base = 2))
            } else if (isTRUE(signed)) {
                intensityMinMax <- c(min_intensity, max_intensity)
                intensityMinMax <- abs(intensityMinMax)
                bitDepthMinMax <- ceiling(log(x = intensityMinMax, base = 2))
                bitDepth <- max(bitDepthMinMax) + 1
            }
        } else {
            stop("There are no positive image intensities. \n Manual datatype
                assignment needed \n")
        }
    } else if (!is.null(quick_INTS_maxval)) {
        if (isTRUE(verbose))
            cat("Selecting compatible datatype for given maximum value \n")
        bitDepth <- .bitdepth(quick_INTS_maxval)
    }

    if (bitDepth > 32 && bitDepth <= 128) {
        bitDepth <- 32
        is_int <- FALSE
        signed <- TRUE
    } else if (bitDepth > 128) {
        bitDepth <- 64
        is_int <- FALSE
        signed <- TRUE
    }

    dataType <- NULL
    # Determine datatype settings
    if (is_int == TRUE) {
        if (signed == FALSE) {
            if (bitDepth <= 8) {
                dataType <- "INT1U"
            } else if (bitDepth <= 16) {
                dataType <- "INT2U"
            } else if (bitDepth <= 32) {
                dataType <- "INT4U"
            }
        } else if (signed == TRUE) {
            if (bitDepth <= 16) {
                dataType <- "INT2S"
            } else if (bitDepth <= 32) {
                dataType <- "INT4S"
            }
        }
    } else if (is_int == FALSE) {
        if (bitDepth <= 32) {
            dataType <- "FLT4S"
        } else if (bitDepth == 64) {
            dataType <- "FLT8S"
            # These are very large numbers. Can't actually tell the difference
            # from FLT4S (less than 2^128) to FLT8S unless you add roughly
            # 10^25 to it.
            # This necessary minimum change would be 10^22, but the log used
            # when determining bitDepth further increases the needed difference.
            # Manual assignment of dataType could be more reliable than
            # automatic assignment for these very large values.
        }
    }
    return(dataType)
}



#' @title writeGiottoLargeImage
#' @name writeGiottoLargeImage
#' @description Write full resolution image to file. Filetype extension should
#' be included in \code{filename} argument. Be careful if write time and disk
#' space needed if image is very large.
#' @param giottoLargeImage giottoLargeImage object
#' @param gobject giotto object
#' @param largeImage_name name of giottoLargeImage
#' @param filename file name and path to write the image to
#' @param dataType (optional) values for \code{dataType} are "INT1U", "INT2U",
#' "INT2S", "INT4U", "INT4S", "FLT4S", "FLT8S". The first three letters
#' indicate whether the dataType is integer (whole numbers) of a real
#' number (decimal numbers), the fourth character indicates the number of
#' bytes used (allowing for large numbers and/or more precision), and the
#' "S" or "U" indicate whether the values are
#' signed (both negative and positive) or unsigned (positive values only).
#' @param max_intensity (optional) image max intensity value from
#' which \code{dataType} can be automatically determined
#' @param overwrite Overwrite if \code{filename} is already existing
#' @param verbose be verbose
#' @returns image local file
#' @export
writeGiottoLargeImage <- function(giottoLargeImage = NULL,
    gobject = NULL,
    largeImage_name = NULL,
    filename = NULL,
    dataType = NULL,
    max_intensity = NULL,
    overwrite = FALSE,
    verbose = TRUE) {
    # 0. Check params
    if (!is.null(giottoLargeImage)) {
        if (!inherits(giottoLargeImage, "giottoLargeImage"))
            stop("giottoLargeImage argument only accepts giottoLargeImage
                objects. \n")
    }
    if (!is.null(max_intensity)) {
        if (!is.numeric(max_intensity))
            stop("max_intensity must be a numeric \n")
    }
    if (!is.null(filename)) {
        if (!is.character(filename))
            stop("filename must be given as character \n")
        # check filename
        if (file.exists(filename)) {
            if (verbose == TRUE) {
                if (overwrite == TRUE)
                    cat("File at", filename, "exists.\n (overwrite == TRUE)
                        Image will be overwritten")
                if (overwrite == FALSE)
                    cat("File at", filename, "exists.\n (overwrite == FALSE)
                        Image will not be overwritten")
            }
        }
    }

    if (is.null(filename))
        stop("Please enter a filename to save the image as. \n")

    filename <- path.expand(filename)

    # Match dataType input if given
    dataTypeChoices <- c("INT1U", "INT2U", "INT2S", "INT4U", "INT4S",
                        "FLT4S", "FLT8S")
    if (!is.null(dataType)) dataType <- match.arg(
        dataType, choices = dataTypeChoices)


    ## 1. get giottoLargeImage if necessary
    if (is.null(giottoLargeImage)) {
        if (!is.null(gobject) && !is.null(largeImage_name)) {
            giottoLargeImage <- get_giottoLargeImage(
                gobject = gobject,
                name = largeImage_name
            )
        } else {
            stop("either a giottoLargeImage or both the gobject and name of
                the giottoLargeImage must be given. \n")
        }
    }

    raster_object <- giottoLargeImage@raster_object

    ## 2. Get likely compatible dataType
    if (is.null(dataType)) {
        dataType <- .terra_writeraster_datatype(
            giottoLargeImage = giottoLargeImage,
            quick_INTS_maxval = max_intensity
        )
    }


    ## 3. Write to disk
    if (verbose == TRUE) cat(paste0("Writing image to disk as ", dataType))
    terra::writeRaster(
        x = raster_object,
        filename = filename,
        datatype = dataType,
        overwrite = overwrite
    )
}



#' @title updateGiottoLargeImage
#' @name updateGiottoLargeImage
#' @description Updates the boundaries of a giotto \code{largeImage} object
#' attached to a \code{giotto} object if both \code{gobject}
#' and \code{largeImage_name} params are given. Alternatively can directly
#' accept and return as \code{largeImage}
#' @inheritParams updateGiottoImage
#' @param gobject \code{giotto} object containing giotto \code{largeImage}
#' object
#' @param giottoLargeImage \code{largeImage} object to directly update
#' @param return_gobject return a \code{giotto} object if \code{TRUE}, a giotto
#' \code{largeImage} object if \code{FALSE}
#' @returns a \code{giotto} object or an updated giotto \code{largeImage} object
#' if \code{return_gobject = FALSE}
#' @export
updateGiottoLargeImage <- function(gobject = NULL,
    largeImage_name = NULL,
    giottoLargeImage = NULL,
    xmax_adj = 0,
    xmin_adj = 0,
    ymax_adj = 0,
    ymin_adj = 0,
    x_shift = 0,
    y_shift = 0,
    scale_factor = NULL,
    scale_x = 1,
    scale_y = 1,
    order = c("first_adj", "first_scale"), # TODO make this a list of
    # operations to perform, include rotation
    xmin_set = NULL,
    xmax_set = NULL,
    ymin_set = NULL,
    ymax_set = NULL,
    return_gobject = TRUE,
    verbose = TRUE) {
    # 0. Check params
    # Check input image
    if (is.null(gobject)) {
        if (is.null(giottoLargeImage))
            stop("Image to be updated must be given as gobject AND
                largeImage_name OR giottoLargeImage argument(s) \n")
        if (verbose == TRUE)
            cat("gobject argument not given \n return_gobject set to FALSE \n")
        return_gobject <- FALSE
    }
    if (is.null(giottoLargeImage) && is.null(largeImage_name))
        stop("The name of the giottoLargeImage that will be updated needs to
            be provided \n")

    if (!is.null(giottoLargeImage)) {
        if (!inherits(giottoLargeImage, "giottoLargeImage"))
            stop("giottoLargeImage argument only accpts giottoLargeImage
                objects \n")
        if (verbose == TRUE && !is.null(gobject))
            cat("giottoLargeImage argument is given and will take priority
                \n return_gobject set to FALSE \n")
        return_gobject <- FALSE
    }

    # Check scalefactors
    if (!is.null(scale_factor)) scale_x <- scale_y <- scale_factor

    # Check spatial anchor values
    spatAnchor <- c(
        xmin_set,
        xmax_set,
        ymin_set,
        ymax_set
    )
    if (length(spatAnchor) < 4 && length(spatAnchor) > 0)
        stop("If set arguments are being used, all four must be given \n")
    if (!is.null(spatAnchor)) {
        if (xmax_set < xmin_set)
            stop("xmax_set must be greater than xmin_set \n")
        if (ymax_set < ymin_set)
            stop("ymax_set must be greater than ymin_set \n")
    }

    # Find order of adjust and scaling
    order <- match.arg(order, choices = c("first_adj", "first_scale"))

    # 1. get giottoImage if necessary
    if (is.null(giottoLargeImage)) {
        if (!is.null(gobject) && !is.null(largeImage_name)) {
            g_imageL <- get_giottoLargeImage(
                gobject = gobject,
                name = largeImage_name
            )
        } else {
            stop("either a giottoLargeImage or both the gobject and name of
                the giottoLargeImage must be given. \n")
        }
    }


    # 2. Find minmax spatial anchor values if set values not supplied
    if (is.null(spatAnchor)) {
        spatAnchor <- terra::ext(x = g_imageL@raster_object)[seq_len(4)]
        # (xmin, xmax, ymin, ymax)
        names(spatAnchor) <- NULL
    }

    # Perform scale if first_scale
    if (order == "first_scale") {
        spatAnchor <- spatAnchor * c(scale_x, scale_x, scale_y, scale_y)
    }

    # 3. Prepare adjustment values
    # Apply shifts
    xmin_adj <- xmin_adj - x_shift
    xmax_adj <- xmax_adj + x_shift
    ymin_adj <- ymin_adj - y_shift
    ymax_adj <- ymax_adj + y_shift

    # Find final bounds
    xmin_final <- spatAnchor[1] - xmin_adj
    xmax_final <- spatAnchor[2] + xmax_adj
    ymin_final <- spatAnchor[3] - ymin_adj
    ymax_final <- spatAnchor[4] + ymax_adj

    # Perform scale if first_adj
    if (order == "first_adj") {
        xmin_final <- xmin_final * scale_x
        xmax_final <- xmax_final * scale_x
        ymin_final <- ymin_final * scale_y
        ymax_final <- ymax_final * scale_y
    }


    # 4. Update the boundaries
    if (return_gobject == FALSE) {
        if (getNamespaceVersion("terra") >= "1.15-12") {
            g_imageL@raster_object <- terra::deepcopy(g_imageL@raster_object)
        } else {
            # g_imageL@raster_object = terra::copy(g_imageL@raster_object)
            if (isTRUE(verbose))
                warning("\n If largeImage was created from a terra raster
                        object, manipulations to the giotto image may be
                        reflected in the raster object as well. Update
                        terra to >= 1.15-12 to avoid this issue. \n")
        }
    }
    terra::ext(g_imageL@raster_object) <- c(
        xmin_final,
        xmax_final,
        ymin_final,
        ymax_final
    )

    # Update the extent tracking slot
    g_imageL@extent <- as.vector(terra::ext(g_imageL@raster_object))

    # 5. Update the scalefactors for x and y
    g_imageL@resolution <- terra::res(g_imageL@raster_object) # (x,y)
    names(g_imageL@resolution) <- c("x", "y")
    g_imageL@scale_factor <- (1 / g_imageL@resolution)


    if (return_gobject == TRUE) {
        gobject@largeImages[[largeImage_name]] <- g_imageL
        return(gobject)
    } else {
        return(g_imageL)
    }
}


#' @title addGiottoLargeImage
#' @name addGiottoLargeImage
#' @description Adds giotto image objects to your giotto object
#' @param gobject giotto object
#' @param largeImages list of giottoLargeImage objects
#' @param spat_loc_name provide spatial location slot in Giotto to align
#' images. (optional)
#' @param scale_factor provide scale of image pixel dimensions relative to
#' spatial coordinates.
#' @param negative_y map image to negative y spatial values if TRUE during
#' automatic alignment. Meaning that origin is in upper left instead of lower
#' left.
#' @param verbose be verbose
#' @returns an updated Giotto object with access to the list of images
#' @export
addGiottoLargeImage <- function(gobject = NULL,
    largeImages = NULL,
    spat_loc_name = NULL,
    scale_factor = NULL,
    negative_y = TRUE,
    verbose = TRUE) {
    # 0. check params
    if (is.null(gobject))
        stop("The giotto object that will be updated needs to be provided")

    if (is.null(largeImages))
        stop("The giotto large image(s) that will be added needs to be
            provided")

    ext_scale_factor <- FALSE
    if (!is.null(scale_factor)) {
        if (!is.numeric(scale_factor))
            stop("Given scale_factor(s) must be numeric")

        if ((length(scale_factor) == length(largeImages)) ||
            length(scale_factor) == 1) {
            # cat('scale_factor(s) external to giottoImage have been given and
            # will be used')
            ext_scale_factor <- TRUE
        } else {
            stop("if scale_factor is given, it must be a numeric with either
                a single value or as many values as there are largeImages are
                provided")
        }
    }

    # 1. expand scale_factors
    if (ext_scale_factor == TRUE) {
        if (length(scale_factor == 1)) {
            scale_factor <- rep(scale_factor, length(largeImages))
        }
    }


    # 2. Add image with for loop
    for (image_i in seq_len(length(largeImages))) {
        im <- largeImages[[image_i]]

        if (inherits(im, "giottoLargeImage")) {
            im_name <- im@name

            all_im_names <- names(gobject@largeImages)

            if (im_name %in% all_im_names) {
                cat("\n ", im_name,
                    " has already been used, will be overwritten \n")
            }

            # Deep copy the raster_object
            if (getNamespaceVersion("terra") >= "1.15-12") {
                im@raster_object <- terra::deepcopy(im@raster_object)
            } else {
                # im@raster_object = terra::copy(im@raster_object)
                if (isTRUE(verbose))
                    warning("\n If largeImage was created from a terra raster
                            object, manipulations to the giotto image may be
                            reflected in the raster object as well. Update
                            terra to >= 1.15-12 to avoid this issue. \n")
            }

            # # 3. Update boundaries if not already done during
            # createGiottoImage() due to lack of spatlocs and gobject
            # if(sum(im@boundaries == c(0,0,0,0)) == 4 &&
            # sum(im@minmax == c(10,0,10,0)) == 4) {
            #   if(!is.null(spat_loc_name)) { # A check for the first
            # # available spatloc was already done
            #     spatlocs = get_spatial_locations(gobject = gobject,
            #                                     spat_loc_name = spat_loc_name)
            #
            #     #Find spatial minmax values
            #     xmin_sloc = min(spatlocs$sdimx)
            #     xmax_sloc = max(spatlocs$sdimx)
            #     ymin_sloc = min(spatlocs$sdimy)
            #     ymax_sloc = max(spatlocs$sdimy)
            #
            #     #Find adjustment values
            #     img_minmax = get_img_minmax(mg_img = im@mg_object,
            #                                 negative_y = negative_y)
            #     if(ext_scale_factor == TRUE) {
            #       adj_values = get_adj_rescale_img(img_minmax = img_minmax,
            #                                        spatial_locs = spatlocs,
            #                           scale_factor = scale_factor[[image_i]])
            #     } else if (ext_scale_factor == FALSE) {
            #       adj_values = get_adj_rescale_img(img_minmax = img_minmax,
            #                                        spatial_locs = spatlocs,
            #                   scale_factor = im@scale_factor[[spat_loc_name]])
            #     }
            #
            #     #Add minmax values to giottoImage@minmax
            #     im@minmax = c('xmax_sloc' = xmax_sloc,
            #                   'xmin_sloc' = xmin_sloc,
            #                   'ymax_sloc' = ymax_sloc,
            #                   'ymin_sloc' = ymin_sloc)
            #
            #     #Add adjustment values to giottoImage@boundaries
            #     im@boundaries = c(
            # 'xmax_adj' = as.numeric(adj_values[['xmax_adj_orig']]),
            # 'xmin_adj' = as.numeric(adj_values[['xmin_adj_orig']]),
            # 'ymax_adj' = as.numeric(adj_values[['ymax_adj_orig']]),
            # 'ymin_adj' = as.numeric(adj_values[['ymin_adj_orig']]))
            #
            #     # Inherit external scaling factors if given
            #     if(ext_scale_factor == TRUE) {
            #       im@scale_factor[[spat_loc_name]] = scale_factor[[image_i]]
            #       im@resolution[[spat_loc_name]] = 1/(scale_factor[[image_i]])
            #     }
            #     ## Externally given scale_factors will only be written
            #     ## in/used if boundary adj values are not pre-existing
            #   }
            #
            # }

            # 4. Add giottoImage to gobject
            gobject@largeImages[[im_name]] <- im
        } else {
            warning("image [", image_i, "] is not a giotto image object")
        }
    }

    return(gobject)
}



#' @title reconnect_giottoLargeImage
#' @name reconnect_giottoLargeImage
#' @description  reconnect giottoLargeImage if image pointer is broken
#' @param giottoLargeImage giottoLargeImage to reconnect
#' @param image_path path to source file of giottoLargeImage
#' @returns reconnected giottoLargeImage
#' @keywords internal
#' @export
reconnect_giottoLargeImage <- function(
        giottoLargeImage,
        image_path) {

    deprecate_soft("0.2.4", "reconnect_giottoLargeImage()", "reconnect()")

    # load in new terra raster objects
    raster_object <- .create_terra_spatraster(image_path = image_path)

    # replace old raster objects and inherit tracked extents
    giottoLargeImage@raster_object <- raster_object
    terra::ext(giottoLargeImage@raster_object) <- giottoLargeImage@extent

    # return reconnected giottoLargeImage
    return(giottoLargeImage)
}




# Generalized Image Tools ####


#' @title Plot a giotto image object
#' @name plotGiottoImage
#' @description Display a giotto image in the viewer panel. Image object to plot
#' can be specified by providing the giotto object containing the
#' image (\code{gobject}), the image object name (\code{image_name}), and the
#' image object type (\code{image_type}). Alternatively, image objects can be
#' directly plotted through their respective associated params.
#' @param gobject gobject containing giotto image object
#' @param image_name name of giotto image object
#' @param image_type type of giotto image object to plot
#' @param giottoImage giottoImage object to plot directly
#' @param giottoLargeImage giottoLargeImage object to plot directly
#' @param largeImage_crop_params_list (optional) named list of params for
#' focusing on a specified region of a giottoLargeImage.
#' @param largeImage_max_intensity (optional) assign override value to treat as
#' maximum intensity in color scale when plotting giottoLargeImage
#' @param ... additional params to pass to image object specific plotting
#' functions
#' @section largeImage-specific additional params:
#'   \code{largeImage_crop_params_list} accepts a named list of the following
#'     possible params to define a region of interest (ROI) to plot through
#'     either a terra extent object OR x and y min and max bounds given as
#'     numerics:
#'   \itemize{
#'     \item{\code{crop_extent} -- terra extent object to define crop ROI}
#'     \item{\code{xmax_crop} -- x max of ROI}
#'     \item{\code{xmin_crop} -- x min of ROI}
#'     \item{\code{ymax_crop} -- y max of ROI}
#'     \item{\code{ymin_crop} -- y min of ROI}
#'   }
#'   \code{largeImage_max_intensity} accepts a numeric value to set the max
#'     value in the plotting color scale. Can be used in case there are high
#'     outlier intensity values in the image and a preview with alternative
#'     color scaling is desired.
#' @family basic image functions
#' @returns image
#' @export
plotGiottoImage <- function(gobject = NULL,
    image_name = NULL,
    image_type = NULL,
    giottoImage = NULL,
    giottoLargeImage = NULL,
    largeImage_crop_params_list = NULL,
    largeImage_max_intensity = NULL,
    ...) {
    # Check params
    if (!is.null(giottoImage) && !is.null(giottoLargeImage))
        stop("Only one of a giottoImage or a giottoLargeImage can be plotted
            at the same time. \n")

    # Get image object
    if (!is.null(gobject)) {
        img_obj <- get_giottoImage(
            gobject = gobject,
            image_type = image_type,
            name = image_name
        )
    }
    if (!is.null(giottoImage)) {
        img_obj <- giottoImage
        image_type <- "image"
        image_name <- img_obj@name
    }
    if (!is.null(giottoLargeImage)) {
        img_obj <- giottoLargeImage
        image_type <- "largeImage"
        image_name <- img_obj@name
    }

    # Select plotting function
    # cat('Plotting ', image_type, ': "', image_name, '" ... \n', sep = '')

    if (image_type == "image") {
        .plot_giottoimage_mg(giottoImage = img_obj)
    }
    if (image_type == "largeImage") {
        .plot_giottolargeimage(
            giottoLargeImage = img_obj,
            crop_extent = largeImage_crop_params_list$crop_extent,
            xmax_crop = largeImage_crop_params_list$xmax_crop,
            xmin_crop = largeImage_crop_params_list$xmin_crop,
            ymax_crop = largeImage_crop_params_list$ymax_crop,
            ymin_crop = largeImage_crop_params_list$ymin_crop,
            max_intensity = largeImage_max_intensity,
            ...
        )
    }
}



#' @title addGiottoImage
#' @name addGiottoImage
#' @description Adds lists of giottoImages and giottoLargeImages to gobjects
#' @param gobject gobject to add images objects to
#' @param images list of giottoImages to add
#' @param largeImages list of giottoLargeImages to add
#' @param spat_loc_name provide spatial location slot in Giotto to align
#' giottoImages. Defaults to first one
#' @param scale_factor provide scale of image pixel dimensions relative to
#' spatial coordinates.
#' @param negative_y Map image to negative y spatial values if TRUE during
#' automatic alignment. Meaning that origin is in upper left instead of lower
#' left.
#' @returns an updated Giotto object with access to the list of images
#' @family basic image functions
#' @export
addGiottoImage <- function(gobject = NULL,
    images = NULL,
    largeImages = NULL,
    spat_loc_name = NULL,
    scale_factor = NULL,
    negative_y = TRUE) {
    if (!is.null(images) && !is.null(largeImages))
        stop("Can only add one type of image to giotto object at a time")
    if (!is.null(images)) {
        addGiottoImageMG(
            gobject = gobject,
            images = images,
            spat_loc_name = spat_loc_name,
            scale_factor = scale_factor,
            negative_y = negative_y
        )
    } else if (!is.null(largeImages)) {
        addGiottoLargeImage(
            gobject = gobject,
            largeImages = largeImages,
            spat_loc_name = spat_loc_name,
            scale_factor = scale_factor,
            negative_y = negative_y
        )
    }
}



#' @title updateGiottoImage
#' @name updateGiottoImage
#' @description Updates the spatial positioning and sizing of a
#' giotto \code{image} or \code{largeImage} attached to a giotto object.
#' @details This function works for all image objects associated with Giotto.
#' @param gobject gobject containing desired image object
#' @param image_name name of giotto \code{image} object
#' @param largeImage_name name of giotto \code{largeImage} object
#' @param xmax_adj,xmin_adj,ymax_adj,ymin_adj adjust image boundaries by
#' increasing maximum and decreasing minimum bounds respectively of xy bounds
#' @param x_shift,y_shift shift entire image along xy axes
#' @param scale_factor set \code{scale_x} and \code{scale_y} params at the
#' same time
#' @param scale_x,scale_y independently scale x or y axis image mapping from
#' coordinate origin
#' @param order order of operations between fine
#' adjustments (adjustment and shift parameters) and scaling
#' @param xmin_set,xmax_set,ymin_set,ymax_set directly set xy image boundaries.
#'   Overrides minmax values as spatial anchor.
#' @param return_gobject return a giotto object if \code{TRUE}, a giotto image
#' object if \code{FALSE}
#' @param verbose be verbose
#' @returns a giotto object or an updated giotto image object if
#' return_gobject = FALSE
#' @family basic image functions
#' @export
updateGiottoImage <- function(gobject = NULL,
    image_name = NULL,
    largeImage_name = NULL,
    xmax_adj = 0,
    xmin_adj = 0,
    ymax_adj = 0,
    ymin_adj = 0,
    x_shift = 0,
    y_shift = 0,
    scale_factor = NULL,
    scale_x = 1,
    scale_y = 1,
    order = c("first_adj", "first_scale"),
    xmax_set = NULL,
    xmin_set = NULL,
    ymax_set = NULL,
    ymin_set = NULL,
    return_gobject = TRUE,
    verbose = TRUE) {
    # 0. Check params
    if (is.null(gobject))
        stop("The giotto object that will be updated needs to be provided \n")
    if (is.null(image_name) && is.null(largeImage_name))
        stop("The name of the giotto image that will be updated needs to be
            provided \n")

    order <- match.arg(order, choices = c("first_adj", "first_scale"))

    if (!is.null(image_name) && !is.null(largeImage_name))
        stop("Adjust only giottoImage OR giottoLargeImage at any one time. \n")


    # 2. Select adjustment function
    if (!is.null(image_name)) {
        out <- updateGiottoImageMG(
            gobject = gobject,
            image_name = image_name,
            xmax_adj = xmax_adj,
            xmin_adj = xmin_adj,
            ymax_adj = ymax_adj,
            ymin_adj = ymin_adj,
            x_shift = x_shift,
            y_shift = y_shift,
            scale_factor = scale_factor,
            scale_x = scale_x,
            scale_y = scale_y,
            order = order,
            xmax_set = xmax_set,
            xmin_set = xmin_set,
            ymax_set = ymax_set,
            ymin_set = ymin_set,
            return_gobject = return_gobject,
            verbose = verbose
        )
    } else if (!is.null(largeImage_name)) {
        out <- updateGiottoLargeImage(
            gobject = gobject,
            largeImage_name = largeImage_name,
            xmax_adj = xmax_adj,
            xmin_adj = xmin_adj,
            ymax_adj = ymax_adj,
            ymin_adj = ymin_adj,
            x_shift = x_shift,
            y_shift = y_shift,
            scale_factor = scale_factor,
            scale_x = scale_x,
            scale_y = scale_y,
            order = order,
            xmax_set = xmax_set,
            xmin_set = xmin_set,
            ymax_set = ymax_set,
            ymin_set = ymin_set,
            return_gobject = return_gobject,
            verbose = verbose
        )
    }
    return(out)
}



#' @title reconnect_image_object
#' @name reconnect_image_object
#' @description Reconnect giotto image object with dead image pointer using
#' a filepath to the original image source
#' @details This is a simple wrapper function for image object-specific
#' reconnection functions and does not include other functionality to find the
#' specific image objects in the giotto object.
#' @param image_object giotto image object
#' @param image_type type of giotto image object
#' @param image_path path to image source to reconnect image object with
#' @returns reconnected image_object
#' @keywords internal
reconnect_image_object <- function(
        image_object,
        image_type,
        image_path) {

    deprecate_soft("0.2.4", "reconnect_image_object()", "reconnect()")

    image_object <- reconnect(
        x = image_object,
        path = image_path
    )

    return(image_object)
}




#' @title select_gimage
#' @name select_gimage
#' @description selects and (possibly resamples) giotto images for plotting
#' @keywords internal
#' @returns giotto image
#' @export
select_gimage <- function(gobject,
    gimage = NULL,
    image_name = NULL,
    largeImage_name = NULL,
    spat_unit = NULL,
    spat_loc_name = NULL,
    feat_type = NULL,
    polygon_feat_type = NULL) {
    if (!is.null(gimage)) {
        gimage <- gimage
    } else if (!is.null(image_name)) {
        if (length(image_name) == 1) {
            gimage <- gobject@images[[image_name]]
            if (is.null(gimage))
                warning("image_name: ", image_name, " does not exists")
        } else {
            gimage <- list()
            for (gim in seq_len(length(image_name))) {
                gimage[[gim]] <- gobject@images[[gim]]
                if (is.null(gimage[[gim]]))
                    warning("image_name: ", gim, " does not exists")
            }
        }
    } else if (!is.null(largeImage_name)) {
        # If there is input to largeImage_name arg

        if (length(largeImage_name) == 1) {
            gimage <- plot_auto_largeImage_resample(
                gobject = gobject,
                largeImage_name = largeImage_name,
                spat_unit = spat_unit,
                spat_loc_name = spat_loc_name,
                polygon_feat_type = polygon_feat_type,
                include_image_in_border = TRUE
            )
        } else {
            gimage <- list()
            for (gim in seq_len(length(largeImage_name))) {
                gimage[[gim]] <- plot_auto_largeImage_resample(
                    gobject = gobject,
                    largeImage_name = largeImage_name[[gim]],
                    spat_unit = spat_unit,
                    spat_loc_name = spat_loc_name,
                    polygon_feat_type = polygon_feat_type,
                    include_image_in_border = TRUE
                )
            }
        }
    } else {
        # Default to first image available in images if no input given to
        # image_name or largeImage_name args
        image_name <- names(gobject@images)[1]
        gimage <- gobject@images[[image_name]]

        if (is.null(gimage))
            warning("image_name: ", image_name, " does not exist \n")
    }

    return(gimage)
}





#' @title Reconnect images with dead pointers
#' @name reconnectGiottoImage
#' @description reconnect a gobject's dead image pointers using filepaths to
#' the original source image files
#' @details Inputs can either be given as both image
#' name (\code{image_name}/\code{largeImage_name}) and
#' filepath (\code{image_path}/\code{largeImage_path}) args or as only a named
#' list through a filepath argument alone. If \code{auto_reconnect = TRUE} then
#' no additional params need to be supplied. As long as giotto image objects
#' were directly created using filepaths, those filepaths are stored within
#' the image objects and will be referenced during reconnection. Issues will
#' only arise if giotto image objects were created directly from the underlying
#' image handling package objects (\emph{magick} or \emph{raster objects}) or
#' if image files have been moved since the the giotto image object was
#' generated. In such cases, use manual reconnection by
#' setting \code{auto_reconnect = FALSE}.
#' @param gobject giotto object
#' @param auto_reconnect automatically reconnect images if TRUE. manual if FALSE
#' @param reconnect_type type of image to reconnect when auto_reconnect = TRUE
#' @param image_name names of images to reconnect
#' @param largeImage_name name of large images to reconnect
#' @param image_path named list of paths to images to reconnect to giottoImages
#' @param largeImage_path named list of paths to images to reconnect to
#' giottoLargeImages
#' @param verbose be verbose
#' @returns a giotto object with updated image pointer
#' @family basic image functions
#' @export
reconnectGiottoImage <- function(gobject,
    auto_reconnect = TRUE,
    reconnect_type = c("all", "image", "largeImage"),
    image_name = NULL,
    largeImage_name = NULL,
    image_path = NULL,
    largeImage_path = NULL,
    verbose = TRUE) {
    # Adding image_types:
    # Manual workflow needs to be updated when adding more image types

    if (is.null(gobject))
        stop("Giotto object containing the giottoImages or giottoLargeImages
            to reconnect must be given \n")
    reconnect_type <- match.arg(reconnect_type,
                                choices = c("all", "image", "largeImage"))

    # 1. Find names and locations of image objects in gobject: ----------------#
    if (reconnect_type %in% c("image", "largeImage")) {
        availableImgs <- list_images(gobject = gobject,
                                    img_type = reconnect_type)
    } else if (reconnect_type == "all") {
        availableImgs <- list_images(gobject = gobject)
    } else {
        default <- stop("reconnect_type input unrecognized")
    }


    # 2. Get reconnection info: -----------------------------------------------#

    # Initialize lists
    name_list <- list()
    img_list <- list()
    img_path <- list()

    ## image_type_list | vector of image types present within images to be
    ## reconnected (vector)
    ## name_list | determines which available images to reconnect,
    ## categorized by image_type | (list)
    ## img_list | list of image objects, categorized by image_type | (list)
    ## img_path | filepaths to reconnect images with, categorized by image_type
    ## | (list)

    #### Auto Workflow
    if (auto_reconnect == TRUE) {
        if (verbose == TRUE) cat("Attempting automatic reconnection...\n\n")

        # Find image_types to reconnect
        image_type_list <- unique(availableImgs$img_type)

        # Run for each image_type given...
        for (image_type in image_type_list) {
            # Images to update will be ANY available images
            # Get list of image object names
            name_list[[image_type]] <- list_images_names(
                gobject = gobject,
                img_type = image_type
            )

            # get image objects
            img_list[[image_type]] <- lapply(
                X = name_list[[image_type]],
                FUN = get_giottoImage,
                gobject = gobject,
                image_type = image_type
            )

            # get file paths from image objects
            img_path[[image_type]] <- lapply(
                X = seq_len(length(img_list[[image_type]])),
                function(x) {
                    img_list[[image_type]][[x]]@file_path
                }
            )

            # print discovered images and paths
            # additionally, set path to NULL if file.exists() == FALSE
            if (verbose == TRUE)
                cat(image_type, "(s) discovered...\n", sep = "")

            for (image_i in seq_len(length(img_path[[image_type]]))) {
                if (!is.null(img_path[[image_type]][[image_i]])) {
                    if (verbose == TRUE)
                        cat("-->", name_list[[image_type]][[image_i]],
                            ": filepath found")
                    if (!file.exists(img_path[[image_type]][[image_i]])) {
                        if (verbose == TRUE) cat("but file is missing\n")
                        img_path[[image_type]][[image_i]] <- NULL
                    } else if (verbose == TRUE) cat("\n")
                } else if (verbose == TRUE)
                    cat("-->", name_list[[image_type]][[image_i]],
                        ": filepath NOT found\n")
            }
            if (verbose == TRUE) cat("\n")
        } # image_type end loop



        #### Manual Workflow
    } else {
        if (verbose == TRUE) cat("Reconnecting with manual input...\n\n")

        # Check params
        # filepath list(s) must be given as input
        if (is.null(image_path) & is.null(largeImage_path))
            stop("Image filepaths must be given for manual reconnection \n")

        # Assemble reconnection info lists - UPDATE THIS WHEN ADDING IMAGE TYPES
        image_type_list <- c()
        if (!is.null(image_path)) {
            image_type_list <- c(image_type_list, "image")
            name_list[["image"]] <- image_name
            img_path[["image"]] <- image_path
        }
        if (!is.null(largeImage_path)) {
            image_type_list <- c(image_type_list, "largeImage")
            name_list[["largeImage"]] <- largeImage_name
            img_path[["largeImage"]] <- largeImage_path
        }


        # Run for each image_type given...
        for (image_type in image_type_list) {
            # Check params
            if (is.null(name_list[[image_type]]))
                name_list[[image_type]] <- names(img_path[[image_type]])
            if (is.null(name_list[[image_type]]))
                stop("Names of ", image_type,
                    "s to be reconnected must be given as named list in ",
                    image_type, "_path arg or together with ",
                    image_type, "_path as a separate vector in ",
                    image_type, "_name arg. \n")
            if (length(unique(name_list[[image_type]])) != length(
                img_path[[image_type]]))
                stop("If ", image_type,
                    "s to be reconnected are selected through ", image_type,
                    "_name arg then names must be unique and length and order
                    must be the same as in ", image_type, "_path arg. \n")
            if (!all(name_list[[image_type]] %in% list_images_names(
                gobject = gobject, img_type = image_type)))
                stop("Names given to ", image_type,
                    "_name argument must match those in the gobject \n")

            # get image objects
            img_list[[image_type]] <- lapply(
                X = name_list[[image_type]],
                FUN = get_giottoImage,
                gobject = gobject,
                image_type = image_type
            )

            # update file_path
            img_list[[image_type]] <- lapply(
                X = seq_len(length(img_list[[image_type]])),
                function(x) {
                    img_list[[image_type]][[x]]@file_path <-
                        img_path[[image_type]][[x]]
                }
            )
        }
    } # Manual workflow-specific end

    # 3. Load new image pointer: ---------------------------------------------#

    image_path_NULL <- list()

    for (image_type in image_type_list) {
        # check for NULLs in image paths
        image_path_NULL[[image_type]] <- unlist(
            lapply(X = img_path[[image_type]], is.null))

        # Remove NULL path entries from name, path, and image object lists
        if (any(image_path_NULL[[image_type]])) {
            # End loop early if there are no imagepaths
            if (sum(image_path_NULL[[image_type]]) == length(
                img_path[[image_type]])) {
                if (verbose == TRUE)
                    cat(image_type, ": no filepaths found. Skipping. \n")
                next
            }

            if (verbose == TRUE) {
                cat("\n Skipping ", image_type,
                    "s with missing filepaths: \n", sep = "")
                for (image_NULL_i in seq_len(sum(image_path_NULL[[image_type]]))) {
                    cat(name_list[[image_type]][[which(
                        image_path_NULL[[image_type]])[[image_NULL_i]]]], "\n")
                }
            }

            img_path[[image_type]] <- img_path[[image_type
                                            ]][!(image_path_NULL[[image_type]])]
            name_list[[image_type]] <- name_list[[image_type
                                            ]][!(image_path_NULL[[image_type]])]
            img_list[[image_type]] <- img_list[[image_type
                                            ]][!(image_path_NULL[[image_type]])]
        }


        # Load pointers
        img_list[[image_type]] <- lapply(
            X = seq_len(length(img_list[[image_type]])),
            function(x) {
                reconnect(
                    x = img_list[[image_type]][[x]],
                    path = img_path[[image_type]][[x]]
                )
            }
        )


        # 4. Update gobject:---------------------------------------------------#

        # Set the image objects into the gobject
        for (image_ii in seq_len(length(img_list[[image_type]]))) {
            gobject <- set_giottoImage(
                gobject = gobject,
                image = img_list[[image_type]][[image_ii]],
                image_type = image_type,
                name = name_list[[image_type]][[image_ii]],
                verbose = FALSE
            )
        }

        if (verbose == TRUE) cat("done \n")
    } # image_type end loop
    return(gobject)
}



#' @title Plot distribution of image intensity values
#' @name distGiottoImage
#' @description Plot distribution of intensity values using either a density
#' plot or a histogram. Useful for finding image artefact outliers and
#' determining reasonable scaling cutoffs.
#' @section Plotting method \code{'dens'}:
#'   Density plot of intensity values for image objects. \strong{N} total values
#'   examined. \strong{Bandwidth} refers to the curve smoothing value applied.
#' @section Plotting method \code{'hist'}:
#'   Histogram of intensity values for image objects.
#' @details Plot is generated from a downsampling of the original image
#' @param gobject giotto object
#' @param image_type image object
#' type (only supports largeImage and is set as default)
#' @param image_name name of image object to use
#' @param giottoLargeImage giotto large image object
#' @param method plot type to show image intensity distribution
#' @param show_max logical. Plot the set max intensity as a vertical red line
#' @param ... additional params to pass to [terra::hist()] or [terra::density()]
#' @returns density or histogram plot
#' @export
distGiottoImage <- function(
        gobject = NULL,
        image_type = "largeImage",
        image_name = NULL,
        giottoLargeImage = NULL,
        method = c("dens", "hist"),
        show_max = TRUE,
        ...) {
    # check params
    if (image_type != "largeImage")
        stop("Only largeImage objects currently supported \n")
    if ((is.null(image_type) | is.null(image_name) |
        is.null(gobject)) & (is.null(giottoLargeImage))) {
        stop("Image must be given through gobject, image_type, and image_name
            params or as the image object itself \n")
    }
    method <- match.arg(arg = method, choices = c("dens", "hist"))

    a = list(gobject = gobject, image_name = image_name,
             giottoLargeImage = giottoLargeImage, method = method,
             show_max = show_max, ...)

    # run specific function
    if (image_type == "largeImage") {
        do.call(.dist_giottolargeimage, args = a)
    }
}



#' @name density
#' @title Density plot
#' @description
#' Create density plots of the pixel values of a giottoLargeImage. Wrapper
#' around [terra::density()].
#' @param x giottoLargeImage
#' @param show_max logical. Plot the set max intensity as a vertical red line
#' @inheritDotParams terra::density
#' @seealso [hist()]
#' @examples
#' f <- system.file(package = "GiottoClass", "extdata/toy_intensity.tif")
#' gimg <- createGiottoLargeImage(f, use_rast_ext = TRUE)
#' density(gimg)
#' @export
setMethod(
    "density", signature("giottoLargeImage"),
    function(x, show_max = TRUE, ...) {
        a <- list(x = x@raster_object, ...)
        res <- do.call(terra::density, args = a)

        if (isFALSE(a$plot)) {
            return(res)
        }

        if (isTRUE(show_max)) {
            graphics::abline(v = x@max_window, col = "red")
        }
    })


#' @name hist
#' @title Histogram
#' @description
#' Create a histogram of the pixel values of a giottoLargeImage. Wrapper around
#' [terra::hist()]
#' @param x giottoLargeImage
#' @param show_max logical. Plot the set max intensity as a vertical red line
#' @inheritDotParams terra::hist
#' @seealso [density()]
#' @examples
#' f <- system.file(package = "GiottoClass", "extdata/toy_intensity.tif")
#' gimg <- createGiottoLargeImage(f, use_rast_ext = TRUE, verbose = FALSE)
#' hist(gimg)
#' @export
setMethod(
    "hist", signature("giottoLargeImage"),
    function(x, show_max = TRUE, ...) {
        a <- list(x = x@raster_object, ...)
        res <- do.call(terra::hist, args = a)

        if (isFALSE(a$plot)) {
            return(res)
        }

        if (isTRUE(show_max)) {
            graphics::abline(v = x@max_window, col = "red")
        }
    }
)



#' @title Add alpha channel to image array
#' @name add_img_array_alpha
#' @details Add 4th alpha channel to 3 channel RGB image arrays
#' @param x image array to use
#' @param alpha global alpha value to use. Numeric. Scales from 0 to 1, with 0
#' being fully transparent and 1 being fully visible
#' @returns image array with 4th channel for transparency
#' @export
add_img_array_alpha <- function(x,
    alpha) {
    img_dims <- dim(x)
    x_alpha <- array(data = alpha, dim = c(img_dims[1], img_dims[2], 4))
    x_alpha[, , seq_len(3)] <- x
    return(x_alpha)
}




# converters ####


#' @title Convert ome.tif to tif
#' @name ometif_to_tif
#' @description
#' Simple converter from .ome.tif to .tif format. Utilizes the python
#' \pkg{tifffile} package.
#' @param input_file character. Filepath to ome.tif to convert
#' @param output_file character. Filepath to write .tif to
#' @param overwrite logical. Default = FALSE. Whether to overwrite if the
#' filename already exists.
#' @returns returns NULL invisibly
#' @export
ometif_to_tif <- function(input_file, output_file, overwrite = FALSE) {
    a <- get_args_list()

    package_check("tifffile", repository = "pip:tifffile")
    ometif2tif_path <- system.file("python", "ometif_convert.py",
                                   package = "GiottoClass")
    reticulate::source_python(ometif2tif_path)
    do.call(ometif_2_tif, args = a)
    invisible(NULL)
}





