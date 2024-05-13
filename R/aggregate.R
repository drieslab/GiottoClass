# collate
#' @include generics.R
NULL

# aggregate expression ####

# Aggregate features based on morphological annotations in order to generate a
# biological spatial unit to study.
# 1. calculate overlap
#   - Finds the features which are overlapped by the morphological annotations
#     represented by a `giottoPolygon` object.
#     This information is then stored within the `giottoPolygon`'s overlaps slot
#
# 2. overlap to matrix
#   - Tabulates the overlapped feature information by cell and then sends it to
#     the expression slot as a new expression matrix.


## calculate overlap between cellular structures and features ####

### ** raster way ####

#' @title Convert polygon to raster
#' @name polygon_to_raster
#' @param polygon SpatVector polygon to rasterize
#' @param field character. Name of attribute of polygon that should be used
#' when rasterizing to assign values. Passing NULL uses the first attribute.
#' @description  convert polygon to raster
#' @returns list
#' @examples
#' df <- data.frame(x = 1:5, y = c(1, 4, 4, 3, 1))
#' d <- data.frame(id = 1, name = "polygon_1")
#' my_polygon <- terra::vect(as.matrix(df), type = "polygons", atts = d)
#'
#' polygon_to_raster(my_polygon)
#' @export
polygon_to_raster <- function(polygon, field = NULL) {
    pol_xmax <- terra::xmax(polygon)
    pol_xmin <- terra::xmin(polygon)
    ncols <- abs(pol_xmax - pol_xmin)

    pol_ymax <- terra::ymax(polygon)
    pol_ymin <- terra::ymin(polygon)
    nrows <- abs(pol_ymax - pol_ymin)

    r <- terra::rast(polygon, ncols = ncols, nrows = nrows)

    if (is.null(field)) {
        field <- names(polygon)[1]
    }

    # ensure that field is numerical
    polygon$poly_i <- seq_len(nrow(unique(polygon[[field]])))
    poly_rast <- terra::rasterize(x = polygon, r, field = "poly_i")

    poly_ID_vector <- polygon[[field]][, 1]
    names(poly_ID_vector) <- polygon[["poly_i"]][, 1]

    return(list("raster" = poly_rast, "ID_vector" = poly_ID_vector))
}


# calculateOverlap methods ####


#' @title Calculate features overlapped by polygons
#' @name calculateOverlap
#' @description
#' Calculate subcellular points/feature info or image values overlapped by
#' polygon annotations. This provides a summary of the spatial data overlapped
#' by the polygon which can be further processed to become an expression matrix.
#' @param x Object with spatial annotations: `giottoPolygon`, or `SpatVector`
#' polygons. Can also be a `giotto` object
#' @param y Object with features to overlap: `giottoPoints`, `giottoLargeImage`,
#' `SpatVector` points or `SpatRaster`
#' @param poly_subset_ids character vector. (optional) Specific poly_IDs to use
#' @param feat_subset_column character. (optional) feature info attribute to
#' subset feature points on when performing overlap calculation.
#' @param feat_subset_ids (optional) values matched against
#' in `feat_subset_column` in order to subset feature points when performing
#' overlap calculation.
#' @param count_info_column character. (optional) column with count information.
#' Useful in cases when more than one detection is reported per point.
#' @param verbose be verbose
#' @param \dots additional params to pass to methods.
#' @details `feat_subset_column`, `feat_subset_ids`, and `count_info_column` are
#' specific to overlaps on feature points info, and should not be provided
#' when overlapping image data. These three params can also be passed to the
#' `giotto` method through the `...` param when working with overlaps on feature
#' points info.
#' @returns Usually an object of the same class as `x`, with the overlaps
#' information appended. `return_*` logical params usually allow return of
#' a lower-level representation of the results instead. Only the
#' `SpatVector,SpatRaster` method is different in that it returns a `data.table`
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#' gpoly <- getPolygonInfo(g,
#'     polygon_name = "aggregate",
#'     return_giottoPolygon = TRUE
#' )
#' gpoints <- getFeatureInfo(g, return_giottoPoints = TRUE)
#' gimg <- getGiottoImage(g, image_type = "largeImage")
#'
#' slot(gpoly, "overlaps") <- NULL
#' overlaps(gpoly) # Should now be NULL
#'
#' # detections from 2 z-layers are provided
#' table(gpoints$global_z)
#'
#' # calculate all transcripts overlapped
#' out_all <- calculateOverlap(gpoly, gpoints)
#' overlaps_all <- overlaps(out_all)
#' overlaps_all$rna
#'
#' # calculate z1 only
#' out_z1 <- calculateOverlap(gpoly, gpoints,
#'     feat_subset_column = "global_z",
#'     feat_subset_ids = c(1)
#' )
#' overlaps_z1 <- overlaps(out_z1)
#' overlaps_z1$rna
#'
#' # overlap image to get sum intensities per cell
#' out_img <- calculateOverlap(gpoly, gimg)
#' overlaps_img <- overlaps(out_img)
#' overlaps_img$intensity
#'
#' # giotto method
#' # calculate z0 overlaps and return as gobject
#' out_g <- calculateOverlap(g,
#'     feat_subset_column = "global_z",
#'     feat_subset_ids = 0
#' )
#' overlaps(getPolygonInfo(out_g, return_giottoPolygon = TRUE))
#'
#' # note that z0 and z1 nrows match that from the table of global z values.
#' # With points overlaps, all points are returned, but non-overlapped points
#' # only have an `NA` value for the `poly_ID` column. Overlapped points will
#' # have the `poly_ID` of their overlapping polygon.
NULL

# * gobject ####
#' @rdname calculateOverlap
#' @param spatial_info character. Name polygon information
#' @param feat_info character. Name of vector feature information to overlap
#' @param image_names character vector. Name(s) of the image feature information
#' to overlap
#' @param return_gobject return giotto object (default: TRUE)
#' @export
setMethod(
    "calculateOverlap", signature(x = "giotto", y = "missing"),
    function(x,
    name_overlap = NULL,
    spatial_info = NULL,
    feat_info = NULL,
    image_names = NULL,
    poly_subset_ids = NULL,
    return_gobject = TRUE,
    verbose = TRUE,
    ...) {
        # 0. guards #
        # --------- #

        if (!is.null(feat_info)) checkmate::assert_character(feat_info)
        if (!is.null(image_names)) checkmate::assert_character(image_names)
        if (!is.null(feat_info) && !is.null(image_names)) {
            stop(wrap_txt(
                "Please calculate overlaps for vector (feat_info) and intensity
                (image_names) data separately",
                errWidth = TRUE
            ))
        }

        # 1. set defaults if not provided #
        # ------------------------------- #

        # If no neither vector nor intensity type data to overlap is selected,
        # try defaulting to first available vector set of features.
        # If there are none in the object, check for intensity data to overlap,
        # then throw the corresponding error.
        #
        # Intensity data is not defaulted to, since they can either be truely
        # useful intensity data such as IF information, but they could also be
        # things like brightfield or H&E images which make less sense to overlap
        if (is.null(feat_info) && is.null(image_names)) {
            feat_info <- names(x@feat_info)[[1L]]

            if (is.null(feat_info)) {
                if (length(list_images_names(x, img_type = "largeImage")) == 0) {
                    stop(wrap_txt(
                        "No vector or intensity feature data discovered in
                        giotto object. Please first attach a set of feature
                        data to overlap.",
                        errWidth = TRUE
                    ))
                } else {
                    stop(wrap_txt(
                        "No vector feature data discovered in giotto object,
                        but image data
            does exist.
            [image_names = NULL]
            If intensity feature data is intended to be overlapped, the names
            of the images to use must be provided to 'image_names' param.
            - See showGiottoImageNames() for attached images",
                        errWidth = TRUE
                    ))
                }
            }
        }

        if (is.null(name_overlap)) {
            if (!is.null(feat_info)) name_overlap <- feat_info
            if (!is.null(image_names)) name_overlap <- "protein"
        }

        if (is.null(spatial_info)) {
            spatial_info <- names(x@spatial_info)[[1]]
        }


        # 2. get information from gobject #
        # ------------------------------- #

        #   ---[polys to overlap with]---
        A <- getPolygonInfo(
            gobject = x,
            polygon_name = spatial_info,
            return_giottoPolygon = TRUE
        )

        #     ---[data to overlap]---
        if (!is.null(feat_info)) {
            B <- getFeatureInfo(
                gobject = x,
                feat_type = feat_info,
                return_giottoPoints = TRUE,
                set_defaults = FALSE
            )
        } else if (!is.null(image_names)) {
            # check for wrong input names
            potential_large_image_names <- list_images_names(x,
                img_type = "largeImage"
            )
            for (img_name in image_names) {
                if (!img_name %in% potential_large_image_names) {
                    warning(
                        "image with the name ", img_name,
                        " was not found and will be skipped \n"
                    )
                }
            }
            image_names <- image_names[image_names %in%
                potential_large_image_names]

            image_list <- lapply(image_names, function(i_n) {
                img <- getGiottoImage(
                    gobject = x,
                    name = i_n,
                    image_type = "largeImage"
                )
                spatrast <- img@raster_object
                names(spatrast) <- i_n # ensure name is applied
                return(spatrast)
            })

            B <- do.call("c", image_list)
        } else {
            .gstop(
                "No feat_info or image_names provided. No data to overlap.",
                .n = 2L
            )
        }



        # 3. run overlap workflow (pass to methods) #
        # ----------------------------------------- #

        overlap_args_list <- list(
            x = A,
            y = B, # points or c(images)
            name_overlap = name_overlap,
            poly_subset_ids = poly_subset_ids,
            verbose = verbose,
            return_gpolygon = isTRUE(return_gobject),
            ...
        )

        overlap_result <- do.call(calculateOverlap,
            args = overlap_args_list
        )

        # 4. return values #
        # ---------------- #

        if (isTRUE(return_gobject)) {
            # expect that overlap_result is a giottoPolygon with attached
            # overlaps information
            x <- setPolygonInfo(
                gobject = x,
                x = overlap_result,
                verbose = FALSE
            )
            return(x)
        } else {
            # expect overlaps information
            return(overlap_result)
        }
    }
)

# * gpoly gpoints ####
#' @rdname calculateOverlap
#' @param name_overlap name for the overlap
#' results (default to feat_info parameter)
#' @param return_gpolygon default = TRUE. Whether to return the entire
#' giottoPolygon provided to `x`, but with the overlaps information appended or
#' as a bare terra `SpatVector`
#' @export
setMethod(
    "calculateOverlap", signature(x = "giottoPolygon", y = "giottoPoints"),
    function(x, y,
    name_overlap = NULL,
    poly_subset_ids = NULL,
    feat_subset_column = NULL,
    feat_subset_ids = NULL,
    count_info_column = NULL,
    return_gpolygon = TRUE,
    verbose = TRUE,
    ...) {
        res <- calculateOverlap(
            x = x[],
            y = y[],
            poly_subset_ids = poly_subset_ids,
            feat_subset_column = feat_subset_column,
            feat_subset_ids = feat_subset_ids,
            count_info_column = count_info_column,
            verbose = verbose,
            ...
        )

        if (isTRUE(return_gpolygon)) {
            if (is.null(name_overlap)) name_overlap <- objName(y)

            # ensure centroids calculated
            if (is.null(centroids(x))) {
                x <- centroids(x, append_gpolygon = TRUE)
            }

            x@overlaps[[name_overlap]] <- res
            return(x)
        } else {
            return(res)
        }
    }
)


# * giottoPolygon giottoLargeImage ####
#' @rdname calculateOverlap
#' @export
setMethod(
    "calculateOverlap", signature(x = "giottoPolygon", y = "giottoLargeImage"),
    function(
        x, y,
        name_overlap = NULL,
        poly_subset_ids = NULL,
        return_gpolygon = TRUE,
        verbose = TRUE,
        ...) {
        calculateOverlap(
            x = x,
            y = y@raster_object,
            name_overlap = objName(y),
            poly_subset_ids = poly_subset_ids,
            verbose = verbose,
            ...
        )
    }
)


# * giottoPolygon SpatRaster ####
#' @rdname calculateOverlap
#' @export
setMethod(
    "calculateOverlap", signature(x = "giottoPolygon", y = "SpatRaster"),
    function(x, y,
    name_overlap = NULL,
    poly_subset_ids = NULL,
    return_gpolygon = TRUE,
    verbose = TRUE,
    ...) {
        if (is.null(name_overlap)) {
            .gstop("calculateOverlap: name_overlap must be given")
        }

        res <- calculateOverlap(
            x = x[],
            y = y,
            poly_subset_ids = poly_subset_ids,
            verbose = verbose,
            ...
        )

        if (isTRUE(return_gpolygon)) {
            # ensure centroids calculated
            if (is.null(centroids(x))) {
                x <- centroids(x, append_gpolygon = TRUE)
            }

            x@overlaps[["intensity"]][[name_overlap]] <- res
            return(x)
        } else {
            return(res)
        }
    }
)


# subsetting operations take place at the bottommost method since it may be
# easier to set those up in code that is closer to the data representation
# level, depending on how the data representation is designed.

# SpatRaster supplied to param y is expected to contain one or more layers of
# raster data

# * SpatVector SpatRaster ####
#' @rdname calculateOverlap
#' @export
setMethod(
    "calculateOverlap", signature(x = "SpatVector", y = "SpatRaster"),
    function(x, y,
    poly_subset_ids = NULL,
    verbose = TRUE,
    ...) {
        checkmate::assert_true(terra::is.polygons(x))
        GiottoUtils::package_check("exactextractr")

        image_names <- names(y)

        # NSE vars
        coverage_fraction <- NULL

        # subset polys if needed
        if (!is.null(poly_subset_ids)) {
            x <- x[x$poly_ID %in% poly_subset_ids]
        }

        # convert polys to sf
        sf_x <- as.sf(x)

        vmsg(.v = verbose, "Start image extract")

        # perform extraction, producing list of results
        extract_res <- exactextractr::exact_extract(
            x = y,
            y = sf_x,
            include_cols = "poly_ID",
            ...
        )

        vmsg(.v = verbose, "End image extract")

        # rbind and convert output to data.table
        dt_exact <- data.table::as.data.table(do.call("rbind", extract_res))

        # prepare output
        colnames(dt_exact)[2:(length(image_names) + 1)] <- image_names
        dt_exact[, coverage_fraction := NULL]

        return(dt_exact)
    }
)


# * SpatVector SpatVector ####
#' @rdname calculateOverlap
#' @export
setMethod(
    "calculateOverlap", signature(x = "SpatVector", y = "SpatVector"),
    function(x, y,
    poly_subset_ids = NULL,
    feat_subset_column = NULL,
    feat_subset_ids = NULL,
    count_info_column = NULL,
    verbose = TRUE) {
        checkmate::assert_true(terra::is.polygons(x))
        checkmate::assert_true(terra::is.points(y)) # TODO allow another poly?
        if (!is.null(poly_subset_ids)) {
            checkmate::assert_character(poly_subset_ids)
        }

        # subset points and polys if needed
        # * subset x
        if (!is.null(poly_subset_ids)) {
            x <- x[x$poly_ID %in% poly_subset_ids]
        }

        # * subset points if needed
        # e.g. to select transcripts within a z-plane
        if (!is.null(feat_subset_column) && !is.null(feat_subset_ids)) {
            bool_vector <- y[[feat_subset_column]][[1]] %in% feat_subset_ids
            y <- y[bool_vector]
        }

        .calculate_overlap_raster(
            spatvec = x,
            pointvec = y,
            count_info_column = count_info_column,
            verbose = verbose
        )
    }
)






#' @title calculateOverlapRaster
#' @name calculateOverlapRaster
#' @description calculate overlap between cellular structures (polygons) and
#' features (points).
#' @param gobject giotto object
#' @param name_overlap name for the overlap
#' results (default to feat_info parameter)
#' @param spatial_info character. name polygon information
#' @param poly_ID_names (optional) list of poly_IDs to use
#' @param feat_info character. name of feature information
#' @param feat_subset_column feature info column to subset features with
#' @param feat_subset_ids ids within feature info column to use for subsetting
#' @param count_info_column column with count information (optional)
#' @param return_gobject return giotto object (default: TRUE)
#' @param verbose be verbose
#' @returns giotto object or spatVector with overlapping information
#' @details Serial overlapping function.
#' @concept overlap
#' @seealso [.calculate_overlap_raster()]
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' calculateOverlapRaster(g)
#' @export
calculateOverlapRaster <- function(
        gobject,
        name_overlap = NULL,
        spatial_info = NULL,
        poly_ID_names = NULL,
        feat_info = NULL,
        feat_subset_column = NULL,
        feat_subset_ids = NULL,
        count_info_column = NULL,
        return_gobject = TRUE,
        verbose = TRUE) {
    # set defaults if not provided
    if (is.null(feat_info)) {
        feat_info <- names(gobject@feat_info)[[1]]
    }

    if (is.null(name_overlap)) {
        name_overlap <- feat_info
    }

    if (is.null(spatial_info)) {
        spatial_info <- names(gobject@spatial_info)[[1]]
    }


    # get information from gobject
    # * spatial vector
    spatvec <- getPolygonInfo(
        gobject = gobject,
        polygon_name = spatial_info,
        return_giottoPolygon = FALSE
    )

    # * point vector
    pointvec <- getFeatureInfo(
        gobject = gobject,
        feat_type = feat_info,
        return_giottoPoints = FALSE,
        set_defaults = FALSE
    )


    # subset points and polys if needed
    # * subset spatvec
    if (!is.null(poly_ID_names)) {
        spatvec <- spatvec[spatvec$poly_ID %in% poly_ID_names]
    }

    # * subset points if needed
    # e.g. to select transcripts within a z-plane
    if (!is.null(feat_subset_column) & !is.null(feat_subset_ids)) {
        bool_vector <- pointvec[[feat_subset_column]][[1]] %in% feat_subset_ids
        pointvec <- pointvec[bool_vector]
    }

    # run overlap workflow
    overlap_points <- .calculate_overlap_raster(
        spatvec = spatvec,
        pointvec = pointvec,
        count_info_column = count_info_column,
        verbose = verbose
    )

    # return values
    if (isTRUE(return_gobject)) {
        if (is.null(name_overlap)) name_overlap <- feat_info
        gobject@spatial_info[[spatial_info]]@overlaps[[name_overlap]] <-
            overlap_points
        return(gobject)
    } else {
        return(overlap_points)
    }
}



#' @name .calculate_overlap_raster
#' @title Find feature points overlapped by rasterized polygon.
#' @description Core workflow function that accepts simple `SpatVector` inputs,
#' performs rasterization of the polys and then checks for overlaps.
#' @param spatvec `SpatVector` polygon from a `giottoPolygon` object
#' @param pointvec `SpatVector` points from a `giottoPoints` object
#' @param count_info_column column with count information (optional)
#' @param verbose be verbose
#' @concept overlap
#' @returns `SpatVector` of overlapped points info
#' @seealso [calculateOverlapRaster()]
#' @keywords internal
.calculate_overlap_raster <- function(spatvec,
    pointvec,
    count_info_column = NULL,
    verbose = TRUE) {
    # DT vars
    poly_ID <- poly_i <- ID <- x <- y <- feat_ID <- feat_ID_uniq <- NULL

    # spatial vector to raster
    if (verbose) GiottoUtils::wrap_msg("1. convert polygon to raster \n")
    spatrast_res <- polygon_to_raster(spatvec, field = "poly_ID")
    spatrast <- spatrast_res[["raster"]]
    ID_vector <- spatrast_res[["ID_vector"]]

    ## overlap between raster and point
    if (verbose) GiottoUtils::wrap_msg("2. overlap raster and points \n")
    overlap_test <- terra::extract(x = spatrast, y = pointvec)

    # add poly_ID information
    if (verbose) GiottoUtils::wrap_msg("3. add polygon information \n")
    overlap_test_dt <- data.table::as.data.table(overlap_test)
    overlap_test_dt[, poly_ID := ID_vector[poly_i]]

    # add point information
    if (verbose) GiottoUtils::wrap_msg("4. add points information \n")
    pointvec_dt <- data.table::as.data.table(pointvec, geom = "XY")

    pointvec_dt_x <- pointvec_dt$x
    names(pointvec_dt_x) <- pointvec_dt$geom
    pointvec_dt_y <- pointvec_dt$y
    names(pointvec_dt_y) <- pointvec_dt$geom
    pointvec_dt_feat_ID <- pointvec_dt$feat_ID
    names(pointvec_dt_feat_ID) <- pointvec_dt$geom
    pointvec_dt_feat_ID_uniq <- pointvec_dt$feat_ID_uniq
    names(pointvec_dt_feat_ID_uniq) <- pointvec_dt$geom

    overlap_test_dt[, x := pointvec_dt_x[ID]]
    overlap_test_dt[, y := pointvec_dt_y[ID]]
    overlap_test_dt[, feat_ID := pointvec_dt_feat_ID[ID]]
    overlap_test_dt[, feat_ID_uniq := pointvec_dt_feat_ID_uniq[ID]]

    if (!is.null(count_info_column)) {
        pointvec_dt_count <- pointvec_dt[[count_info_column]]
        names(pointvec_dt_count) <- pointvec_dt$geom
        overlap_test_dt[, c(count_info_column) := pointvec_dt_count[ID]]
    }

    if (verbose) GiottoUtils::wrap_msg("5. create overlap polygon
                                    information \n")
    overlap_test_dt_spatvector <- terra::vect(
        x = as.matrix(overlap_test_dt[, c("x", "y"), with = FALSE]),
        type = "points",
        atts = overlap_test_dt[, c(
            "poly_ID", "feat_ID", "feat_ID_uniq",
            count_info_column
        ), with = FALSE]
    )
    names(overlap_test_dt_spatvector) <- c(
        "poly_ID", "feat_ID", "feat_ID_uniq", count_info_column
    )
    return(overlap_test_dt_spatvector)
}



#' @title Overlap points -- single polygon
#' @name .overlap_points_single_polygon
#' @description  overlap for a single polygon
#' @returns terra::intersect
#' @keywords internal
.overlap_points_single_polygon <- function(spatvec,
    poly_ID_name,
    pointvec_dt) {
    # define for data.table
    x <- y <- NULL

    ## extract single polygon and get spatextent
    one_polygon_spatvector <- spatvec[spatvec$poly_ID == poly_ID_name]
    ext_limits <- terra::ext(one_polygon_spatvector)

    ## extract potential features (points) based on limits
    one_polygon_pointvec_dt <- pointvec_dt[x > terra::xmin(ext_limits) &
        x < terra::xmax(ext_limits)][
        y > terra::ymin(ext_limits) & y < terra::ymax(ext_limits)
    ]
    one_polygon_pointsvec <- .dt_to_spatvector_points(one_polygon_pointvec_dt)

    ## calculate intersection between single polygon and points
    one_polygon_overlap <- terra::intersect(
        x = one_polygon_spatvector,
        y = one_polygon_pointsvec
    )

    if (nrow(one_polygon_overlap) > 0) {
        return(one_polygon_overlap)
    } else {
        return(NULL)
    }
}





#' @title calculateOverlapPolygonImages
#' @name calculateOverlapPolygonImages
#' @description calculate overlap between cellular structures (polygons)
#' and images (intensities)
#' @param gobject giotto object
#' @param name_overlap name for the overlap
#' results (default to feat_info parameter)
#' @param spatial_info polygon information
#' @param poly_ID_names (optional) list of poly_IDs to use
#' @param image_names names of the images with raw data
#' @param poly_subset numerical values to subset the polygon spatVector
#' @param return_gobject return giotto object (default: TRUE)
#' @param verbose be verbose
#' @param ... additional params to \code{\link[exactextractr]{exact_extract}}
#' @concept overlap
#' @returns giotto object or data.table with overlapping information
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' calculateOverlapPolygonImages(g,
#'     spatial_info = "z0",
#'     image_names = "dapi_z0"
#' )
#' @export
calculateOverlapPolygonImages <- function(gobject,
    name_overlap = "protein",
    spatial_info = "cell",
    poly_ID_names = NULL,
    image_names = NULL,
    poly_subset = NULL,
    return_gobject = TRUE,
    verbose = TRUE,
    ...) {
    # TODO consider deprecating poly_ID_names (it does nothing here.)
    # poly_subset is being used instead

    # data.table vars
    coverage_fraction <- NULL

    if (is.null(image_names)) {
        stop("image_names = NULL, you need to provide the names of the
            images you want to use, see showGiottoImageNames() for
            attached images")
    }

    # need for package exactextractr for fast overlap calculations
    package_check("exactextractr")

    ## get polygon information
    poly_info <- getPolygonInfo(
        gobject = gobject,
        polygon_name = spatial_info,
        return_giottoPolygon = TRUE
    )


    # calculate centroids for poly_info if not present
    if (is.null(poly_info@spatVectorCentroids)) {
        poly_info <- .calculate_centroids_polygons(
            gpolygon = poly_info,
            name = "centroids",
            append_gpolygon = TRUE
        )
    }


    potential_large_image_names <- list_images_names(gobject,
        img_type = "largeImage"
    )

    # check for wrong input names
    for (img_name in image_names) {
        if (!img_name %in% potential_large_image_names) {
            warning(
                "image with the name ", img_name,
                " was not found and will be skipped \n"
            )
        }
    }
    image_names <- image_names[image_names %in% potential_large_image_names]


    image_list <- list()

    for (i in seq_len(length(image_names))) {
        img_name <- image_names[i]

        if (!img_name %in% potential_large_image_names) {
            warning(
                "image with the name ", img_name,
                " was not found and will be skipped \n"
            )
        }

        intensity_image <- get_giottoLargeImage(
            gobject = gobject,
            name = img_name
        )
        intensity_image <- intensity_image@raster_object

        names(intensity_image) <- img_name

        image_list[[i]] <- intensity_image
    }

    if (verbose) print("0. create image list")

    image_vector_c <- do.call("c", image_list)

    # convert spatVector to sf object
    if (!is.null(poly_subset)) {
        poly_info_spatvector_sf <- as.sf(poly_info@spatVector[poly_subset])
    } else {
        poly_info_spatvector_sf <- as.sf(poly_info)
    }


    if (verbose) print("1. start extraction")

    extract_intensities_exact <- exactextractr::exact_extract(
        x = image_vector_c,
        y = poly_info_spatvector_sf,
        include_cols = "poly_ID",
        ...
    )

    # rbind and convert output to data.table
    dt_exact <- data.table::as.data.table(
        do.call("rbind", extract_intensities_exact)
    )

    # prepare output
    if (verbose) print(dt_exact)
    colnames(dt_exact)[2:(length(image_names) + 1)] <- image_names # probably not needed anymore
    dt_exact[, coverage_fraction := NULL]
    if (verbose) print(dt_exact)

    if (return_gobject) {
        poly_info@overlaps[["intensity"]][[name_overlap]] <- dt_exact
        gobject <- set_polygon_info(
            gobject = gobject,
            polygon_name = spatial_info,
            gpolygon = poly_info
        )
        return(gobject)
    } else {
        return(dt_exact)
    }
}









### ** polygon way ####


#' @title Overlap points per polgyon
#' @name .overlap_points_per_polygon
#' @description Loop to overlap each single polygon
#' @keywords internal
#' @returns spatVector
#' @seealso \code{\link{.overlap_points_single_polygon}}
.overlap_points_per_polygon <- function(spatvec,
    pointvec,
    poly_ID_names,
    verbose = TRUE) {
    # spatial polygon
    spatvec <- spatvec[terra::is.valid(spatvec)]

    # points polygon
    pointvec_dt <- .spatvector_to_dt(pointvec)

    # get polygon names
    unique_cell_names <- unique(spatvec$poly_ID)
    poly_ID_names <- poly_ID_names[poly_ID_names %in% unique_cell_names]


    # final_vect = terra::vect()
    final_list <- list()
    i <- 1
    for (poly_ID_name in poly_ID_names) {
        if (verbose == TRUE) print(poly_ID_name)

        result <- .overlap_points_single_polygon(
            spatvec = spatvec,
            poly_ID_name = poly_ID_name,
            pointvec_dt = pointvec_dt
        )

        if (!is.null(result)) {
            final_list[[i]] <- result
            i <- i + 1
            # final_vect = rbind(final_vect, result)
        }
    }

    final_vect <- do.call("rbind", final_list)

    return(final_vect)
}



#' @title calculateOverlapSerial
#' @name calculateOverlapSerial
#' @description calculate overlap between cellular
#' structures (polygons) and features (points)
#' @param gobject giotto object
#' @param name_overlap name for the overlap
#' results (default to feat_info parameter)
#' @param spatial_info polygon information
#' @param feat_info feature information
#' @param poly_ID_names list of poly_IDs to use
#' @param polygon_group_size number of polygons to process per group
#' @param return_gobject return giotto object (default: TRUE)
#' @param verbose be verbose
#' @returns giotto object or spatVector with overlapping information
#' @details Serial overlapping function that works on groups of polygons
#' at a time. Number of polygons per group is defined
#' by \code{polygon_group_size} param
#' @concept overlap
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' calculateOverlapSerial(g, spatial_info = "z1")
#' @export
calculateOverlapSerial <- function(gobject,
    name_overlap = NULL,
    spatial_info = "cell",
    feat_info = "rna",
    poly_ID_names = "all",
    polygon_group_size = 500,
    return_gobject = TRUE,
    verbose = FALSE) {
    # spatial polygon
    spatvec <- gobject@spatial_info[[spatial_info]]@spatVector

    # points polygon
    pointvec <- gobject@feat_info[[feat_info]]@spatVector

    if (length(poly_ID_names) == 1) {
        if (poly_ID_names == "all") {
            poly_ID_names <- unique(spatvec$poly_ID)
        }
    }


    total_polygons <- length(poly_ID_names)
    total_nr_groups <- ceiling(total_polygons / polygon_group_size)
    groupnames <- cut(seq_len(total_polygons),
        breaks = total_nr_groups,
        labels = seq_len(total_nr_groups)
    )
    names(poly_ID_names) <- groupnames


    final_result <- list()
    for (i in seq_len(total_nr_groups)) {
        if (verbose) print((total_nr_groups - i))

        selected_poly_ID_names <- poly_ID_names[names(poly_ID_names) == i]
        selected_spatvec <- spatvec[spatvec$poly_ID %in% selected_poly_ID_names]



        spatvec_result <- .overlap_points_per_polygon(
            spatvec = selected_spatvec,
            pointvec = pointvec,
            poly_ID_names = selected_poly_ID_names,
            verbose = verbose
        )

        final_result[[i]] <- spatvec_result
    }

    final_result <- do.call("rbind", final_result)


    if (return_gobject == TRUE) {
        if (is.null(name_overlap)) {
            name_overlap <- feat_info
        }

        gobject@spatial_info[[spatial_info]]@overlaps[[name_overlap]] <-
            final_result
        return(gobject)
    } else {
        return(final_result)
    }
}



#' @title Overlap points per polygon -- wrapped
#' @name .overlap_points_per_polygon_wrapped
#' @description overlap wrapped polygons
#' @returns Packed object
#' @keywords internal
.overlap_points_per_polygon_wrapped <- function(spatvec_wrapped,
    pointvec_wrapped,
    poly_ID_names) {
    unwrap_spatvec <- terra::vect(spatvec_wrapped)
    unwrap_pointvec <- terra::vect(pointvec_wrapped)

    if (length(poly_ID_names) == 1) {
        if (poly_ID_names == "all") {
            poly_ID_names <- unique(unwrap_spatvec$poly_ID)
        }
    }

    intersect_res <- .overlap_points_per_polygon(
        spatvec = unwrap_spatvec,
        pointvec = unwrap_pointvec,
        poly_ID_names = poly_ID_names,
        verbose = FALSE
    )

    return(terra::wrap(intersect_res))
}



#' @title calculateOverlapParallel
#' @name calculateOverlapParallel
#' @description calculate overlap between cellular
#' structures (polygons) and features (points)
#' @param gobject giotto object
#' @param name_overlap name for the overlap
#' results (default to feat_info parameter)
#' @param spatial_info polygon information
#' @param feat_info feature information
#' @param poly_ID_names list of poly_IDs to use
#' @param polygon_group_size number of polygons to process per
#' parallelization group
#' @param return_gobject return giotto object (default: TRUE)
#' @param verbose be verbose
#' @returns giotto object or spatVector with overlapping information
#' @details parallel follows the future approach. This means that
#' plan(multisession) does not work,
#' since the underlying terra objects are internal C pointers.
#' plan(multicore) is also not supported for
#' Rstudio users.
#' @concept overlap
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' calculateOverlapParallel(g, spatial_info = "z1")
#' @export
calculateOverlapParallel <- function(gobject,
    name_overlap = NULL,
    spatial_info = "cell",
    feat_info = "rna",
    poly_ID_names = "all",
    polygon_group_size = 500,
    return_gobject = TRUE,
    verbose = TRUE) {
    # spatial polygon
    spatvec <- gobject@spatial_info[[spatial_info]]@spatVector

    # points polygon
    pointvec <- gobject@feat_info[[feat_info]]@spatVector


    if (length(poly_ID_names) == 1) {
        if (poly_ID_names == "all") {
            poly_ID_names <- unique(spatvec$poly_ID)
        }
    }

    total_polygons <- length(poly_ID_names)
    total_nr_groups <- ceiling(total_polygons / polygon_group_size)
    groupnames <- cut(seq_len(total_polygons),
        breaks = total_nr_groups,
        labels = seq_len(total_nr_groups)
    )
    names(poly_ID_names) <- groupnames

    # wrap SpatVector for points
    pointvec_wrap <- terra::wrap(pointvec)

    # wrap SpatVectors for polygons
    spatvec_wrap_list <- list()
    for (i in seq_len(total_nr_groups)) {
        selected_poly_ID_names <- poly_ID_names[names(poly_ID_names) == i]
        selected_spatvec <- spatvec[spatvec$poly_ID %in% selected_poly_ID_names]
        spatvec_wrap_list[[i]] <- terra::wrap(selected_spatvec)
    }


    # first intersect in parallel on wrapped terra objects
    result1 <- lapply_flex(
        X = seq_len(length(spatvec_wrap_list)),
        FUN = function(x) {
            test <- .overlap_points_per_polygon_wrapped(
                spatvec_wrapped = spatvec_wrap_list[[x]],
                pointvec_wrapped = pointvec_wrap,
                poly_ID_names = "all"
            )
        }
    )

    # unwrap overlap results
    final_result <- lapply(X = seq_len(length(result1)), FUN = function(x) {
        terra::vect(result1[x][[1]])
    })

    # rbind all results together
    final_result <- do.call("rbind", final_result)


    if (return_gobject == TRUE) {
        if (is.null(name_overlap)) {
            name_overlap <- feat_info
        }

        gobject@spatial_info[[spatial_info]]@overlaps[[name_overlap]] <-
            final_result
        return(gobject)
    } else {
        return(final_result)
    }
}





# overlapToMatrix methods ####


## transfer overlap results to matrix ####

#' @title overlapToMatrix
#' @name overlapToMatrix
#' @description create a count matrix based on overlap results from
#' \code{\link{calculateOverlap}}
#' @param x object containing overlaps info. Can be giotto object or SpatVector
#' points or data.table of overlaps generated from `calculateOverlap`
#' @param name name for the overlap count matrix
#' @param count_info_column column with count information
#' @param \dots additional params to pass to methods
#' @concept overlap
#' @returns giotto object or count matrix
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#' gpoly <- getPolygonInfo(g,
#'     polygon_name = "aggregate",
#'     return_giottoPolygon = TRUE
#' )
#' gpoints <- getFeatureInfo(g, return_giottoPoints = TRUE)
#'
#' # calculate all transcripts overlapped
#' out_all <- calculateOverlap(gpoly, gpoints)
#'
#' overlapToMatrix(out_all)
NULL


# * gobject ####
#' @rdname overlapToMatrix
#' @param poly_info character. Polygon information to use
#' @param feat_info character. Feature information to use
#' @param type character. Type of overlap data (either 'point' or 'intensity')
#' @param return_gobject return giotto object (default: TRUE)
#' @param verbose be verbose
#' @export
setMethod(
    "overlapToMatrix", signature("giotto"), function(x,
    name = "raw",
    poly_info = NULL,
    feat_info = NULL,
    type = c("point", "intensity"),
    count_info_column = NULL,
    aggr_function = "sum",
    return_gobject = TRUE,
    verbose = TRUE,
    ...) {
        type <- match.arg(type, choices = c("point", "intensity"))
        checkmate::assert_character(name, len = 1L)
        if (!is.null(count_info_column)) {
            checkmate::assert_character(count_info_column, len = 1L)
        }
        checkmate::assert_logical(return_gobject)

        poly_info <- set_default_spat_unit(
            gobject = x,
            spat_unit = poly_info
        )
        feat_info <- set_default_feat_type(
            gobject = x,
            spat_unit = poly_info,
            feat_type = feat_info
        )

        # get data
        gpoly <- getPolygonInfo(
            gobject = x,
            polygon_name = poly_info,
            return_giottoPolygon = TRUE,
            verbose = verbose
        )

        o2m_args <- list(
            x = gpoly,
            col_names = spatIDs(x, spat_unit = poly_info),
            row_names = featIDs(x, feat_type = feat_info),
            feat_info = feat_info,
            count_info_column = count_info_column,
            aggr_function = aggr_function,
            # output = 'Matrix', # Do not specify here. methods must return
            # something that operates similarly to a [matrix]
            # object by default.
            type = type,
            verbose = verbose,
            ...
        )

        # pass to giottoPolygon method
        overlapmatrix <- do.call(overlapToMatrix, args = o2m_args)

        # order matrix row/col
        mat_r_names <- rownames(overlapmatrix)
        mat_c_names <- colnames(overlapmatrix)
        overlapmatrix <- overlapmatrix[
            match(mixedsort(mat_r_names), mat_r_names),
            match(mixedsort(mat_c_names), mat_c_names)
        ]

        overlapExprObj <- create_expr_obj(
            name = name,
            exprMat = overlapmatrix,
            spat_unit = poly_info,
            feat_type = feat_info,
            provenance = poly_info
        )

        if (isTRUE(return_gobject)) {
            centroidsDT <- centroids(gpoly) %>%
                data.table::as.data.table(geom = "XY")
            centroidsDT_loc <- centroidsDT[, c("poly_ID", "x", "y")]
            data.table::setnames(
                centroidsDT_loc,
                old = c("poly_ID", "x", "y"),
                new = c("cell_ID", "sdimx", "sdimy")
            )

            spatlocs <- createSpatLocsObj(
                coordinates = centroidsDT_loc,
                name = name,
                spat_unit = poly_info,
                provenance = poly_info,
                verbose = FALSE
            )

            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            x <- setSpatialLocations(
                gobject = x,
                x = spatlocs,
                initialize = FALSE,
                verbose = FALSE
            )
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            x <- setExpression(
                gobject = x,
                x = overlapExprObj,
                initialize = TRUE,
                verbose = FALSE
            )
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

            return(x)
        } else {
            return(overlapExprObj)
        }
    }
)


# * giottoPolygon ####
#' @rdname overlapToMatrix
#' @param output data format/class to return the results as
#' @export
setMethod(
    "overlapToMatrix", signature("giottoPolygon"), function(x,
    feat_info = "rna",
    type = c("point", "intensity"),
    count_info_column = NULL,
    output = c("Matrix", "data.table"),
    ...) {
        type <- match.arg(type, choices = c("point", "intensity"))

        overlaps_data <- switch(type,
            "point" = overlaps(x)[[feat_info]],
            "intensity" = overlaps(x)[["intensity"]][[feat_info]]
        )

        # ensure data exists
        if (is.null(overlaps_data)) {
            .gstop(
                "No overlaps found between", objName(x), "and", feat_info, "
        Please run calculateOverlap() first.",
                .n = 2L
            )
        }

        argslist <- list(
            x = overlaps_data,
            count_info_column = count_info_column,
            output = output,
            ...
        )

        # remove args not accepted by specific method
        if (type == "intensity") {
            argslist$count_info_column <- NULL
            argslist$col_names <- NULL
            argslist$row_names <- NULL
            argslist$verbose <- NULL
        }

        # pass to SpatVector method
        do.call(overlapToMatrix, args = argslist)
    }
)


# * SpatVector ####
# points
#' @rdname overlapToMatrix
#' @param col_names,row_names character vector. (optional) Set of row and col
#' names that are expected to exist. This fixes the dimensions of the matrix
#' since the overlaps information does not directly report rows and cols where
#' no values were detected.
#' @export
setMethod(
    "overlapToMatrix", signature("SpatVector"), function(x,
    col_names = NULL,
    row_names = NULL,
    count_info_column = NULL,
    output = c("Matrix", "data.table"),
    verbose = TRUE,
    ...) {
        output <- match.arg(
            toupper(output),
            choices = c("MATRIX", "DATA.TABLE")
        )

        # NSE vars
        poly_ID <- NULL

        # 1. convert to DT and cleanup
        dtoverlap <- data.table::as.data.table(x, geom = c("XY"))
        # remove points that have no overlap with any polygons
        dtoverlap <- dtoverlap[!is.na(poly_ID)]


        # 2. Perform aggregation to counts DT
        if (!is.null(count_info_column)) { # if there is a counts col

            if (!count_info_column %in% colnames(dtoverlap)) {
                .gstop("count_info_column ", count_info_column,
                    " does not exist",
                    .n = 2L
                )
            }

            # aggregate counts of features
            dtoverlap[, c(count_info_column) := as.numeric(
                get(count_info_column)
            )]
            aggr_dtoverlap <- dtoverlap[, base::sum(get(count_info_column)),
                by = c("poly_ID", "feat_ID")
            ]
            data.table::setnames(aggr_dtoverlap, "V1", "N")
        } else { # if no counts col

            # aggregate individual features
            aggr_dtoverlap <- dtoverlap[, .N, by = c("poly_ID", "feat_ID")]
        }

        # 3. missing IDs repair

        if (!is.null(col_names) && !is.null(row_names)) {
            # get all feature and cell information
            missing_feats <- row_names[!row_names %in%
                unique(aggr_dtoverlap$feat_ID)]
            missing_ids <- col_names[!col_names %in%
                unique(aggr_dtoverlap$poly_ID)]

            # create missing cell values, only if there are missing cell IDs!
            if (!length(missing_ids) == 0) {
                first_feature <- aggr_dtoverlap[["feat_ID"]][[1]]
                missing_dt <- data.table::data.table(
                    poly_ID = missing_ids,
                    feat_ID = first_feature, N = 0
                )
                aggr_dtoverlap <- rbind(aggr_dtoverlap, missing_dt)
            }

            if (!length(missing_feats) == 0) {
                first_cell <- aggr_dtoverlap[["poly_ID"]][[1]]
                missing_dt <- data.table::data.table(
                    poly_ID = first_cell,
                    feat_ID = missing_feats, N = 0
                )
                aggr_dtoverlap <- rbind(aggr_dtoverlap, missing_dt)
            }

            # TODO: creating missing feature values
        } else {
            if (isTRUE(verbose) && output == "MATRIX") {
                warning(GiottoUtils::wrap_txt(
                    "[overlapToMatrix] expected col_names and row_names
                    not provided together. Points aggregation Matrix output
                    may be missing some cols and rows where no detections
                    were found."
                ), call. = FALSE)
            }
        }


        # 4. return
        switch(output,
            "DATA.TABLE" = return(aggr_dtoverlap),
            "MATRIX" = {
                # create matrix
                overlapmatrixDT <- data.table::dcast(
                    data = aggr_dtoverlap,
                    formula = feat_ID ~ poly_ID,
                    value.var = "N",
                    fill = 0
                )
                return(dt_to_matrix(overlapmatrixDT))
            }
        )
    }
)

# * data.frame ####
# images
#' @rdname overlapToMatrix
#' @param aggr_function function to aggregate image information (default = sum)
#' @export
setMethod(
    "overlapToMatrix", signature("data.table"), function(x,
    aggr_function = "sum",
    output = c("Matrix", "data.table")) {
        output <- match.arg(
            toupper(output),
            choices = c("MATRIX", "DATA.TABLE")
        )

        # NSE vars
        value <- poly_ID <- feat_ID <- NULL

        melt_image_info <- data.table::melt.data.table(
            data = x,
            id.vars = "poly_ID",
            variable.name = "feat_ID"
        )

        aggr_fun <- get(aggr_function)
        aggr_comb <- melt_image_info[, aggr_fun(value),
            by = .(poly_ID, feat_ID)
        ]
        data.table::setnames(aggr_comb, "V1", "aggregation")

        switch(output,
            "DATA.TABLE" = return(aggr_comb),
            "MATRIX" = {
                # create matrix
                overlapmatrixDT <- data.table::dcast(
                    data = aggr_comb,
                    formula = feat_ID ~ poly_ID,
                    value.var = "aggregation",
                    fill = 0
                )
                return(dt_to_matrix(overlapmatrixDT))
            }
        )
    }
)





#' @title overlapToMatrixMultiPoly
#' @name overlapToMatrixMultiPoly
#' @description create a count matrix based on overlap results
#' from \code{\link{calculateOverlapRaster}},
#' \code{\link{calculateOverlapSerial}},
#' or \code{\link{calculateOverlapParallel}}
#' and aggregate information from multiple polygon
#' layers (e.g. z-stacks) together
#' @param gobject giotto object
#' @param name name for the overlap count matrix
#' @param poly_info vector with polygon information
#' @param feat_info feature information
#' @param new_poly_info name for new aggregated polygon information
#' @param return_gobject return giotto object (default: TRUE)
#' @concept overlap
#' @returns giotto object or count matrix
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' overlapToMatrixMultiPoly(g, poly_info = "z0")
#' @export
overlapToMatrixMultiPoly <- function(gobject,
    name = "raw",
    poly_info = "cell",
    feat_info = "rna",
    new_poly_info = "multi",
    return_gobject = TRUE) {
    # define for data.table
    i <- j <- x <- NULL

    result_list <- list()
    cell_ids_list <- list()

    for (poly_info_i in seq_len(length(poly_info))) {
        poly_info_set <- poly_info[[poly_info_i]]

        expr_names <- list_expression_names(
            gobject = gobject,
            spat_unit = poly_info_set,
            feat_type = feat_info
        )

        # check if matrix already exists, if not try to make it
        if (!name %in% expr_names) {
            gobject <- overlapToMatrix(
                x = gobject,
                poly_info = poly_info_set,
                feat_info = feat_info,
                name = name
            )
        }

        testmat <- getExpression(
            gobject = gobject,
            spat_unit = poly_info_set,
            feat_type = feat_info,
            values = name,
            output = "matrix"
        )

        featnames <- dimnames(testmat)[[1]]
        names(featnames) <- seq_len(length(featnames))

        colnames <- dimnames(testmat)[[2]]
        names(colnames) <- seq_len(length(colnames))

        testmat_DT <- data.table::as.data.table(Matrix::summary(testmat))
        testmat_DT[, i := featnames[i]]
        testmat_DT[, j := colnames[j]]

        result_list[[poly_info_i]] <- testmat_DT

        # cell ids
        # cell_ids = gobject@cell_ID[[poly_info_set]]

        # cell_ids_list[[poly_info_i]] = cell_ids
    }

    final_DT <- data.table::rbindlist(result_list)
    final_DT_aggr <- final_DT[, sum(x), by = .(i, j)]


    # combined_cell_IDs = sort(unique(unlist(cell_ids_list)))


    # create matrix
    overlapmatrixDT <- data.table::dcast(
        data = final_DT_aggr,
        formula = i ~ j,
        value.var = "V1", fill = 0
    )
    overlapmatrix <- dt_to_matrix(overlapmatrixDT)




    # combined_cell_IDs = combined_cell_IDs[combined_cell_IDs %in%
    # colnames(overlapmatrix)]

    # overlapmatrix = overlapmatrix[match(gobject@feat_ID[[feat_info]],
    # rownames(overlapmatrix)),
    # match(combined_cell_IDs, colnames(overlapmatrix))]



    if (return_gobject == TRUE) {
        gobject@expression[[new_poly_info]][[feat_info]][[name]] <-
            overlapmatrix
        gobject@cell_ID[[new_poly_info]] <- colnames(overlapmatrix)

        gobject@cell_metadata[[new_poly_info]][[feat_info]] <-
            data.table::data.table(cell_ID = colnames(overlapmatrix))
        gobject@feat_metadata[[new_poly_info]][[feat_info]] <-
            data.table::data.table(feat_ID = rownames(overlapmatrix))

        return(gobject)
    } else {
        return(overlapmatrix)
    }
}





#' @title overlapImagesToMatrix
#' @name overlapImagesToMatrix
#' @description create a count matrix based on overlap results
#' from \code{\link{calculateOverlapPolygonImages}}
#' @param gobject giotto object
#' @param name name for the overlap count matrix
#' @param poly_info polygon information
#' @param feat_info feature information
#' @param name_overlap name of the overlap
#' @param aggr_function function to aggregate image information (default = sum)
#' @param image_names names of images you used
#' @param spat_locs_name name for spatial centroids / locations associated
#' with matrix
#' @param return_gobject return giotto object (default: TRUE)
#' @concept overlap
#' @returns giotto object or data.table with aggregated information
#' @export
overlapImagesToMatrix <- function(gobject,
    name = "raw",
    poly_info = "cell",
    feat_info = "protein",
    name_overlap = "images",
    aggr_function = "sum",
    image_names = NULL,
    spat_locs_name = "raw",
    return_gobject = TRUE) {
    # data.table vars
    value <- poly_ID <- feat_ID <- x <- y <- NULL

    ## get polygon information
    polygon_info <- getPolygonInfo(
        gobject = gobject,
        polygon_name = poly_info,
        return_giottoPolygon = TRUE
    )


    image_info <-
        gobject@spatial_info[[poly_info]]@overlaps[["intensity"]][[feat_info]]


    melt_image_info <- data.table::melt.data.table(
        data = image_info,
        id.vars = "poly_ID", variable.name = "feat_ID"
    )

    aggr_fun <- get(aggr_function)
    aggr_comb <- melt_image_info[, aggr_fun(value), by = .(poly_ID, feat_ID)]
    data.table::setnames(aggr_comb, "V1", "aggregation")


    if (return_gobject) {
        cell_IDs <- unique(as.character(aggr_comb$poly_ID))
        feat_IDs <- unique(as.character(aggr_comb$feat_ID))



        # create cell and feature metadata
        S4_cell_meta <- createCellMetaObj(
            metadata = data.table::data.table(cell_ID = cell_IDs),
            spat_unit = poly_info, feat_type = feat_info
        )
        gobject <- setCellMetadata(
            gobject = gobject,
            x = S4_cell_meta
        )

        S4_feat_meta <- createFeatMetaObj(
            metadata = data.table::data.table(feat_ID = feat_IDs),
            spat_unit = poly_info, feat_type = feat_info
        )
        gobject <- setFeatureMetadata(
            gobject = gobject,
            x = S4_feat_meta
        )


        # add feat_ID and cell_ID
        gobject@feat_ID[[feat_info]] <- feat_IDs
        gobject@cell_ID[[poly_info]] <- cell_IDs

        # add spatial locations
        centroidsDT <- gobject@spatial_info[[poly_info]]@spatVectorCentroids
        centroidsDT <- .spatvector_to_dt(centroidsDT)
        centroidsDT_loc <- centroidsDT[, .(poly_ID, x, y)]
        colnames(centroidsDT_loc) <- c("cell_ID", "sdimx", "sdimy")

        S4_spat_locs <- createSpatLocsObj(
            name = name,
            coordinates = centroidsDT_loc,
            spat_unit = poly_info
        )

        gobject <- setSpatialLocations(
            gobject = gobject,
            x = S4_spat_locs
        )

        # create matrix
        overlapmatrixDT <- data.table::dcast(
            data = aggr_comb,
            formula = feat_ID ~ poly_ID,
            value.var = "aggregation", fill = 0
        )
        overlapmatrix <- dt_to_matrix(overlapmatrixDT)

        overlapmatrix <- overlapmatrix[
            match(gobject@feat_ID[[feat_info]], rownames(overlapmatrix)),
            match(gobject@cell_ID[[poly_info]], colnames(overlapmatrix))
        ]

        S4_expr_obj <- createExprObj(
            name = name,
            expression_data = overlapmatrix,
            spat_unit = poly_info,
            feat_type = feat_info
        )

        gobject <- setExpression(
            gobject = gobject,
            x = S4_expr_obj
        )
    } else {
        return(aggr_comb)
    }
}









# aggregate stacks ####

# Functions for combining multiple layers of information into a single
# aggregated layer for analysis. Examples include combining multiple z stacks
# that sample the cells multiple times. Combining these z stacks provides a
# volumetric understanding of the cell's expression


.combine_matrices <- function(mat_list,
    summarize = "sum") {
    # data.table vars
    i <- j <- x <- i2 <- j2 <- NULL

    DT_list <- list()
    feats_list <- list()
    samples_list <- list()

    # loop through all matrices
    # create a triplet data.table (i, j, x)
    for (mat_i in seq_len(length(mat_list))) {
        mat <- mat_list[[mat_i]]

        if (!inherits(mat, c("matrix", "dgCMatrix"))) {
            .gstop("Matrix needs to be a base or sparse matrix from the
                Matrix package")
        }

        if (inherits(mat, "matrix")) mat <- methods::as(mat, "dgCMatrix")

        mat_feats <- mat@Dimnames[[1]]
        names(mat_feats) <- seq_len(mat@Dim[[1]])
        feats_list[[mat_i]] <- mat_feats

        mat_samples <- mat@Dimnames[[2]]
        names(mat_samples) <- seq_len(mat@Dim[[2]])
        samples_list[[mat_i]] <- mat_samples

        matDT <- data.table::as.data.table(Matrix::summary(mat))
        matDT[, c("i", "j") := list(mat_feats[i], mat_samples[j])]
        DT_list[[mat_i]] <- matDT
    }


    # combine matrices in data.table format and aggregate (sum by default)
    new_dt <- data.table::rbindlist(l = DT_list)

    if (summarize == "sum") {
        test <- new_dt[, sum(x), by = .(i, j)]
    } else {
        "not implemented yet"
    }

    # feature list
    all_features <- unique(unlist(feats_list))
    featnames <- seq_len(length(all_features))
    names(featnames) <- all_features

    # sample list
    all_samples <- unique(unlist(samples_list))
    samplenames <- seq_len(length(all_samples))
    names(samplenames) <- all_samples

    # convert i and j to numericals for dgCmatrix
    test[, i2 := featnames[i]]
    test[, j2 := samplenames[j]]

    # convert triplet data.table to sparseMatrix
    featnames_rev <- names(featnames)
    names(featnames_rev) <- featnames

    samplenames_rev <- names(samplenames)
    names(samplenames_rev) <- samplenames

    combined_matrix <- Matrix::sparseMatrix(
        i = test$i2,
        j = test$j2,
        x = test$V1,
        dims = c(length(featnames), length(samplenames)),
        dimnames = list(featnames_rev, samplenames_rev)
    )


    return(combined_matrix)
}


#' @title aggregateStacksExpression
#' @name aggregateStacksExpression
#' @description aggregate expression matrices from different z-stacks
#' @param gobject giotto object
#' @param spat_units spatial units to aggregate
#' @param feat_type feature type
#' @param values values to use
#' @param summarize method to summarize expression information
#' @param new_spat_unit new name for aggregated spatial unit
#' @param verbose verbosity
#' @family aggregate stacks
#' @returns giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' aggregateStacksExpression(g, spat_units = c("z0", "z1"), feat_type = "rna")
#' @export
aggregateStacksExpression <- function(gobject,
    spat_units,
    feat_type,
    values = "raw",
    summarize = "sum",
    new_spat_unit = "aggregate",
    verbose = TRUE) {
    # aggregate matrices
    matrix_list <- list()
    for (spat_unit in spat_units) {
        mat <- get_expression_values(gobject,
            spat_unit = spat_unit,
            feat_type = feat_type,
            values = values,
            output = "matrix"
        )
        matrix_list[[spat_unit]] <- mat
    }
    combined_matrix <- .combine_matrices(matrix_list,
        summarize = summarize
    )

    new_expr_obj <- create_expr_obj(
        name = values,
        exprMat = combined_matrix,
        spat_unit = new_spat_unit,
        feat_type = feat_type,
        provenance = spat_units,
        misc = NULL
    )

    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    gobject <- set_expression_values(gobject = gobject, values = new_expr_obj)
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

    # set new cell IDs
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    gobject <- set_cell_id(
        gobject = gobject,
        spat_unit = new_spat_unit,
        cell_IDs = colnames(combined_matrix)
    )
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

    # set new cell metadata
    cell_meta_S4 <- create_cell_meta_obj(
        metaDT = data.table::data.table("cell_ID" = colnames(combined_matrix)),
        spat_unit = new_spat_unit,
        feat_type = feat_type,
        provenance = spat_units
    )

    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    gobject <- setCellMetadata(
        gobject = gobject,
        cell_meta_S4,
        verbose = verbose,
        initialize = FALSE
    )
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

    # set new feat metadata
    feat_meta_S4 <- create_feat_meta_obj(
        metaDT = data.table::data.table("feat_ID" = rownames(combined_matrix)),
        spat_unit = new_spat_unit,
        feat_type = feat_type,
        provenance = spat_units
    )

    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    gobject <- setFeatureMetadata(
        gobject = gobject,
        x = feat_meta_S4,
        verbose = verbose,
        initialize = FALSE
    )
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

    return(gobject)
}



.combine_spatlocs <- function(spatlocs_list,
    summarize = "mean") {
    # data.table vars
    sdimx <- sdimy <- sdimz <- NULL

    newlocs <- data.table::rbindlist(spatlocs_list)

    if (summarize == "mean") {
        if ("sdimz" %in% colnames(newlocs)) {
            newlocs <- unique(newlocs[, c("sdimx", "sdimy", "sdimz") := list(
                mean(sdimx), mean(sdimy), mean(sdimz)
            ),
            by = "cell_ID"
            ])
        } else {
            newlocs <- unique(newlocs[, c("sdimx", "sdimy") := list(
                mean(sdimx), mean(sdimy)
            ), by = "cell_ID"])
        }
    }

    return(newlocs)
}



#' @title aggregateStacksLocations
#' @name aggregateStacksLocations
#' @description aggregate expression matrices from different z-stacks
#' @param gobject giotto object
#' @param spat_units spatial units to aggregate
#' @param values values to use
#' @param summarize method to summarize spatial location information
#' @param new_spat_unit new name for aggregated spatial unit
#' @returns giotto object
#' @family aggregate stacks
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' aggregateStacksLocations(g, spat_units = c("z0", "z1"))
#' @export
aggregateStacksLocations <- function(gobject,
    spat_units,
    values = "raw",
    summarize = "mean",
    new_spat_unit = "aggregate") {
    # aggregate locations
    locs_list <- list()
    for (spat_unit in spat_units) {
        locDT <- get_spatial_locations(
            gobject = gobject,
            spat_unit = spat_unit,
            spat_loc_name = values,
            output = "data.table"
        )
        locs_list[[spat_unit]] <- locDT
    }
    combined_locs <- .combine_spatlocs(
        spatlocs_list = locs_list,
        summarize = summarize
    )


    new_spatlocs_obj <- create_spat_locs_obj(
        name = values,
        coordinates = combined_locs,
        spat_unit = new_spat_unit,
        provenance = spat_units,
        misc = NULL
    )

    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    gobject <- set_spatial_locations(
        gobject = gobject,
        spatlocs = new_spatlocs_obj,
        set_defaults = FALSE
    )
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

    return(gobject)
}



.combine_polygons <- function(polygon_list) {
    polygon_DT <- data.table::rbindlist(polygon_list)

    polygon <- .dt_to_spatvector_polygon(polygon_DT)

    # TODO: maybe replace step 1 and 2 with polygon_to_raster()
    # TODO: how to define number of columns and rows?

    # step 1: convert polygon into detailed raster
    pol_xmax <- terra::xmax(polygon)
    pol_xmin <- terra::xmin(polygon)
    ncols <- abs(pol_xmax - pol_xmin)
    ncols <- ncols * 2

    pol_ymax <- terra::ymax(polygon)
    pol_ymin <- terra::ymin(polygon)
    nrows <- abs(pol_ymax - pol_ymin)
    nrows <- nrows * 2

    myraster <- terra::rast(polygon, ncols = ncols, nrows = nrows)

    # step 2: transfer vector data to a raster based on
    poly_rast <- terra::rasterize(x = polygon, y = myraster, field = "poly_ID")

    # create new combined polygon
    aggr_polygon <- terra::as.polygons(poly_rast)

    return(aggr_polygon)
}


#' @title .combine_stack_spatvectors
#' @description combines/aggregates polygons with the same cell ID from
#' different z-stacks
#' @returns SpatRaster
#' @keywords internal
.combine_stack_spatvectors <- function(gobject,
    spat_units,
    for_loop = FALSE,
    for_loop_group_size = 100) {
    # 1. combine all spatVectors across all stacks
    stack_list <- list()
    for (spat_i in seq_len(length(spat_units))) {
        spat <- spat_units[[spat_i]]
        stackspatvector <- get_polygon_info(
            gobject = gobject,
            polygon_name = spat,
            polygon_overlap = NULL,
            return_giottoPolygon = FALSE
        )
        # stackspatvector = gobject@spatial_info[[spat]]@spatVector
        stackspatvector[["stack"]] <- spat
        stack_list[[spat_i]] <- stackspatvector
    }
    stack_spatvector <- do.call("rbind", stack_list)

    # TODO: check if all stackspatvectors are identical
    # skip polygon aggregation step and simply keep one spatvector
    # dt_z0 = .spatvector_to_dt(stackspatvector_z0)
    # dt_z1 = .spatvector_to_dt(stackspatvector_z1)
    # identical(dt_z0[,.(x,y)], dt_z1[,.(x,y)])



    # 2. make sure spatvectors are valid
    stack_spatvector <- terra::makeValid(stack_spatvector)

    # 3. aggregate individual cells/polys
    all_poly_ids <- mixedsort(unique(stack_spatvector$poly_ID))

    # run in for loop if data is very very big
    if (isTRUE(for_loop)) {
        poly_list <- list()
        poly_id_groups <- split(
            all_poly_ids,
            ceiling(seq_along(all_poly_ids) / for_loop_group_size)
        )

        for (group_i in seq_len(length(poly_id_groups))) {
            selected_poly_ids <- poly_id_groups[[group_i]]
            selected_poly <- stack_spatvector[stack_spatvector$poly_ID %in%
                selected_poly_ids]
            selected_poly_aggr <- terra::aggregate(selected_poly,
                by = "poly_ID", dissolve = TRUE
            )
            poly_list[[group_i]] <- selected_poly_aggr
        }
        aggr_spatvectors <- do.call("rbind", poly_list)
    } else {
        aggr_spatvectors <- terra::aggregate(stack_spatvector,
            by = "poly_ID",
            dissolve = TRUE
        )
    }


    # 4. add valid information to aggregated spatvector
    aggr_spatvectors[["valid"]] <- terra::is.valid(aggr_spatvectors)

    return(aggr_spatvectors)
}



#' @title aggregateStacksPolygons
#' @name aggregateStacksPolygons
#' @description aggregate polygons from different z-stacks
#' @param gobject giotto object
#' @param spat_units spatial units to aggregate
#' @param new_spat_unit new name for aggregated spatial unit
#' @param for_loop aggregate polygons in for loop (default = FALSE)
#' @param for_loop_group_size size of polygon groups to aggregate in each loop
#' @returns giotto object
#' @family aggregate stacks
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' aggregateStacksPolygons(g, spat_units = c("z0", "z1"))
#' @export
aggregateStacksPolygons <- function(gobject,
    spat_units,
    new_spat_unit = "aggregate",
    for_loop = FALSE,
    for_loop_group_size = 100) {
    # aggregate spatvectors
    aggregated_spatVec <- .combine_stack_spatvectors(
        gobject = gobject,
        spat_units = spat_units,
        for_loop = for_loop,
        for_loop_group_size = for_loop_group_size
    )

    gpolygon <- create_giotto_polygon_object(
        name = new_spat_unit,
        spatVector = aggregated_spatVec,
        spatVectorCentroids = NULL,
        overlaps = NULL
    )

    gobject <- set_polygon_info(
        gobject = gobject,
        polygon_name = new_spat_unit,
        gpolygon = gpolygon,
        verbose = FALSE
    )

    return(gobject)
}



#' @title aggregateStacksPolygonOverlaps
#' @name aggregateStacksPolygonOverlaps
#' @description aggregate polygons overlap information from different z-stacks
#' @param gobject giotto object
#' @param spat_units spatial units to aggregate
#' @param feat_type feature type used for overlap calculations
#' @param new_spat_unit new name for aggregated spatial unit
#' @returns giotto object
#' @family aggregate stacks
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' aggregateStacksPolygonOverlaps(g,
#'     spat_units = c("z0", "z1"),
#'     feat_type = "rna"
#' )
#' @export
aggregateStacksPolygonOverlaps <- function(gobject,
    spat_units,
    feat_type,
    new_spat_unit = "aggregate") {
    # aggregate spatvectors
    polygon_list <- list()

    for (i in seq_len(length(spat_units))) {
        spat_unit <- spat_units[i]
        vecDT <- gobject@spatial_info[[spat_unit]]@overlaps[[feat_type]]

        if (!is.null(vecDT)) {
            vecDT <- .spatvector_to_dt(vecDT)
            vecDT[, "stack" := i]
            polygon_list[[spat_unit]] <- vecDT
        }
    }

    if (length(polygon_list) == 0) {
        wrap_msg("No feature overlaps found for stack aggregation \n")
    } else {
        polygon_DT <- data.table::rbindlist(polygon_list)
        polygon <- .dt_to_spatvector_points(
            dt = polygon_DT,
            include_values = TRUE
        )
        gobject@spatial_info[[new_spat_unit]]@overlaps[[feat_type]] <- polygon
    }

    return(gobject)
}

#' @title aggregateStacks
#' @name aggregateStacks
#' @description aggregate expression matrices from different z-stacks
#' @param gobject giotto object
#' @param spat_units spatial units to aggregate
#' @param feat_type feature type
#' @param values values to use
#' @param summarize_expression method to summarize expression information
#' @param summarize_locations method to summarize spatial location information
#' @param for_loop aggregate polygons in for loop (default = FALSE)
#' @param for_loop_group_size size of polygon groups to aggregate in each loop
#' @param new_spat_unit new name for aggregated spatial unit
#' @param verbose verbosity
#' @details Combines both \code{\link{aggregateStacksExpression}}
#' and \code{\link{aggregateStacksLocations}}
#' @returns giotto object
#' @family aggregate stacks
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' aggregateStacks(g,
#'     spat_units = c("z0", "z1"), feat_type = "rna",
#'     values = "raw"
#' )
#' @export
aggregateStacks <- function(gobject,
    spat_units,
    feat_type,
    values,
    summarize_expression = "sum",
    summarize_locations = "mean",
    for_loop = FALSE,
    for_loop_group_size = 100,
    new_spat_unit = "aggregate",
    verbose = TRUE) {
    if (isTRUE(verbose)) {
        wrap_msg("1. Start aggregating expression data")
    }
    gobject <- aggregateStacksExpression(
        gobject = gobject,
        spat_units = spat_units,
        feat_type = feat_type,
        values = values,
        summarize = summarize_expression,
        new_spat_unit = new_spat_unit
    )
    if (isTRUE(verbose)) {
        wrap_msg("1. Aggregating expression data completed")
    }

    # gobject = aggregateStacksLocations(gobject = gobject,
    #                                    spat_units = spat_units,
    #                                    values = values,
    #                                    summarize = summarize_locations,
    #                                    new_spat_unit = new_spat_unit)


    if (isTRUE(verbose)) {
        wrap_msg("2. Start aggregating Polygon data")
    }
    gobject <- aggregateStacksPolygons(
        gobject = gobject,
        spat_units = spat_units,
        new_spat_unit = new_spat_unit,
        for_loop = for_loop,
        for_loop_group_size = for_loop_group_size
    )
    if (isTRUE(verbose)) {
        wrap_msg("2. Aggregating polygon data completed")
    }


    if (isTRUE(verbose)) {
        wrap_msg("3. Start aggregating centroid location data")
    }
    gobject <- addSpatialCentroidLocations(
        gobject = gobject,
        feat_type = feat_type,
        poly_info = new_spat_unit,
        provenance = spat_units,
        spat_loc_name = "raw",
        return_gobject = TRUE
    )

    if (isTRUE(verbose)) {
        wrap_msg("3. Aggregating centroid location data completed")
    }



    if (isTRUE(verbose)) {
        wrap_msg("4. Start aggregating polygon overlap data")
    }
    gobject <- aggregateStacksPolygonOverlaps(gobject,
        spat_units = spat_units,
        feat_type = feat_type,
        new_spat_unit = new_spat_unit
    )
    if (isTRUE(verbose)) {
        wrap_msg("4. Aggregating polygon overlap data completed")
    }

    return(gobject)
}
