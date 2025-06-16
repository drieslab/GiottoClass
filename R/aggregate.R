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

#' @name aggregateFeatures
#' @title Aggregate Spatial Features Covered by Polygon Geometries
#' @description
#' Aggregate features (either `feat_info` OR `image_names`) with
#' polygons (`spat_info`). Under the hood, this is performed in two steps:
#'
#' 1. Find the overlapped features via the lower-level generic
#' [calculateOverlap()]
#' 2. Summarize the overlapped features as a matrix via [overlapToMatrix()]
#' @param gobject `giotto` object containing spatial data to aggregate.
#' @param spat_info character. Name of polygon information to use to aggregate
#' features with.
#' @param feat_info character. Name of feature point detections to be
#' aggregated.
#' @param image_names character. Name of image(s) containing intensity values
#' to be aggregated.
#' @param new_spat_unit character (optional). Name of spatial unit to assign
#' the expression info to. Default is the same as `spat_info`.
#' @param new_feat_type character (optional). Name of feature type to assign
#' the expression info to. Default is the same as `feat_info` when used. When
#' `image_names` is used instead, default is "protein".
#' @param name character. (default = "raw") Name to assign the output
#' expresssion values information.
#' @param poly_subset_ids character vector. (optional) Specific poly_IDs to use
#' @param feat_subset_column character. (optional) feature info attribute to
#' subset feature points on when performing overlap calculation.
#' @param feat_subset_values (optional) values matched against
#' in `feat_subset_column` in order to subset feature points when performing
#' overlap calculation.
#' @param feat_count_column character. (optional) feature info column with counts
#' information. Useful in cases when more than one detection is reported per
#' point.
#' @param fun character (default = "sum"). A function usable by
#' [exactextractr::exact_extract()] to aggregate image intensity values.
#' @param return_gobject logical (default = TRUE). Whether to return the
#' `giotto` object or just the aggregated expression values as `exprObj` class.
#' @param verbose logical. Whether to be verbose.
#' @param \dots Additional params to pass to the overlap calculation method.
#' None implemented for point overlaps. For intensity overlaps, passes to
#' [exactextractr::exact_extract()] and additionally the function requested
#' with the `fun` param.
#' @returns `giotto` when `return_gobject=TRUE`, `exprObj` when
#' `return_gobject=FALSE`
#' @details `feat_subset_column`, `feat_subset_values`, and `feat_count_column`
#' are specific to overlaps on feature points info, and should not be provided
#' when overlapping image data.
#' @export
aggregateFeatures <- function(gobject,
    spat_info = NULL,
    feat_info = NULL,
    image_names = NULL,
    new_spat_unit = NULL,
    new_feat_type = NULL,
    name = "raw",
    poly_subset_ids = NULL,
    feat_subset_column = NULL,
    feat_subset_values = NULL,
    feat_count_column = NULL,
    fun = "sum",
    return_gobject = TRUE,
    verbose = TRUE,
    ...
) {
    checkmate::assert_character(spat_info, len = 1L, null.ok = TRUE)
    checkmate::assert_character(feat_info, null.ok = TRUE)
    checkmate::assert_character(new_spat_unit, len = 1L, null.ok = TRUE)
    checkmate::assert_character(new_feat_type, len = 1L, null.ok = TRUE)
    checkmate::assert_character(image_names, null.ok = TRUE)
    checkmate::assert_character(fun, len = 1L)
    checkmate::assert_character(name, len = 1L)
    checkmate::assert_logical(return_gobject)
    checkmate::assert_character(feat_count_column, null.ok = TRUE)

    # catch improper feature input usage
    fun_tag <- "[aggregateFeatures] "
    if (!is.null(feat_info) && !is.null(image_names)) {
        stop(fun_tag, "Only one of 'feat_info' or 'image_names' may ",
             "be provided.\n", call. = FALSE)
    }

    # decide polygons to use
    spat_info <- spat_info %null% names(gobject@spatial_info)[[1]]

    # decide target spatial unit
    new_spat_unit <- new_spat_unit %null% spat_info
    # decide target feature type
    if (is.null(new_feat_type)) {
        if (!is.null(feat_info)) new_feat_type <- feat_info
        if (!is.null(image_names)) new_feat_type <- "protein"
    }

    # select overlap type (point geoms vs intensity rasters)
    overlap_type <- if (!is.null(feat_info)) {
        "point"
    } else if (!is.null(image_names)) {
        "intensity"
    } else {
        "point" # fallback assumption
    }

    # calculate overlap --------------------------------------------------- #
    calculate_overlap_params <- list( # common params
        x = gobject,
        spat_info = spat_info,
        feat_info = feat_info,
        name_overlap = new_feat_type,
        poly_subset_ids = poly_subset_ids,
        return_gobject = TRUE,
        verbose = verbose,
        ...
    )

    # method specific params
    switch(overlap_type,
        "point" = {
            calculate_overlap_params <- c(calculate_overlap_params,
                list(
                    feat_subset_column = feat_subset_column,
                    feat_subset_values = feat_subset_values,
                    feat_count_column = feat_count_column
                )
            )
        },
        "intensity" = {
            calculate_overlap_params <- c(calculate_overlap_params,
                list(image_names = image_names)
            )
        }
    )

    gobject <- do.call(calculateOverlap, args = calculate_overlap_params)

    # overlap to matrix --------------------------------------------------- #

    overlap_to_matrix_params <- list(
        x = gobject,
        name = name,
        spat_info = spat_info,
        feat_info = new_feat_type, # this difference is on purpose
        type = overlap_type,
        return_gobject = FALSE,
        verbose = verbose
    )

    switch(overlap_type,
        "point" = {
            overlap_to_matrix_params <- c(overlap_to_matrix_params,
                list(feat_count_column = feat_count_column)
            )
        },
        "intensity" = {
            overlap_to_matrix_params <- c(overlap_to_matrix_params,
                list(fun = fun)
            )
        }
    )

    ex <- do.call(overlapToMatrix, args = overlap_to_matrix_params)
    # this is moved out of the gobject since overlapToMatrix doesn't have a
    # way to set the spat unit (if it doesn't match spat_info)
    spatUnit(ex) <- new_spat_unit

    gobject <- setGiotto(gobject, ex, verbose = verbose)
    return(gobject)
}







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
    polygon$poly_i <- seq_len(nrow(polygon))
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
#' @param feat_subset_values (optional) values matched against
#' in `feat_subset_column` in order to subset feature points when performing
#' overlap calculation.
#' @param feat_subset_ids deprecated. Use `feat_subset_values` instead.
#' @param feat_count_column character. (optional) column with count information.
#' Useful in cases when more than one detection is reported per point.
#' @param verbose be verbose
#' @param count_info_column deprecated. Use `feat_count_column` instead.
#' @param \dots additional params to pass to methods.
#' @details `feat_subset_column`, `feat_subset_values`, and `feat_count_column`
#' are specific to overlaps on feature points info, and should not be provided
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
#'     feat_subset_values = c(1)
#' )
#' overlaps_z1 <- overlaps(out_z1)
#' overlaps_z1$rna
#'
#' # overlap image to get sum intensities per cell
#' out_img <- calculateOverlap(gpoly, gimg, progress = FALSE)
#' overlaps_img <- overlaps(out_img)
#' overlaps_img$intensity
#'
#' # giotto method
#' # calculate z0 overlaps and return as gobject
#' out_g <- calculateOverlap(g,
#'     feat_subset_column = "global_z",
#'     feat_subset_values = 0
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
    spat_info = NULL,
    feat_info = NULL,
    image_names = NULL,
    poly_subset_ids = NULL,
    return_gobject = TRUE,
    verbose = TRUE,
    spatial_info = deprecated(),
    ...) {
        # deprecations
        spat_info <- GiottoUtils::deprecate_param(spatial_info, spat_info,
            fun = "calculateOverlap", when = "0.4.7"
        )

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

        if (is.null(spat_info)) {
            spat_info <- names(x@spatial_info)[[1]]
        }


        # 2. get information from gobject #
        # ------------------------------- #

        #   ---[polys to overlap with]---
        A <- getPolygonInfo(
            gobject = x,
            polygon_name = spat_info,
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
                        "[calculateOverlap] image with the name ", img_name,
                        " was not found and will be skipped \n", call. = FALSE
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
    feat_subset_values = NULL,
    feat_count_column = NULL,
    return_gpolygon = TRUE,
    verbose = TRUE,
    feat_subset_ids = deprecated(),
    count_info_column = deprecated(),
    ...) {
        # deprecations
        feat_subset_values <- GiottoUtils::deprecate_param(
            feat_subset_ids, feat_subset_values,
            fun = "calculateOverlap", when = "0.4.7"
        )
        feat_count_column <- GiottoUtils::deprecate_param(
            count_info_column, feat_count_column,
            fun = "calculateOverlap", when = "0.4.7"
        )

        # return an overlap info object
        res <- calculateOverlap(
            x = x[],
            y = y[],
            poly_subset_ids = poly_subset_ids,
            feat_subset_column = feat_subset_column,
            feat_subset_values = feat_subset_values,
            feat_count_column = feat_count_column,
            verbose = verbose,
            ...
        )

        if (isTRUE(return_gpolygon)) {
            # update schema metadata in overlap object
            if (is.null(name_overlap)) name_overlap <- objName(y)
            prov(res) <- spatUnit(x)
            spatUnit(res) <- spatUnit(x)
            featType(res) <- name_overlap

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
            name_overlap = name_overlap %null% objName(y),
            poly_subset_ids = poly_subset_ids,
            verbose = verbose,
            ...
        )
    }
)

# * giottoPolygon giottoAffineImage ####
#' @rdname calculateOverlap
#' @export
setMethod(
    "calculateOverlap", signature(x = "giottoPolygon", y = "giottoAffineImage"),
    function(x, y,
    name_overlap = NULL,
    poly_subset_ids = NULL,
    return_gpolygon = TRUE,
    verbose = TRUE,
    ...) {
        aff <- y@affine
        # perform the reverse of the image's affine on the polys
        inv_aff_poly <- affine(x, aff, inv = TRUE)
        res <- calculateOverlap(
            x = inv_aff_poly,
            y = y@raster_object,
            name_overlap = name_overlap %null% objName(y),
            poly_subset_ids = poly_subset_ids,
            verbose = verbose,
            ...
        )
        x@overlaps <- res@overlaps
        if (is.null(centroids(x))) {
            x <- centroids(x, append_gpolygon = TRUE)
        }
        x
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
            stop("calculateOverlap: name_overlap must be given", call. = FALSE)
        }

        # return overlaps information
        res <- calculateOverlap(
            x = x[],
            y = y,
            poly_subset_ids = poly_subset_ids,
            verbose = verbose,
            ...
        )

        if (isTRUE(return_gpolygon)) {
            # update schema metadata in overlap object
            prov(res) <- spatUnit(x)
            spatUnit(res) <- spatUnit(x)
            featType(res) <- name_overlap

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
    fun = "sum",
    ...) {
        checkmate::assert_true(terra::is.polygons(x))
        GiottoUtils::package_check("exactextractr")

        # channel naming (catch if none or too few)
        image_names <- names(y)
        nchannel <- terra::nlyr(y)
        if (is.null(image_names) ||
            nchannel > 1L && length(unique(image_names)) == 1L) {
            names(y) <- sprintf("channel_%d", seq_len(nchannel))
        }

        # subset polys if needed
        if (!is.null(poly_subset_ids)) {
            x <- x[x$poly_ID %in% poly_subset_ids]
        }

        # convert polys to sf
        sf_x <- as.sf(x)

        vmsg(.v = verbose, "Start image extract")

        eer_cname_fun <- function( # how to construct output colnames
            values, # name of value layer
            weights, # name of weight layer
            fun_name, # value of fun
            fun_value, # value associated with fun (quantile/frac/wfrac)
            nvalues, # number of value layers
            nweights # number of weight layers
        ) {
            values
        }

        # perform extraction
        extract_res <- exactextractr::exact_extract(
            x = y,
            y = sf_x,
            append_cols = "poly_ID",
            fun = fun,
            colname_fun = eer_cname_fun,
            ...
        )
        data.table::setDT(extract_res)
        extract_res <- .create_overlap_intensity_dt(extract_res)

        vmsg(.v = verbose, "End image extract")

        return(extract_res)
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
    feat_subset_values = NULL,
    feat_count_column = NULL,
    method = c("raster", "vector"),
    verbose = TRUE,
    feat_subset_ids = deprecated(),
    count_info_column = deprecated()) {
        method <- match.arg(method, choices = c("raster", "vector"))
        feat_subset_values <- GiottoUtils::deprecate_param(
            feat_subset_ids, feat_subset_values,
            fun = "calculateOverlap", when = "0.4.7"
        )
        feat_count_column <- GiottoUtils::deprecate_param(
            count_info_column, feat_count_column,
            fun = "calculateOverlap", when = "0.4.7"
        )

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

        # preserve total all feat ids present
        feat_ids <- unique(y$feat_ID)

        # * subset points if needed
        # e.g. to select transcripts within a z-plane
        if (!is.null(feat_subset_column) && !is.null(feat_subset_values)) {
            bool_vector <- y[[feat_subset_column]][[1]] %in% feat_subset_values
            y <- y[bool_vector]
        }

        res <- switch(method,
            "raster" = .calculate_overlap_raster(
                spatvec = x,
                pointvec = y,
                count_info_column = feat_count_column,
                verbose = verbose
            ),
            "vector" = .calculate_overlap_vector(
                spatvec = x,
                pointvec = y,
                keep = feat_count_column
            )
        )

        .create_overlap_point_dt(x, y, res, feat_ids = feat_ids)
    }
)


#' @param spatvec SpatVector polygon
#' @param pointvec SpatVector points
#' @param keep other col(s) to keep
#' @keywords internal
#' @noRd
.calculate_overlap_vector <- function(spatvec, pointvec, keep = NULL) {
    checkmate::assert_character(keep, null.ok = TRUE)
    res <- terra::extract(spatvec, pointvec)
    cn <- colnames(res)
    if (all(c("id.y", "poly_ID") %in% cn)) {
        res_keep <- c("id.y", "poly_ID")
    } else {
        res_keep <- cn[c(1L, 2L)]
    }
    res <- res[!is.na(res[[2]]), res_keep] # drop NAs (sparsify) + col select
    if (!is.null(keep)) {
        feat_keep <- do.call(
            data.frame, terra::as.list(pointvec[][res[[1]], keep])
        ) # list of vectors
        res <- cbind(res, feat_keep)
    }
    res
}





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
#' @param feat_subset_values value(s) within feature info `feat_subset_column`
#' to use for subsetting
#' @param feat_count_column column with count information (optional)
#' @param return_gobject return giotto object (default: TRUE)
#' @param verbose be verbose
#' @param feat_subset_ids deprecated. Use `feat_subset_values` instead.
#' @param count_info_column deprecated. Use `feat_count_column` instead.
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
        feat_subset_values = NULL,
        feat_count_column = NULL,
        return_gobject = TRUE,
        verbose = TRUE,
        feat_subset_ids = deprecated(),
        count_info_column = deprecated()) {
    deprecate_warn(
        when = "0.4.7",
        what = "calculateOverlapRaster()",
        with = "aggregateFeatures()",
        details = "`calculateOverlap()` is another option if only the overlap
        step is desired."
    )
    feat_subset_values <- GiottoUtils::deprecate_param(
        feat_subset_ids, feat_subset_values,
        fun = "calculateOverlapRaster", when = "0.4.7"
    )
    feat_count_column <- GiottoUtils::deprecate_param(
        count_info_column, feat_count_column,
        fun = "calculateOverlapRaster", when = "0.4.7"
    )

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
    if (!is.null(feat_subset_column) & !is.null(feat_subset_values)) {
        bool_vector <- pointvec[[feat_subset_column]][[1]] %in%
            feat_subset_values
        pointvec <- pointvec[bool_vector]
    }

    # run overlap workflow
    overlap_points <- .calculate_overlap_raster(
        spatvec = spatvec,
        pointvec = pointvec,
        count_info_column = feat_count_column,
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

#' @param overlap_data `data.table` of extracted intensity values per poly_ID
#' @noRd
.create_overlap_intensity_dt <- function(overlap_data) {
    odt <- new("overlapIntensityDT", data = overlap_data)
    odt@nfeats <- ncol(overlap_data) - 1L
    odt
}

#' @param x from data (SpatVector)
#' @param y to data (SpatVector)
#' @param overlap_data relationships (data.frame). Expected to be numeric row
#' indices between x and y
#' @param keep additional col(s) in `y` to keep
#' @noRd
.create_overlap_point_dt <- function(x, y,
        overlap_data, keep = NULL, feat_ids) {
    poly <- feat_idx <- feat <- feat_id_index <- NULL # NSE vars
    # cleanup input overlap_data
    checkmate::assert_data_frame(overlap_data)
    data.table::setDT(overlap_data)
    cnames <- colnames(overlap_data)
    data.table::setnames(overlap_data,
        old = c(cnames[[2]], cnames[[1]]),
        new = c("poly", "feat_idx")
    )
    # make relationships table sparse by removing non-overlapped features
    # these results are indexed by all features, so no need to filter
    # non-overlapped polys
    overlap_data <- overlap_data[!is.na(poly)]

    # extract needed info from y
    keep <- c("feat_ID", "feat_ID_uniq", keep)
    ytab <- terra::as.data.frame(y[overlap_data$feat_idx, keep])

    # initialize overlap object and needed ids
    sids <- x$poly_ID
    fids <- unique(ytab$feat_ID)
    odt <- new("overlapPointDT",
        spat_ids = sids,
        feat_ids = feat_ids,
        nfeats = as.integer(nrow(y))
    )

    # Ensure data is stored as integer or integer-based mapping
    ## - if poly/feat_idx contents are NOT integer coercible, establish a map #
    if (!overlap_data[, checkmate::test_integerish(head(poly, 100))]) {
        overlap_data[, poly := match(poly, sids)]
    }
    if (!overlap_data[, checkmate::test_integerish(head(feat_idx, 100))]) {
        overlap_data[, feat_idx := match(feat_idx, fids)]
    }
    ## -- if still not integer, coerce to integer --------------------------- #
    if (!is.integer(overlap_data$poly[1])) {
        overlap_data[, poly := as.integer(poly)]
    }
    if (!is.integer(overlap_data$feat_idx[1])) {
        overlap_data[, feat_idx := as.integer(feat_idx)]
    }

    # append y attribute info
    overlap_data <- cbind(overlap_data, ytab)
    data.table::setnames(overlap_data,
        old = c("feat_ID_uniq", "feat_ID"),
        new = c("feat", "feat_id_index")
    )
    if (!is.integer(overlap_data$feat[1])) {
        overlap_data[, feat := as.integer(feat)]
    }
    # add feat_ID map
    overlap_data[, feat_id_index := match(feat_id_index, odt@feat_ids)]
    # remove feat_idx which may not be reliable after feature subsets
    overlap_data[, feat_idx := NULL]
    # set indices
    data.table::setkeyv(overlap_data, "feat")
    data.table::setindexv(overlap_data, "poly")
    data.table::setcolorder(overlap_data, c("poly", "feat", "feat_id_index"))
    # add to object
    odt@data <- overlap_data

    odt
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
    overlap_res <- terra::extract(x = spatrast, y = pointvec)

    return(overlap_res)
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
                "[calculateOverlap] image with the name ", img_name,
                " was not found and will be skipped \n", call. = FALSE
            )
        }
    }
    image_names <- image_names[image_names %in% potential_large_image_names]


    image_list <- list()

    for (i in seq_len(length(image_names))) {
        img_name <- image_names[i]

        if (!img_name %in% potential_large_image_names) {
            warning(
                "[calculateOverlap] image with the name ", img_name,
                " was not found and will be skipped \n", call. = FALSE
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
#' @param feat_count_column column with count information
#' @param count_info_column deprecated. Use `feat_count_column` instead.
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
#' @param spat_info character. Polygon information to use
#' @param feat_info character. Feature information to use
#' @param type character. Type of overlap data (either 'point' or 'intensity')
#' @param return_gobject return giotto object (default: TRUE)
#' @param verbose be verbose
#' @param poly_info deprecated. Please use spat_info.
#' @export
setMethod(
    "overlapToMatrix", signature("giotto"), function(x,
    name = "raw",
    spat_info = NULL,
    feat_info = NULL,
    type = c("point", "intensity"),
    feat_count_column = NULL,
    fun = "sum",
    return_gobject = TRUE,
    verbose = TRUE,
    aggr_function = deprecated(),
    poly_info = deprecated(),
    count_info_column = deprecated(),
    ...) {
        # deprecations
        spat_info <- GiottoUtils::deprecate_param(poly_info, spat_info,
            fun = "overlapToMatrix", when = "0.4.7"
        )
        feat_count_column <- GiottoUtils::deprecate_param(
            count_info_column, feat_count_column,
            fun = "overlapToMatrix", when = "0.4.7"
        )
        fun <- GiottoUtils::deprecate_param(aggr_function, fun,
            fun = "overlapToMatrix", when = "0.4.7"
        )

        type <- match.arg(type, choices = c("point", "intensity"))
        checkmate::assert_character(name, len = 1L)
        checkmate::assert_character(feat_count_column,
            len = 1L, null.ok = TRUE)
        checkmate::assert_logical(return_gobject)

        spat_info <- set_default_spat_unit(
            gobject = x,
            spat_unit = spat_info
        )
        feat_info <- set_default_feat_type(
            gobject = x,
            spat_unit = spat_info,
            feat_type = feat_info
        )

        # get data
        gpoly <- getPolygonInfo(
            gobject = x,
            polygon_name = spat_info,
            return_giottoPolygon = TRUE,
            verbose = verbose
        )

        o2m_args <- list(
            x = gpoly,
            feat_info = feat_info,
            feat_count_column = feat_count_column,
            output = "exprobj",
            sort = TRUE,
            type = type,
            ...
        )

        # pass to giottoPolygon method
        overlapExprObj <- do.call(overlapToMatrix, args = o2m_args)
        objName(overlapExprObj) <- name

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
                spat_unit = spat_info,
                provenance = spat_info,
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
    feat_count_column = NULL,
    output = c("Matrix", "data.table"),
    count_info_column = deprecated(),
    ...) {
        # deprecations
        feat_count_column <- GiottoUtils::deprecate_param(
            count_info_column, feat_count_column,
            fun = "overlapToMatrix", when = "0.4.7"
        )

        type <- match.arg(type, choices = c("point", "intensity"))

        overlaps_data <- switch(type,
            "point" = overlaps(x)[[feat_info]],
            "intensity" = overlaps(x)[["intensity"]][[feat_info]]
        )

        # ensure data exists
        if (is.null(overlaps_data)) {
            stop(wrap_txt(
                "No overlaps found between", objName(x), "and", feat_info, "
                Please run calculateOverlap() first."
            ), call. = FALSE)
        }

        argslist <- list(
            x = overlaps_data,
            feat_count_column = feat_count_column,
            output = output,
            ...
        )

        # remove args not accepted by specific method
        if (type == "intensity") {
            argslist$feat_count_column <- NULL
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
    feat_count_column = NULL,
    output = c("Matrix", "data.table"),
    verbose = TRUE,
    count_info_column = deprecated(),
    ...) {
        # deprecations
        feat_count_column <- GiottoUtils::deprecate_param(
            count_info_column, feat_count_column,
            fun = "overlapToMatrix", when = "0.4.7"
        )

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
        if (!is.null(feat_count_column)) { # if there is a counts col

            if (!feat_count_column %in% colnames(dtoverlap)) {
                .gstop("feat_count_column ", feat_count_column,
                    " does not exist",
                    .n = 2L
                )
            }

            # aggregate counts of features
            dtoverlap[, c(feat_count_column) := as.numeric(
                get(feat_count_column)
            )]
            aggr_dtoverlap <- dtoverlap[, base::sum(get(feat_count_column)),
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
#' @param fun character. Function to aggregate image information
#' (default = "sum")
#' @param aggr_function deprecated. Use `fun` instead.
#' @export
setMethod(
    "overlapToMatrix", signature("data.table"), function(x,
    fun = "sum",
    output = c("Matrix", "data.table"),
    aggr_function = deprecated()) {

        fun <- GiottoUtils::deprecate_param(aggr_function, fun,
            fun = "overlapToMatrix", when = "0.4.7"
        )

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

        aggr_fun <- get(fun)
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

# * overlapPointDT ####

#' @rdname overlapToMatrix
#' @param sort logical (default = TRUE). Whether to perform a mixed sort on
#' output matrix row and col names.
#' @export
setMethod("overlapToMatrix", signature("overlapPointDT"),
    function(x,
    name = "raw",
    sort = TRUE,
    feat_count_column = NULL,
    output = c("Matrix", "exprObj"),
    ...) {
        output <- match.arg(tolower(output), choices = c("matrix", "exprobj"))
        m <- as.matrix(x, feat_count_column = feat_count_column, ...)
        if (isTRUE(sort)) m <- .mixedsort_rowcols(m)
        switch(output,
            "matrix" = m,
            "exprobj" = createExprObj(
                expression_data = m,
                name = name,
                spat_unit = spatUnit(x),
                feat_type = featType(x),
                provenance = prov(x)
            )
        )
})

# * overlapIntensityDT ####

#' @rdname overlapToMatrix
#' @export
setMethod("overlapToMatrix", signature("overlapIntensityDT"),
    function(x,
             name = "raw",
             sort = TRUE,
             output = c("Matrix", "exprObj"),
             ...) {
        output <- match.arg(tolower(output), choices = c("matrix", "exprobj"))
        m <- as.matrix(x, ...)
        if (isTRUE(sort)) m <- .mixedsort_rowcols(m)
        switch(output,
            "matrix" = m,
            "exprobj" = createExprObj(
                expression_data = m,
                name = name,
                spat_unit = spatUnit(x),
                feat_type = featType(x),
                provenance = prov(x)
            )
        )
})



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


.mixedsort_rowcols <- function(x) {
    # order matrix row/col
    mat_r_names <- rownames(x)
    mat_c_names <- colnames(x)
    x[
        match(mixedsort(mat_r_names), mat_r_names),
        match(mixedsort(mat_c_names), mat_c_names),
        drop = FALSE
    ]
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

    deprecate_warn(
        when = "0.4.7",
        what = "overlapImagesToMatrix()",
        with = "aggregateFeatures()",
        details = "`overlapToMatrix()` is another option if only the matrix
        construction from overlaps information step is desired."
    )

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
        mat <- getExpression(gobject,
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
        ovlp <- getPolygonInfo(gobject,
            polygon_name = spat_unit,
            polygon_overlap = feat_type
        )
        # vecDT <- gobject@spatial_info[[spat_unit]]@overlaps[[feat_type]]

        if (!is.null(ovlp)) {
            ovlp@data[, "stack" := i]
            polygon_list[[spat_unit]] <- ovlp
        }
    }

    if (length(polygon_list) == 0) {
        wrap_msg("No feature overlaps found for stack aggregation \n")
    } else {
        comb_ovlp <- do.call(rbind, polygon_list)
        gobject@spatial_info[[new_spat_unit]]@overlaps[[feat_type]] <-
            comb_ovlp
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
