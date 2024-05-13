## ** cell shape polygons ####

#' @title Do giottoPolygon
#' @name .do_gpoly
#' @description giottoPolygon objects carry 3 pieces of spatial information.
#' The polygons, their centroids, and the polygon overlapped features. All of
#' these need to be updated when spatial manipulations are applied. This
#' function simplifies performing functions on all SpatVector-based slots.
#' @param x giottoPolygon
#' @param what a call to do
#' @param args a \code{list} of additional args
#' @keywords internal
#' @returns giottoPolygon
.do_gpoly <- function(x, what, args = NULL) {
    x@spatVector <- do.call(what, args = append(list(x@spatVector), args))
    if (!is.null(x@spatVectorCentroids)) {
        x@spatVectorCentroids <- do.call(
            what,
            args = append(list(x@spatVectorCentroids), args)
        )
    }
    if (!is.null(x@overlaps)) {
        x@overlaps <- lapply(x@overlaps, function(sv) {
            spatial_classes <- c(
                "SpatVector", "sf", "SpatialPolygonsDataFrame",
                "SpatialPointsDataFrame", "stars"
            )
            if (inherits(sv, spatial_classes)) {
                do.call(what, args = append(list(sv), args))
            } else {
                sv
            }
        })
    }
    return(x)
}











#' @title Identify background range polygons
#' @name .identify_background_range_polygons
#' @description function to remove background polygon by comparing individual
#' polygon vertex xy ranges against the overall `SpatVector` extent. Each
#' `SpatVector` that this function is run on MUST contain only those polygons
#' that were generated as part of the same FOV. This is because the individual
#' polygons are compared against the overall extent which is assumed to be
#' roughly that of the original mask image.
#' @param spatVector terra polygon spatvector
#' @param threshold if a polygon's x AND y range exceeds this fraction of the
#' overall `SpatVector` extent, it will be flagged for removal.
#' @returns A character vector of polygon IDs of variable length (but usually
#' only either length 0 i.e. no background poly to remove, or 1, being the
#' background poly to remove) if used with a correct threshold.
#' @keywords internal
.identify_background_range_polygons <- function(spatVector, threshold = 0.9) {
    # define for data.table
    x <- y <- geom <- V1 <- NULL

    xythresholds <- range(terra::ext(spatVector)) * threshold

    # identify polygon with the largest average range for x and y
    gDT <- data.table::as.data.table(terra::geom(spatVector))

    range_geom_x <- gDT[, diff(range(x)), by = geom]
    range_geom_y <- gDT[, diff(range(y)), by = geom]
    xbool <- range_geom_x[, V1 > xythresholds[["x"]]]
    ybool <- range_geom_y[, V1 > xythresholds[["y"]]]
    bool <- xbool & ybool

    # get original mask id for identified 'background' polygon
    values <- terra::values(spatVector)
    poly_id <- values[bool, "poly_ID"]

    return(poly_id)
}




#' @title Calculate polygon centroids
#' @name .calculate_centroids_polygons
#' @description calculates centroids from selected polygons
#' @keywords internal
#' @returns SpatVector or giotto polygon
.calculate_centroids_polygons <- function(gpolygon,
    name = "centroids",
    append_gpolygon = TRUE) {
    terra_polygon_centroids <- terra::centroids(slot(gpolygon, "spatVector"))

    if (isTRUE(append_gpolygon)) {
        slot(gpolygon, "spatVectorCentroids") <- terra_polygon_centroids
        gpolygon
    } else {
        terra_polygon_centroids
    }
}



# TODO Remove this? This is not necessarily an error with the polygons and may
# be desired in some cases. Also the code is not currently used anywhere.

#' @title Split multi-part polygons
#' @name .fix_multipart_geoms
#' @description function to split geoms (polygons) that have multiple parts
#' @keywords internal
#' @returns SpatVector
.fix_multipart_geoms <- function(spatVector) {
    # data.table variables
    x <- y <- geom <- part <- NULL

    spatVecDT <- .spatvector_to_dt(spatVector)
    uniq_multi <- unique(spatVecDT[part == 2]$geom)

    # geoms to keep
    tokeepDT <- spatVecDT[!geom %in% uniq_multi]
    tokeepDT <- tokeepDT[, .(x, y, geom)]

    # rename
    total_geoms <- length(unique(tokeepDT$geom))

    uniq_geom_vec <- seq_len(total_geoms)
    names(uniq_geom_vec) <- unique(tokeepDT$geom)
    tokeepDT[, geom := uniq_geom_vec[[as.character(geom)]],
        by = seq_len(nrow(tokeepDT))
    ]

    new_list <- list()
    add_i <- 1
    for (multi in uniq_multi) {
        tosplit <- spatVecDT[geom == multi]

        intern_list <- list()
        for (part_i in unique(tosplit$part)) {
            tempsplit <- tosplit[part == part_i]
            tempsplit <- tempsplit[, .(x, y, geom)]
            tempsplit[, geom := (total_geoms + add_i)]

            add_i <- add_i + 1

            intern_list[[part_i]] <- tempsplit
        }

        final_intern <- do.call("rbind", intern_list)

        new_list[[multi]] <- final_intern
    }

    final_new <- do.call("rbind", new_list)

    finalDT <- rbind(tokeepDT[, .(x, y, geom)], final_new)

    # return(finalDT)

    test <- createGiottoPolygonsFromDfr(segmdfr = finalDT)

    return(test@spatVector)
}










#' @title Combine giottoPolygon geometries
#' @name combineToMultiPolygon
#' @param x giottoPolygon
#' @param groups data.frame with columns 'poly_ID' and 'group_ID' that relates
#' which polygons should now be combined under which group_ID
#' @param name (optional) name for the giottoPolygon object
#' @description
#' Combine multiple giottoPolygon geometries into a set of multipolygons. Note
#' that attributes cannot be kept
#' @returns giottoPolygon
#' @examples
#' gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")
#' groups <- data.table::data.table(
#'     poly_ID = gpoly$poly_ID,
#'     group_ID = sort(rep(LETTERS[seq_len(5)], length.out = nrow(gpoly)))
#'     # make 5 groups
#' )
#'
#' multi_gp <- combineToMultiPolygon(gpoly, groups)
#'
#' plot(multi_gp["A"])
#' @export
combineToMultiPolygon <- function(x, groups, name = NULL) {
    # DT vars
    poly_ID <- group_ID <- part <- group_n <- geom <- NULL

    if (!inherits(groups, "data.frame") ||
        !all(c("poly_ID", "group_ID") %in% colnames(groups))) {
        stop(wrap_txt(
            "groups must be a data.frame with columns 'poly_ID' and 'group_ID'",
            errWidth = TRUE
        ))
    }

    # default name
    if (is.null(name)) name <- paste0(objName(x), "_grouped")
    # create a new table instead of setDT() since provided DF is from user
    groups_dt <- data.table::as.data.table(groups)
    if (!groups_dt[, is.character(poly_ID)]) {
        groups_dt[, poly_ID := as.character(poly_ID)]
    }
    if (!groups_dt[, is.character(group_ID)]) {
        groups_dt[, group_ID := as.character(group_ID)]
    }
    groups_dt[, part := seq(.N), by = "group_ID"] # part col
    groups_dt[, group_n := .N, by = "group_ID"] # number of polys per group
    g_ID <- groups_dt[, unique(group_ID)]
    groups_dt[, geom := match(group_ID, g_ID)] # geom col

    poly_dt <- data.table::as.data.table(x, geom = "XY")
    poly_dt <- poly_dt[, c("poly_ID", "x", "y", "hole")] # subset to only needed
    multi_dt <- groups_dt[poly_dt, on = "poly_ID"] # join on poly_ID
    multi_dt[, "poly_ID" := NULL] # then remove old poly_ID col
    # change group_ID to new poly_ID
    data.table::setnames(multi_dt, old = "group_ID", new = "poly_ID")
    data.table::setcolorder(multi_dt, "poly_ID")

    multi_sv <- .dt_to_spatvector_polygon(
        multi_dt,
        include_values = TRUE,
        sort_geom = TRUE
    )

    # makeValid is necessary. Very common that there will be cell polys that
    # overlap each other. This is fine when the polys are independent, however,
    # when they are combined as a multipolygon, they become self intersecting
    # geometry and thus not allowed.
    multi_sv <- terra::makeValid(multi_sv)

    giottoPolygon(
        spatVector = multi_sv,
        name = name,
        unique_ID_cache = g_ID
    )
}











# helper functions ####

# Conversion helpers moved to methods-coerce.R #


#' @title Convert spline to polygon
#' @name .spline_poly
#' @description spline polynomial to smooth polygon
#' @param xy xy
#' @param vertices vertices
#' @param k k
#' @param ... additional params to pass
#' @returns polygon
#' @keywords internal
.spline_poly <- function(xy, vertices = 20, k = 3, ...) {
    # Assert: xy is an n by 2 matrix with n >= k.

    # Wrap k vertices around each end.
    n <- dim(xy)[1]
    if (k >= 1) {
        data <- rbind(xy[(n - k + 1):n, ], xy, xy[seq_len(k), ])
    } else {
        data <- xy
    }

    # Spline the x and y coordinates.
    data.spline <- stats::spline(seq_len(n + 2 * k), data[, 1],
        n = vertices, ...
    )
    x <- data.spline$x
    x1 <- data.spline$y
    x2 <- stats::spline(seq_len(n + 2 * k), data[, 2], n = vertices, ...)$y

    # Retain only the middle part.
    cbind(x1, x2)[k < x & x <= n + k, ]
}




#' @title smoothGiottoPolygons
#' @name smoothGiottoPolygons
#' @description Smooths Giotto polygon object
#' @param gpolygon giotto polygon object
#' @param vertices number of vertices
#' @param k k
#' @param set_neg_to_zero set negative values to zero (default: TRUE)
#' @param ... additional params to pass to \code{spline}
#' @returns Smoothed Giotto polygon object with reduced vertices
#' @concept polygon
#' @seealso \code{\link[stats]{spline}}
#' @examples
#' gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")
#'
#' smoothGiottoPolygons(gpolygon = gpoly)
#' @export
smoothGiottoPolygons <- function(gpolygon,
    vertices = 20,
    k = 3,
    set_neg_to_zero = TRUE,
    ...) {
    # NSE vars
    x <- NULL
    y <- NULL

    # define for data.table [] subsetting
    geom <- NULL

    polygDT <- .spatvector_to_dt(gpolygon@spatVector)

    # store other values
    all_colnames <- colnames(polygDT)
    geom_values <- c("geom", "part", "x", "y", "hole")
    other_values <- all_colnames[!all_colnames %in% geom_values]
    other_values_uniq_dt <- unique(
        polygDT[, c("geom", "part", "hole", other_values), with = FALSE]
    )

    # apply smoothing to each polygon
    comb <- lapply(seq_along(unique(polygDT$geom)), function(z) {
        polygMat <- as.matrix(polygDT[geom == z, .(x, y)])

        # adjust k to maximum value
        max_k <- nrow(polygMat)
        if (k >= max_k) {
            wrap_msg("k will be set to ", max_k)
            k <- max_k
        }

        polygDT_smooth <- data.table::as.data.table(
            .spline_poly(polygMat, vertices = vertices, k = k, ...)
        )
        polygDT_smooth[, geom := z]
    })
    comb_res <- do.call("rbind", comb)


    # add other columns back
    comb_res <- data.table::merge.data.table(
        comb_res, other_values_uniq_dt,
        by = "geom"
    )
    comb_res <- comb_res[,
        c("geom", "part", "x1", "x2", "hole", other_values),
        with = FALSE
    ]
    colnames(comb_res)[3:4] <- c("x", "y")

    if (set_neg_to_zero == TRUE) {
        comb_res[, x := ifelse(x < 0, 0, x)]
        comb_res[, y := ifelse(y < 0, 0, y)]
    }

    new_spatvec <- .dt_to_spatvector_polygon(comb_res)

    new_gpolygon <- create_giotto_polygon_object(
        name = gpolygon@name,
        spatVector = new_spatvec,
        spatVectorCentroids = gpolygon@spatVectorCentroids
    )

    return(new_gpolygon)
}










## ** feature points ####







#' @title Create terra spatvector object from a data.frame
#' @name .create_spatvector_object_from_dfr
#' @description create terra spatvector from a data.frame where cols 1 and 2
#' must be x and y coordinates respectively. Additional columns are set as
#' attributes to the points where the first additional (col 3) should be the
#' feat_ID.
#' @param x data.frame object
#' @param x_colname column name for x-coordinates
#' @param y_colname column name for y-coordinates
#' @param feat_ID_colname column name for feature ids
#' @param verbose be verbose
#' @returns SpatVector
#' @keywords internal
.create_spatvector_object_from_dfr <- function(
        x,
        x_colname = NULL,
        y_colname = NULL,
        feat_ID_colname = NULL,
        verbose = TRUE) {
    x <- data.table::as.data.table(x)

    # MANUAL OPTION
    # user has defined 3 columns to be used as x-coordinates, y-coordinates
    # and feature ids

    # check if user selected a name for one of the columns
    if (!is.null(c(x_colname, y_colname, feat_ID_colname))) {
        # stop if one or more column names are missing
        if (list(NULL) %in% list(x_colname, y_colname, feat_ID_colname)) {
            stop("For manual selection of x, y, and feat_ID columns all
                column name need to be specified.\n")
        } else {
            if (!all(c(x_colname, y_colname, feat_ID_colname) %in%
                colnames(x))) {
                stop("Not all column names were found in the data.frame or
                    data.table.\n")
            }

            feat_ID_col <- which(colnames(x) == feat_ID_colname)
            x_col <- which(colnames(x) == x_colname)
            y_col <- which(colnames(x) == y_colname)
        }
    } else {
        # AUTOMATIC OPTION

        # data.frame like object needs to have 2 coordinate columns and
        # at least one other column as the feat_ID
        if (ncol(x) < 3) stop("At minimum, columns for xy coordinates and
                            feature ID are needed.\n")
        col_classes <- vapply(x, class, FUN.VALUE = character(1L))
        ## find feat_ID as either first character col or named column
        ## if not detected, select 3rd column
        if ("feat_ID" %in% colnames(x)) {
            feat_ID_col <- which(colnames(x) == "feat_ID")
        } else {
            feat_ID_col <- which(col_classes == "character")
            if (length(feat_ID_col) < 1) {
                feat_ID_col <- 3
            } # case if no char found: default to 3rd
            else {
                feat_ID_col <- feat_ID_col[[1]]
            } # case if char is found
        }



        ## find first two numeric cols as x and y respectively or named column
        ## if not detected select 1st and 2nd cols for x and y respectively
        if (all(c("x", "y") %in% colnames(x))) {
            x_col <- which(colnames(x) == "x")
            y_col <- which(colnames(x) == "y")
        } else {
            x_col <- which(col_classes == "numeric")
            if (length(x_col) < 2) {
                x_col <- 1
            } # case if no/too few num found: default to 1st
            else {
                x_col <- x_col[[1]]
            } # case if num found
            y_col <- which(col_classes == "numeric")
            if (length(y_col) < 2) {
                y_col <- 2
            } # case if no/too few num found: default to 2nd
            else {
                y_col <- y_col[[2]]
            } # case if num found
        }
    }

    ## message and force data type
    if (isTRUE(verbose)) {
        message(paste0(
            '  Selecting col "',
            colnames(x[, feat_ID_col, with = FALSE]),
            '" as feat_ID column'
        ))
    }
    colnames(x)[feat_ID_col] <- "feat_ID"
    if (!inherits(x$feat_ID, "character")) {
        x$feat_ID <- as.character(x$feat_ID) # ensure char
    }


    if (isTRUE(verbose)) {
        message(paste0(
            '  Selecting cols "',
            colnames(x[, x_col, with = FALSE]), '" and "',
            colnames(x[, y_col, with = FALSE]),
            '" as x and y respectively'
        ))
    }
    colnames(x)[x_col] <- "x"
    colnames(x)[y_col] <- "y"
    if (!inherits(x$x, "numeric")) x$x <- as.numeric(x$x) # ensure numeric
    if (!inherits(x$y, "numeric")) x$y <- as.numeric(x$y) # ensure numeric


    ## select location and attribute dataframes
    # Use unique() to set column order
    ordered_colnames <- unique(c("feat_ID", "x", "y", colnames(x)))
    x <- x[, ordered_colnames, with = FALSE]
    loc_dfr <- x[, 2:3]
    att_dfr <- x[, -c(2:3)]

    spatvec <- terra::vect(as.matrix(loc_dfr), type = "points", atts = att_dfr)

    # will be given and is a unique numerical barcode for each feature
    spatvec[["feat_ID_uniq"]] <- seq_len(nrow(spatvec))

    return(spatvec)
}










# ** feature networks ####

#' @title Create kNN spatial feature network using dbscan
#' @name createSpatialFeaturesKNNnetwork_dbscan
#' @description  to create a feature kNN spatial network using dbscan
#' @param gobject giotto object
#' @param feat_type feature type
#' @param name name to assign generated feature network
#' @param k number of neighbors for kNN to find
#' @param maximum_distance network maximum distance allowed
#' @param minimum_k minimum neighbors allowed
#' @param add_feat_ids boolean. whether to add feature information
#' @param verbose be verbose
#' @param ... additional parameters to pass to \code{\link[dbscan]{kNN}}
#' @returns kNN spatial feature network
#' @keywords internal
createSpatialFeaturesKNNnetwork_dbscan <- function(gobject,
    feat_type = NULL,
    name = "knn_feats_network",
    k = 4,
    maximum_distance = NULL,
    minimum_k = 0,
    add_feat_ids = FALSE,
    verbose = TRUE,
    ...) {
    # define for data.table
    from_feat <- from <- to_feat <- to <- from_to_feat <- NULL

    ## 1. specify feat_type
    if (is.null(feat_type)) {
        gobject@feat_info[[1]]@feat_type
    }

    ## 2. get spatial feature info and convert to matrix
    if (verbose == TRUE) wrap_msg("Convert feature spatial info to matrix")
    featDT <- .spatvector_to_dt(gobject@feat_info[[feat_type]]@spatVector)
    spatial_locations_matrix <- as.matrix(
        featDT[, c("x", "y", NULL), with = FALSE]
    )

    # store lookup table to keep information about unique ID
    # important with multiple joined objects where row id is not always equal
    # to unique gene
    network_id_lookup_table <- data.table::data.table(
        row = seq_len(nrow(featDT)),
        id = featDT$feat_ID_uniq
    )

    ## 3. create kNN network
    if (verbose == TRUE) wrap_msg("Create kNN network with dbscan")
    knn_spatial <- dbscan::kNN(
        x = spatial_locations_matrix,
        k = k,
        ...
    )

    knn_sptial.norm <- data.table::data.table(
        from = rep(seq_len(nrow(knn_spatial$id)), k),
        to = as.vector(knn_spatial$id),
        # weight = 1/(1 + as.vector(knn_spatial$dist)),
        distance = as.vector(knn_spatial$dist)
    )

    ## 3. keep minimum and filter
    if (verbose == TRUE) {
        wrap_msg("Filter output for distance and minimum neighbours")
    }
    knn_sptial.norm[, rank := seq_len(.N), by = "from"]

    if (minimum_k != 0) {
        filter_bool <- knn_sptial.norm$rank <= minimum_k
    } else {
        filter_bool <- rep(TRUE, nrow(knn_sptial.norm))
    }


    if (!is.null(maximum_distance)) {
        maximum_distance_bool <- knn_sptial.norm$distance <= maximum_distance
        filter_bool <- filter_bool + maximum_distance_bool
        filter_bool[filter_bool > 0] <- 1
        filter_bool <- as.logical(filter_bool)
    }


    knn_sptial.norm <- knn_sptial.norm[filter_bool]

    ## 3. add feature information and sort
    if (add_feat_ids == TRUE) {
        if (verbose == TRUE) wrap_msg("Add feat IDs and sort output")

        featDT_vec <- featDT$feat_ID
        names(featDT_vec) <- featDT$feat_ID_uniq

        knn_sptial.norm[, from_feat := featDT_vec[from]]
        knn_sptial.norm[, to_feat := featDT_vec[to]]
        knn_sptial.norm[, from_to_feat := paste0(from_feat, "--", to_feat)]

        knn_sptial.norm <- dt_sort_combine_two_columns(
            DT = knn_sptial.norm,
            column1 = "from_feat", column2 = "to_feat",
            myname = "comb_feat"
        )
    }


    knn_sptial.norm_object <- create_featureNetwork_object(
        name = name,
        network_datatable = knn_sptial.norm,
        network_lookup_id = network_id_lookup_table,
        full = FALSE
    )

    return(knn_sptial.norm_object)
}





#' @title Create kNN spatial feature network
#' @name createSpatialFeaturesKNNnetwork
#' @description Calculates the centroid locations for the giotto polygons
#' @param gobject giotto object
#' @param method kNN algorithm method
#' @param feat_type feature type to build feature network
#' @param name name of network
#' @param k number of neighbors
#' @param maximum_distance maximum distance bewteen features
#' @param minimum_k minimum number of neighbors to find
#' @param add_feat_ids add feature id
#' names (default = FALSE, increases object size)
#' @param verbose be verbose
#' @param return_gobject return giotto object (default: TRUE)
#' @param toplevel_params toplevel value to pass when updating giotto params
#' @param ... additional parameters to pass to \code{\link[dbscan]{kNN}}
#' @returns If \code{return_gobject = TRUE} a giotto object containing the
#'     network will be returned. If \code{return_gobject = FALSE} the network
#'     will be returned as a datatable.
#' @concept feature
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' createSpatialFeaturesKNNnetwork(g)
#' @export
createSpatialFeaturesKNNnetwork <- function(gobject,
    method = "dbscan",
    feat_type = NULL,
    name = "knn_feats_network",
    k = 4,
    maximum_distance = NULL,
    minimum_k = 0,
    add_feat_ids = FALSE,
    verbose = TRUE,
    return_gobject = TRUE,
    toplevel_params = 2,
    ...) {
    # 1. select feat_type
    if (is.null(feat_type)) {
        feat_type <- gobject@expression_feat[[1]]
    }

    # 2. select method
    method <- match.arg(method, choices = c("dbscan"))


    if (method == "dbscan") {
        knn_feat_network_obj <- createSpatialFeaturesKNNnetwork_dbscan(
            gobject = gobject,
            feat_type = feat_type,
            name = name,
            k = k,
            maximum_distance = maximum_distance,
            minimum_k = minimum_k,
            add_feat_ids = add_feat_ids,
            verbose = verbose,
            ...
        )
    }




    if (return_gobject == TRUE) {
        network_names <- names(gobject@feat_info[[feat_type]]@networks)

        if (name %in% network_names) {
            wrap_msg(name, " has already been used, will be overwritten")
        }

        gobject@feat_info[[feat_type]]@networks[[name]] <- knn_feat_network_obj


        ## update parameters used ##
        gobject <- update_giotto_params(gobject,
            description = "_featNetwork",
            return_gobject = TRUE,
            toplevel = toplevel_params
        )
        return(gobject)
    } else {
        return(knn_feat_network_obj@network_datatable)
    }
}






## * ####
## ** giotto structure functions ####


#' @title addSpatialCentroidLocationsLayer
#' @name addSpatialCentroidLocationsLayer
#' @description Calculates the centroid locations for the polygons within one
#' selected layer
#' @param gobject giotto object
#' @param poly_info polygon information
#' @param feat_type feature type
#' @param spat_loc_name name to give to the created spatial locations
#' @param provenance (optional) provenance to assign to generated spatLocsObj.
#' If not provided, provenance will default to \code{poly_info}
#' @param return_gobject return giotto object (default: TRUE)
#' @returns If \code{return_gobject = TRUE} the giotto object containing the
#'     calculated polygon centroids will be returned.
#'     If \code{return_gobject = FALSE} only the generated polygon centroids
#'     will be returned as spatLocsObj.
#' @concept centroid
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' addSpatialCentroidLocationsLayer(g, poly_info = "aggregate")
#' @export
addSpatialCentroidLocationsLayer <- function(gobject,
    poly_info = "cell",
    feat_type = NULL,
    provenance = poly_info,
    spat_loc_name = "raw",
    return_gobject = TRUE) {
    # data.table vars
    x <- y <- poly_ID <- NULL

    # Set feat_type and spat_unit
    poly_info <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = poly_info
    )
    # feat_type = set_default_feat_type(gobject = gobject,
    #                                   spat_unit = poly_info,
    #                                   feat_type = feat_type)
    feat_type <- slot(gobject, "expression_feat")[[1]] # Specifically
    # preferable over set_default function
    # There may be no existing data in expression slot to find feat_type
    # nesting from

    gpoly <- get_polygon_info(gobject,
        polygon_name = poly_info,
        return_giottoPolygon = TRUE
    )

    extended_spatvector <- .calculate_centroids_polygons(
        gpolygon = gpoly,
        name = "centroids",
        append_gpolygon = TRUE
    )

    centroid_spatvector <- .spatvector_to_dt(
        extended_spatvector@spatVectorCentroids
    )

    # this could be 3D
    spatial_locs <- centroid_spatvector[, .(x, y, poly_ID)]
    colnames(spatial_locs) <- c("sdimx", "sdimy", "cell_ID")

    spatial_locs <- create_spat_locs_obj(
        name = spat_loc_name,
        coordinates = spatial_locs,
        spat_unit = poly_info,
        provenance = provenance
    )

    if (return_gobject == TRUE) {
        # spatial location
        spat_locs_names <- list_spatial_locations_names(gobject,
            spat_unit = poly_info
        )
        if (spat_loc_name %in% spat_locs_names) {
            wrap_msg(
                '> spatial locations for polygon information layer "',
                poly_info,
                '" and name "', spat_loc_name,
                '" already exists and will be replaced\n'
            )
        }

        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <- set_spatial_locations(
            gobject = gobject,
            spatlocs = spatial_locs,
            verbose = FALSE
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


        # cell ID
        gpoly_IDs <- gpoly@spatVector$poly_ID
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <- set_cell_id(gobject,
            spat_unit = poly_info,
            cell_IDs = gpoly_IDs
        )
        # gobject@cell_ID[[poly_info]] = gobject@spatial_info[[poly_info
        # ]]@spatVector$poly_ID
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



        # add centroids information
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <- set_polygon_info(gobject,
            polygon_name = poly_info,
            gpolygon = extended_spatvector,
            verbose = FALSE
        )
        # gobject@spatial_info[[poly_info]] = extended_spatvector
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


        return(gobject)
    } else {
        return(spatial_locs)
    }
}


#' @title addSpatialCentroidLocations
#' @name addSpatialCentroidLocations
#' @description Calculates the centroid locations for the polygons within one
#' or more selected layers
#' @param gobject giotto object
#' @param poly_info polygon information
#' @param feat_type feature type
#' @param spat_loc_name name to give to the created spatial locations
#' @param provenance (optional) provenance to assign to generated spatLocsObj.
#' If not provided, provenance will default to \code{poly_info}
#' @param return_gobject return giotto object (default: TRUE)
#' @param verbose be verbose
#' @returns If \code{return_gobject = TRUE} the giotto object containing the
#'     calculated polygon centroids will be returned.
#'     If \code{return_gobject = FALSE} only the generated polygon centroids
#'     will be returned as \code{spatLocObj}.
#' @concept centroid
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' addSpatialCentroidLocations(g, poly_info = "aggregate")
#' @export
addSpatialCentroidLocations <- function(gobject,
    poly_info = "cell",
    feat_type = NULL,
    spat_loc_name = "raw",
    provenance = poly_info,
    return_gobject = TRUE,
    verbose = TRUE) {
    # provenance setup #
    # Require that provenance is a user-provided named list if length of
    # poly_info is greater than 1.
    # Provenance may often have length greater than 1, but map to a single
    # spat_unit, however at least one provenance is expected per spat_unit. We
    # differentiate these situations by ensuring that each poly_info/spat_unit
    # maps to an entry within a list object. The entry within that list may be
    # a character vector of length greater than 1.
    if (length(poly_info) > 1) {
        if (!inherits(provenance, "list") ||
            length(provenance) != length(poly_info)) {
            stop(wrap_txt(
                "If more than one poly_info is supplied at a time, then
                provenance must",
                "be a list of equal length",
                errWidth = TRUE
            ))
        }
    }

    # Ensure that provenance is a list in remaining cases
    if (!inherits(provenance, "list")) {
        provenance <- list(provenance)
    }

    # name provenance list by poly_info
    p_names <- names(provenance)
    if (is.null(p_names)) names(provenance) <- poly_info
    if (!setequal(names(provenance), poly_info)) {
        stop(wrap_txt(
            "Names of provenance list:", names(provenance),
            "\nBut expected from poly_info:", poly_info
        ))
    }

    potential_polygon_names <- list_spatial_info_names(gobject)

    return_list <- list()

    for (poly_layer in unique(poly_info)) {
        if (!poly_layer %in% potential_polygon_names) {
            warning(
                "Polygon info layer with name ", poly_layer,
                " has not been found and will be skipped"
            )
        } else {
            if (verbose == TRUE) {
                wrap_msg(
                    "Start centroid calculation for polygon information
                    layer: ",
                    poly_layer, "\n"
                )
            }

            if (return_gobject == TRUE) {
                gobject <- addSpatialCentroidLocationsLayer(
                    gobject = gobject,
                    poly_info = poly_layer,
                    feat_type = feat_type,
                    provenance = provenance[[poly_layer]],
                    spat_loc_name = spat_loc_name,
                    return_gobject = return_gobject
                )
            } else {
                return_list[[poly_layer]] <- addSpatialCentroidLocationsLayer(
                    gobject = gobject,
                    poly_info = poly_layer,
                    feat_type = feat_type,
                    provenance = provenance[[poly_layer]],
                    spat_loc_name = spat_loc_name,
                    return_gobject = return_gobject
                )
            }
        }
    }

    if (isTRUE(return_gobject)) {
        return(gobject)
    } else {
        return_list
    }
}
