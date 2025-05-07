## * spatial queries ####


# If the polys are to be clipped, then the returned info MUST be a new polygon
# object

#' @title Spatial Query
#' @name spatQuery
#' @description Select spatial geometries based on a list of spatial `filters`.
#' The final item in provided in the list is the layer of information
#' being queried.\cr
#' By default, results will be returned as a new polygon-based spatial unit
#' with selection information recorded in the associated cell metadata. \cr
#' Spatial queries may perform queries on the geometries themselves, so
#' `intersect()` operations are performed under the hood. For a lighter weight
#' option that just finds spatial relationships, see [relate()]
#' @param gobject `giotto` object
#' @param filters named list of characters and/or `giottoPolygons` to use as
#' spatial filters for the final item in the list.
#'
#' * \[`character`\] list name designates the gobject spatial unit to use as a
#' filter. The actual character values should either be `"all"` or a specific
#' vector of cell_IDs to use.
#' * \[`giottoPolygon`\] inputs are directly used as filters. List names are
#' used when reporting the spatial relationships in output geometry objects.
#' These can also be used as centroids and additionally can be buffered.
#' * \[`SpatVector`] inputs are directly used. Can also be converted to
#' centroids and/or buffered.
#' * \[`numeric`\] input is read as XY pairs (e.g. `c(x1, y1, x2, y2, ...)`),
#' to be used as centroids. These are bufferable.
#' * \['spatLocsObj'\] inputs are directly used as centroids. These are
#' bufferable.
#' @param name (optional) character. If not `NULL`, a new spatial unit of this
#' name will be generated from the results.
#' @param clip logical. Default = `TRUE`. Whether final round of querying should
#' produce polygons clipped by the polygons used to select them.
#' @param use_centroids character vector. Values must correspond to names in
#' `filters`. Selected `filters` will be converted to centroids. (prefers
#' usage of the first set of spatlocs for that spat_unit)
#' @param buffer numeric. Or named vector of numerics. Names must correspond to
#' those in `centroids`. Applies the specified buffer to the centroid to allow
#' it to be used in `filter`. A `0` will skip buffering, but this is only
#' permitted if is also the the last item in `filter`. Unbuffered points may
#' only return results as IDs (`return_ids = TRUE`). Do note that buffering on
#' a large number of elements can cause significant slowdowns.
#' @param combine_fragments logical. (default = `FALSE`). Whether to combine
#' geoms fragmented by the intersections as multipolygons based on the
#' `poly_ID` col. If `TRUE`, the operation may introduce `NA`s in the spatial
#' relationship information.
#' @param dissolve logical. (default = `FALSE`). If `combine_fragments = TRUE`,
#' whether to also merge the multipolygon into a single polygon.
#' @param return_table logical. (Default = `FALSE`) Overrides `return__object`.
#' If `TRUE`, return only the relationships as a `data.table`
#' @param return_ids logical. (Default = `FALSE`) Overrides `return_gobject`.
#' If `TRUE`, return only the poly_IDs of the final entry in `filters`
#' @param return_gobject logical. (Default = `TRUE)`. Whether to return the new
#' set of polygons attached to the giotto object.
#' @param verbose verbosity
#' @returns `character` (IDs), `giottoPolygon`, or `giotto` depending on
#' `return_ids` and `return_gobject`.
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#' pz0 <- getPolygonInfo(g, "z0")
#' boxgrid <- tessellate(
#'     extent = ext(g),
#'     shape = "square",
#'     shape_size = 50,
#'     name = "boxgrid"
#' )
#' hexarray <- tessellate(
#'     extent = ext(g),
#'     shape = "hexagon",
#'     shape_size = 80,
#'     name = "hexarray"
#' )
#' g <- setGiotto(g, boxgrid)
#' g <- setGiotto(g, hexarray)
#'
#' hex_ids <- sprintf("ID_%d", c(1, 3, 6, 8, 17, 19, 23))
#' box_ids <- sprintf("ID_%d", c(12, 14, 15, 16, 22, 41, 44, 45, 51, 52, 62))
#'
#' g <- spatQueryGiottoPolygons(g,
#'     filters = list(
#'         hexarray = hex_ids,
#'         boxgrid = box_ids,
#'         z0 = "all"
#'     ),
#'     return_gobject = TRUE
#' )
#' # extract polys since we attached it to the giotto object
#' qp <- g[[, "query_polys"]][[1]]
#'
#' qp2 <- spatQueryGiottoPolygons(g,
#'     filters = list(
#'         hexarray = hex_ids[3],
#'         boxgrid = box_ids,
#'         z0 = "all"
#'     ),
#'     buffer = c(hexarray = 150),
#'     return_gobject = FALSE
#' )
#'
#' # check that extracted polys are being clipped as expected
#' plot(pz0)
#' plot(hexarray[hex_ids], border = "blue", add = TRUE)
#' plot(boxgrid[box_ids], add = TRUE, border = "red")
#' plot(qp, col = rainbow(20), add = TRUE) # selection by hex and box
#' plot(buffer(hexarray[hex_ids[3]], width = 150), add = TRUE) # buffered hex
#' plot(qp2, col = "black", add = TRUE) # selection by buffered hex and box
#'
#' # query for polys that fall within 100 units of a point
#' res <- spatQueryGiottoPolygons(g,
#'     filters = list(
#'         pts = c(6500, -4900),
#'         z0 = "all"
#'     ),
#'     buffer = c(pts = 100),
#'     return_gobject = FALSE,
#'     make_valid = TRUE,
#'     clip = FALSE
#' )
#'
#' pt_buffer <- createSpatLocsObj(c(6500, -4900)) %>%
#'     terra::as.points() %>%
#'     buffer(100)
#'
#' plot(pz0)
#' plot(pt_buffer, add = TRUE, border = "dodgerblue") # the selecting shape.
#' # note that clip = FALSE for this selection
#' plot(res, col = "red", add = TRUE)
#'
#' # only return the ids
#' ids <- spatQueryGiottoPolygons(g,
#'     filters = list(
#'         pts = c(6500, -4900),
#'         z0 = "all"
#'     ),
#'     buffer = c(pts = 100),
#'     return_ids = TRUE,
#'     make_valid = TRUE
#' )
#' head(ids)
#' length(ids)
#'
#' # only return the table of relations
#' tab <- spatQueryGiottoPolygons(g,
#'     filters = list(
#'         hexarray = hex_ids,
#'         boxgrid = box_ids,
#'         z0 = "all"
#'     ),
#'     return_table = TRUE,
#'     make_valid = TRUE
#' )
#' force(tab)
#'
#' @seealso [relate()]
#' @export
spatQuery <- function(gobject,
    filters,
    name = "query_polys",
    clip = TRUE,
    use_centroids = NULL,
    buffer = 0,
    make_valid = FALSE,
    combine_fragments = FALSE,
    dissolve = FALSE,
    return_table = FALSE,
    return_ids = FALSE,
    return_gobject = TRUE,
    verbose = NULL) {
    # input type validation -------------------------------------------- #
    if (!missing(gobject)) assert_giotto(gobject)
    if (!is.null(name)) checkmate::assert_character(name)
    checkmate::assert_list(filters,
        types = c("character", "giottoPolygon", "spatLocsObj", "numeric",
                  "integer", "SpatVector")
    )
    checkmate::assert_character(use_centroids, null.ok = TRUE)
    checkmate::assert_numeric(buffer)
    checkmate::assert_character(name)
    checkmate::assert_logical(clip)
    checkmate::assert_logical(return_ids)
    checkmate::assert_logical(combine_fragments)
    checkmate::assert_logical(return_gobject)

    # more specific checks on inputs  ----------------------------------- #
    if (length(filters) < 2L) {
        stop(wrap_txt("At least two elements in filters are needed."),
             call. = FALSE)
    }
    # `filters` input must be named.
    filter_names <- names(filters)
    if (any(vapply(filter_names, is_empty_char, FUN.VALUE = logical(1L)))) {
        stop(wrap_txt("All elements in filters list must be named"),
             call. = FALSE)
    }
    if (!is.null(use_centroids)) {
        if (!all(use_centroids %in% filter_names)) {
            stop("all entries in `use_centroids` must be names in `filters`\n",
                 call. = FALSE)
        }
    }
    if (length(buffer) > 1L) {
        buffer_names <- names(buffer)
        if (is.null(buffer_names)) {
            stop("if multiple `buffer` values given, they must be named\n",
                 call. = FALSE)
        }
        if (!all(buffer_names %in% filter_names)) {
            stop("all names for `buffer` values must be names in `filters`\n",
                 call. = FALSE)
        }
    }

    # main ---------------------------------------------------------- #
    last_info <- tail(filter_names, 1L) # name of final filter layer
    if (is.null(name)) name <- last_info

    # check buffer behavior
    for (f_i in seq_along(filters)) {
        b_res <- .check_filter_buffer_allowed(
            i = f_i,
            filters = filters,
            buffer = buffer
        )
    }

    # contains all logic for getting the ith filter SpatVector
    .filter_get <- function(i) {
        fname <- filter_names[[i]]
        .squery_get_sv(
            x = filters[[fname]],
            gobject = gobject,
            spat_unit = fname,
            centroids = fname %in% use_centroids, # logical
            buffer = .squery_guess_buffer(buffer, fname)
        )
    }

    # iterate intersections
    # sv1 is the filter poly (or result of previous intersect iteration)
    # sv2 is the data poly
    sv1 <- .filter_get(1L) # get initial sv1
    if (make_valid) sv1 <- terra::makeValid(sv1)
    for (f_i in 2:length(filters)) {
        sv2 <- .filter_get(f_i)
        vmsg(.v = verbose, sprintf("processing [%s] vs [%s]...",
            filter_names[f_i - 1L], filter_names[f_i]
        ))
        if (make_valid) sv2 <- terra::makeValid(sv2)
        sv1 <- terra::intersect(sv1, sv2)
    }

    # update colnames of output geoms
    is_pid_idx <- which(names(sv1) == "poly_ID")
    names(sv1)[is_pid_idx] <-
        c(filter_names[seq_len(length(filter_names) - 1L)], "poly_ID")
    # reorder with "poly_ID" col first
    sv1 <- sv1[, unique(c(tail(is_pid_idx, 1L), is_pid_idx))]

    if (return_table) {
        return(data.table::as.data.table(sv1))
    }

    uids <- unique(sv1$poly_ID)
    if (return_ids) return(uids)

    # if NOT clip, return the original polys that are selected.
    if (!clip) {
        sv1 <- .filter_get(length(filters))
        sv1 <- sv1[sv1$poly_ID %in% uids]
    }

    # package as giottoPolygon
    poly <- giottoPolygon(
        spatVector = sv1,
        name = name,
        unique_ID_cache = uids
    )

    if (combine_fragments && clip) {
        poly[] <- terra::aggregate(poly[],
            by = "poly_ID",
            dissolve = dissolve
        )
    }

    if (!return_gobject) return(poly)

    # set values
    gobject <- setPolygonInfo(gobject = gobject, x = poly, initialize = FALSE)

    return(gobject)
}

# internals ####

# f name of filter
# fset set of all filter names in order
# buffer numeric. buffering value to use for centroids
.check_filter_buffer_allowed <- function(
        i, filters, buffer) {
    fname <- names(filters)[[i]]
    is_sv_points <- if (inherits(filters[[i]], "SpatVector")) {
        terra::is.points(filters[[i]])
    } else {
        FALSE
    }
    is_point_class <- inherits(
        filters[[i]], c("numeric", "integer", "spatLocsObj")
    )

    need_id <- FALSE
    has_buffer <- .squery_guess_buffer(buffer, fname) > 0

    # checks are only relevant for point classes.
    # poly can be buffered or not whenever
    if (!is_point_class) return(res)

    if (i == length(filters)) { # if last filter
        if (!has_buffer) {
            # only IDs return is allowed
            stop(wrap_txt(
                "final layer of query is centroids and buffer to use is 0",
                "Please use return_ids = TRUE"), call. = FALSE)
        }
    } else if (!has_buffer) { # not last but has no buffer
        # not allowed.
        stop(wrap_txtf(
            "'%s' is not the last layer of query.
                Assigned 'buffer' may not be 0", fname
        ), call. = FALSE)
    }
}

#' @describeIn spatQuery deprecated alias.
#' @export
spatQueryGiottoPolygons <- spatQuery


# function to get subsetted spatvector
# `x` is the element from the filter list (may be an object or IDs to use)
# `centroids` is a logical for whether to use spatlocs/centroids instead of poly
# `buffer` is a logical. When centroids are used, the amount of buffer to apply

.squery_get_sv <- function(x, ...) {
    UseMethod(".squery_get_sv")
}

.squery_get_sv.default <- function(x, ...) {
    stop(wrap_txt("[spatQuery] unrecognized filter input type:", class(x)),
         call. = FALSE)
}

.squery_get_sv.character <- function(x, gobject, centroids, spat_unit, ...) {
    x <- .squery_get_sv_handle_char(
        x = x,
        gobject = gobject,
        centroids = centroids,
        spat_unit = spat_unit
    )
    .squery_get_sv(x, ...)
}

.squery_get_sv.giottoPolygon <- function(x, centroids, ...) {
    x <- x[] # coerce to sv poly
    if (centroids) {
        x <- centroids(x)
    }
    .squery_get_sv(x, ...)
}

.squery_get_sv.numeric <- function(x, ...) {
    x <- createSpatLocsObj(x, verbose = FALSE)
    # setup default IDs
    x[]$cell_ID <- sprintf("point_%d", nrow(x))
    .squery_get_sv(x, ...)
}

.squery_get_sv.spatLocsObj <- function(x, ...) {
    x <- .squery_sl_to_svpts(x)
    .squery_get_sv(x, ...)
}

.squery_get_sv.SpatVector <- function(x, buffer, ...) {
    if (buffer > 0) {
        x <- buffer(x, width = buffer)
    }
    x
}



.squery_get_sv_handle_char <- function(gobject, centroids, spat_unit, x) {
    avail_poly <- list_spatial_info_names(gobject)
    sv <- NULL # initialize as NULL
    if (centroids) {
        avail_sl <- list_spatial_locations_names(gobject,
            spat_unit = spat_unit
        )
        if (spat_unit %in% avail_sl) { # centroid from spatlocs
            sv <- getSpatialLocations(gobject,
                spat_unit = spat_unit,
                output = "spatLocsObj"
            )
            sv <- .squery_sl_to_svpts(sv)
        } else if (spat_unit %in% avail_poly) { # centroids from SpatVector
            sv <- getPolygonInfo(
                gobject = gobject,
                polygon_name = spat_unit,
                return_giottoPolygon = FALSE
            )
            sv <- centroids(sv)
        }
        # if in neither, spatlocs or poly, sv remains as NULL.
    } else if (spat_unit %in% avail_poly) {
        sv <- getPolygonInfo(
            gobject = gobject,
            polygon_name = spat_unit,
            return_giottoPolygon = FALSE
        )
    }

    if (is.null(sv)) {
        stop(sprintf("Requested filter '%s' not found in giotto object\n", x),
             call. = FALSE)
    }

    # filter by x if needed
    if (identical(x, "all")) {
        return(sv) # x = "all" is passed, use all
    } else {
        return(sv[sv$poly_ID %in% x]) # otherwise, filter by x ids
    }
}

# convert spatlocs to expected spatvector pts.
.squery_sl_to_svpts <- function(x) {
    x <- as.points(x)
    id_idx <- which(names(x) == "cell_ID")
    names(x)[id_idx] <- "poly_ID" # rename IDs to match
    x
}

.squery_guess_buffer <- function(b, spat_unit) {
    if (length(b) == 1 && is.null(names(b))) return(b)
    if (!spat_unit %in% names(b)) return(0)
    b[[spat_unit]]
}
