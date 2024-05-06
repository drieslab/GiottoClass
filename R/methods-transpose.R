## transpose ####

# S4 methods
#' @title Transpose
#' @name transpose-generic
#' @param x object to be transposed
#' @aliases t
#' @returns transposed object
#' @examples
#' m <- matrix(rnorm(10), nrow = 5)
#'
#' t(m)
NULL



#' @rdname transpose-generic
#' @export
setMethod(
    "t", signature("giotto"),
    function(x) {
        # spat_unit and feat_type params are not allowed since t() has no ...
        # param

        # polygons --------------------------------------------------------- #
        poly <- get_polygon_info_list(
            gobject = x, return_giottoPolygon = TRUE
        )
        if (!is.null(poly)) {
            for (p in poly) {
                p <- do.call(t, args = list(x = p))
                x <- setPolygonInfo(x, p, verbose = FALSE, initialize = FALSE)
            }
        }

        # spatlocs --------------------------------------------------------- #
        sls <- get_spatial_locations_list(
            gobject = x,
            spat_unit = ":all:",
            output = "spatLocsObj",
            copy_obj = FALSE
        )
        if (!is.null(sls)) {
            for (sl in sls) {
                sl <- do.call(t, args = list(x = sl))
                x <- setSpatialLocations(x, sl,
                    verbose = FALSE,
                    initialize = FALSE
                )
            }

            # TODO remove this after spatial info is removed from
            # spatialNetwork objs
            sn_list <- get_spatial_network_list(
                gobject = x,
                spat_unit = ":all:",
                output = "spatialNetworkObj",
                copy_obj = FALSE
            )
            if (length(sn_list) > 0) {
                warning(wrap_txt(
                    "spatial locations have been modified.
          Relevant spatial networks may need to be regenerated"
                ), call. = FALSE)
            }
        }



        # points ----------------------------------------------------------- #
        pts <- get_feature_info_list(
            gobject = x, return_giottoPoints = TRUE
        )
        if (!is.null(pts)) {
            for (pt in pts) {
                pt <- do.call(t, args = list(x = pt))
                x <- setFeatureInfo(x, pt, verbose = FALSE, initialize = FALSE)
            }
        }


        return(initialize(x)) # init not necessarily needed
    }
)


#' @rdname transpose-generic
#' @export
setMethod("t", signature("spatLocsObj"), function(x) {
    sdimy <- sdimx <- NULL
    x <- data.table::copy(x)
    x@coordinates[, c("sdimx", "sdimy") := .(sdimy, sdimx)]
    return(x)
})
#' @rdname transpose-generic
#' @export
setMethod("t", signature("spatialNetworkObj"), function(x) {
    sdimx_begin <- sdimx_end <- sdimy_begin <- sdimy_end <- NULL
    x <- data.table::copy(x)
    x@networkDT[, c("sdimx_begin", "sdimy_begin", "sdimx_end", "sdimy_end") := .(sdimy_begin, sdimx_begin, sdimy_end, sdimx_end)]
    if (!is.null(x@networkDT_before_filter)) {
        x@networkDT_before_filter[, c(
            "sdimx_begin", "sdimy_begin",
            "sdimx_end", "sdimy_end"
        ) := .(
            sdimy_begin, sdimx_begin, sdimy_end,
            sdimx_end
        )]
    }
    return(x)
})
#' @rdname transpose-generic
#' @export
setMethod("t", signature("giottoPoints"), function(x) {
    x[] <- t(x[])
    x
})
#' @rdname transpose-generic
#' @export
setMethod("t", signature("giottoPolygon"), function(x) {
    x <- .do_gpoly(x, "t")
    x
})

# s3 methods
#' @rdname transpose-generic
#' @method t spatLocsObj
#' @export
t.spatLocsObj <- function(x) {
    sdimy <- sdimx <- NULL
    x <- data.table::copy(x)
    x@coordinates[, c("sdimx", "sdimy") := .(sdimy, sdimx)]
    return(x)
}


#' @rdname transpose-generic
#' @method t spatialNetworkObj
#' @export
t.spatialNetworkObj <- function(x) {
    sdimx_begin <- sdimx_end <- sdimy_begin <- sdimy_end <- NULL
    x <- data.table::copy(x)
    x@networkDT[, c("sdimx_begin", "sdimy_begin", "sdimx_end", "sdimy_end") := .(sdimy_begin, sdimx_begin, sdimy_end, sdimx_end)]
    if (!is.null(x@networkDT_before_filter)) {
        x@networkDT_before_filter[, c(
            "sdimx_begin", "sdimy_begin",
            "sdimx_end", "sdimy_end"
        ) := .(
            sdimy_begin, sdimx_begin, sdimy_end,
            sdimx_end
        )]
    }
    return(x)
}
