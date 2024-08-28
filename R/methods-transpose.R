## transpose ####

# S4 methods
#' @title Transpose
#' @name transpose
#' @description
#' Spatially transpose an object
#' @param x object to be transposed
#' @aliases t
#' @returns transposed object
#' @examples
#' sl <- GiottoData::loadSubObjectMini("spatLocsObj")
#'
#' plot(t(sl))
NULL


#* giotto ####
#' @rdname transpose
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
        }

        # spatnets --------------------------------------------------------- #
        # TODO remove this after spatial info is removed from
        # spatialNetwork objs
        sn_list <- get_spatial_network_list(
            gobject = x,
            spat_unit = ":all:",
            output = "spatialNetworkObj",
            copy_obj = FALSE
        )
        if (length(sn_list) > 0) {
            for (sn in sn_list) {
                sn <- t(sn)
                x <- setGiotto(x, sn, verbose = FALSE, initialize = FALSE)
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
        
        # images ----------------------------------------------------------- #
        imgs <- get_giotto_image_list(x)
        if (!is.null(imgs)) {
            for (img in imgs) {
                img <- t(img)
                x <- setGiotto(x, img, verbose = FALSE)
            }
        }

        return(initialize(x)) # init not necessarily needed
    }
)

# * spatLocsObj ####
#' @rdname transpose
#' @export
setMethod("t", signature("spatLocsObj"), function(x) {
    sdimy <- sdimx <- NULL
    x <- data.table::copy(x)
    x@coordinates[, c("sdimx", "sdimy") := .(sdimy, sdimx)]
    return(x)
})

# * spatialNetworkObj ####
#' @rdname transpose
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

# * giottoPoints ####
#' @rdname transpose
#' @export
setMethod("t", signature("giottoPoints"), function(x) {
    x[] <- t(x[])
    x
})

# * giottoPolygon ####
#' @rdname transpose
#' @export
setMethod("t", signature("giottoPolygon"), function(x) {
    x <- .do_gpoly(x, "t")
    x
})

# * giottoLargeImage ####
#' @rdname transpose
#' @export
setMethod("t", signature("giottoLargeImage"), function(x) {
    x <- as(x, "giottoAffineImage") # convert to giottoAffineImage
    x <- t(x)
    return(x)
})

# * giottoAffineImage ####
#' @rdname transpose
#' @export
setMethod("t", signature("giottoAffineImage"), function(x) {
    aff <- x@affine
    # update affine
    x@affine <- t(aff)
    
    return(initialize(x))
})

# * affine2d ####
#' @rdname transpose
#' @export
setMethod("t", signature("affine2d"), function(x) {
    x <- flip(x, direction = "vertical")
    spin(x, -90, x0 = 0, y0 = 0)
})



# s3 methods
#' @rdname transpose
#' @method t spatLocsObj
#' @export
t.spatLocsObj <- function(x) {
    sdimy <- sdimx <- NULL
    x <- data.table::copy(x)
    x@coordinates[, c("sdimx", "sdimy") := .(sdimy, sdimx)]
    return(x)
}


#' @rdname transpose
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
