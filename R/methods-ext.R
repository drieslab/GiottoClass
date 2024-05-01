## ext ####

#' @name ext
#' @aliases ext<-
#' @title Get a SpatExtent
#' @description Get a SpatExtent of an object. This is the spatial minmax x
#' and y that the object is mapped to.
#' @param x spatial object
#' @param value value to set. Accepts any object that `ext()` will work on
#' @param ... additional params to pass
#' @returns SpatExtent
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#' # giotto %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' ext(g)
#'
#' # spatLocsObj %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' sl <- getSpatialLocations(g)
#' ext(sl)
#'
#' # giottoPolygon %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # get extent
#' #' gpoly <- getPolygonInfo(g, return_giottoPolygon = TRUE)
#' ext(gpoly)
#'
#' # set extent
#' plot(gpoly) # before
#' ext(gpoly) <- ext(0, 20, 30, 60)
#' plot(gpoly) # after
#'
#' # giottoPoints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # get extent
#' gpoints <- getFeatureInfo(g, return_giottoPoints = TRUE)
#' ext(gpoints)
#'
#' # set extent
#' plot(gpoints) # before
#' ext(gpoints) <- ext(0, 2000, 3000, 6000)
#' plot(gpoints) # after
NULL


#' @rdname ext
#' @export
setMethod("ext", signature("spatLocsObj"), function(x, ...) {
    sdimx <- sdimy <- NULL # dt vars
    terra::ext(c(range(x[][, sdimx]), range(x[][, sdimy])))
})

#' @rdname ext
#' @export
setMethod("ext", signature("giottoPolygon"), function(x, ...) {
    terra::ext(x@spatVector, ...)
})

#' @rdname ext
#' @export
setMethod("ext", signature("giottoPoints"), function(x, ...) {
    terra::ext(x@spatVector, ...)
})

#' @rdname ext
#' @export
setMethod("ext", signature("spatialNetworkObj"), function(x, ...) {
    sdimx_begin <- sdimx_end <- sdimy_begin <- sdimy_end <- NULL # dt vars
    terra::ext(c(
        x[][, range(c(sdimx_begin, sdimx_end))],
        x[][, range(c(sdimy_begin, sdimy_end))]
    ))
})

#' @rdname ext
#' @export
setMethod("ext", signature("giottoLargeImage"), function(x, ...) {
    terra::ext(x@raster_object)
})

#' @rdname ext
#' @export
setMethod("ext", signature("giottoImage"), function(x, ...) {
    terra::ext((x@boundaries + x@minmax)[c(2, 1, 4, 3)])
})

#' @rdname ext
#' @param spat_unit character. spatial unit (optional)
#' @param feat_type character. feature type (optional)
#' @param prefer character vector. Order of preferred data to get extent from.
#' allowed terms are "polygon", "spatlocs", "points". This is also the default
#' ordering.
#' @export
setMethod("ext", signature("giotto"), function(
        x,
        spat_unit = NULL,
        feat_type = NULL,
        prefer = c("polygon", "spatlocs", "points"),
        verbose = NULL,
        ...
) {
    dots <- list(...)

    spat_unit = set_default_spat_unit(
        gobject = x,
        spat_unit = spat_unit
    )
    feat_type <- set_default_feat_type(
        gobject = x,
        spat_unit = spat_unit,
        feat_type = feat_type
    )

    has_poly <- spat_unit %in% list_spatial_info_names(x)
    has_ctrs <- spat_unit %in% list_spatial_locations(x)$spat_unit
    has_pnts <- feat_type %in% list_feature_info(x)$feat_info

    if (sum(has_poly, has_ctrs, has_pnts) == 0) {
        vmsg(.v = verbose, "No spatial info in giotto object")
        return(invisible())
    }

    # find first available type of info in gobject according to `prefer`
    pref_order <- c()
    for (ptype in prefer) {
        pref_order <- c(
            pref_order,
            switch(ptype,
                "polygon" = c(polygon = has_poly),
                "spatlocs" = c(spatlocs = has_ctrs),
                "points" = c(points = has_pnts)
            )
        )
    }
    use_type <- names(which(pref_order)[1L])

    spat_obj <- switch(use_type,
        "polygon" = getPolygonInfo(
            gobject = x,
            polygon_name = spat_unit,
            return_giottoPolygon = TRUE,
            verbose = verbose
        ),
        "spatlocs" = getSpatialLocations(
            gobject = x,
            spat_unit = spat_unit,
            output = "spatLocsObj",
            copy_obj = FALSE,
            set_defaults = TRUE,
            name = dots$name,
            verbose = verbose
        ),
        "points" = getFeatureInfo(
            gobject = x,
            feat_type = feat_type,
            return_giottoPoints = TRUE,
            verbose = verbose
        )
    )

    e <- ext(spat_obj)
    return(e)
})











#' @rdname ext
#' @export
setMethod(
    "ext<-", signature(x = "giottoPoints", value = "SpatExtent"),
    function(x, value) {
        old_ext <- .ext_to_num_vec(ext(x))
        new_ext <- .ext_to_num_vec(value)
        xy_scale <- c(
            diff(new_ext[c(2, 1)]) / diff(old_ext[c(2, 1)]),
            diff(new_ext[c(4, 3)]) / diff(old_ext[c(4, 3)])
        )
        x@spatVector <- terra::rescale(x@spatVector,
            fx = xy_scale[1],
            fy = xy_scale[2], x0 = old_ext[1L],
            y0 = old_ext[3L]
        )
        x <- spatShift(x,
            dx = new_ext[1L] - old_ext[1L],
            dy = new_ext[3L] - old_ext[3L]
        )
        x
    }
)

#' @rdname ext
#' @export
setMethod(
    "ext<-", signature(x = "giottoPolygon", value = "SpatExtent"),
    function(x, value) {
        old_ext <- .ext_to_num_vec(ext(x))
        new_ext <- .ext_to_num_vec(value)
        xy_scale <- c(
            diff(new_ext[c(2, 1)]) / diff(old_ext[c(2, 1)]),
            diff(new_ext[c(4, 3)]) / diff(old_ext[c(4, 3)])
        )
        x <- .do_gpoly(x, terra::rescale,
            args = list(
                fx = xy_scale[1], fy = xy_scale[2],
                x0 = old_ext[1L], y0 = old_ext[3L]
            )
        )
        x <- spatShift(x,
            dx = new_ext[1L] - old_ext[1L], dy = new_ext[3L] - old_ext[3L]
        )
        x
    }
)

#' @rdname ext
#' @export
setMethod("ext<-", signature(
    x = "giottoLargeImage",
    value = "SpatExtent"
), function(x, value) {
    terra::ext(x@raster_object) <- value
    x@extent <- value
    x
})

# Convert numeric inputs to SpatExtent and have terra deal with inconsistencies
#' @rdname ext
#' @export
setMethod("ext<-", signature(x = "ANY", value = "ANY"), function(x, value) {
    value <- terra::ext(value)
    methods::callGeneric(x, value)
})

# Helper function to convert a SpatExtent object to a simple numeric vector
# and strip the names
.ext_to_num_vec <- function(x) {
    out <- x[]
    names(out) <- NULL
    out
}
