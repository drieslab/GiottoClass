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
#' ext(g) # defaults to checking first giottoPolygon extent
#' ext(g, prefer = "spatlocs") # check first spatLocsObj extent
#' # first spatLocsObj from a different spat_unit
#' ext(g, spat_unit = "aggregate", prefer = "spatlocs")
#'
#' # from first image object
#' ext(g, prefer = "image")
#'
#' # add a dummy image with different spatial extent
#' r <- terra::rast(array(seq(25), dim = c(5, 5)))
#' test <- createGiottoLargeImage(r)
#' ext(test) <- c(1e5, 1.1e5, 0, 10)
#' g <- setGiotto(g, test) # add image
#'
#' # combined from all image objects
#' ext(g, prefer = "image", name = list(images = list_images_names(g)))
#'
#' # combined from all spatial data types in giotto object
#' ext(g, all_data = TRUE, name = list(images = list_images_names(g)))
#'
#' # spatLocsObj %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' sl <- getSpatialLocations(g)
#' ext(sl)
#'
#' # giottoPolygon %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # get extent
#' gpoly <- getPolygonInfo(g, return_giottoPolygon = TRUE)
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
    # invert negative adjustments
    adj <- x@boundaries
    adj[c(2, 4)] <- -adj[c(2, 4)]
    terra::ext((adj + x@minmax)[c(2, 1, 4, 3)])
})

#' @rdname ext
#' @param spat_unit character. Spatial unit to limit search to. If not provided,
#' a default will be set.
#' @param feat_type character. Feature type to limit search to for "points"
#' information. If not provided, a default will be set.
#' @param all_data logical. When TRUE (default), all spatial information
#' designated by `prefer` will be searched and a combined `SpatExtent` will be
#' returned. When FALSE, only the `SpatExtent` of the first existing data as
#' ordered by `prefer` will be returned.
#' @param prefer character vector. Order of preferred data to get extent from.
#' allowed terms are "polygon", "spatlocs", "points", "images". This is also the
#' default ordering. Omitting terms removes them from the search.
#' @param name named list. Specific object names to check. List names should
#' correspond to allowed terms in `prefer`. More than one name is allowed for
#' only "images" at the moment, which produces a combined `SpatExtent`
#' @param verbose be verbose
#' @export
setMethod("ext", signature("giotto"), function(
        x,
        spat_unit = ":all:",
        feat_type = ":all:",
        all_data = TRUE,
        prefer = c("polygon", "spatlocs", "points", "images"),
        name = list(
            spatlocs = ":all:"
        ),
        verbose = NULL,
        ...) {
    data_types <- c("polygon", "spatlocs", "points", "images")

    if (!is.null(name)) {
        checkmate::assert_list(name)
        checkmate::assert_subset(names(name), choices = data_types)
    }

    prefer <- match.arg(prefer, choices = data_types, several.ok = TRUE)

    spat_unit <- set_default_spat_unit(
        gobject = x, spat_unit = spat_unit
    )
    feat_type <- set_default_feat_type(
        gobject = x, spat_unit = spat_unit, feat_type = feat_type
    )

    if (identical(spat_unit, ":all:")) {
        has_poly <- length(list_spatial_info_names(x)) > 0
        has_ctrs <- length(list_spatial_locations(x)$spat_unit) > 0
        has_pnts <- length(list_feature_info(x)$feat_info) > 0
    } else {
        has_poly <- spat_unit %in% list_spatial_info_names(x)
        has_ctrs <- spat_unit %in% list_spatial_locations(x)$spat_unit
        has_pnts <- feat_type %in% list_feature_info(x)$feat_info
    }
    has_imgs <- length(list_images_names(x)) > 0

    if (sum(has_poly, has_ctrs, has_pnts) == 0) {
        vmsg(.v = verbose, "No spatial info in giotto object")
        return(invisible())
    }

    # iterate through gobject available data types according to `prefer`
    pref_order <- c()
    for (ptype in prefer) {
        pref_order <- c(
            pref_order,
            switch(ptype,
                "polygon" = c(polygon = has_poly),
                "spatlocs" = c(spatlocs = has_ctrs),
                "points" = c(points = has_pnts),
                "images" = c(images = has_imgs)
            )
        )
    }

    # find datatype(s) with available data
    use_type <- names(which(pref_order))
    if (!all_data) { # if not all data_types, select first (ordered by pref)
        use_type <- use_type[1L]
    }

    # get the object(s)
    # for each type of data, get all
    elist2 <- lapply(use_type, function(type) {
        spat_obj <- switch(type,
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
                name = name$spatlocs,
                verbose = verbose
            ),
            "points" = getFeatureInfo(
                gobject = x,
                feat_type = feat_type,
                return_giottoPoints = TRUE
            ),
            "images" = getGiottoImage(
                gobject = x,
                name = name$images # more than one element is accepted
            )
        )

        # catch multiple object inputs (images)
        if (inherits(spat_obj, "list")) {
            elist <- lapply(spat_obj, ext)
            e <- Reduce(`+`, elist)
        } else {
            e <- ext(spat_obj)
        }
        return(e)
    })

    # reduce when more than one data_type is queried
    e <- Reduce(`+`, elist2)

    return(e)
})

#' @rdname ext
#' @export
setMethod("ext", signature("giottoAffineImage"), function(x, ...) {
    ext(x@extent)
})

#' @rdname ext
#' @export
setMethod("ext", signature("affine2d"), function(x, ...) ext(x@anchor))







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

#' @rdname ext
#' @export
setMethod("ext<-", signature(
    x = "giottoImage",
    value = "SpatExtent"
), function(x, value) {
    v <- ext(value)
    bnames <- names(x@boundaries)
    mm <- x@minmax
    extnum <- .ext_to_num_vec(v)[c(2, 1, 4, 3)]

    adj <- extnum - mm
    adj[c(2, 4)] <- -adj[c(2, 4)]
    names(adj) <- bnames

    x@boundaries <- adj

    return(x)
})

#' @rdname ext
#' @export
setMethod("ext<-", signature("affine2d"), function(x, value) {
    x@anchor <- .ext_to_num_vec(value)
    return(initialize(x))
})
