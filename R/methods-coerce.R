# collate
#' @include package_imports.R
NULL

# docs ----------------------------------------------------------- #
#' @title Coerce to data.table
#' @name as.data.table
#' @description Coerce to data.table if possible
#' @param x The object to coerce
#' @param keep.rownames This argument is ignored
#' @param geom character or NULL. If not NULL, either "XY", "WKT", or "HEX", to
#' get the geometry included in coordinates of each point or vertex,
#' Well-Known-Text or hexadecimal notation.
#' @param include_values whether to include attributes information when geom
#' is 'XY'
#' @param \dots additional arguments to pass
#' @family As coercion functions
#' @returns data.table
#' @examples
#' g <- GiottoData::loadSubObjectMini("giottoPolygon")
#'
#' data.table::as.data.table(g)
NULL

#' @title Coerce to matrix
#' @name as.matrix
#' @description Coerce to matrix
#' @param x object to coerce
#' @param id_rownames logical. Retain the spatial IDs as the rownames
#' @param \dots additional params to pass (none implemented)
#' @examples
#' sl <- GiottoData::loadSubObjectMini("spatLocsObj")
#' m <- as.matrix(sl)
#' @family As coercion functions
#' @returns matrix
NULL


#' @title Coerce to SpatVector polygons
#' @name as.polygons
#' @description Coversion to a SpatVector of polygons.
#' @param x SpatRaster, SpatVector, SpatExtent, or correctly formatted
#' data.frame
#' @returns SpatVector polygons
#' @seealso [terra::as.polygons()]
#' @family As coercion functions
#' @examples
#' g <- GiottoData::loadSubObjectMini("giottoPolygon")
#'
#' as.polygons(slot(g, "spatVector"))
NULL

#' @title Coerce to SpatVector points
#' @name as.points
#' @description Coversion to a SpatVector of points.
#' @param x SpatRaster, SpatVector, SpatExtent, or correctly formatted
#' data.frame
#' @returns SpatVector points
#' @seealso [terra::as.points()]
#' @family As coercion functions
#' @examples
#' g <- GiottoData::loadSubObjectMini("giottoPoints")
#'
#' as.points(slot(g, "spatVector"))
NULL


#' @title R spatial conversions
#' @name r_spatial_conversions
#' @aliases as.sf as.sp as.stars as.terra
#' @param x The object to coerce
#' @param drop When TRUE, returned object will be of the desired object type
#' instead of wrapped in a `giottoPoints` or `giottoPolygon` object
#' @returns  sf, sp, stars or terra
#' @family As coercion functions
#' @examples
#' g <- GiottoData::loadSubObjectMini("giottoPoints")
#'
#' as.sf(g)
NULL


# ---------------------------------------------------------------- #

# to DT ####
#' @rdname as.data.table
#' @method as.data.table SpatVector
#' @export
as.data.table.SpatVector <- function(
        x, keep.rownames = FALSE, geom = NULL,
        include_values = TRUE, ...) {
    # if looking for polygon XY...
    if (terra::is.polygons(x)) {
        if (!is.null(geom)) {
            if (geom == "XY") {
                return(.spatvector_to_dt(x, include_values = include_values))
            }
        }
    }
    # all other conditions: pass to terra then set as DT
    out <- terra::as.data.frame(x, geom = geom, ...) %>%
        data.table::setDT()
    return(out)
}

#' @rdname as.data.table
#' @method as.data.table giottoPolygon
#' @export
as.data.table.giottoPolygon <- function(x, ...) {
    as.data.table(x[], ...)
}

#' @rdname as.data.table
#' @method as.data.table giottoPoints
#' @export
as.data.table.giottoPoints <- function(x, ...) {
    as.data.table(x[], ...)
}




# to matrix ####


#' @rdname as.matrix
#' @export
setMethod("as.matrix", signature("spatLocsObj"), function(
        x, id_rownames = TRUE, ...) {

    x <- x[] # drop to DT
    spat_cols <- c("sdimx", "sdimy", "sdimz")
    spat_cols <- spat_cols %in% colnames(x)
    
    m <- x[, spat_cols, with = FALSE] %>%
        as.matrix()
    
    if (id_rownames) {
        rownames(m) <- x$cell_ID
    }
    return(m)
})




# image types ####

methods::setAs("giottoLargeImage", "giottoImage", function(from) {
    mgobj <- .spatraster_sample_values(
        raster_object = from,
        size = getOption("giotto.plot_img_max_crop", 1e8),
        output = "magick",
        verbose = TRUE
    )

    # Create giottoImage
    mImg <- createGiottoImage(
        name = objName(from),
        mg_object = mgobj,
        verbose = FALSE
    )

    ext(mImg) <- ext(from)

    # # Set scalefactor
    im_dims <- magick::image_info(mImg@mg_object)
    x_scalefactor <- im_dims[["width"]] / dim(from)[2]
    y_scalefactor <- im_dims[["height"]] / dim(from)[1]

    mImg@scale_factor <- c("x" = x_scalefactor, "y" = y_scalefactor)
    mImg@resolution <- 1 / mImg@scale_factor

    return(mImg)
})

# TODO redo this as `as.array`. 
# Careful: There are already usages of this `as()` method in the code
methods::setAs("giottoLargeImage", "array", function(from) {
    .spatraster_sample_values(
        raster_object = from,
        size = getOption("giotto.plot_img_max_crop", 1e8),
        output = "array",
        verbose = FALSE
    )
})




# to SpatVector ####
# TODO
# as.points


#' @rdname as.polygons
#' @param include_values `logical`. Whether to include additional columns other
#' than the geometry information as `SpatVector` attributes. Default is TRUE.
#' @param specific_values `character`. Specific subset of columns to include as
#' attributes if `include_values = TRUE`.
#' @param sort_geom `logical`. Whether to sort key the data.table input by
#' 'geom', 'part', and 'hole' columns.
#' @export
setMethod(
    "as.polygons", signature("data.frame"),
    function(
        x, include_values = TRUE, specific_values = NULL,
        sort_geom = FALSE) {
        .dt_to_spatvector_polygon(
            dt = data.table::setDT(x),
            include_values = include_values,
            specific_values = specific_values,
            sort_geom = sort_geom
        )
    }
)


#' @rdname as.points
#' @param include_values `logical`. Whether to include additional columns other
#' than the geometry information as `SpatVector` attributes. Default is TRUE.
#' @param specific_values `character`. Specific subset of columns to include as
#' attributes if `include_values = TRUE`.
#' @export
setMethod(
    "as.points", signature("data.frame"),
    function(x, include_values = TRUE, specific_values = NULL) {
        .dt_to_spatvector_points(
            dt = data.table::setDT(x),
            include_values = include_values,
            specific_values = specific_values
        )
    }
)






# Spatial Ecosystem Converters ####

# * to sp ####
# Spatial class covers both SpatialPolygonsDataFrame and SpatialPointsDataFrame

#' @rdname r_spatial_conversions
#' @export
setMethod("as.sp", signature("sf"), function(x) {
    GiottoUtils::package_check("sp")
    as(x, "Spatial")
})

#' @rdname r_spatial_conversions
#' @export
setMethod("as.sp", signature("SpatVector"), function(x) {
    GiottoUtils::package_check("sp")
    GiottoUtils::package_check("raster") # needed for this conversion
    as(x, "Spatial")
})

#' @rdname r_spatial_conversions
#' @export
setMethod("as.sp", signature("stars"), function(x) {
    GiottoUtils::package_check("sp")
    as(x, "Spatial")
})

#' @rdname r_spatial_conversions
#' @export
setMethod("as.sp", signature("Spatial"), function(x) {
    GiottoUtils::package_check("sp")
    x
})

#' @rdname r_spatial_conversions
#' @export
setMethod("as.sp", signature("giottoPolygon"), function(x, drop = TRUE) {
    if (isTRUE(drop)) {
        return(as.sp(x[]))
    } else {
        x <- .do_gpoly(x = x, what = as.sp, args = list())
        return(x)
    }
})

#' @rdname r_spatial_conversions
#' @export
setMethod("as.sp", signature("giottoPoints"), function(x, drop = TRUE) {
    s <- as.sp(x[])

    if (isTRUE(drop)) {
        return(s)
    } else {
        x[] <- s
        return(x)
    }
})


# * to sf ####
#' @rdname r_spatial_conversions
#' @export
setMethod("as.sf", signature("SpatVector"), function(x) {
    .spatvector_to_sf(x)
})

#' @rdname r_spatial_conversions
#' @export
setMethod("as.sf", signature("Spatial"), function(x) {
    GiottoUtils::package_check("sf")
    sf::st_as_sf(x)
})

#' @rdname r_spatial_conversions
#' @export
setMethod("as.sf", signature("stars"), function(x) {
    GiottoUtils::package_check("sf")
    sf::st_as_sf(x)
})

#' @rdname r_spatial_conversions
#' @export
setMethod("as.sf", signature("sf"), function(x) {
    GiottoUtils::package_check("sf")
    x
})

#' @rdname r_spatial_conversions
#' @export
setMethod(
    "as.sf", signature("giottoPolygon"),
    function(x, drop = TRUE) {
        if (isTRUE(drop)) {
            return(as.sf(x[]))
        } else {
            x <- .do_gpoly(x = x, what = as.sf, args = list())
            return(x)
        }
    }
)

#' @rdname r_spatial_conversions
#' @export
setMethod(
    "as.sf", signature("giottoPoints"),
    function(x, drop = TRUE) {
        s <- as.sf(x[])

        if (isTRUE(drop)) {
            return(s)
        } else {
            x[] <- s
            return(x)
        }
    }
)

# * to stars ####

# st_as_stars does not handle SpatVector. Only SpatRaster
# however, conversions from sf work fine
#' @rdname r_spatial_conversions
#' @export
setMethod(
    "as.stars", signature("SpatVector"),
    function(x) {
        GiottoUtils::package_check("stars")
        as.sf(x) %>%
            stars::st_as_stars()
    }
)

#' @rdname r_spatial_conversions
#' @export
setMethod(
    "as.stars", signature("sf"),
    function(x) {
        GiottoUtils::package_check("stars")
        stars::st_as_stars(x)
    }
)

#' @rdname r_spatial_conversions
#' @export
setMethod(
    "as.stars", signature("Spatial"),
    function(x) {
        GiottoUtils::package_check("stars")
        stars::st_as_stars(x)
    }
)

#' @rdname r_spatial_conversions
#' @export
setMethod(
    "as.stars", signature("stars"),
    function(x) {
        GiottoUtils::package_check("stars")
        x
    }
)

#' @rdname r_spatial_conversions
#' @export
setMethod(
    "as.stars", signature("giottoPolygon"),
    function(x, drop = TRUE) {
        if (isTRUE(drop)) {
            return(as.stars(x[]))
        } else {
            x <- .do_gpoly(x = x, what = as.stars, args = list())
            return(x)
        }
    }
)

#' @rdname r_spatial_conversions
#' @export
setMethod(
    "as.stars", signature("giottoPoints"),
    function(x, drop = TRUE) {
        s <- as.stars(x[])

        if (isTRUE(drop)) {
            return(s)
        } else {
            x[] <- s
            return(x)
        }
    }
)

# * to terra ####

#' @rdname r_spatial_conversions
#' @export
setMethod(
    "as.terra", signature("SpatVector"),
    function(x) {
        x
    }
)

#' @rdname r_spatial_conversions
#' @export
setMethod(
    "as.terra", signature("sf"),
    function(x) {
        terra::vect(x)
    }
)

#' @rdname r_spatial_conversions
#' @param type whether data is 'vector' or 'raster'
#' @export
setMethod(
    "as.terra", signature("stars"),
    function(x, type = c("vector", "raster")) {
        GiottoUtils::package_check("sf")
        type <- match.arg(type, choices = c("vector", "raster"))

        x <- switch(type,
            "vector" = {
                x %>%
                    sf::st_as_sf() %>%
                    terra::vect()
            },
            "raster" = {
                x %>%
                    terra::rast()
            }
        )

        return(x)
    }
)

#' @rdname r_spatial_conversions
#' @export
setMethod("as.terra", signature("Spatial"), function(x) {
    terra::vect(x)
})

#' @rdname r_spatial_conversions
#' @export
setMethod(
    "as.terra", signature("giottoPolygon"),
    function(x, drop = TRUE) {
        if (isTRUE(drop)) {
            return(as.terra(x[]))
        } else {
            x <- .do_gpoly(x = x, what = as.terra, args = list())
            return(x)
        }
    }
)

#' @rdname r_spatial_conversions
#' @export
setMethod(
    "as.terra", signature("giottoPoints"),
    function(x, drop = TRUE) {
        s <- as.terra(x[])

        if (isTRUE(drop)) {
            return(s)
        } else {
            x[] <- s
            return(x)
        }
    }
)


# internals ####


.spatvector_to_sf <- function(x) {
    package_check("sf", repository = "CRAN")

    out <- try(expr = sf::st_as_sf(x), silent = TRUE)

    # workaround if st_as_sf does not work on a spatvector
    if (inherits(out, "try-error")) {
        d <- terra::as.data.frame(x, geom = "hex")
        d$geometry <- structure(as.list(d$geometry), class = "WKB")
        out <- sf::st_as_sf(x = d, crs = x@ptr$get_crs("wkt"))
    }
    checkmate::assert_class(out, "sf")
    return(out)
}


# convert spatVector to data.table

#' @title Convert spatVector to data.table
#' @name .spatvector_to_dt
#' @returns data.table
#' @description  convert spatVector to data.table
#' @keywords internal
.spatvector_to_dt <- function(spatvector,
    include_values = TRUE) {
    # NSE var
    geom <- NULL

    DT_geom <- data.table::as.data.table(terra::geom(spatvector))

    if (isTRUE(include_values)) {
        DT_values <- data.table::as.data.table(terra::values(spatvector))
        DT_values[, geom := seq_len(nrow(DT_values))]
        DT_full <- data.table::merge.data.table(DT_geom, DT_values, by = "geom")
        return(DT_full)
    } else {
        return(DT_geom)
    }
}


#' @title Convert data.table to polygon spatVector
#' @name .dt_to_spatvector_polygon
#' @description convert data.table to spatVector for polygons
#' @param dt `data.table`. \pkg{terra} geometry information
#' @param include_values `logical`. Whether to include additional columns other
#' than the geometry information as `SpatVector` attributes. Default is TRUE.
#' @param specific_values `character`. Specific subset of columns to include as
#' attributes if `include_values = TRUE`.
#' @param sort_geom `logical`. Whether to sort key the data.table input by
#' 'geom', 'part', and 'hole' columns.
#' @returns polygon spatVector
#' @keywords internal
.dt_to_spatvector_polygon <- function(dt,
    include_values = TRUE,
    specific_values = NULL,
    sort_geom = FALSE) {
    # DT vars
    geom <- NULL

    checkmate::assert_data_table(dt)
    checkmate::assert_logical(include_values)
    if (!is.null(specific_values)) checkmate::assert_character(specific_values)

    # if values are not in order across these cols, an incorrect number of
    # geometries may be generated
    if (sort_geom) data.table::setkeyv(dt, c("geom", "part", "hole"))
    all_colnames <- colnames(dt)
    geom_values <- c("geom", "part", "x", "y", "hole")
    if (!all(geom_values %in% all_colnames)) {
        stop(
            "All columns for '",
            paste0(geom_values, collapse = "', '"), "' are needed"
        )
    }
    other_values <- all_colnames[!all_colnames %in% geom_values]

    # geometry information
    geom_matrix <- as.matrix(dt[, geom_values, with = FALSE])

    # attributes information
    attr_values <- NULL
    if (include_values) {
        # subset for specific columns to include as attributes
        if (!is.null(specific_values)) {
            other_values <- other_values[other_values %in% specific_values]
        }

        attr_values <- unique(dt[, other_values, with = FALSE])
        if (nrow(attr_values) > 0L &&
            nrow(attr_values) != max(dt[, max(geom)])) {
            warning(wrap_txt(".dt_to_spatvector_polygon:
                            Number of attributes does not match number of
                            polygons to create. Attributes are ignored."), call. = FALSE)
        }
    }

    terra::vect(
        x = geom_matrix,
        type = "polygons",
        atts = attr_values
    )
}



#' @title Convert point data data.table to spatVector
#' @name .dt_to_spatvector_points
#' @description data.table to spatVector for points
#' @param dt data.table
#' @param include_values boolean. Include additional values from data.table as
#' attributes paired with created terra spatVector
#' @param specific_values specific values to include as attributes if
#' include_values == TRUE
#' @returns spatVector for points
#' @keywords internal
.dt_to_spatvector_points <- function(dt,
    include_values = TRUE,
    specific_values = NULL) {
    all_colnames <- colnames(dt)
    geom_values <- c("geom", "part", "x", "y", "hole")
    other_values <- all_colnames[!all_colnames %in% geom_values]

    if (include_values == TRUE) {
        if (!is.null(specific_values)) {
            other_values <- other_values[other_values %in% specific_values]
        }


        spatVec <- terra::vect(
            x = as.matrix(dt[, geom_values, with = FALSE]),
            type = "points", atts = dt[, other_values, with = FALSE]
        )
    } else {
        spatVec <- terra::vect(
            x = as.matrix(dt[, geom_values, with = FALSE]),
            type = "points", atts = NULL
        )
    }

    return(spatVec)
}
