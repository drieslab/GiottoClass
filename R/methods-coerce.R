

# docs ----------------------------------------------------------- #
#' @title Coerce to data.table
#' @name as.data.table
#' @description Coerce to data.table if possible
#' @param x The object to coerce
#' @param keep.rownames This argument is ignored
#' @param geom character or NULL. If not NULL, either "XY", "WKT", or "HEX", to
#' get the geometry included in coordinates of each point or vertex, Well-Known-Text
#' or hexadecimal notation.
#' @param include_values whether to include attributes information when geom is 'XY'
#' @param \dots additional arguments to pass
#' @family As coercion functions
NULL

#' @title Coerce to sp
#' @name as.sp
#' @param x The object to coerce
#' @param drop When TRUE, returned object will be the sp object instead of
#' wrapped in a `giottoPoints` or `giottoPolygon` object
#' @family As coercion functions
NULL

#' @title Coerce to sf
#' @name as.sf
#' @param x The object to coerce
#' @param drop When TRUE, returned object will be the sf object instead of
#' wrapped in a `giottoPoints` or `giottoPolygon` object
#' @family As coercion functions
NULL

#' @title Coerce to stars
#' @name as.stars
#' @param x The object to coerce
#' @param drop When TRUE, returned object will be the stars object instead of
#' wrapped in a `giottoPoints` or `giottoPolygon` object
#' @family As coercion functions
NULL

#' @title Coerce to terra
#' @name as.terra
#' @param x The object ot coerce
#' @param drop When TRUE, returned object will be the terra object instead of
#' wrapped in a `giottoPoints` or `giottoPolygon` object
#' @family As coercion functions
NULL

# ---------------------------------------------------------------- #

# to DT ####
#' @rdname as.data.table
#' @method as.data.table SpatVector
#' @export
as.data.table.SpatVector <- function(x, keep.rownames = FALSE, geom = NULL, include_values = TRUE, ...) {
  # if looking for polygon XY...
  if(terra::is.polygons(x)) {
    if(!is.null(geom)) {
      if(geom == 'XY') {
        return(spatVector_to_dt(x, include_values = include_values))
      }
    }
  }
  # all other conditions: pass to terra then set as DT
  out = terra::as.data.frame(x, geom = geom, ...) %>%
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


# to SpatVector ####
# TODO
# as.points / as.polygon generics from terra are an option, but terra deals with
# this kind of conversion using vect() usually











# Spatial Ecosystem Converters ####

# * to sp ####

#' @rdname as.sp
#' @export
setMethod('as.sp', signature('sf'), function(x) {
  GiottoUtils::package_check('sp')
  as(x, "Spatial")
})

#' @rdname as.sp
#' @export
setMethod('as.sp', signature('SpatVector'), function(x) {
  GiottoUtils::package_check('sp')
  as(x, "Spatial")
})

#' @rdname as.sp
#' @export
setMethod('as.sp', signature('stars'), function(x) {
  GiottoUtils::package_check('sp')
  as(x, "Spatial")
})

#' @rdname as.sp
#' @export
setMethod('as.sp', signature('sp'), function(x) {
  GiottoUtils::package_check('sp')
  x
})

#' @rdname as.sp
#' @export
setMethod('as.sp', signature('giottoPolygon'), function(x, drop = TRUE) {
  if (isTRUE(drop)) {
    return(as.sp(x[]))
  } else {
    x <- do_gpoly(x = x, what = as.sp, args = list())
    return(x)
  }
})

#' @rdname as.sp
#' @export
setMethod('as.sp', signature('giottoPoints'), function(x, drop = TRUE) {
  s <- as.sp(x[])

  if (isTRUE(drop)) {
    return(s)
  } else {
    x[] <- s
    return(x)
  }
})


# * to sf ####
#' @rdname as.sf
#' @export
setMethod('as.sf', signature('SpatVector'), function(x) {
  spatvector_to_sf(x)
})

#' @rdname as.sf
#' @export
setMethod('as.sf', signature('sp'), function(x) {
  GiottoUtils::package_check('sf')
  sf::st_as_sf()
})

#' @rdname as.sf
#' @export
setMethod('as.sf', signature('stars'), function(x) {
  GiottoUtils::package_check('sf')
  sf::st_as_sf(x)
})

#' @rdname as.sf
#' @export
setMethod('as.sf', signature('sf'), function(x) {
  GiottoUtils::package_check('sf')
  x
})

#' @rdname as.sf
#' @export
setMethod('as.sf', signature('giottoPolygon'),
          function(x, drop = TRUE) {

            if (isTRUE(drop)) {
              return(as.sf(x[]))
            } else {
              x <- do_gpoly(x = x, what = as.sf, args = list())
              return(x)
            }
          })

#' @rdname as.sf
#' @export
setMethod('as.sf', signature('giottoPoints'),
          function(x, drop = TRUE) {
            s <- as.sf(x[])

            if (isTRUE(drop)) {
              return(s)
            } else {
              x[] <- s
              return(x)
            }
          })

# * to stars ####

# st_as_stars does not handle SpatVector. Only SpatRaster
# however, conversions from sf work fine
#' @rdname as.stars
#' @export
setMethod('as.stars', signature('SpatVector'),
          function(x) {
            GiottoUtils::package_check('stars')
            as.sf(x) %>%
              stars::st_as_stars()
          })

#' @rdname as.stars
#' @export
setMethod('as.stars', signature('sf'),
          function(x) {
            GiottoUtils::package_check('stars')
            stars::st_as_stars(x)
          })

#' @rdname as.stars
#' @export
setMethod('as.stars', signature('sp'),
          function(x) {
            GiottoUtils::package_check('stars')
            stars::st_as_stars(x)
          })

#' @rdname as.stars
#' @export
setMethod('as.stars', signature('stars'),
          function(x) {
            GiottoUtils::package_check('stars')
            x
          })

#' @rdname as.stars
#' @export
setMethod('as.stars', signature('giottoPolygon'),
          function(x, drop = TRUE) {
            if (isTRUE(drop)) {
              return(as.stars(x[]))
            } else {
              x <- do_gpoly(x = x, what = as.stars, args = list())
              return(x)
            }
          })

#' @rdname as.stars
#' @export
setMethod('as.stars', signature('giottoPoints'),
          function(x, drop = TRUE) {
            s <- as.stars(x[])

            if (isTRUE(drop)) {
              return(s)
            } else {
              x[] <- s
              return(x)
            }
          })

# * to terra ####

#' @rdname as.terra
#' @export
setMethod('as.terra', signature('SpatVector'),
          function(x) {
            x
          })

#' @rdname as.terra
#' @export
setMethod('as.terra', signature('sf'),
          function(x) {
            terra::vect(x)
          })

#' @rdname as.terra
#' @param type whether data is 'vector' or 'raster'
#' @export
setMethod('as.terra', signature('stars'),
          function(x, type = c('vector', 'raster')) {
            GiottoUtils::package_check('sf')
            type = match.arg(type, choices = c('vector', 'raster'))

            x <- switch(
              type,
              'vector' = {
                x %>%
                  sf::st_as_sf() %>%
                  terra::vect()
              },
              'raster' = {
                x %>%
                  terra::rast()
              }
            )

            return(x)
          })

#' @rdname as.terra
#' @export
setMethod('as.terra', signature('sp'), function(x) {
  terra::vect(x)
})

#' @rdname as.terra
#' @export
setMethod('as.terra', signature('giottoPolygon'),
          function(x, drop = TRUE, ...) {
            if (isTRUE(drop)) {
              return(as.terra(x[], ...))
            } else {
              x <- do_gpoly(x = x, what = as.terra, args = list(...))
              return(x)
            }
          })

#' @rdname as.terra
#' @export
setMethod('as.terra', signature('giottoPoints'),
          function(x, drop = TRUE) {
            s <- as.terra(x[])

            if (isTRUE(drop)) {
              return(s)
            } else {
              x[] <- s
              return(x)
            }
          })


# internals ####


spatvector_to_sf = function(x) {
  package_check('sf', repository = 'CRAN')

  out <- try(expr = sf::st_as_sf(x), silent = TRUE)

  # workaround if st_as_sf does not work on a spatvector
  if(inherits(out, 'try-error')) {
    d <- terra::as.data.frame(x, geom = 'hex')
    d$geometry <- structure(as.list(d$geometry), class = 'WKB')
    out <- sf::st_as_sf(x = d, crs = x@ptr$get_crs('wkt'))
  }
  assert_class(out, 'sf')
  return(out)
}

