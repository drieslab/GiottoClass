

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

#' @title Coerce to sf
#' @name as.sf
#' @param x The object to coerce
#' @family As coercion functions
NULL
# ---------------------------------------------------------------- #

# SpatVector -> DT ####
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


# DT -> SpatVector ####

# TODO













# SpatVector -> sf ####
#' @rdname as.sf
#' @export
setMethod('as.sf', signature('SpatVector'),
          function(x) spatvector_to_sf(x))

#' @rdname as.sf
#' @export
setMethod('as.sf', signature('giottoPolygon'),
          function(x) spatvector_to_sf(x[]))

#' @rdname as.sf
#' @export
setMethod('as.sf', signature('giottoPoints'),
          function(x) spatvector_to_sf(x[]))

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

