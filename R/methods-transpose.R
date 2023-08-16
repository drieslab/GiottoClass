

## transpose ####

# S4 methods
#' @title Transpose
#' @name transpose-generic
#' @param x object to be transposed
#' @aliases t
NULL


#' @rdname transpose-generic
#' @export
setMethod('t', signature('spatLocsObj'), function(x) {
  sdimy = sdimx = NULL
  x = data.table::copy(x)
  x@coordinates[, c('sdimx', 'sdimy') := .(sdimy, sdimx)]
  return(x)
})
#' @rdname transpose-generic
#' @export
setMethod('t', signature('spatialNetworkObj'), function(x) {
  sdimx_begin = sdimx_end = sdimy_begin = sdimy_end = NULL
  x = data.table::copy(x)
  x@networkDT[, c('sdimx_begin', 'sdimy_begin', 'sdimx_end', 'sdimy_end') := .(sdimy_begin, sdimx_begin, sdimy_end, sdimx_end)]
  if(!is.null(x@networkDT_before_filter)) {
    x@networkDT_before_filter[, c('sdimx_begin', 'sdimy_begin', 'sdimx_end', 'sdimy_end') := .(sdimy_begin, sdimx_begin, sdimy_end, sdimx_end)]
  }
  return(x)
})

# s3 methods
#' @rdname transpose-generic
#' @method t spatLocsObj
#' @export
t.spatLocsObj = function(x) {
  sdimy = sdimx = NULL
  x = data.table::copy(x)
  x@coordinates[, c('sdimx', 'sdimy') := .(sdimy, sdimx)]
  return(x)
}


#' @rdname transpose-generic
#' @method t spatialNetworkObj
#' @export
t.spatialNetworkObj = function(x) {
  sdimx_begin = sdimx_end = sdimy_begin = sdimy_end = NULL
  x = data.table::copy(x)
  x@networkDT[, c('sdimx_begin', 'sdimy_begin', 'sdimx_end', 'sdimy_end') := .(sdimy_begin, sdimx_begin, sdimy_end, sdimx_end)]
  if(!is.null(x@networkDT_before_filter)) {
    x@networkDT_before_filter[, c('sdimx_begin', 'sdimy_begin', 'sdimx_end', 'sdimy_end') := .(sdimy_begin, sdimx_begin, sdimy_end, sdimx_end)]
  }
  return(x)
}
