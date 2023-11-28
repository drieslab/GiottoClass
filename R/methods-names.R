#' @include generics.R
NULL

#' @title Row and column names
#' @name row-plus-colnames-generic
#' @aliases colnames rownames
#' @description Retrieve or set the row or column names of an object
#' @param x object
#' @return A character vector of row or col names
NULL

#' @rdname row-plus-colnames-generic
#' @export
setMethod("colnames", signature(x = "exprObj"), function(x) colnames(x[]))

#' @rdname row-plus-colnames-generic
#' @export
setMethod("rownames", signature(x = "exprObj"), function(x) rownames(x[]))
