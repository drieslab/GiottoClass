#' @include generics.R
NULL


# Documentations ------------------------------------------------------------ #


#' @title Extract or replace parts of an object
#' @name extract-methods
#' @docType methods
#' @aliases `[` `[<-` `$` `$<-`
#' @description Operators Giotto S4 internal data.tables to extract
#' or replace parts.
#' @param x Giotto S4 object to extract columns from
#' @param i,j indices specifying elements to extract or replace. Indices are
#' numeric or character vectors or empty
#' @param name A literal character string (possibly backtick quoted).
#' @param value value(s) to set
#' This is normally matched to the colnames of the data.table object within
#' the S4.
#' @returns columns or values from a data.table
#' @section \code{`$`} methods:
#' @section \code{`$<-`} methods:
#' @section \code{`[`} methods:
#' @section \code{`[<-`} methods:
#' @examples
#' g <- GiottoData::loadSubObjectMini("spatEnrObj")
#'
#' g$cell_ID
NULL


# --------------------------------------------------------------------------- #

# $ S4 access generic ####

## * coordDataDT ####

#' @rdname extract-methods
#' @section \code{`$`} methods:
#'   Select by colname from giotto S4 data.table coordinates slot.
#' @export
setMethod(
    "$", signature(x = "coordDataDT"),
    function(x, name) x@coordinates[[name]]
)


#' @rdname extract-methods
#' @section \code{`$<-`} methods:
#'   Set values by colname into giotto S4 data.table coordinates slot.
#'   Works via data.table methods
#' @export
setMethod(
    "$<-", signature(x = "coordDataDT"),
    function(x, name, value) {
        if (x@coordinates[, .N] == 0) x@coordinates <- data.table::data.table()
        x@coordinates[, (name) := value]
        x
    }
)



#' @export
.DollarNames.spatLocsObj <- function(x, pattern) {
    names(x@coordinates)
}



## * spatEnrObj ####
#' @rdname extract-methods
#' @section \code{`$`} methods:
#'   Select by colname from giotto S4 enrObj
#' @export
setMethod(
    "$", signature(x = "spatEnrObj"),
    function(x, name) {
        x@enrichDT[[as.character(name)]]
    }
)

#' @rdname extract-methods
#' @section \code{`$`} methods:
#'   Set values by colname into giotto S4 dimObj.
#' @export
setMethod(
    "$<-", signature(x = "spatEnrObj"),
    function(x, name, value) {
        x@enrichDT[[as.character(name)]] <- value
        return(x)
    }
)

#' @export
.DollarNames.spatEnrObj <- function(x, pattern) {
    names(x@enrichDT)
}


## * dimObj ####

#' @rdname extract-methods
#' @section \code{`$`} methods:
#'   Select entries in misc slot from giotto S4 dimObj.
#' @export
setMethod(
    "$", signature(x = "dimObj"),
    function(x, name) x@misc[[name]]
)

#' @rdname extract-methods
#' @section \code{`$`} methods:
#'   Set entries in misc slot from giotto S4 dimObj.
#' @export
setMethod(
    "$<-", signature(x = "dimObj"),
    function(x, name, value) {
        x@misc[[name]] <- value
        return(x)
    }
)

#' @export
.DollarNames.dimObj <- function(x, pattern) {
    names(x@misc)
}

## * metaData ####

#' @export
.DollarNames.metaData <- function(x, pattern) {
    colnames(x@metaDT)
}

#' @rdname extract-methods
#' @section \code{`$`} methods:
#'   Select by colname from giotto S4 data.table metaDT slot.
#' @export
setMethod(
    "$", signature(x = "metaData"),
    function(x, name) x@metaDT[[name]]
)


#' @rdname extract-methods
#' @section \code{`$<-`} methods:
#'   Set values by colname into giotto S4 data.table metaDT slot.
#'   Works via data.table methods
#' @export
setMethod(
    "$<-", signature(x = "metaData"),
    function(x, name, value) {
        if (x@metaDT[, .N] == 0) x@metaDT <- data.table::data.table()
        x@metaDT[, (name) := value]
        x
    }
)

## * terraVectData * ####

#' @rdname extract-methods
#' @section \code{`$`} methods:
#'   Select by colname from giotto S4 spatVector slot.
#' @export
setMethod(
    "$", signature(x = "terraVectData"),
    function(x, name) terra::as.list(x@spatVector)[[name]]
)

#' @rdname extract-methods
#' @section \code{`$<-`} methods:
#'   Set values by colname into giotto S4 spatVector slot.
#' @export
setMethod(
    "$<-", signature(x = "terraVectData"),
    function(x, name, value) {
        x@spatVector[[name]] <- value
        x
    }
)

#' @export
.DollarNames.terraVectData <- function(x, pattern) {
    names(x@spatVector)
}

#' @rdname extract-methods
#' @section \code{`$`} methods:
#'   Select piecewise transform values from `affine2d`
#' @export
setMethod("$", signature("affine2d"), function(x, name) {
    accessible <- c("affine", "order", "rotate", "shear", "scale", "translate")
    if (name %in% accessible) {
        return(slot(x, name))
    } else {
        return(NULL)
    }
})

#' @export
.DollarNames.affine2d <- function(x, pattern) {
    c("affine", "order", "rotate", "shear", "scale", "translate")
}


# [ S4 access generic ####

## * gdtData ####

# Make it so that i and j subsets can be written independently
#' @rdname extract-methods
#' @export
setMethod(
    "[", signature(x = "gdtData", i = "gIndex", j = "gIndex", drop = "missing"),
    function(x, i, j) {
        x <- x[i = i]
        x <- x[j = j]
        x
    }
)

#' @rdname extract-methods
#' @export
setMethod(
    "[", signature(
        x = "gdtData", i = "logical", j = "missing",
        drop = "missing"
    ),
    function(x, i, j) {
        x_nrow <- nrow(x)
        i_len <- length(i)

        if (i_len > x_nrow) {
            stop("logical subset vector is longer than number of rows",
                call. = FALSE
            )
        }
        if (i_len < x_nrow) { # handle recycling
            i <- rep(i, length.out = x_nrow)
        }

        x[] <- x[][i]
        x
    }
)

#' @rdname extract-methods
#' @export
setMethod(
    "[", signature(
        x = "gdtData", i = "character", j = "missing",
        drop = "missing"
    ),
    function(x, i, j) {
        # only appropriate for objects where the spatIDs are in the same
        # order and number of repeats as the contained data.table
        x_colnames <- colnames(x[])
        ids <- if ("cell_ID" %in% x_colnames) {
            spatIDs(x)
        } else if ("feat_ID" %in% x_colnames) {
            featIDs(x)
        } else {
            stop(wrap_txt(
                "Subset object does not contain either cell_ID or feat_ID
                column"
            ))
        }

        # make idx vector
        idx <- match(i, ids)
        x[] <- x[][idx]
        x
    }
)

# enforce subsetting by character for gdtData so that cols to keep can be
# checked for id col
#' @rdname extract-methods
#' @export
setMethod(
    "[", signature(
        x = "gdtData", i = "missing", j = "numeric",
        drop = "missing"
    ),
    function(x, i, j) {
        c_names <- colnames(x[])
        x <- x[j = c_names[j]]
        x
    }
)

#' @rdname extract-methods
#' @export
setMethod(
    "[", signature(
        x = "gdtData", i = "missing", j = "logical",
        drop = "missing"
    ),
    function(x, i, j) {
        c_names <- colnames(x[j])
        x <- x[j = c_names[j]]
        x
    }
)



## * coordDataDT ####

#' @rdname extract-methods
#' @export
setMethod(
    "[", signature(x = "coordDataDT", i = "ANY", j = "ANY", drop = "missing"),
    function(x, i, j) {
        x@coordinates <- x@coordinates[i = i, j = j, with = FALSE]
        x
    }
)

#' @rdname extract-methods
#' @export
setMethod(
    "[", signature(
        x = "coordDataDT", i = "missing", j = "ANY",
        drop = "missing"
    ),
    function(x, i, j) {
        x@coordinates <- x@coordinates[j = j]
        x
    }
)

#' @rdname extract-methods
#' @export
setMethod(
    "[", signature(
        x = "coordDataDT", i = "missing", j = "character",
        drop = "missing"
    ),
    function(x, i, j) {
        x@coordinates <- x@coordinates[
            j = unique(c(j, "cell_ID")),
            with = FALSE
        ]
        x
    }
)

# setMethod('[', signature(x = 'giotto', i = 'character', j = 'missing',
# drop = 'missing'),
# function(x, i, spat_unit = NULL, feat_type = NULL, name = NULL) {
#
# # set defaults
# spat_unit = set_default_spat_unit(gobject = x,
#                                   spat_unit = spat_unit)
# feat_type = set_default_feat_type(gobject = x,
#                                   spat_unit = spat_unit,
#                                   feat_type = feat_type)
# if(is.null(name)) name = 1L
#
# switch(i,
#       'expression' = slot(x, 'expression')[[spat_unit]][[feat_type]][[name]],
#       'spatial_locs' = slot(x, 'spatial_locs')[[spat_unit]][[name]],
#       'spatial_info' = slot(x, 'spatial_info')[[name]],
#       'feat_info' = slot(x, 'feat_info')[[feat_type]])
# })

# setMethod('[', signature(x = 'giotto', i = 'character', j = 'numeric',
#                          drop = 'missing'),
#           function(x, i, j, spat_unit = NULL, feat_type = NULL, name = NULL) {
#
#               avail_data = switch(i,
#                                   'expression' = list_expression(gobject = x,
#                                                       spat_unit = spat_unit,
#                                                       feat_type = feat_type),
#                                   'spatial_locs' = list_spatial_locations(
#                                                       gobject = x,
#                                                       spat_unit = spat_unit),
#                                   'spatial_info' = list_spatial_info(
#                                                       gobject = x),
#                                   'feat_info' = list_feature_info(
#                                                       gobject = x))
#
#               switch(i,
#                      'expression' = get_expression_values(gobject = x,
#                                       spat_unit = avail_data$spat_unit[[j]],
#                                       feat_type = avail_data$feat_type[[j]],
#                                       values = avail_data$name[[j]],
#                                       output = 'exprObj',
#                                       set_defaults = FALSE))
#
#           })

#' @rdname extract-methods
#' @export
setMethod(
    "[",
    signature(x = "coordDataDT", i = "ANY", j = "missing", drop = "missing"),
    function(x, i, j) {
        x@coordinates <- x@coordinates[i = i]
        x
    }
)

#' @rdname extract-methods
#' @aliases [,coordDataDT,missing,missing,missing-method
#' @section \code{`[`} methods:
#'   Return \code{coordinates} slot data.table from giotto S4
#' @export
setMethod(
    "[",
    signature(
        x = "coordDataDT", i = "missing", j = "missing",
        drop = "missing"
    ),
    function(x, i, j) {
        x@coordinates
    }
)

#' @rdname extract-methods
#' @aliases [<-,coordDataDT,missing,missing,
#' ANY-method [<-,coordDataDT,missing,missing-method
#' @docType methods
#' @section \code{`[<-`} methods:
#'   Assign to \code{coordinates} slot in giotto S4
setReplaceMethod(
    "[",
    signature(x = "coordDataDT", i = "missing", j = "missing", value = "ANY"),
    function(x, i, j, value) {
        x@coordinates <- value
        x
    }
)

#' @rdname extract-methods
#' @export
setMethod(
    "[",
    signature(
        x = "giottoPoints", i = "gIndex", j = "missing",
        drop = "missing"
    ),
    function(x, i, j) {
        x@spatVector <- x@spatVector[i]
        x@unique_ID_cache <- featIDs(x, uniques = TRUE, use_cache = FALSE)
        x
    }
)


# setMethod("[[")

## * metaData ####

#' @rdname extract-methods
#' @section \code{`[`} methods:
#'   Select rows (i) and cols (j) from giotto S4 metaDT slot
#' @export
setMethod(
    "[", signature(x = "metaData", i = "missing", j = "ANY", drop = "missing"),
    function(x, i, j) {
        x@metaDT <- x@metaDT[j = j]
        x
    }
)

#' @rdname extract-methods
#' @export
setMethod(
    "[",
    signature(x = "metaData", i = "missing", j = "character", drop = "missing"),
    function(x, i, j) {
        id_col <- colnames(x@metaDT)[1L]
        x@metaDT <- x@metaDT[, j = unique(c(id_col, j)), with = FALSE]
        x
    }
)

#' @rdname extract-methods
#' @export
setMethod(
    "[", signature(x = "metaData", i = "ANY", j = "missing", drop = "missing"),
    function(x, i, j) {
        x@metaDT <- x@metaDT[i = i, ]
        x
    }
)

#' @rdname extract-methods
#' @section \code{`[`} methods:
#'   Return \code{metaDT} slot data.table from giotto S4
#' @export
setMethod(
    "[",
    signature(x = "metaData", i = "missing", j = "missing", drop = "missing"),
    function(x, i, j) {
        x@metaDT
    }
)

#' @rdname extract-methods
#' @aliases [<-,metaData,missing,missing,
#' ANY-method [<-,metaData,missing,missing-method
#' @docType methods
#' @section \code{`[<-`} methods:
#'   Assign to \code{metaDT} slot in giotto S4
#' @export
setMethod(
    "[<-",
    signature(x = "metaData", i = "missing", j = "missing", value = "ANY"),
    function(x, i, j, value) {
        x@metaDT <- value
        x
    }
)


## * dimObj ####

#' @rdname extract-methods
#' @export
setMethod(
    "[", signature(x = "dimObj", i = "ANY", j = "ANY", drop = "missing"),
    function(x, i, j) {
        x@coordinates <- x@coordinates[i = i, j = j]
        x
    }
)

#' @rdname extract-methods
#' @section \code{`[`} methods:
#'    Return \code{coordinates} slot matrix from giotto S4 dimObj
#' @export
setMethod(
    "[",
    signature(x = "dimObj", i = "missing", j = "missing", drop = "missing"),
    function(x, i, j) {
        x@coordinates
    }
)

#' @rdname extract-methods
#' @aliases [<-,dimObj,missing,missing,
#' ANY-method [<-,dimObj,missing,missing-method
#' @docType methods
#' @section \code{`[<-`} methods:
#'   Assign to \code{coordinates} slot in giotto S4 dimObj
#' @export
setMethod(
    "[<-", signature(x = "dimObj", i = "missing", j = "missing", value = "ANY"),
    function(x, i, j, value) {
        x@coordinates <- value
        x
    }
)

## * exprData ####

#' @rdname extract-methods
#' @section \code{`[`} methods:
#'   Select rows (i) and cols (j) from giotto S4 exprMat slot
#' @export
setMethod(
    "[", signature(x = "exprData", i = "missing", j = "ANY", drop = "missing"),
    function(x, i, j) {
        x@exprMat <- x@exprMat[, j = j]
        x
    }
)

#' @rdname extract-methods
#' @export
setMethod(
    "[", signature(x = "exprData", i = "ANY", j = "missing", drop = "missing"),
    function(x, i, j) {
        x@exprMat <- x@exprMat[i = i, ]
        x
    }
)

#' @rdname extract-methods
#' @export
setMethod(
    "[", signature(x = "exprData", i = "ANY", j = "ANY", drop = "missing"),
    function(x, i, j) {
        x@exprMat <- x@exprMat[i = i, j = j]
        x
    }
)

#' @rdname extract-methods
#' @section \code{`[`} methods:
#'   Return \code{exprMat} slot Matrix object from giotto S4
#' @export
setMethod(
    "[",
    signature(x = "exprData", i = "missing", j = "missing", drop = "missing"),
    function(x, i, j) {
        x@exprMat
    }
)

#' @rdname extract-methods
#' @aliases [<-,exprData,missing,missing,
#' ANY-method [<-,exprData,missing,missing-method
#' @docType methods
#' @section \code{`[<-`} methods:
#'   Assign to \code{exprMat} slot in giotto S4
#' @export
setMethod(
    "[<-",
    signature(x = "exprData", i = "missing", j = "missing", value = "ANY"),
    function(x, i, j, value) {
        x@exprMat <- value
        x
    }
)

# * spatNetData ####
#' @rdname extract-methods
#' @section \code{`[`} methods:
#'   Return \code{spatNetData} slot network data.table object from giotto S4
#' @export
setMethod(
    "[",
    signature(
        x = "spatNetData", i = "missing", j = "missing",
        drop = "missing"
    ),
    function(x, i, j) {
        x@networkDT
    }
)

#' @rdname extract-methods
#' @aliases [<-,spatNetData,missing,missing,
#' ANY-method [<-,spatNetData,missing,missing-method
#' @docType methods
#' @section \code{`[<-`} methods:
#'   Assign to \code{networkDT} slot in giotto S4
#' @export
setMethod(
    "[<-",
    signature(x = "spatNetData", i = "missing", j = "missing", value = "ANY"),
    function(x, i, j, value) {
        x@networkDT <- value
        x
    }
)


# * nnData ####
#' @rdname extract-methods
#' @section \code{`[`} methods:
#'   Return \code{nnData} slot igraph object from giotto S4
#' @export
setMethod(
    "[",
    signature(x = "nnData", i = "missing", j = "missing", drop = "missing"),
    function(x, i, j) {
        x@igraph
    }
)

#' @rdname extract-methods
#' @aliases [<-,nnData,missing,missing,
#' ANY-method [<-,nnData,missing,missing-method
#' @docType methods
#' @section \code{`[<-`} methods:
#'   Assign to \code{igraph} slot in giotto S4
#' @export
setMethod(
    "[<-", signature(x = "nnData", i = "missing", j = "missing", value = "ANY"),
    function(x, i, j, value) {
        x@igraph <- value
        x
    }
)


# * enrData ####
#' @rdname extract-methods
#' @section \code{`[`} methods:
#'   Return \code{enrData} slot enrichment data.table object from giotto S4
#' @export
setMethod(
    "[",
    signature(x = "enrData", i = "missing", j = "missing", drop = "missing"),
    function(x, i, j) {
        x@enrichDT
    }
)

#' @rdname extract-methods
#' @export
setMethod(
    "[", signature(x = "enrData", i = "ANY", j = "missing", drop = "missing"),
    function(x, i, j) {
        x@enrichDT <- x@enrichDT[i = i, ]
        x
    }
)

#' @rdname extract-methods
#' @export
setMethod(
    "[", signature(x = "enrData", i = "missing", j = "ANY", drop = "missing"),
    function(x, i, j) {
        x@enrichDT <- x@enrichDT[j = j]
        x
    }
)

#' @rdname extract-methods
#' @export
setMethod(
    "[",
    signature(x = "enrData", i = "missing", j = "character", drop = "missing"),
    function(x, i, j) {
        x@enrichDT <- x@enrichDT[j = unique(c(j, "cell_ID")), with = FALSE]
        x
    }
)



#' @rdname extract-methods
#' @aliases [<-,enrData,missing,missing,
#' ANY-method [<-,enrData,missing,missing-method
#' @docType methods
#' @section \code{`[<-`} methods:
#'   Assign to \code{enrichDT} slot in giotto S4
#' @export
setMethod(
    "[<-",
    signature(x = "enrData", i = "missing", j = "missing", value = "ANY"),
    function(x, i, j, value) {
        x@enrichDT <- value
        x
    }
)

# * spatGridData ####
#' @rdname extract-methods
#' @section \code{`[`} methods:
#'   Return \code{spatGridData} slot data.table object from giotto S4
#' @export
setMethod(
    "[",
    signature(
        x = "spatGridData", i = "missing", j = "missing",
        drop = "missing"
    ),
    function(x, i, j) {
        x@gridDT
    }
)

#' @rdname extract-methods
#' @aliases [<-,spatGridData,missing,missing,
#' ANY-method [<-,spatGridData,missing,missing-method
#' @docType methods
#' @section \code{`[<-`} methods:
#'   Assign to \code{gridDT} slot in giotto S4
#' @export
setMethod(
    "[<-",
    signature(x = "spatGridData", i = "missing", j = "missing", value = "ANY"),
    function(x, i, j, value) {
        x@gridDT <- value
        x
    }
)

# * giottoPoints ####
#' @rdname extract-methods
#' @section \code{`[`} methods:
#'   Return \code{giottoPoints} spatVector slot
#' @export
setMethod(
    "[",
    signature(
        x = "giottoPoints", i = "missing", j = "missing",
        drop = "missing"
    ),
    function(x, i, j) {
        x@spatVector
    }
)

#' @rdname extract-methods
#' @export
setMethod(
    "[",
    signature(
        x = "giottoPoints", i = "gIndex", j = "missing",
        drop = "missing"
    ),
    function(x, i, j) {
        x@spatVector <- x@spatVector[i]
        x@unique_ID_cache <- featIDs(x, uniques = TRUE, use_cache = FALSE)
        x
    }
)

# this behavior is different from normal spatvectors
# SpatVector defaults to col subsetting when character is provided to i
# subsetting on feat_ID col makes more sense for giottoPoints
#' @rdname extract-methods
#' @export
setMethod(
    "[",
    signature(
        x = "giottoPoints", i = "character", j = "missing",
        drop = "missing"
    ),
    function(x, i, j) {
        sel_bool <- x$feat_ID %in% i
        x[sel_bool]
    }
)

#' @rdname extract-methods
#' @export
setMethod(
    "[",
    signature(
        x = "giottoPoints", i = "missing", j = "gIndex",
        drop = "missing"
    ),
    function(x, i, j) {
        x@spatVector <- x@spatVector[, j]
        if (!"feat_ID" %in% names(x@spatVector)) {
            .gstop(
                "feat_ID must be kept as a column",
                .n = 2L
            )
        }
        x
    }
)

#' @rdname extract-methods
#' @aliases [<-,giottoPoints,missing,missing,
#' ANY-method [<-,giottoPoints,missing,missing-method
#' @docType methods
#' @section \code{`[<-`} methods:
#'   Assign to \code{spatVector} slot in giotto S4
#' @export
setMethod(
    "[<-",
    signature(x = "giottoPoints", i = "missing", j = "missing", value = "ANY"),
    function(x, i, j, value) {
        x@spatVector <- value
        x
    }
)

# * giottoPolygon ####
#' @rdname extract-methods
#' @section \code{`[`} methods:
#'   Return \code{giottoPolygon} spatVector slot
#' @export
setMethod(
    "[",
    signature(
        x = "giottoPolygon", i = "missing", j = "missing",
        drop = "missing"
    ),
    function(x, i, j) {
        x@spatVector
    }
)

#' @rdname extract-methods
#' @export
setMethod(
    "[",
    signature(
        x = "giottoPolygon", i = "gIndex", j = "missing",
        drop = "missing"
    ),
    function(x, i, j) {
        x@spatVector <- x@spatVector[i]
        x@spatVectorCentroids <- x@spatVectorCentroids[i]
        x@unique_ID_cache <- spatIDs(x, uniques = TRUE, use_cache = FALSE)

        if (is.null(x@overlaps)) {
            return(x)
        } # if no overlaps, skip following

        for (feat in names(x@overlaps)) {
            cell_id_bool <- terra::as.list(
                x@overlaps[[feat]]
            )$poly_ID %in% x@unique_ID_cache
            x@overlaps[[feat]] <- x@overlaps[[feat]][cell_id_bool]
        }

        x
    }
)

# this behavior is different from normal spatvectors
# SpatVector defaults to col subsetting when character is provided to i
# subsetting on poly_ID col makes more sense for giottoPolygon
#' @rdname extract-methods
#' @export
setMethod(
    "[",
    signature(
        x = "giottoPolygon", i = "character", j = "missing",
        drop = "missing"
    ),
    function(x, i, j) {
        sel_bool <- x$poly_ID %in% i
        x[sel_bool]
    }
)

#' @rdname extract-methods
#' @export
setMethod(
    "[",
    signature(
        x = "giottoPolygon", i = "missing", j = "gIndex",
        drop = "missing"
    ),
    function(x, i, j) {
        x@spatVector <- x@spatVector[, j]
        x@spatVectorCentroids <- x@spatVectorCentroids[, j]
        if (!"poly_ID" %in% names(x@spatVector)) {
            .gstop(
                "poly_ID must be kept as a column",
                .n = 2L
            )
        }
        x
    }
)

#' @rdname extract-methods
#' @export
setMethod(
    "[",
    signature(
        x = "terraVectData", i = "gIndex", j = "gIndex",
        drop = "missing"
    ),
    function(x, i, j) {
        x <- x[, j]
        x <- x[i, ]
        x
    }
)



#' @rdname extract-methods
#' @aliases [<-,giottoPolygon,missing,missing,
#' ANY-method [<-,giottoPolygon,missing,missing-method
#' @docType methods
#' @section \code{`[<-`} methods:
#'   Assign to \code{spatVector} slot in giottoPolygon
#' @export
setMethod(
    "[<-",
    signature(x = "giottoPolygon", i = "missing", j = "missing", value = "ANY"),
    function(x, i, j, value) {
        x@spatVector <- value
        x
    }
)

#' @rdname extract-methods
#' @export
setMethod(
    "[",
    signature(
        x = "affine2d", i = "missing", j = "missing", drop = "missing"
    ),
    function(x) {
        x@affine
    }
)

#' @rdname extract-methods
#' @aliases [<-,affine2d,missing,missing,
#' ANY-method [<-,affine2d,missing,missing-method
#' @docType methods
#' @section \code{`[<-`} methods:
#'   Assign to \code{affine} slot in affine2d
#' @export
setMethod(
    "[<-",
    signature(x = "affine2d", i = "missing", j = "missing", value = "ANY"),
    function(x, value) {
        x@affine <- value
        return(initialize(x))
    }
)
