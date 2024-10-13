#' @include generics.R
NULL


# Documentations ------------------------------------------------------------ #

#' @title Subset part of an object with `[`
#' @name subset_bracket
#' @aliases `[`
#' @description Extract values from Giotto classes. Providing empty brackets
#' such as: `x[]` will usually extract the main contained data representation.
#' @param x Giotto S4 object to subset information from
#' @param i,j indices specifying elements to extract. Indices are numeric or
#' character vectors, or empty
#' @returns Same as `x` unless brackets are empty in which case, the main
#' internal representation is returned.
#' @examples
#' gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
#'
#' # extract contained `SpatVector`
#' gpoints[]
#'
#' # subset by feature
#' gpoints[c("Mlc1", "Gfap")]
#'
#' # subset by feature and colname
#' gpoints["Mlc1", c("feat_ID", "feat_ID_uniq")]
#'
#' # subset by index
#' gpoints[seq(20)]
#'
#' @seealso [replace_bracket] [subset_dollar] [replace_dollar]
NULL

#' @title Replace part of an object with `[<-`
#' @name replace_bracket
#' @aliases `[<-`
#' @description Replace values from Giotto Classes. Providing empty brackets
#' such as `x[] <- value` will usually replace the entire contained data
#' representation.
#' @param x Giotto S4 object to replace information in
#' @param i,j indices specifying elements to replace. Indices are numeric or
#' character vectors or empty
#' @param value values(s) to set
#' @returns same as `x`
#' @examples
#' gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
#'
#' gpoints[] <- gpoints[]
#'
#' @seealso [subset_bracket] [subset_dollar] [replace_dollar]
NULL

#' @title Replace part of an object with `$<-`
#' @name replace_dollar
#' @aliases `$<-`
#' @description
#' Replace values from Giotto Classes using `$<-` operator.
#' @param x Giotto S4 object to replace columns from
#' @param name A literal character string (possibly backtick quoted).
#' This is normally matched to the colnames.
#' @param value values(s) to set to a column
#' @returns same as `x`
#' @examples
#' gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
#'
#' gpoints$new_col <- sprintf("feat_%d", seq(nrow(gpoints)))
#'
#' @seealso [subset_bracket] [replace_bracket] [subset_dollar]
NULL

#' @title Subset part of an object with `$`
#' @name subset_dollar
#' @aliases `$`
#' @description Subset values from a Giotto Class using `$` operator.
#' @param x Giotto S4 object to extract columns from
#' @param name A literal character string (possibly backtick quoted).
#' This is normally matched to the colnames.
#' @returns vector of values from a requested column
#' @section \code{`$`} methods:
#' @examples
#' enr <- GiottoData::loadSubObjectMini("spatEnrObj")
#'
#' enr$cell_ID
#' @seealso [subset_bracket] [replace_bracket] [replace_dollar]
NULL

#' @title Subset a `giotto` object
#' @name subset_giotto
#' @aliases `[.giotto`
#' @description Subset a giotto object with `[` or `subset()` generic. The
#' implementation is different from [subsetGiotto()] in that all spatial units
#' will always be affected. The feature type to subset can be specified.
#' @param x a `giotto` object
#' @param feat_ids,i character vector. Feature IDs to subset the object for.
#' @param cell_ids,j character vector. Cell/spatial IDs to subset the object
#' for.
#' @param drop not used
#' @param spat_unit character. Controls which spatial unit to pull subsetting
#' information from when using `cell_ids`/`j` and `subset` params. However,
#' all spatial units will always be affected by the subset.
#' @param feat_type character. Subset affects these feature type(s). Default
#' is `"rna"`
#' @param \dots additional args to pass (none implemented)
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' # `[` examples
#' g[1:5]
#' g[, 2:10]
#' g[1:5, 2:10]
#' g[c(TRUE, FALSE), ]
#'
#' # subset() examples
#' subset(g, nr_feats > 300)
#' subset(g, nr_feats > 300,
#'     cell_ids = c("GAATCGCCGGACACGG-1", "GAGGGCATCGCGTATC-1")
#' )
#' subset(g, Gfap + Gna12 > 10)
#' @returns giotto object
NULL

#' @title Subset `giotto` subobjects
#' @name subset_giotto_subobjects
#' @aliases `[[.giotto`
#' @description
#' Subset a `giotto` object with `[[` to disassemble it into a list of Giotto
#' S4 subobjects. If `drop` is `FALSE`, the selected subobjects
#' will be reassembled into a new `giotto` object. Note that indexing within 
#' the `[[` filters for only those subobjects that have those attributes.
#' This may remove some unexpected information. For specifically splitting the
#' `giotto` object by spatial unit and/or feature type while keeping all
#' expected information, use [sliceGiotto()]
#' @param x giotto object
#' @param spat_unit spatial unit (e.g. "cell")
#' @param feat_type feature type to use (e.g. "rna", "protein")
#' @param i character. Indicates the slot name
#' @param j character. Indicates the subobject name
#' @param drop logical. Default = TRUE
#' @param \dots additional arguments
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#' force(g)
#' 
#' # return as lists of subobjects with drop = TRUE (default)
#' g[[, "raw"]]
#' g[["expression", spat_unit = "aggregate"]]
#' 
#' # return as a subset giotto object with drop = FALSE
#' g[[, "raw", drop = FALSE]]
#' g[[spat_unit = "aggregate", drop = FALSE]]
#' @returns giotto subobject
NULL

# --------------------------------------------------------------------------- #

# $ S4 access generic ####


#' @describeIn subset_dollar Subset giotto object
setMethod(
    "$", signature("giotto"), function(x, name) {
        spatValues(x, feats = name)[[name]]
    }
)

#' @rdname replace_dollar
setMethod(
    "$<-", signature("giotto"), function(x, name, value) {
        cx <- getCellMetadata(x, output = "data.table", copy_obj = FALSE)
        cx[, (name) := value]
        return(x)
    }
)

#' @export
.DollarNames.giotto <- function(x, pattern) {
    colnames(pDataDT(x))
}


## * coordDataDT ####
#' @rdname subset_dollar
#' @section \code{`$`} methods:
#'   Select by colname from giotto S4 data.table coordinates slot.
#' @export
setMethod(
    "$", signature(x = "coordDataDT"),
    function(x, name) x@coordinates[[name]]
)


#' @rdname replace_dollar
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
#' @rdname subset_dollar
#' @section \code{`$`} methods:
#'   Select by colname from giotto S4 spatEnrObj
#' @export
setMethod(
    "$", signature(x = "spatEnrObj"),
    function(x, name) {
        x@enrichDT[[as.character(name)]]
    }
)

#' @rdname replace_dollar
#' @section \code{`$<-`} methods:
#'   Set values by colname into giotto S4 spatEnrObj.
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

#' @rdname subset_dollar
#' @section \code{`$`} methods:
#'   Select entries in misc slot from giotto S4 dimObj.
#' @export
setMethod(
    "$", signature(x = "dimObj"),
    function(x, name) x@misc[[name]]
)

#' @rdname replace_dollar
#' @section \code{`$<-`} methods:
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

#' @rdname subset_dollar
#' @section \code{`$`} methods:
#'   Select by colname from giotto S4 data.table metaDT slot.
#' @export
setMethod(
    "$", signature(x = "metaData"),
    function(x, name) x@metaDT[[name]]
)


#' @rdname replace_dollar
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

#' @rdname subset_dollar
#' @section \code{`$`} methods:
#'   Select by colname from giotto S4 spatVector slot.
#' @export
setMethod(
    "$", signature(x = "terraVectData"),
    function(x, name) terra::as.list(x@spatVector)[[name]]
)

#' @rdname replace_dollar
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

#' @rdname subset_dollar
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
#' @rdname subset_bracket
#' @export
setMethod(
    "[", signature(x = "gdtData", i = "gIndex", j = "gIndex", drop = "missing"),
    function(x, i, j) {
        x <- x[i = i]
        x <- x[j = j]
        x
    }
)

#' @rdname subset_bracket
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

#' @rdname subset_bracket
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
#' @rdname subset_bracket
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

#' @rdname subset_bracket
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

#' @rdname subset_bracket
#' @export
setMethod(
    "[", signature(x = "coordDataDT", i = "ANY", j = "ANY", drop = "missing"),
    function(x, i, j) {
        x@coordinates <- x@coordinates[i = i, j = j, with = FALSE]
        x
    }
)

#' @rdname subset_bracket
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

#' @rdname subset_bracket
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

#' @rdname subset_bracket
#' @export
setMethod(
    "[",
    signature(x = "coordDataDT", i = "ANY", j = "missing", drop = "missing"),
    function(x, i, j) {
        x@coordinates <- x@coordinates[i = i]
        x
    }
)

#' @rdname subset_bracket
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

#' @rdname replace_bracket
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

#' @rdname subset_bracket
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

#' @rdname subset_bracket
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

#' @rdname subset_bracket
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

#' @rdname subset_bracket
#' @export
setMethod(
    "[", signature(x = "metaData", i = "ANY", j = "missing", drop = "missing"),
    function(x, i, j) {
        x@metaDT <- x@metaDT[i = i, ]
        x
    }
)

#' @rdname subset_bracket
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

#' @rdname replace_bracket
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

#' @rdname subset_bracket
#' @export
setMethod(
    "[", signature(x = "dimObj", i = "ANY", j = "ANY", drop = "missing"),
    function(x, i, j) {
        x@coordinates <- x@coordinates[i = i, j = j]
        x
    }
)

#' @rdname subset_bracket
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

#' @rdname replace_bracket
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

#' @rdname subset_bracket
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

#' @rdname subset_bracket
#' @export
setMethod(
    "[", signature(x = "exprData", i = "ANY", j = "missing", drop = "missing"),
    function(x, i, j) {
        x@exprMat <- x@exprMat[i = i, ]
        x
    }
)

#' @rdname subset_bracket
#' @export
setMethod(
    "[", signature(x = "exprData", i = "ANY", j = "ANY", drop = "missing"),
    function(x, i, j) {
        x@exprMat <- x@exprMat[i = i, j = j]
        x
    }
)

#' @rdname subset_bracket
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

#' @rdname replace_bracket
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
#' @rdname subset_bracket
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

#' @rdname replace_bracket
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
#' @rdname subset_bracket
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

#' @rdname replace_bracket
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
#' @rdname subset_bracket
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

#' @rdname subset_bracket
#' @export
setMethod(
    "[", signature(x = "enrData", i = "ANY", j = "missing", drop = "missing"),
    function(x, i, j) {
        x@enrichDT <- x@enrichDT[i = i, ]
        x
    }
)

#' @rdname subset_bracket
#' @export
setMethod(
    "[", signature(x = "enrData", i = "missing", j = "ANY", drop = "missing"),
    function(x, i, j) {
        x@enrichDT <- x@enrichDT[j = j]
        x
    }
)

#' @rdname subset_bracket
#' @export
setMethod(
    "[",
    signature(x = "enrData", i = "missing", j = "character", drop = "missing"),
    function(x, i, j) {
        x@enrichDT <- x@enrichDT[j = unique(c(j, "cell_ID")), with = FALSE]
        x
    }
)



#' @rdname replace_bracket
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
#' @rdname subset_bracket
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

#' @rdname replace_bracket
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
#' @rdname subset_bracket
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

#' @rdname subset_bracket
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
#' @rdname subset_bracket
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

#' @rdname subset_bracket
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

#' @rdname replace_bracket
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
#' @rdname subset_bracket
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

#' @rdname subset_bracket
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
#' @rdname subset_bracket
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

#' @rdname subset_bracket
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

#' @rdname subset_bracket
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



#' @rdname replace_bracket
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

# * giottoLargeImage ####
#' @rdname subset_bracket
#' @export
setMethod(
    "[",
    signature(x = "giottoLargeImage", i = "missing", j = "missing", drop = "missing"),
    function(x, i, j) {
        x@raster_object
    }
)

#' @rdname replace_bracket
#' @export
setMethod(
    "[<-",
    signature(x = "giottoLargeImage", i = "missing", j = "missing", value = "ANY"),
    function(x, i, j, value) {
        x@raster_object <- value
        return(initialize(x))
    }
)

# * giottoImage ####
#' @rdname subset_bracket
#' @export
setMethod(
    "[",
    signature(x = "giottoImage", i = "missing", j = "missing", drop = "missing"),
    function(x, i, j) {
        x@mg_object
    }
)

#' @rdname replace_bracket
#' @export
setMethod(
    "[<-",
    signature(x = "giottoImage", i = "missing", j = "missing", value = "ANY"),
    function(x, i, j, value) {
        x@mg_object <- value
        return(initialize(x))
    }
)

#' @rdname subset_bracket
#' @export
setMethod(
    "[",
    signature(
        x = "affine2d", i = "missing", j = "missing", drop = "missing"
    ),
    function(x, i, j) {
        x@affine
    }
)

#' @rdname replace_bracket
#' @aliases [<-,affine2d,missing,missing,
#' ANY-method [<-,affine2d,missing,missing-method
#' @docType methods
#' @section \code{`[<-`} methods:
#'   Assign to \code{affine} slot in affine2d
#' @export
setMethod(
    "[<-",
    signature(x = "affine2d", i = "missing", j = "missing", value = "ANY"),
    function(x, i, j, value) {
        x@affine <- value
        return(initialize(x))
    }
)





# giotto subsets ####



# * [ ####

#' @rdname subset_giotto
#' @export
setMethod(
    "[", signature(x = "giotto", i = "gIndex", j = "missing", drop = "missing"),
    function(x, i, ..., drop) {
        subset(x, feat_ids = i, ...)
    }
)

#' @rdname subset_giotto
#' @export
setMethod(
    "[", signature(x = "giotto", i = "missing", j = "gIndex", drop = "missing"),
    function(x, j, ..., drop) {
        subset(x, cell_ids = j, ...)
    }
)

#' @rdname subset_giotto
#' @export
setMethod(
    "[", signature(x = "giotto", i = "gIndex", j = "gIndex", drop = "missing"),
    function(x, i, j, ..., drop) {
        subset(x, feat_ids = i, cell_ids = j, ...)
    }
)

#' @describeIn subset_giotto Subset giotto objects
setMethod(
    "[", signature(x = "giotto", i = "missing", j = "missing", drop = "missing"),
    function(x, ...) {
        x[[...]]
    }
)

# * [[ ####

#' @rdname subset_giotto_subobjects
#' @export
setMethod(
    "[[", signature(x = "giotto", i = "missing", j = "missing"),
    function(x, spat_unit = NULL, feat_type = NULL, drop = TRUE, ...) {
        res <- as.list(
            x, spat_unit = spat_unit, feat_type = feat_type, ...
        )
        if (drop) return(res)
        else {
            g <- giotto(initialize = FALSE, instructions = instructions(x))
            g <- setGiotto(g, res, verbose = FALSE)
            if (!is.null(spat_unit)) activeSpatUnit(g) <- spat_unit[[1]]
            if (!is.null(feat_type)) activeFeatType(g) <- feat_type[[1]]
            return(g)
        }
    }
)

#' @rdname subset_giotto_subobjects
#' @export
setMethod(
    "[[", signature(x = "giotto", i = "character", j = "missing"),
    function(x, i, spat_unit = NULL, feat_type = NULL, drop = TRUE, ...) {
        res <- as.list(
            x, slots = i, spat_unit = spat_unit, feat_type = feat_type, ...
        )
        if (drop) return(res)
        else {
            g <- giotto(initialize = FALSE, instructions = instructions(x))
            g <- setGiotto(g, res, verbose = FALSE)
            if (!is.null(spat_unit)) activeSpatUnit(g) <- spat_unit[[1]]
            if (!is.null(feat_type)) activeFeatType(g) <- feat_type[[1]]
            return(g)
        }
    }
)

#' @rdname subset_giotto_subobjects
#' @export
setMethod(
    "[[", signature(x = "giotto", i = "missing", j = "character"),
    function(x, j, spat_unit = NULL, feat_type = NULL, drop = TRUE, ...) {
        res <- as.list(x, 
            name = j,
            spat_unit = spat_unit, 
            feat_type = feat_type, 
            ...
        )
        if (drop) return(res)
        else {
            g <- giotto(initialize = FALSE, instructions = instructions(x))
            g <- setGiotto(g, res, verbose = FALSE)
            if (!is.null(spat_unit)) activeSpatUnit(g) <- spat_unit[[1]]
            if (!is.null(feat_type)) activeFeatType(g) <- feat_type[[1]]
            if (is.null(activeSpatUnit(g))) {
                su <- spatUnit(res)
                activeSpatUnit(g) <- su[!is.na(su)][[1L]]
            }
            if (is.null(activeFeatType(g))) {
                ft <- featType(res)
                activeFeatType(g) <- ft[!is.na(ft)][[1L]]
            }
            return(g)
        }
    }
)

#' @rdname subset_giotto_subobjects
#' @export
setMethod(
    "[[", signature(x = "giotto", i = "character", j = "character"),
    function(x, i, j, spat_unit = NULL, feat_type = NULL, drop = TRUE, ...) {
        res <- as.list(x, 
            slots = i, 
            name = j, 
            spat_unit = spat_unit, 
            feat_type = feat_type,
            ...
        )
        if (drop) return(res)
        else {
            g <- giotto(initialize = FALSE, instructions = instructions(x))
            g <- setGiotto(g, res, verbose = FALSE)
            if (!is.null(spat_unit)) activeSpatUnit(g) <- spat_unit[[1]]
            if (!is.null(feat_type)) activeFeatType(g) <- feat_type[[1]]
            if (is.null(activeSpatUnit(g))) {
                su <- spatUnit(res)
                activeSpatUnit(g) <- su[!is.na(su)][[1L]]
            }
            if (is.null(activeFeatType(g))) {
                ft <- featType(res)
                activeFeatType(g) <- ft[!is.na(ft)][[1L]]
            }
            return(g)
        }
    }
)



#' @rdname subset_giotto
#' @param subset Logical expression evaluated in expression values
#' @param negate logical. if `TRUE` all IDs that are **not** in the `subset` 
#' are selected
#' @param quote logical. If `TRUE`, the `subset` param will be quoted with 
#' `substitute()`. Set this to `FALSE` when calling from a function, although 
#' that may not be recommended since NSE output can be unexpected when not used
#' interactively.
#' @param \dots additional params to pass to `spatValues` used with the
#' subset param
#' @export
setMethod("subset", signature("giotto"), function(x,
    subset,
    feat_ids = NULL,
    cell_ids = NULL,
    spat_unit = NULL,
    feat_type = NULL,
    negate = FALSE,
    quote = TRUE,
    ...) {
    spat_unit <- set_default_spat_unit(
        x, spat_unit
    )
    feat_type <- set_default_feat_type(
        x,
        spat_unit = spat_unit, feat_type = feat_type
    )

    # setup vars for subsetting IDs
    fids <- NULL
    sids <- NULL

    # indexing and specified IDs
    if (!is.null(feat_ids)) {
        if (is.numeric(feat_ids) || is.logical(feat_ids)) {
            fx <- fDataDT(x,
                spat_unit = spat_unit,
                feat_type = feat_type
            )
            if (is.logical(feat_ids)) {
                fn <- nrow(fx)
                if (length(feat_ids) != fn) {
                    feat_ids <- rep_len(feat_ids, length.out = fn)
                }
            }
            fids <- fx[feat_ids]$feat_ID
        } else if (is.character(feat_ids)) {
            fids <- feat_ids
        } else if (is.factor(feat_ids)) {
            fids <- as.character(fids)
        }
    }

    if (!is.null(cell_ids)) {
        if (is.numeric(cell_ids) || is.logical(cell_ids)) {
            cx <- pDataDT(x,
                spat_unit = spat_unit,
                feat_type = feat_type
            )
            if (is.logical(cell_ids)) {
                cn <- nrow(cx)
                if (length(cell_ids) != cn) {
                    cell_ids <- rep_len(cell_ids, length.out = cn)
                }
            }
            sids <- cx[cell_ids]$cell_ID
        } else if (is.character(cell_ids)) {
            sids <- cell_ids
        } else if (is.factor(cell_ids)) {
            sids <- as.character(sids)
        }
    }

    # expression evals ------------------------------------------------- #
    if (quote) sub_s <- substitute(subset)
    else sub_s <- subset
    if (negate) sub_s <- call("!", sub_s)

    if (!missing(sub_s)) {
        vars <- all.vars(sub_s)
        vals <- lapply(vars, function(v) {
            spatValues(x,
                feats = v,
                spat_unit = spat_unit,
                feat_type = feat_type,
                verbose = FALSE,
                ...
            )
        })
        .dtjoin <- function(x, y) {
            x[y, on = "cell_ID"]
        }
        vals_dt <- Reduce(.dtjoin, vals)
        if (identical(getOption("giotto.verbose"), "debug")) {
            message("data.table used in subset")
            print(vals_dt)
        }
        sids_s <- subset.data.frame(vals_dt, subset = eval(sub_s))$cell_ID

        if (is.null(sids)) {
            sids <- sids_s
        } else {
            checkmate::assert_character(sids)
            sids <- intersect(sids_s, sids)
        }
    }

    subsetGiotto(x,
        spat_unit = ":all:",
        feat_type = feat_type,
        feat_ids = fids,
        cell_ids = sids
    )
})





#' @name sliceGiotto
#' @title Slice `giotto` object by `spat_unit` and `feat_type`
#' @description Extract specific spatial units and feature types from a
#' `giotto` object as independent `giotto` objects.
#' @param gobject `giotto` object
#' @param spat_unit character vector. Spatial units to slice out. ":all:"
#' means keeping all of them in the output
#' @param feat_type character vector. Feature types to slice out. ":all:"
#' means keeping all of them in the output
#' @param verbose be verbose
#' @returns `giotto` object
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#' res <- sliceGiotto(g, spat_unit = "aggregate")
#' force(res)
#' @seealso [subsetGiotto()] [subset_giotto]
#' @export
sliceGiotto <- function(
        gobject, spat_unit = ":all:", feat_type = ":all:", verbose = FALSE
) {
    spat_unit <- spat_unit %null% ":all:"
    feat_type <- feat_type %null% ":all:"
    x <- gobject # shorter name

    if (identical(spat_unit, ":all:") && identical(feat_type, ":all:")) {
        return(x) # return early if no slicing needed
    }

    # data slots
    spat_only_slots <- c("spatial_info", "spatial_locs", "spatial_network")
    feat_only_slots <- c("feat_info")
    spat_feat_slots <- c(
        "expression", "cell_metadata", "feat_metadata", "spatial_enrichment",
        "nn_network", "dimension_reduction", "multiomics"
    )
    
    spat_only <- x[[spat_only_slots]]
    feat_only <- x[[feat_only_slots]]
    spat_feat <- x[[spat_feat_slots]]

    # select data
    if (!identical(spat_unit, ":all:")) { # select if not all
        activeSpatUnit(x) <- spat_unit[[1L]]
        spat_only <- spat_only[spatUnit(spat_only) %in% spat_unit]
        spat_feat <- spat_feat[spatUnit(spat_feat) %in% spat_unit]
    }

    if (!identical(feat_type, ":all:")) {
        activeFeatType(x) <- feat_type[[1L]]
        feat_only <- feat_only[featType(feat_only) %in% feat_type]
        spat_feat <- spat_feat[featType(spat_feat) %in% feat_type]
    }

    # combine selected data
    datalist <- c(spat_only, feat_only, spat_feat)

    g <- giotto(
        images = x@images,
        parameters = x@parameters,
        instructions = x@instructions,
        offset_file = x@offset_file,
        versions = x@versions,
        join_info = x@join_info,
        h5_file = x@h5_file,
        initialize = FALSE
    )
    g <- setGiotto(g,
        datalist,
        initialize = FALSE,
        verbose = FALSE
    )

    return(initialize(g))
}

# * as.list ####

#' @rdname as.list
#' @title Coerce to a list
#' @docType methods
#' @method as.list giotto
#' @description Generic to coerce to a list if possible
#' @param x the object to coerce
#' @param slots character vector. Which data slots to include in list. See
#'   details
#' @param spat_unit spatial unit (e.g. "cell")
#' @param feat_type feature type to use (e.g. "rna", "protein")
#' @param name name of the elements to select from the slot
#' @param \dots additional arguments
#' @details
#' * Giotto method - the slots argument currently accepts any or multiple of:
#' `"spatial_info", "spatial_locs", "spatial_network", "feat_info",
#' "expression", "cell_metadata", "feat_metadata", "spatial_enrichment",
#' "nn_network", "dimension_reduction", "multiomics"`
#' @returns list
#' @exportMethod as.list
setMethod("as.list", signature("giotto"), function(
        x, slots, spat_unit = NULL, feat_type = NULL, name = NULL, ...
) {
    dataslots <- c(
        "spatial_info", "spatial_locs", "spatial_network", "feat_info",
        "expression", "cell_metadata", "feat_metadata", "spatial_enrichment",
        "nn_network", "dimension_reduction", "multiomics", "images"
    )
    
    .giotto_datalist <- function(x, slots) {
        lapply(slots, function(gslot) methods::slot(x, gslot)) |>
            unlist(recursive = TRUE, use.names = FALSE)
    }
    
    if (missing(slots)) slots <- dataslots
    slots <- match.arg(slots, choices = dataslots, several.ok = TRUE)
    res <- do.call(.giotto_datalist, list(x = x, slots = slots))
    
    if (!is.null(name)) {
        res <- .dbrkt_on_filter(res, name)
    }
    if (!is.null(spat_unit)) {
        res <- .dbrkt_su_filter(res, spat_unit)
    }
    if (!is.null(feat_type)) {
        res <- .dbrkt_ft_filter(res, feat_type)
    }
    return(res)
})


# internals ####
# suspend slot checking until all items in list are supplied


.dbrkt_su_filter <- function(x, y) {
    x[spatUnit(x) %in% y | inherits(x, "giottoLargeImage")]
}
.dbrkt_ft_filter <- function(x, y) {
    x[featType(x) %in% y | inherits(x, "giottoLargeImage")]
}
.dbrkt_on_filter <- function(x, y) {
    x[objName(x) %in% y]
}
