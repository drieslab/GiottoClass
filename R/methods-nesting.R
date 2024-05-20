#' @include generics.R
NULL

# docs ----------------------------------------------------------- #

#' @title Spatial unit information
#' @name spatUnit-generic
#' @description access and set spat_unit slot of S4 subobject
#' @param x a Giotto S4 class subobject with spatial unit
#' @param value value to set as spatial unit
#' @aliases spatUnit spatUnit<-
#' @returns spat_unit
#' @examples
#' g <- GiottoData::loadSubObjectMini("featMetaObj")
#'
#' spatUnit(g)
NULL

#' @title Feature type information
#' @name featType-generic
#' @description access and set feat_type slot of S4 subobject
#' @param x a Giotto S4 class subobject with feature type
#' @param value value to set as feature type
#' @aliases featType featType<-
#' @returns feat_type
#' @examples
#' g <- GiottoData::loadSubObjectMini("featMetaObj")
#'
#' featType(g)
NULL

#' @title Giotto object name information
#' @name objName-generic
#' @description access and set name slot fo S4 subobject
#' @param x a Giotto S4 class subobject with name data
#' @param value value to set as object name
#' @aliases objName objName<-
#' @returns name slot
#' @examples
#' g <- GiottoData::loadSubObjectMini("exprObj")
#'
#' objName(g)
NULL

#' @title Provenance information
#' @name prov-generic
#' @description access and set provenance slot of S4 subobject
#' @param x a Giotto S4 class subobject
#' @param value value to set as provenance
#' @aliases prov prov<-
#' @returns provenance slot
#' @examples
#' g <- GiottoData::loadSubObjectMini("exprObj")
#'
#' prov(g)
NULL

# ---------------------------------------------------------------- #






# spatUnit ####

#' @rdname spatUnit-generic
#' @export
setMethod("spatUnit", signature("list"), function(x) {
    vapply(x, spatUnit, FUN.VALUE = character(1L))
})

#' @describeIn spatUnit-generic Get spatial unit information
#' @export
setMethod("spatUnit", signature = "spatData", function(x) x@spat_unit)

#' @describeIn spatUnit-generic Get spatial unit information
#' @export
setMethod("spatUnit", signature("giottoPolygon"), function(x) x@name)


#' @describeIn spatUnit-generic Set spatial unit information
#' @export
setMethod("spatUnit<-", signature = "spatData", function(x, value) {
    value <- as.character(value)
    x@spat_unit <- value
    x
})

# giottoPolygon name describes the same thing as the spat_unit
#' @describeIn spatUnit-generic Set giottoPolygon spat_unit
#' @export
setMethod("spatUnit<-", signature("giottoPolygon"), function(x, value) {
    value <- as.character(value)
    x@name <- value
    x
})






# featType ####

#' @rdname featType-generic
#' @export
setMethod("featType", signature("list"), function(x) {
    vapply(x, featType, FUN.VALUE = character(1L))
})

#' @describeIn featType-generic Get feature type information
#' @export
setMethod("featType", signature = "featData", function(x) x@feat_type)


#' @describeIn featType-generic Set feature type information
#' @export
setMethod("featType<-", signature = "featData", function(x, value) {
    value <- as.character(value)
    x@feat_type <- value
    x
})





# objName ####

#' @rdname objName-generic
#' @export
setMethod("objName", signature = "list", function(x) {
    vapply(x, objName, FUN.VALUE = character(1L), USE.NAMES = FALSE)
})

#' @describeIn objName-generic Get name information
#' @export
setMethod("objName", signature = "nameData", function(x) x@name)

#' @describeIn objName-generic Get name giottoPoints
#' @export
setMethod("objName", signature = "giottoPoints", function(x) x@feat_type)

#' @rdname objName-generic
#' @export
setMethod("objName", signature("giottoLargeImage"), function(x) x@name)

#' @rdname objName-generic
#' @export
setMethod("objName", signature("giottoImage"), function(x) x@name)

#' @rdname objName-generic
#' @export
setMethod("objName<-", signature = "list", function(x, value) {
    if (length(x) != length(value)) {
        stop("Number of names to set must be the same as the length of list",
             call. = FALSE)
    }
    lapply(seq_along(x), function(i) {
        y <- x[[i]]
        objName(y) <- value[[i]]
        return(y)
    })
})

#' @rdname objName-generic
#' @export
setMethod("objName<-", signature = "nameData", function(x, value) {
    value <- as.character(value)
    x@name <- value
    x
})

#' @rdname objName-generic
#' @export
setMethod("objName<-", signature = "giottoImage", function(x, value) {
    value <- as.character(value)
    x@name <- value
    x
})

#' @rdname objName-generic
#' @export
setMethod("objName<-", signature("giottoLargeImage"), function(x, value) {
    value <- as.character(value)
    x@name <- value
    x
})

# name describes the same thing as feat_type for giottoPoints
#' @describeIn objName-generic Set name giottoPoints
#' @export
setMethod("objName<-", signature = "giottoPoints", function(x, value) {
    value <- as.character(value)
    x@feat_type <- value
    x
})






# prov ####
#' @describeIn prov-generic Get provenance information
#' @export
setMethod("prov", signature = "provData", function(x) x@provenance)


#' @describeIn prov-generic Set provenance information
#' @export
setMethod("prov<-", signature = "provData", function(x, value) {
    x@provenance <- value
    x
})




# internals ####

#' @name objectlist_name_utils
#' @title Name wrangling for subobject lists
#' @param obj_list list containing giotto subobjects
#' @examples
#' \dontrun{
#' e <- new("exprObj")
#' a <- replicate(3L, e)
#' # name the objects
#' objName(a) <- letters[1:3]
#'
#' # assign the object stored names to the list
#' names(a)
#' a <- assign_objnames_2_list(a)
#' names(a)
#'
#' # assign list names to the object
#' b <- list("name_to_set" = new("exprObj"), "name2" = new("exprObj"))
#' objName(b)
#' b <- assign_listnames_2_obj(b)
#' objName(b)
#'
#'
#' # ensure that character values are unique
#' input <- c("a", "b", "b", "c", "a", "a", "a")
#' .uniquify_dups(input, verbose = FALSE)
#' }
NULL

#' @rdname objectlist_name_utils
#' @param force_replace logical. default = FALSE. Whether to replace the
#' names of objects for which the name already has a name for
#' @keywords internal
assign_objnames_2_list <- function(obj_list, force_replace = FALSE) {
    if (is.null(obj_list)) {
        return(obj_list)
    }
    # find list items with no names
    list_names <- names(obj_list)
    if (is.null(list_names)) {
        list_names <- rep(NA_character_, length(obj_list))
        obj_missing_names <- rep(TRUE, length(obj_list))
    } else {
        obj_missing_names <- is.na(names(obj_list)) | names(obj_list) == ""
    }

    # find and subset to list items that can contain nameData
    is_obj <- vapply(obj_list, inherits, "nameData", FUN.VALUE = logical(1L))

    if (isTRUE(force_replace)) {
        obj_missing_names <- is_obj
    } else {
        obj_missing_names <- obj_missing_names & is_obj
    }

    # get object names info
    obj_names <- lapply(obj_list[obj_missing_names], objName)
    list_names[obj_missing_names] <- obj_names

    names(obj_list) <- list_names

    return(obj_list)
}


#' @name objectlist_name_utils
#' @keywords internal
assign_listnames_2_obj <- function(obj_list) {
    list_names <- names(obj_list)
    if (is.null(list_names)) {
        stop("<assign_listnames_2_obj> List has no names\n")
    }
    obj_index <- which(vapply(
        obj_list, inherits, "nameData", FUN.VALUE = logical(1L)
    ))
    list_obj_names <- list_names[obj_index]

    for (obj_i in seq_along(obj_index)) {
        objName(obj_list[[obj_index[[obj_i]]]]) <- list_obj_names[obj_i]
    }

    return(obj_list)
}

#' @name objectlist_name_utils
#' @param x character vector
#' @param sep character. Separator used when making names unique. Default is "."
#' @param what character (optional). Description of character vector input
#' used when printing verbose messages about what was made unique
#' @param verbose be verbose
#' @keywords internal
.uniquify_dups <- function(x, sep = ".", what = "", verbose = NULL) {
    changed <- vector(mode = "character")
    for (y in x) {
        dup <- x == y
        if (sum(dup) > 1) {
            changed <- c(changed, y)
            x[dup] <- paste(x[dup], seq_len(sum(dup)), sep = sep)
        }
    }
    if (length(changed) > 0) {
        vmsg(.v = verbose, sprintf(
            "%s duplicates found and made unique for:\n %s",
            what,
            paste0(changed, collapse = ", ")
        ))
    }
    return(x)
}
