#' @include generics.R
NULL

# docs ----------------------------------------------------------- #

#' @title Giotto schema
#' @name giotto_schema
#' @aliases spatUnit spatUnit<- featType featType<- objName objName<- prov prov<-
#' @description Data within the `giotto` object is organized in a schema
#' largely revolving around the **spatial unit** (which spatial length scale or
#' polygonal annotation that is used as the unit of study) and the 
#' **feature type** (data modality). Information is then further organized
#' based on the **name** or key of the object. In cases where a single
#' spatial unit is comprised of information from multiple others, 
#' **provenance** is tracked to keep a record of which spatial units were the
#' sources of that data. The functions to get and set these aspects of the
#' schema on the Giotto object and subobjects are:
#' 
#' * **spatial unit:** `spatUnit()`, `spatUnit<-()`
#' * **feature type:** `featType()`, `featType<-()`
#' * **name:**         `objName()`, `objName<-()`
#' * **provenance:**   `prov()`, `prov<-()`
#' 
#' @param x `giotto` or \{Giotto\} S4 subobject
#' @param old character. Old value to replace
#' @param value value to set for this schema component
#' @returns character. NA is returned when schema component is not applicable
#' to target object. If using the replacement function, the `giotto` object
#' or subobject is returned
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#' 
#' ########### Get/set existing schema values within giotto object ####
#' spatUnit(g)
#' featType(g)
#' 
#' # rename a spatial unit
#' spatUnit(g, old = "z0") <- "slice1"
#' spatUnit(g)
#' 
#' # rename a feature type
#' featType(g, old = "rna") <- "feature1"
#' featType(g)
#' 
#' ########### Get schema values from a list of objects ###############
#' 
#' glist <- as.list(g)
#' spatUnit(glist)
#' featType(glist)
#' objName(glist)
#' 
#' ########### Get and set schema values with single subobject ########
#' 
#' fx <- g[["feat_meta", spat_unit = "aggregate"]][[1]]
#' 
#' spatUnit(fx)
#' spatUnit(fx) <- "foo"
#' spatUnit(fx)
#' 
#' featType(fx)
#' featType(fx) <- "bar"
#' featType(fx)
#' 
#' ex <- g[["expression", spat_unit = "aggregate"]][[1]]
#' 
#' objName(ex)
#' objName(ex) <- "baz"
#' objName(ex)
#' 
#' prov(ex)
#' prov(ex) <- "qux"
#' prov(ex)
NULL

# ---------------------------------------------------------------- #






# spatUnit ####

# default for unknown types
#' @rdname giotto_schema
#' @export
setMethod("spatUnit", signature("ANY"), function(x) NA_character_)

#' @rdname giotto_schema
#' @export
setMethod("spatUnit", signature("giotto"), function(x) {
    raw_su <- mixedsort(unique(spatUnit(as.list(x))))
    raw_su[!is.na(raw_su)]
})

#' @rdname giotto_schema
#' @export
setMethod("spatUnit", signature("list"), function(x) {
    vapply(x, spatUnit, FUN.VALUE = character(1L))
})

#' @rdname giotto_schema
#' @export
setMethod("spatUnit", signature = "spatData", function(x) x@spat_unit)

#' @rdname giotto_schema
#' @export
setMethod("spatUnit", signature("giottoPolygon"), function(x) x@name)

#' @rdname giotto_schema
#' @export
setMethod("spatUnit<-", signature("ANY"), function(x, value) x)

#' @rdname giotto_schema
#' @export
setMethod("spatUnit<-", signature = "spatData", function(x, value) {
    value <- as.character(value)
    x@spat_unit <- value
    x
})

# giottoPolygon name describes the same thing as the spat_unit
#' @rdname giotto_schema
#' @export
setMethod("spatUnit<-", signature("giottoPolygon"), function(x, value) {
    value <- as.character(value)
    x@name <- value
    x
})

#' @rdname giotto_schema
#' @export
setMethod("spatUnit<-", signature = "list", function(x, value) {
    if (length(x) != length(value)) {
        stop("Number of names to set must be the same as the length of list",
            call. = FALSE
        )
    }
    lapply(seq_along(x), function(i) {
        y <- x[[i]]
        spatUnit(y) <- value[[i]]
        return(y)
    })
})

#' @rdname giotto_schema
#' @export
setMethod("spatUnit<-", signature("giotto"), function(x, old, value) {
    checkmate::assert_character(old, len = 1L)
    checkmate::assert_character(value, len = 1L)
    if (!isTRUE(old %in% spatUnit(x))) {
        stop("spat_unit replace: spatial unit ", old, " does not exist\n", 
             call. = FALSE)
    }
    glist <- as.list(x)
    for(item_i in seq_along(glist)) {
        if (isTRUE(spatUnit(glist[[item_i]]) == old))
        spatUnit(glist[[item_i]]) <- value
    }
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
    g <- setGiotto(g, glist, initialize = TRUE, verbose = FALSE)

    active_spat <- activeSpatUnit(g)
    if (!isTRUE(active_spat %in% spatUnit(g)) &&
        length(spatUnit(g)) >= 1L) {
        first_spat <- spatUnit(g)[[1]]
        activeSpatUnit(g) <- first_spat
        warning(wrap_txtf(
            "activeSpatUnit \'%s\' no longer exists.
            activeSpatUnit defaulted to \'%s\'", 
            active_spat, first_spat
        ),
        call. = FALSE)
    }
    return(g)
})




# featType ####

# default for unknown types
#' @rdname giotto_schema
#' @export
setMethod("featType", signature("ANY"), function(x) {
    NA_character_
})

#' @rdname giotto_schema
#' @export
setMethod("featType", signature("giotto"), function(x) {
    raw_ft <- mixedsort(unique(featType(as.list(x))))
    raw_ft[!is.na(raw_ft)]
})

#' @rdname giotto_schema
#' @export
setMethod("featType", signature("list"), function(x) {
    vapply(x, featType, FUN.VALUE = character(1L))
})

#' @rdname giotto_schema
#' @export
setMethod("featType", signature = "featData", function(x) x@feat_type)

#' @rdname giotto_schema
#' @export
setMethod("featType<-", signature("ANY"), function(x, value) x)

#' @rdname giotto_schema
#' @export
setMethod("featType<-", signature = "featData", function(x, value) {
    value <- as.character(value)
    x@feat_type <- value
    x
})

#' @rdname giotto_schema
#' @export
setMethod("featType<-", signature = "list", function(x, value) {
    if (length(x) != length(value)) {
        stop("Number of names to set must be the same as the length of list",
            call. = FALSE
        )
    }
    lapply(seq_along(x), function(i) {
        y <- x[[i]]
        featType(y) <- value[[i]]
        return(y)
    })
})

#' @rdname giotto_schema
#' @export
setMethod("featType<-", signature("giotto"), function(x, old, value) {
    checkmate::assert_character(old, len = 1L)
    checkmate::assert_character(value, len = 1L)
    if (!isTRUE(old %in% featType(x))) {
        stop("feat_type replace: feature type ", old, " does not exist\n", 
             call. = FALSE)
    }
    glist <- as.list(x)
    for(item_i in seq_along(glist)) {
        if (isTRUE(featType(glist[[item_i]]) == old))
            featType(glist[[item_i]]) <- value
    }
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
    g <- setGiotto(g, glist, initialize = TRUE, verbose = FALSE)
    
    active_feat <- activeFeatType(g)
    if (!isTRUE(active_feat %in% featType(g)) &&
        length(featType(g)) >= 1L) {
        first_feat <- featType(g)[[1]]
        activeFeatType(g) <- first_feat
        warning(wrap_txtf(
            "activeFeatType \'%s\' no longer exists. 
            activeFeatType defaulted to \'%s\'",
            active_feat, first_feat
        ), call. = FALSE)
    }
    return(g)
})



# objName ####

# default for unknown types
#' @rdname giotto_schema
#' @export
setMethod("objName", signature("ANY"), function(x) NA_character_)

#' @rdname giotto_schema
#' @export
setMethod("objName", signature("list"), function(x) {
    vapply(x, objName, FUN.VALUE = character(1L), USE.NAMES = FALSE)
})

#' @rdname giotto_schema
#' @export
setMethod("objName", signature("nameData"), function(x) x@name)

#' @rdname giotto_schema
#' @export
setMethod("objName", signature("giottoPoints"), function(x) x@feat_type)

#' @rdname giotto_schema
#' @export
setMethod("objName", signature("giottoLargeImage"), function(x) x@name)

#' @rdname giotto_schema
#' @export
setMethod("objName", signature("giottoImage"), function(x) x@name)

#' @rdname giotto_schema
#' @export
setMethod("objName<-", signature = "list", function(x, value) {
    if (length(x) != length(value)) {
        stop("Number of names to set must be the same as the length of list",
            call. = FALSE
        )
    }
    lapply(seq_along(x), function(i) {
        y <- x[[i]]
        objName(y) <- value[[i]]
        return(y)
    })
})

#' @rdname giotto_schema
#' @export
setMethod("objName<-", signature = "nameData", function(x, value) {
    value <- as.character(value)
    x@name <- value
    x
})

#' @rdname giotto_schema
#' @export
setMethod("objName<-", signature = "giottoImage", function(x, value) {
    value <- as.character(value)
    x@name <- value
    x
})

#' @rdname giotto_schema
#' @export
setMethod("objName<-", signature("giottoLargeImage"), function(x, value) {
    value <- as.character(value)
    x@name <- value
    x
})

# name describes the same thing as feat_type for giottoPoints
#' @rdname giotto_schema
#' @export
setMethod("objName<-", signature = "giottoPoints", function(x, value) {
    value <- as.character(value)
    x@feat_type <- value
    x
})






# prov ####
#' @rdname giotto_schema
#' @export
setMethod("prov", signature = "provData", function(x) x@provenance)


#' @rdname giotto_schema
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
#' @returns list
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
        obj_list, inherits, "nameData",
        FUN.VALUE = logical(1L)
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
