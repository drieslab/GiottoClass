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
NULL

#' @title Feature type information
#' @name featType-generic
#' @description access and set feat_type slot of S4 subobject
#' @param x a Giotto S4 class subobject with feature type
#' @param value value to set as feature type
#' @aliases featType featType<-
#' @returns feat_type
NULL

#' @title Giotto object name information
#' @name objName-generic
#' @description access and set name slot fo S4 subobject
#' @param x a Giotto S4 class subobject with name data
#' @param value value to set as object name
#' @aliases objName objName<-
#' @returns name slot
NULL

#' @title Provenance information
#' @name prov-generic
#' @description access and set provenance slot of S4 subobject
#' @param x a Giotto S4 class subobject
#' @param value value to set as provenance
#' @aliases prov prov<-
#' @returns provenance slot
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
#' @noRd
#' @name assign_objnames_2_list
#' @title Assign object names to list names
#' @param obj_list list including giotto S4 objects
#' @param force_replace boolean, default = FALSE. Whether to replace the 
#' names of objects for which the name already has a name for
#' @examples
#' \dontrun{
#' e <- new("exprObj")
#' t_l <- replicate(3L, e)
#' # name the object
#' t_l <- lapply(seq_along(t_l), function(i) {
#'     objName(t_l[[i]]) <- c("a", "b", "c")[[i]]
#'     return(t_l[[i]])
#' })
#' # assign the names to the listnames
#' t_l <- assign_objnames_2_list(t_l)
#' }
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
    is_obj <- sapply(obj_list, inherits, "nameData")

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


#' @noRd
#' @name assign_listnames_2_obj
#' @title Assign list names to objects
#' @param obj_list list including giotto S4 objects
#' @examples
#' \dontrun{
#' t_l <- list("name_to_set" = new("exprObj"))
#' t_l <- assign_listnames_2_obj(t_l)
#' }
#' @keywords internal
assign_listnames_2_obj <- function(obj_list) {
    list_names <- names(obj_list)
    if (is.null(list_names)) stop("List has no names\n")
    obj_index <- which(sapply(obj_list, inherits, "nameData"))
    list_obj_names <- list_names[obj_index]

    for (obj_i in seq_along(obj_index)) {
        objName(obj_list[[obj_index[[obj_i]]]]) <- list_obj_names[obj_i]
    }

    return(obj_list)
}
