#' @include package_imports.R
#' @include classes.R
NULL


# Giotto Object Settings ####
# Methods and documentations found in methods-IDs.R
setGeneric("spatIDs", function(x, ...) standardGeneric("spatIDs"))
setGeneric("featIDs", function(x, ...) standardGeneric("featIDs"))

## instructions ####
# Methods and documentations found in methods-instructions.R
setGeneric(
    "instructions",
    function(gobject, param, ...) standardGeneric("instructions")
)
setGeneric(
    "instructions<-",
    function(gobject, param, initialize, ..., value) {
        standardGeneric("instructions<-")
    }
)

## set defaults ####
# Methods and documentations found in methods-instructions.R
setGeneric(
    "activeSpatUnit",
    function(gobject, ...) standardGeneric("activeSpatUnit")
)
setGeneric(
    "activeSpatUnit<-",
    function(gobject, ..., value) standardGeneric("activeSpatUnit<-")
)



# Object creation ####
setGeneric(
    "createGiottoPoints",
    function(x, ...) standardGeneric("createGiottoPoints")
)
setGeneric(
    "createGiottoPolygon",
    function(x, ...) standardGeneric("createGiottoPolygon")
)


# Object reconnection ####
# Some objects may operate base on on-disk files.
setGeneric("reconnect", function(x, ...) standardGeneric("reconnect"))


# Object Characteristics ####

## nrow() S4 generic ####
if (!isGeneric("nrow")) setOldClass("nrow")
if (!isGeneric("ncol")) setOldClass("ncol")
if (!isGeneric("dim")) setOldClass("dim")

## colnames and rownames generics ####
if (!isGeneric("colnames")) setOldClass("colnames")
if (!isGeneric("rownames")) setOldClass("rownames")


# copy() S4 generic ####
setGeneric("copy",
    function(x) standardGeneric("copy"),
    useAsDefault = data.table::copy
)


# spatial operations ####
setGeneric(
    "calculateOverlap",
    function(x, y, ...) standardGeneric("calculateOverlap")
)
setGeneric(
    "overlapToMatrix",
    function(x, ...) standardGeneric("overlapToMatrix")
)

# expression value normalization
#' @title Normalize an object
#' @name normalize
#' @description Generics for normalizing an object containing measured values.
#' `normalize()` is intended as the central API for accessing
#' normalization workflows. Specific methods should be defined for it to
#' help with pre or post processing specific to a matrix class type.
#' The other generics are specific normalization generics made available for 
#' other packages to attach methods, which may differ depending on the input
#' matrix. No methods for normalization are exported from GiottoClass.
#' @param object,x a data object
#' @param ... additional arguments, for use in specific methods
#' @returns An object of the same class containing the normalized data
#' @export normalize
setGeneric("normalize", function(object, ...) standardGeneric("normalize"))
#' @rdname normalize
#' @export norm_lib
setGeneric("norm_lib", function(x, ...) standardGeneric("norm_lib"))
#' @rdname normalize
#' @export norm_log
setGeneric("norm_log", function(x, ...) standardGeneric("norm_log"))
#' @rdname normalize
#' @export norm_quantile
setGeneric("norm_quantile", function(x, ...) standardGeneric("norm_quantile"))
#' @rdname normalize
#' @export norm_pearson
setGeneric("norm_pearson", function(x, ...) standardGeneric("norm_pearson"))
#' @rdname normalize
#' @export norm_osmfish
setGeneric("norm_osmfish", function(x, ...) standardGeneric("norm_osmfish"))


# Methods and documentations found in methods-spatShift.R
setGeneric("spatShift", function(x, ...) standardGeneric("spatShift"))
setGeneric("affine", function(x, y, ...) standardGeneric("affine"))
setGeneric("shear", function(x, ...) standardGeneric("shear"))
setGeneric("XY", function(x, ...) standardGeneric("XY"))
setGeneric("XY<-", function(x, ..., value) standardGeneric("XY<-"))

# Methods and documentations found in methods-overlaps.R
setGeneric("overlaps", function(x, ...) standardGeneric("overlaps"))



# Giotto subnesting ####
# All methods and documentations found in methods-nesting.R

## prov() S4 generic ####
setGeneric("prov", function(x) standardGeneric("prov"))
setGeneric("prov<-", function(x, value) standardGeneric("prov<-"))

## spatUnit() S4 generic ####
setGeneric("spatUnit", function(x) standardGeneric("spatUnit"))
setGeneric("spatUnit<-", function(x, value) standardGeneric("spatUnit<-"))

## featType() S4 generic ####
setGeneric("featType", function(x) standardGeneric("featType"))
setGeneric("featType<-", function(x, value) standardGeneric("featType<-"))

## objName() generic ####
setGeneric("objName", function(x) standardGeneric("objName"))
setGeneric("objName<-", function(x, value) standardGeneric("objName<-"))

## setGiotto() generic ####
setGeneric("setGiotto", function(gobject, x, ...) standardGeneric("setGiotto"))


# coerce ####
setGeneric("as.sf", function(x, ...) standardGeneric("as.sf"))
setGeneric("as.sp", function(x, ...) standardGeneric("as.sp"))
setGeneric("as.stars", function(x, ...) standardGeneric("as.stars"))
setGeneric("as.terra", function(x, ...) standardGeneric("as.terra"))
