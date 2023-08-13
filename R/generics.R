
#' @include classes.R
NULL


# Giotto Object Settings ####
# Methods and documentations found in methods-IDs.R
setGeneric('spatIDs', function(x, spat_unit, ...) standardGeneric('spatIDs'))
setGeneric('featIDs', function(x, feat_type, ...) standardGeneric('featIDs'))

## instructions ####
# Methods and documentations found in methods-instructions.R
setGeneric('instructions', function(gobject, param, ...) standardGeneric('instructions'))
setGeneric('instructions<-', function(gobject, param, initialize, ..., value) standardGeneric('instructions<-'))

## set defaults ####
# Methods and documentations found in methods-instructions.R
setGeneric('activeSpatUnit', function(gobject, ...) standardGeneric('activeSpatUnit'))
setGeneric('activeSpatUnit<-', function(gobject, ..., value) standardGeneric('activeSpatUnit<-'))


# centroids() S4 generic ####
#' @title centroids-generic
#' @name centroids-generic
#' @description Access centroids information from polygon objects
#' @param x object
#' @aliases centroids
#' @details For giottoPolygon, if centroids already exist, pulls from
#' \code{spatVectorCentroids} slot. Otherwise, generates from
#' \code{spatVector} slot de novo
#' @importMethodsFrom terra centroids
NULL

#' @rdname centroids-generic
#' @export
setMethod('centroids', signature(x = 'giottoPolygon'),
          function(x) {
            if(!is.null(x@spatVectorCentroids)) {
              return(x@spatVectorCentroids)
            } else {
              return(terra::centroids(x@spatVector))
            }
          })





# Methods and documentations found in methods-spatShift.R
setGeneric('spatShift', function(x, ...) standardGeneric('spatShift'))


#' @title overlaps-generic
#' @name overlaps-generic
#' @description Access list of overlaps information from object
#' @param x object
#' @aliases overlaps
setGeneric('overlaps', function(x, ...) standardGeneric('overlaps'))

#' @describeIn overlaps-generic Get overlaps information from giottoPolygon
#' @param name (optional) name of overlaps information to retrieve
#' @export
setMethod('overlaps', signature(x = 'giottoPolygon'),
          function(x, name = NULL) {
            if(is.null(name)) {
              # return entire list
              return(x@overlaps)
            } else {
              # return named entry
              return(x@overlaps[[name]])
            }
          })



# Object Characteristics ####

## nrow() S4 generic ####
if(!isGeneric('nrow')) setOldClass('nrow')
if(!isGeneric('ncol')) setOldClass('ncol')
if(!isGeneric('dim')) setOldClass('dim')

## colnames and rownames generics ####
if(!isGeneric('colnames')) setOldClass('colnames')
if(!isGeneric('rownames')) setOldClass('rownames')


# copy() S4 generic ####
if(!isGeneric('copy')) setGeneric('copy', function(x) {
  standardGeneric('copy')
})


# Giotto subnesting ####
# All methods and documentations found in methods-nesting.R

## prov() S4 generic ####
setGeneric('prov', function(x) standardGeneric('prov'))
setGeneric('prov<-', function(x, value) standardGeneric('prov<-'))

## spatUnit() S4 generic ####
setGeneric('spatUnit', function(x) standardGeneric('spatUnit'))
setGeneric('spatUnit<-', function(x, value) standardGeneric('spatUnit<-'))

## featType() S4 generic ####
setGeneric('featType', function(x) standardGeneric('featType'))
setGeneric('featType<-', function(x, value) standardGeneric('featType<-'))

## objName() generic ####
setGeneric('objName', function(x) standardGeneric('objName'))
setGeneric('objName<-', function(x, value) standardGeneric('objName<-'))






