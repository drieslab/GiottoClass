#' Create VisiumV1 Class
#' VisiumV1
#' @include interoperability.R
#' @description Creates a VisiumV1 object
#' @slot image array. 
#' @slot scale.factors scalefactors. 
#' @slot coordinates data.frame. 
#' @slot spot.radius numeric. 
#' @importClassesFrom SeuratObject SpatialImage
#' @return visium object
#' @export

  VisiumV1 <- setClass(Class = 'VisiumV1',
                          contains = 'SpatialImage',
                          slots = list('image' = 'array',
                                       'scale.factors' = 'scalefactors',
                                       'coordinates' = 'data.frame',
                                       'spot.radius' = 'numeric' ))
  