#' @include generics.R
#' @include classes.R

# docs ----------------------------------------------------------- #
#' @title Apply a shear tranform
#' @name shear
#' @description Apply shear transformation to a spatial object.
#' Currently only works for 2D transforms. This implementation
#' applies a shear along one axis by adding the value of the other
#' axis after a multiplicative factor `fx` or `fy` is applied.
#' @param x object
#' @param fx numeric. x shear
#' @param fy numeric. y shear
#' @param x0 numeric. x-origin of shear
#' @param y0 numeric. y-origin of shear
#' @param ... additional args to pass (none implemented)
#' @returns shear transformed object
#' @examples
#' sl <- GiottoData::loadSubObjectMini("spatLocsObj")
#' 
#' plot(shear(sl, fx = 2))
#' 
#' # equivalent affine transform
#' shear_m <- diag(rep(1, 3))
#' shear_m[2, 1] <- 2
#' plot(affine(sl, shear_m))
#' plot(shear(sl, fx = 2, x0 = 0, y0 = 0))
NULL
# ---------------------------------------------------------------- #

setMethod("shear", signature("spatLocsObj"), function(
        x, fx = 0, fy = 0, x0, y0, ...
) {
    x <- data.table::copy(x)
    sdimx <- sdimy <- NULL
    
    # find center
    if (missing(x0)) x0 <- x[][, mean(range(sdimx))]
    if (missing(y0)) y0 <- x[][, mean(range(sdimy))]
    scenter <- c(x0, y0)
    
    if (!all(scenter == c(0, 0))) {
        # center values
        x <- spatShift(x, dx = -x0, dy = -y0)
    }
    
    # perform shears
    if (fx != 0) {
        x[][, sdimx := sdimy * fx + sdimx]
    }
    if (fy != 0) {
        x[][, sdimy := sdimx * fy + sdimy]
    }
    
    if (!all(scenter == c(0, 0))) {
        # return values to original positions
        x <- spatShift(x, dx = x0, dy = y0)
    }
    
    return(x)
})

