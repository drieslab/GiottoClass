# ---------------------------------------------------------------------------- #
# Flex functions provide a way for Giotto to dispatch to specific methods in a
# way that lies outside of the standard S3 and S4 dispatch. This is useful for
# cases where the method desired is not the one that was originally defined for
# the class in its native package as dispatching in this way will not pollute
# the user's environment.
#
# Code within Giotto SHOULD use these flex functions as drop-in replacements for
# their normal generic counterparts.
# ---------------------------------------------------------------------------- #

# * lapply_flex is in GiottoUtils


#' @title mean_flex
#' @name mean_flex
#' @param x data to use
#' @param ... other arguments to pass
#' @returns numeric
#' @keywords internal
#' @examples
#' m <- matrix(rnorm(100), nrow = 10)
#'
#' mean_flex(m)
#' @export
mean_flex <- function(x, ...) {
    if (inherits(x, "HDF5Matrix")) {
        return(Matrix::mean(x, ...))
    } else if (inherits(x, "dgCMatrix")) {
        return(Matrix::mean(x, ...)) # replace with sparseMatrixStats
    } else if (inherits(x, "Matrix")) {
        return(Matrix::mean(x, ...))
    } else {
        return(base::mean(x, ...))
    }
}



#' @title rowSums_flex
#' @name rowSums_flex
#' @param mymatrix matrix to use
#' @returns numeric
#' @keywords internal
#' @examples
#' m <- matrix(rnorm(100), nrow = 10)
#'
#' rowSums_flex(m)
#' @export
rowSums_flex <- function(mymatrix) {
    if (inherits(mymatrix, "DelayedArray")) {
        # return(Matrix::rowSums(mymatrix))
        # } else if(inherits(mymatrix, 'DelayedMatrix')) {
        return(DelayedMatrixStats::rowSums2(mymatrix))
    } else if (inherits(mymatrix, "dgCMatrix")) {
        return(Matrix::rowSums(mymatrix)) # replace with sparseMatrixStats
    } else if (inherits(mymatrix, "Matrix")) {
        return(Matrix::rowSums(mymatrix))
    } else {
        temp_matrix <- as.matrix(mymatrix)
        temp_res <- matrixStats::rowSums2(temp_matrix)
        names(temp_res) <- rownames(temp_matrix)
        return(temp_res)
    }
}



#' @title rowMeans_flex
#' @name rowMeans_flex
#' @param mymatrix matrix to use
#' @returns numeric
#' @keywords internal
#' @examples
#' m <- matrix(rnorm(100), nrow = 10)
#'
#' rowMeans_flex(m)
#' @export
rowMeans_flex <- function(mymatrix) {
    # replace by MatrixGenerics?
    if (inherits(mymatrix, "DelayedArray")) {
        # return(Matrix::rowMeans(mymatrix))
        # } else  if(inherits(mymatrix, 'DelayedMatrix')) {
        return(DelayedMatrixStats::rowMeans2(mymatrix))
    } else if (inherits(mymatrix, "dgCMatrix")) {
        return(Matrix::rowMeans(mymatrix)) # replace with sparseMatrixStats
    } else if (inherits(mymatrix, "Matrix")) {
        return(Matrix::rowMeans(mymatrix))
    } else {
        temp_matrix <- as.matrix(mymatrix)
        temp_res <- matrixStats::rowMeans2(temp_matrix)
        names(temp_res) <- rownames(temp_matrix)
        return(temp_res)
    }
}



#' @title colSums_flex
#' @name colSums_flex
#' @param mymatrix matrix to use
#' @returns numeric
#' @keywords internal
#' @examples
#' m <- matrix(rnorm(100), nrow = 10)
#'
#' colSums_flex(m)
#' @export
colSums_flex <- function(mymatrix) {
    if (inherits(mymatrix, "DelayedArray")) {
        return(DelayedMatrixStats::colSums2(mymatrix))
        # return(Matrix::colSums(mymatrix))
        # } else if(inherits(mymatrix, 'DelayedMatrix')) {
    } else if (inherits(mymatrix, "dgCMatrix")) {
        return(Matrix::colSums(mymatrix)) # replace with sparseMatrixStats
    } else if (inherits(mymatrix, "Matrix")) {
        return(Matrix::colSums(mymatrix))
    } else {
        temp_matrix <- as.matrix(mymatrix)
        temp_res <- matrixStats::colSums2(temp_matrix)
        names(temp_res) <- colnames(temp_matrix)
        return(temp_res)
    }
}



#' @title colMeans_flex
#' @name colMeans_flex
#' @param mymatrix matrix to use
#' @returns numeric
#' @keywords internal
#' @examples
#' m <- matrix(rnorm(100), nrow = 10)
#'
#' colMeans_flex(m)
#' @export
colMeans_flex <- function(mymatrix) {
    if (inherits(mymatrix, "DelayedArray")) {
        # return(Matrix::colMeans(mymatrix))
        # } else if(inherits(mymatrix, 'DelayedMatrix')) {
        return(DelayedMatrixStats::colMeans2(mymatrix))
    } else if (inherits(mymatrix, "dgCMatrix")) {
        return(Matrix::colMeans(mymatrix)) # replace with sparseMatrixStats
    } else if (inherits(mymatrix, "Matrix")) {
        return(Matrix::colMeans(mymatrix))
    } else {
        temp_matrix <- as.matrix(mymatrix)
        temp_res <- matrixStats::colMeans2(temp_matrix)
        names(temp_res) <- colnames(temp_matrix)
        return(temp_res)
    }
}



#' @title t_flex
#' @name t_flex
#' @param mymatrix matrix to use
#' @returns matrix
#' @keywords internal
#' @examples
#' m <- matrix(rnorm(100), nrow = 10)
#'
#' t_flex(m)
#' @export
t_flex <- function(mymatrix) {
    if (inherits(mymatrix, "DelayedArray")) {
        # return(methods::as(t(mymatrix), 'HDF5Matrix'))
        return(DelayedArray::t(mymatrix))
    } else if (inherits(mymatrix, "dgCMatrix")) {
        return(Matrix::t(mymatrix)) # replace with sparseMatrixStats
    } else if (inherits(mymatrix, "Matrix")) {
        return(Matrix::t(mymatrix))
    } else if (inherits(mymatrix, "spatLocsObj")) {
        return(t(mymatrix))
    } else if (inherits(mymatrix, "spatialNetworkObj")) {
        return(t(mymatrix))
    } else {
        mymatrix <- as.matrix(mymatrix)
        mymatrix <- base::t(mymatrix)
        return(mymatrix)
    }
}



#' @title cor_flex
#' @name cor_flex
#' @param x data to use
#' @param ... other arguments passed to stats::cor()
#' @returns numeric
#' @keywords internal
#' @examples
#' m <- matrix(rnorm(100), nrow = 10)
#'
#' cor_flex(m)
#' @export
cor_flex <- function(x, ...) {
    x <- as.matrix(x)
    return(stats::cor(x, ...))
}






#' @title my_arowMeans
#' @name  my_arowMeans
#' @param x data to use
#' @returns numeric
#' @keywords internal
#' @examples
#' m <- matrix(rnorm(100), nrow = 10)
#'
#' my_arowMeans(m)
#' @export
my_arowMeans <- function(x) {
    if (is.null(nrow(x))) {
        x # if only one column is selected
        # mean(x)
    } else {
        rowMeans_flex(x)
    }
}



#' @title my_growMeans
#' @name  my_growMeans
#' @param x data to use
#' @param offset offset
#' @returns numeric
#' @keywords internal
#' @examples
#' m <- matrix(rnorm(100), nrow = 10)
#'
#' my_growMeans(abs(m))
#' @export
my_growMeans <- function(x, offset = 0.1) {
    if (is.null(nrow(x))) {
        x # if only one column is selected
        # exp(mean(log(x+offset)))-offset
    } else {
        exp(rowMeans_flex(log(x + offset))) - offset
    }
}

#' @title my_rowMeans
#' @name  my_rowMeans
#' @param x data to use
#' @param method method is either "arithmic" or "geometric"
#' @param offset offset
#' @returns numeric
#' @keywords internal
#' @examples
#' m <- matrix(rnorm(100), nrow = 10)
#'
#' my_rowMeans(m)
#' @export
my_rowMeans <- function(x, method = c("arithmic", "geometric"), offset = 0.1) {
    method <- match.arg(method, c("arithmic", "geometric"))
    if (method == "arithmic") {
        return(my_arowMeans(x = x))
    }
    if (method == "geometric") {
        return(my_growMeans(x = x, offset = offset))
    }
}



#' @title standardise_flex
#' @name standardise_flex
#' @description standardizes a matrix
#' @param x matrix
#' @param center center data
#' @param scale scale data
#' @returns standardized matrix
#' @keywords internal
#' @examples
#' m <- matrix(rnorm(100), nrow = 10)
#'
#' standardise_flex(m)
#' @export
standardise_flex <- function(x, center = TRUE, scale = TRUE) {
    if (inherits(x, "DelayedArray")) {
        package_check("ScaledMatrix")

        y <- ScaledMatrix::ScaledMatrix(x = x, center = center, scale = scale)
    } else {
        if (center & scale) {
            y <- t_flex(x) - colMeans_flex(x)
            y <- y / sqrt(rowSums_flex(y^2)) * sqrt((dim(x)[1] - 1))
            y <- t_flex(y)
        } else if (center & !scale) {
            y <- t_flex(x) - colMeans_flex(x)
            y <- t_flex(y)
        } else if (!center & scale) {
            csd <- matrixStats::colSds(x)
            # csd = DelayedMatrixStats::colSds(x)
            y <- t_flex(t_flex(x) / csd)
        } else {
            y <- x
        }
    }
}
