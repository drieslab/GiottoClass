% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flex_functions.R
\name{rowSums_flex}
\alias{rowSums_flex}
\title{rowSums_flex}
\usage{
rowSums_flex(mymatrix, ...)
}
\arguments{
\item{mymatrix}{matrix to use}

\item{...}{other arguments passed to \code{\link{rowSums}}}
}
\value{
numeric
}
\description{
rowSums_flex
}
\examples{
m <- matrix(rnorm(100), nrow = 10)

rowSums_flex(m)
}
\keyword{internal}
