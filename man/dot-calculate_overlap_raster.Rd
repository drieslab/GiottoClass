% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregate.R
\name{.calculate_overlap_raster}
\alias{.calculate_overlap_raster}
\title{Find feature points overlapped by rasterized polygon.}
\usage{
.calculate_overlap_raster(
  spatvec,
  pointvec,
  count_info_column = NULL,
  verbose = TRUE
)
}
\arguments{
\item{spatvec}{\code{SpatVector} polygon from a \code{giottoPolygon} object}

\item{pointvec}{\code{SpatVector} points from a \code{giottoPoints} object}

\item{count_info_column}{column with count information (optional)}

\item{verbose}{be verbose}
}
\value{
\code{SpatVector} of overlapped points info
}
\description{
Core workflow function that accepts simple \code{SpatVector} inputs,
performs rasterization of the polys and then checks for overlaps.
}
\seealso{
\code{\link[=calculateOverlapRaster]{calculateOverlapRaster()}}
}
\concept{overlap}
\keyword{internal}
