% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregate.R
\name{polygon_to_raster}
\alias{polygon_to_raster}
\title{Convert polygon to raster}
\usage{
polygon_to_raster(polygon, field = NULL)
}
\arguments{
\item{polygon}{SpatVector polygon to rasterize}

\item{field}{character. Name of attribute of polygon that should be used
when rasterizing to assign values. Passing NULL uses the first attribute.}
}
\description{
convert polygon to raster
}
