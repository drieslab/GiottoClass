% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_poly.R
\name{makePseudoVisium}
\alias{makePseudoVisium}
\title{makePseudoVisium}
\usage{
makePseudoVisium(
  extent = NULL,
  micron_scale = 1,
  micron_size = deprecated(),
  name = "pseudo_visium"
)
}
\arguments{
\item{extent}{SpatExtent or anything else a SpatExtent can be extracted or
created from}

\item{micron_scale}{scalefactor needed to convert the target coordinate
space to microns. For supported datasets, this can be found from
\code{instructions(gobject, "micron_scale")}. See details.}

\item{micron_size}{deprecated. Use \code{micron_scale}}

\item{name}{character. (default is 'pseudo_visium') Name of giottoPolygon
object to create}
}
\value{
A giottoPolygon for the pseudo-visium spots.
}
\description{
Generates a visium-like array of spots across a provided
spatial extent.
}
\details{
This function generates a pseudo-Visium array of spots across the
spatial extent provided. The \code{micron_scale} param is used to determine the
scaling of the array relative to the target coordinate system.
}
\section{\code{micron_scale}}{

If \code{a} is microns and \code{b} is dataset coordinate units, \code{micron_scale} is
calculated as \code{a / b}.
}

\examples{
e <- ext(0, 2000, 0, 2000)
x <- makePseudoVisium(extent = e, micron_scale = 1)
plot(x)
}
\concept{spatial location}
