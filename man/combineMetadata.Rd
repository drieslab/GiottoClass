% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combine_metadata.R
\name{combineMetadata}
\alias{combineMetadata}
\title{combineMetadata}
\usage{
combineMetadata(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  spat_loc_name = "raw",
  spat_enr_names = NULL,
  verbose = TRUE
)
}
\arguments{
\item{gobject}{Giotto object}

\item{spat_unit}{spatial unit}

\item{feat_type}{feature type}

\item{spat_loc_name}{name of spatial locations to include}

\item{spat_enr_names}{names of spatial enrichment results to include}

\item{verbose}{verbosity}
}
\value{
Extended cell metadata in data.table format.
}
\description{
This function combines the cell metadata with spatial locations
and enrichment results from runSpatialEnrich.
}
\examples{
g <- GiottoData::loadGiottoMini("visium")

combineMetadata(g)
}
