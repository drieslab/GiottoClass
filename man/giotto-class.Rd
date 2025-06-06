% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\docType{class}
\name{giotto-class}
\alias{giotto-class}
\alias{giotto}
\title{S4 giotto Class}
\value{
giotto object
}
\description{
Giotto's core object that encapsulates all the components
of a spatial-omic project and facilitates analyses.
}
\details{
[\strong{initialize}]
The \code{giotto} class has a robust \code{initialize()} method that is automatically
called upon setting data into the object, updates of
the \code{giottoInstructions}, and loading of saved objects.
It performs the following steps:
\enumerate{
\item Update the object and subobjects for class definition changes if needed
\item Ensure a set of \code{giottoInstructions} are available, otherwise generate
defaults
\item Ensure a giotto python environment is accessible when the options
giotto.has_conda and giotto.use_conda are TRUE
\item Check the active spat_unit and feat_type
\item Ensure spatial/cell ID consistency and initialize the cell_ID and feat_ID
slots for the active spat_unit and feat_type, as well as cell and feature
metadata if they do not exist. Values for IDs and metadata are pulled
from any existing data in spatial_info/feat_info or expression slots,
with a preference for the latter.
\item Perform slot-specific and hierarchical checks that ensure dependent pieces
of information are only added AFTER the data that they depend on and that
existing information is consistent across slots.
\item Object validity checking
}
}
\section{Slots}{

\describe{
\item{\code{expression}}{expression information}

\item{\code{expression_feat}}{The different features or modalities such as rna,
protein, metabolites, ... that are provided in the expression slot.}

\item{\code{spatial_locs}}{spatial location coordinates for cells/spots/grids}

\item{\code{spatial_info}}{information about spatial units (Giotto spatVector)}

\item{\code{cell_metadata}}{metadata for cells}

\item{\code{feat_metadata}}{metadata for available features}

\item{\code{feat_info}}{information about features (Giotto spatVector)}

\item{\code{cell_ID}}{unique cell IDs}

\item{\code{feat_ID}}{unique feature IDs for all features or modalities}

\item{\code{spatial_network}}{spatial network in data.table/data.frame format}

\item{\code{spatial_grid}}{spatial grid in data.table/data.frame format}

\item{\code{spatial_enrichment}}{slot to save spatial enrichment-like results}

\item{\code{dimension_reduction}}{slot to save dimension reduction coordinates}

\item{\code{nn_network}}{nearest neighbor network in igraph format}

\item{\code{images}}{slot to store giotto image objects}

\item{\code{parameters}}{slot to save parameters that have been used}

\item{\code{instructions}}{slot for global function instructions}

\item{\code{offset_file}}{offset file used to stitch together image fields}

\item{\code{versions}}{giotto object metadata and versioning info}

\item{\code{join_info}}{information about joined Giotto objects}

\item{\code{multiomics}}{multiomics integration results}

\item{\code{h5_file}}{path to h5 file}
}}

\examples{
giotto()
}
\concept{giotto object}
