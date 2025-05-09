% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slot_accessors.R
\name{getSpatialGrid}
\alias{getSpatialGrid}
\title{Get spatial grid}
\usage{
getSpatialGrid(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  name = NULL,
  return_grid_Obj = FALSE,
  set_defaults = TRUE
)
}
\arguments{
\item{gobject}{giotto object}

\item{spat_unit}{spatial unit (e.g. "cell")}

\item{feat_type}{feature type (e.g. "rna", "dna", "protein")}

\item{name}{name of spatial grid}

\item{return_grid_Obj}{return grid object (default = FALSE)}

\item{set_defaults}{set default spat_unit and feat_type. Change to FALSE
only when
expression and spat_info are not expected to exist.}
}
\value{
spatialGridObj
}
\description{
Function to get spatial grid
}
\examples{
g <- GiottoData::loadGiottoMini("visium")
g <- createSpatialGrid(g, sdimx_stepsize = 5, sdimy_stepsize = 5)

getSpatialGrid(g)
}
\seealso{
Other spatial grid data accessor functions: 
\code{\link{setSpatialGrid}()}

Other functions to get data from giotto object: 
\code{\link{getCellMetadata}()},
\code{\link{getDimReduction}()},
\code{\link{getExpression}()},
\code{\link{getFeatureInfo}()},
\code{\link{getFeatureMetadata}()},
\code{\link{getGiottoImage}()},
\code{\link{getMultiomics}()},
\code{\link{getNearestNetwork}()},
\code{\link{getPolygonInfo}()},
\code{\link{getSpatialEnrichment}()},
\code{\link{getSpatialLocations}()},
\code{\link{getSpatialNetwork}()},
\code{\link{get_multiomics}()}
}
\concept{functions to get data from giotto object}
\concept{spatial grid data accessor functions}
