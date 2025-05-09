% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-copy.R
\name{copy}
\alias{copy}
\alias{copy,coordDataDT-method}
\alias{copy,giottoPoints-method}
\alias{copy,giottoPolygon-method}
\alias{copy,giottoLargeImage-method}
\title{Copy an entire object}
\usage{
\S4method{copy}{coordDataDT}(x)

\S4method{copy}{giottoPoints}(x)

\S4method{copy}{giottoPolygon}(x)

\S4method{copy}{giottoLargeImage}(x)
}
\arguments{
\item{x}{a Giotto S4 class subobject}
}
\value{
giotto subobjects
}
\description{
S4 generic for Giotto's S4 subobjects to return with full
copies of certain subobjects that usually return referenced information.
}
\examples{
g <- GiottoData::loadSubObjectMini("exprObj")

copy(g)
}
\seealso{
\code{\link[data.table]{copy}} \code{\link[terra]{deepcopy}}
}
