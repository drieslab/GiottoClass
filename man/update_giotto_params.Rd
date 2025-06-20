% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function_logging.R
\name{update_giotto_params}
\alias{update_giotto_params}
\title{Update giotto parameters}
\usage{
update_giotto_params(
  gobject,
  description = "_test",
  return_gobject = TRUE,
  toplevel = 2,
  attachments = NULL
)
}
\arguments{
\item{gobject}{giotto object}

\item{description}{description of function run}

\item{return_gobject}{logical. Whether the giotto object should be returned}

\item{toplevel}{expected relative stackframe where call that is being
recorded was made. If negative, param recording is skipped}

\item{attachments}{named list. Items to attach. These are intended for lightweight
param classes containing settings. No large items should be added here.}
}
\value{
giotto object or list of parameters
}
\description{
For developer use. Adds an entry to the \code{giotto} object object history.
Care currently needs to be taken when a function that contains a call to
this function is called from within yet another function. In such cases,
a \code{toplevel < 0} or setting a temporary \code{"giotto.update_param" = FALSE} with
\code{GiottoUtils::gwith_option()} may be the best option to avoid either
evaluation errors or strange history entries. A new \code{update_giotto_params()}
call can then be added that describes the function of the topmost function
if desired.
}
\examples{
g <- GiottoData::loadGiottoMini("visium")

update_giotto_params(g, toplevel = 1)
}
