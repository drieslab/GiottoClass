% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/python_environment.R
\name{.check_giotto_python_modules}
\alias{.check_giotto_python_modules}
\title{Check if Giotto python modules are in python environment}
\usage{
.check_giotto_python_modules(my_python_path)
}
\arguments{
\item{my_python_path}{path to python environment}
}
\value{
character or NULL
}
\description{
Check for some core python packages that Giotto uses. This is an internal
currently only called by the \code{giotto} \code{initialize()} method
}
\keyword{internal}
