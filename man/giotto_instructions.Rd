% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/instructions.R, R/methods-instructions.R
\name{createGiottoInstructions}
\alias{createGiottoInstructions}
\alias{giotto_instructions}
\alias{instructions}
\alias{instructions<-}
\alias{instructions,missing,missing-method}
\alias{instructions,giotto,missing-method}
\alias{instructions,giotto,character-method}
\alias{instructions,giottoInstructions,character-method}
\alias{instructions<-,giotto,missing,missing-method}
\alias{instructions<-,giotto,missing,logical-method}
\alias{instructions<-,giotto,character,missing-method}
\alias{instructions<-,giotto,character,logical-method}
\alias{instructions<-,giottoInstructions,character,ANY-method}
\title{Giotto instructions}
\usage{
createGiottoInstructions(
  python_path = getOption("giotto.py_path"),
  show_plot = NULL,
  return_plot = NULL,
  save_plot = NULL,
  save_dir = NULL,
  plot_format = NULL,
  dpi = NULL,
  units = NULL,
  height = NULL,
  width = NULL,
  is_docker = FALSE,
  plot_count = 0,
  fiji_path = NULL,
  no_python_warn = FALSE
)

\S4method{instructions}{missing,missing}(gobject, param, ...)

\S4method{instructions}{giotto,missing}(gobject)

\S4method{instructions}{giotto,character}(gobject, param)

\S4method{instructions}{giottoInstructions,character}(gobject, param)

\S4method{instructions}{giotto,missing,missing}(gobject, initialize) <- value

\S4method{instructions}{giotto,missing,logical}(gobject, initialize) <- value

\S4method{instructions}{giotto,character,missing}(gobject, param, initialize) <- value

\S4method{instructions}{giotto,character,logical}(gobject, param, initialize) <- value

\S4method{instructions}{giottoInstructions,character,ANY}(gobject, param) <- value
}
\arguments{
\item{python_path}{path to python binary to use or directory one level
up from the \code{env} directory (similar to output of
\code{reticulate::miniconda_path()})}

\item{show_plot}{print plot to console, default = TRUE}

\item{return_plot}{return plot as object, default = TRUE}

\item{save_plot}{automatically save plot, dafault = FALSE}

\item{save_dir}{path to directory where to save plots}

\item{plot_format}{format of plots (defaults to png)}

\item{dpi}{resolution for raster images}

\item{units}{units of format (defaults to in)}

\item{height}{height of plots}

\item{width}{width of  plots}

\item{is_docker}{using docker implementation of Giotto (defaults to FALSE)}

\item{plot_count}{(global option) start count for creating automatic unique
plots}

\item{fiji_path}{path to fiji executable}

\item{no_python_warn}{turn off warning that no compatible python env has
been detected}

\item{gobject}{giotto object}

\item{param}{Specific param in instructions to access or modify}

\item{\dots}{params to pass to \code{createGiottoInstructions()}}

\item{initialize}{(boolean, default = TRUE) whether to initialize the giotto
object}

\item{value}{value to set}
}
\value{
\code{giottoInstructions}, instructions settings, or \code{giotto} objects
with modified instructions
}
\description{
Giotto instructions are default settings that are applied at the \code{giotto}
object level. Once added to an object, they affect the way that the object
behaves. You can create a \code{giottoInstructions} object using
\code{createGiottoInstructions()} and add them to the \code{giotto} object during
creation or using the \code{instructions()} generic. Specific settings can be
replaced or retrieved using the \code{param} argument. Additionally, when using
\verb{instructions<-()} as a replacement function, \code{initialize()} will be called
on the \code{giotto} object if \code{initialize = TRUE}.

If no \code{giottoInstructions} object is provided during \code{giotto} object
creation, then a default one will be created during \code{giotto} object
initialization.
}
\examples{
g <- GiottoData::loadGiottoMini("visium")

# create instructions
ins <- instructions()

# get instructions
instrs <- instructions(g)
force(instrs)

# get single instructions param
instructions(g, "show_plot")

# replace an instruction param
instructions(g, "show_plot") <- FALSE
instructions(g, "show_plot")

# replace multiple instruction params
instructions(g)
instructions(g, c("show_plot", "dpi")) <- list(TRUE, 600)
instructions(g)

# replace instructions
i <- createGiottoInstructions()
instructions(g) <- i
instructions(g)

}
