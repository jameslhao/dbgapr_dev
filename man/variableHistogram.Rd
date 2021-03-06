% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/study_public.R
\docType{methods}
\name{variableHistogram}
\alias{variableHistogram}
\alias{variableHistogram,Study,character-method}
\title{Variable histogram}
\usage{
variableHistogram(object, phvAcc, ...)

\S4method{variableHistogram}{Study,character}(object, phvAcc, ...,
  withDensity = TRUE, saveToDir = "", showPlot = TRUE)
}
\arguments{
\item{object}{Study class object.}

\item{phvAcc}{a character string. The dbGaP variable accession.}

\item{...}{There are optional arguments.}

\item{withDensity}{a logical value. (optional) If TRUE (default), draws the histograms with a kernel density plot; If FALSE, draws without a kernel density plot.}

\item{saveToDir}{a character string. (optional) The path to the directory where the plot PDF file is saved. If not provided, the file is saved in the 'temp' directory under the user project directory.}

\item{showPlot}{a logical value. (optional) If TRUE (default), shows the created graph; Not show if FALSE. The created graph is saved as files regardless.}
}
\value{
a data frame. The data used for plotting.
}
\description{
The method draws the histograms of a given dbGaP variable.
}
\section{Methods (by class)}{
\itemize{
\item \code{object = Study,phvAcc = character}: of class Study
}}

