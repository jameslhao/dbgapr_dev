% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/study_public.R
\docType{methods}
\name{variableVenndiagram}
\alias{variableVenndiagram}
\alias{variableVenndiagram,Study,character-method}
\title{Subject overlapping with Venndigram}
\usage{
variableVenndiagram(object, phvAccList, ...)

\S4method{variableVenndiagram}{Study,character}(object, phvAccList, ...,
  saveToDir = "", showPlot = T)
}
\arguments{
\item{object}{Study class object}

\item{phvAccList}{a character vector. A list of dbGaP variable accessions. The maximum number of variable accessions accepted for the drawing is 5, so that only the first 5 variables in the input list are included in the Venndiagram.}

\item{...}{There are optional argument.}

\item{saveToDir}{a character string. (optional) The path to the directory where the plot PDF file is saved. If not provided, the file is saved in the 'temp' directory under the user project directory.}

\item{showPlot}{a logical value. (optional) If TRUE (default), shows the created graph; Not show if FALSE.}
}
\value{
a data frame. The data used for plotting.
}
\description{
The method shows the overlapping subjects of given dbGaP variables by Venndiagram. The number of input variables is limited to five.
}
\section{Methods (by class)}{
\itemize{
\item \code{object = Study,phvAccList = character}: of class Study
}}

\examples{
\dontrun{

s <- Study(phsAcc = 'phs000001.v3.p1')
accList = c('phv00054139.v1', 'phv00053796.v2', 'phv00000089.v2')
variableVenndiagram(s, phvAccList = accList) 
}
}
