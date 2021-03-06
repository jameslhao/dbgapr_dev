% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/study_public.R
\docType{methods}
\name{variableScatterplot}
\alias{variableScatterplot}
\alias{variableScatterplot,Study,character-method}
\title{Variable scatter-plot}
\usage{
variableScatterplot(object, numPhvAcc_1, ...)

\S4method{variableScatterplot}{Study,character}(object, numPhvAcc_1, ...,
  numPhvAcc_2 = "", catPhvAcc = "", saveToDir = "", showPlot = TRUE)
}
\arguments{
\item{object}{Study class object.}

\item{numPhvAcc_1}{a character string. A dbGaP variable accession of the numeric variable type.}

\item{...}{There are optional argument.}

\item{numPhvAcc_2}{a character string. (optional) A dbGaP variable accession of the numeric variable type.}

\item{catPhvAcc}{a character string. (optional) A dbGaP variable accession of the categorical variable type. This value is used only if the arguments of numPhvAcc_1 and numPhvAcc_2 are both provided.}

\item{saveToDir}{a character string. (optional) The path to the directory where the plot PDF file is saved. If not provided, the file is saved in the 'temp' directory under the user project directory.}

\item{showPlot}{a logical value. (optional) If TRUE (default), shows the created graph; Not show if FALSE.}
}
\value{
a data frame. The data used for plotting.
}
\description{
The method draws the scatterplot of two numeric variables for each category of a categorical variable. If only one numeric variable accession (numPhvAcc_1) is provided, it draws the numeric variable values as the function of the respective subject indices. If both numeric variables (numPhvAcc_1 and numPhvAcc_2) are provided, it draws the values of the first variable as the function of the second. All variables should belong to the same study and have overlapping subjects. The created graph is saved as PDF and PNG files.
}
\section{Methods (by class)}{
\itemize{
\item \code{object = Study,numPhvAcc_1 = character}: of class Study
}}

\examples{
\dontrun{

s <- Study(phsAcc = 'phs000001.v3.p1')
numv1 = 'phv00000027.v2'
numv2 = 'phv00053747.v2'
catv = 'phv00053757.v2'
variableScatterplot(s, numPhvAcc_1=numv1, numPhvAcc_2=numv2, catPhvAcc=catv)
}
}
