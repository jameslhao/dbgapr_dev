% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/study_internal.R
\docType{methods}
\name{getVarNameAccCombos}
\alias{getVarNameAccCombos}
\alias{getVarNameAccCombos,Study,character-method}
\title{(internal) Get concatenated variable name and accession combo strings}
\usage{
getVarNameAccCombos(object, phvAccList, ...)

\S4method{getVarNameAccCombos}{Study,character}(object, phvAccList, ...,
  studyDataDicDF = data.frame(), checkList = TRUE)
}
\arguments{
\item{object}{Study class object.}

\item{phvAccList}{a character vector. A list of the dbGaP variable accessions.}

\item{...}{There are optional arguments.}

\item{studyDataDicDF}{a data frame. (optional) Study variable meta-info (data dictionary).}

\item{checkList}{a logical value. (optional) If TRUE, check the input phvAccList. Not check if FALSE.}
}
\value{
a character vector. The concatenated combo strings of the variable name and accession.
}
\description{
The method returns a list of concatenated combo strings of variable name and accession given a data dictionary. When dealing with large phvAccList, it is important to set checkList FALSE. Otherwise the process takes will take too long to finish.
}
\section{Methods (by class)}{
\itemize{
\item \code{object = Study,phvAccList = character}: A method of class Study
}}

\examples{
\dontrun{

s <- Study(phsAcc = 'phs000001.v3.p1')
accList = c('phv00054119.v1.p1.c2', 'phv00053735.v2', 'phv00053732.v2')
getVarNameAccCombos(s, phvAccList = accList)
}
}
\keyword{internal}
