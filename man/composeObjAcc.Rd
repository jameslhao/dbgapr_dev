% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/commons_internal.R
\docType{methods}
\name{composeObjAcc}
\alias{composeObjAcc}
\alias{composeObjAcc,Commons-method}
\title{Compose dbGaP object accession}
\usage{
composeObjAcc(object, objId, type, ...)

\S4method{composeObjAcc}{Commons}(object, objId, type, ..., objVer = NULL)
}
\arguments{
\item{object}{Commons class object.}

\item{objId}{an integer. A dbGaP object id.}

\item{type}{an integer. The type of object, such as 'study', 'dataset', or 'variable'.}

\item{...}{There are optional arguments.}

\item{objVer}{an integer. (optional) The object version.}
}
\value{
a character string. The object accession with or without version.
}
\description{
The method composes the study, dataset, variable accession from the given object id and version. If object version is not provided, it returns the object accession without version.
}
\section{Methods (by class)}{
\itemize{
\item \code{Commons}: A method of class Commons
}}

\examples{
\dontrun{

c <- Commons()
composeObjAcc(c, objId = 1255, type = 'study', objVer = 1)
}
}
\keyword{internal}
