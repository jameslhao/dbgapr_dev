% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/commons_internal.R
\docType{methods}
\name{detectVariableDataType}
\alias{detectVariableDataType}
\alias{detectVariableDataType,Commons,character-method}
\title{(internal, deprecated) Detect variable data type}
\usage{
detectVariableDataType(object, phsAcc, phtAcc, phvAcc, varName, ...)

\S4method{detectVariableDataType}{Commons,character}(object, phsAcc, phtAcc,
  phvAcc, varName, ..., codeValCombo = NA, fileInfoDF = data.frame())
}
\arguments{
\item{object}{Commons class object.}

\item{phsAcc}{a character string. The dbGaP study accession of the given variable.}

\item{phtAcc}{a character string. The dbGaP dataset accession of the given variable.}

\item{phvAcc}{a character string. The dbGaP phenotype variable accession.}

\item{varName}{a character string. The dbGaP phenotype variable name.}

\item{...}{There are optional arguments.}

\item{codeValCombo}{a character string. The combo string of a categorical variable value code and the respective code meaning.}

\item{fileInfoDF}{a data frame. The file info data.}
}
\value{
a character string. The data types of the input variable accession.
}
\description{
The method determines the data type of a given variable by looking into the data values. The funciont is called through \code{\link{dataDicXmlParser}}. Running this method is resource expensive.
}
\section{Methods (by class)}{
\itemize{
\item \code{object = Commons,phsAcc = character}: A method of class Study
}}

\keyword{internal}
