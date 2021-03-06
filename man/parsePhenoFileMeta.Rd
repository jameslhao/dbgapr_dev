% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/commons_internal.R
\docType{methods}
\name{parsePhenoFileMeta}
\alias{parsePhenoFileMeta}
\alias{parsePhenoFileMeta,Commons,character-method}
\title{(internal) Parse dataset file info}
\usage{
parsePhenoFileMeta(object, dbgapFile, ...)

\S4method{parsePhenoFileMeta}{Commons,character}(object, dbgapFile, ...,
  parseHeader = FALSE)
}
\arguments{
\item{object}{Commons class object.}

\item{dbgapFile}{a variable string. The path to a dbGaP phenotype file or a file name of the phenotype file.}

\item{...}{There are optional arguments.}

\item{parseHeader}{a logical value. (optional) If TRUE, opens the file and parses out the info from the file header; If FALSE (default), only the file name is parsed. Note: Always leave it as default FALSE, since parsing data file header takes too long, and parsed header info is not used for anything in this package.}
}
\value{
Returns a character list.  The phenotype file attributes, such as fStAcc (file study accession), fPhtAcc (file phenotype accession) etc.
}
\description{
The method parses the meta-info from pthe henotype dataset file name and headers, and saves the file associated attributes to a json file. It is called from \code{\link{writeFileInfoJson}}.
}
\section{Methods (by class)}{
\itemize{
\item \code{object = Commons,dbgapFile = character}: Resturns fileInfoSet
}}

\examples{
\dontrun{

c <- Commons() 
phtFile = '/home/foo/data/phs000001.AREDS.pht000001.v1.p1.c1.Eye_Disease_Only.txt.gz' 
parsePhenoFileMeta(c, dbgapFile = phtFile) 
}
}
\keyword{internal}
