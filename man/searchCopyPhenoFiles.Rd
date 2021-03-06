% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/commons_public.R
\docType{methods}
\name{searchCopyPhenoFiles}
\alias{searchCopyPhenoFiles}
\alias{searchCopyPhenoFiles,Commons,character-method}
\title{Copy phenotype files to project directory}
\usage{
searchCopyPhenoFiles(object, userDataDir, ...)

\S4method{searchCopyPhenoFiles}{Commons,character}(object, userDataDir, ...,
  phsAcc = "", copyOrMove = "copy", showErr = TRUE)
}
\arguments{
\item{object}{Commons class object.}

\item{userDataDir}{a character string. The path to the top level directory of decrypted dbGaP phenotype files. The files can be from different studies and located in different sub-directories.}

\item{...}{There are optional arguments.}

\item{phsAcc}{a character string. The dbGaP study accession.}

\item{copyOrMove}{a character string. (optional) If the string value is 'copy' (default), copies the files to the project directory. If the string value is 'move', movies the files from the user data directory to the project directory.}

\item{showErr}{a logical value. (optional) If TRUE, display the error message; If FALSE (default), not display.}
}
\value{
(invisible) data frame. The meta-info of all copied files.
}
\description{
The method searches for the decrypted dbGaP phenotype files in a given directory and copies (or moves) the files to the configured project directory. When a study accession is provided, only the files of specified study are copied.
}
\section{Methods (by class)}{
\itemize{
\item \code{object = Commons,userDataDir = character}: A method of class Commons
}}

\examples{
\dontrun{

c <- Commons()
searchCopyPhenoFiles(c, userDataDir = '/home/user/data')
# or
searchCopyPhenoFiles(c, userDataDir = '/home/user/data', phsAcc="phs000001.v3.p1")
}
}
