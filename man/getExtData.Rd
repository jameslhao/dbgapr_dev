% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/commons_internal.R
\docType{methods}
\name{getExtData}
\alias{getExtData}
\alias{getExtData,Commons,character-method}
\title{(internal) Get the supplemental (external) data}
\usage{
getExtData(object, type, ...)

\S4method{getExtData}{Commons,character}(object, type, ..., phsAcc = "",
  dataStudyOnly = TRUE)
}
\arguments{
\item{object}{Commons class object.}

\item{type}{a character string. The object type that is either 'study' (all_study_info), 'dataset' (study_dataset_info), 'variable' (study_variable_info), 'id' (study_id_variable_name), 'code' (study_variable_code_value), or 'manifest' (study_file_manifest).}

\item{...}{There are optional arguments.}

\item{phsAcc}{a character string. (optional) The study accession. It is required when the type argument value is not 'study'.}

\item{dataStudyOnly}{a logical value. When TRUE (default), only downloads the dataset and variable metadata of the stdudies that have data files in the project directory.  When FALSE, downloads the dataset and variable metadata of all dbGaP released studies, regardless the actual phenotype data files of the studies are downloaded or not.}
}
\value{
a data frame. The meta-data of respective type provided through the input.
}
\description{
The method returns the supplemental data from the files downloaded from the dbGaP ftp site.
}
\section{Methods (by class)}{
\itemize{
\item \code{object = Commons,type = character}: A method of class Commons
}}

\examples{
\dontrun{

c <- Commons()
getExtData(c, type = 'study')
# or
getExtData(c, type = 'dataset', phsAcc = 'phs000001.v3')
# or
getExtData(c, type = 'variable', phsAcc = 'phs000001.v3')
# or
getExtData(c, type = 'id', phsAcc = 'phs000001.v3')
# or
getExtData(c, type = 'code', phsAcc = 'phs000001.v3')
# or
getExtData(c, type = 'manifest', phsAcc = 'phs000001.v3')
}
}
\keyword{internal}
