% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/study_public.R
\docType{methods}
\name{getStudyVariableInfo}
\alias{getStudyVariableInfo}
\alias{getStudyVariableInfo,Study-method}
\title{Get meta-info of study variables}
\usage{
getStudyVariableInfo(object, ...)

\S4method{getStudyVariableInfo}{Study}(object, ..., phtAcc = "",
  phvAccList = vector(), dataType = "", dataStudyOnly = TRUE,
  showAs = "", editor = "")
}
\arguments{
\item{object}{Study class object.}

\item{...}{There are optional arguments.}

\item{phtAcc}{a character string. The dbGaP dataset accession.}

\item{phvAccList}{a character vector. The dbGaP phenotype variable accessions.}

\item{dataType}{a character string. Specifies the data type of returned variables. The possible value is either 'num' (for numeric variable) or 'cat' (for categorical variable).}

\item{dataStudyOnly}{a logical value. When TRUE (default), only downloads the dataset and variable metadata of the stdudies that have data files in the project directory.  When FALSE, downloads the dataset and variable metadata of all dbGaP released studies, regardless the actual phenotype data files of the studies are downloaded or not.}

\item{showAs}{a character string. (optional) When the value is 'table', displays the data as a table through a platform specific table viewer; When it is 'json', displays the json text through a plain text editor; When it is 'text', displays in a brief left-justified text format.}

\item{editor}{a character string. (optional) The name of your favorite plain text editor. It should be executable from a command-line prompt of the respective platform. For example, notepad (Windows), vim, emacs (Unix), gedit (Ubuntu), nedit (CentOS), etc.}
}
\value{
a data frame. The meta-info of input variables.
}
\description{
The method returns the meta-info of variables under the study. It takes either a list of variable accessions or a dataset accession and returns the meta-info of the respective variables. When neither a list of variable accessions nor a dataset accession is provided, it returns the meta-info of all variable of the class study. When the data type is provided, only the variables of the respective data type are returned.
}
\section{Methods (by class)}{
\itemize{
\item \code{Study}: of class Study
}}

\examples{
\dontrun{

s <- Study(phsAcc = 'phs000001.v3.p1')
getStudyVariableInfo(s)
# or
getStudyVariableInfo(s, phtAcc = 'pht000370.v2.p1')
or
getStudyVariableInfo(s, phtAcc = 'pht000370.v2.p1', dataType = 'num')
# or
accList = c('phv00054119.v1.p1.c2', 'phv00054118.v1.p1', 'phv00053733.v2')
getStudyVariableInfo(s, phvAccList = accList) 
# or
getStudyVariableInfo(s, phvAccList = accList, showAs = 'table')) 
}
}
