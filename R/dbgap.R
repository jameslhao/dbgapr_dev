#' dbgapr: A dbGaP Phenotype Data Organizer
#'
#' dbgapr is a collection of the functions that help to organize, search, review, and retrieve the phenotype data downloaded from the Database of Genotypes and Phenotypes (\href{https://www.ncbi.nlm.nih.gov/gap}{dbGaP}) sponsored by the National Institutes of Health (NIH).
#'
#' The main functions of the package are:
#'
#' \itemize{
#' \item Organize downloaded phenotype data files by studies in a central location.
#' \item Merge the data files of different consents.  
#' \item Make data meta-info readily available for search and view.
#' \item Query data meta-info and retrieve data by dbGaP object accessions.
#' \item Basic plotting for data visualization.
#' }
#'
#' \emph{Vignettes}
#'
#' To learn more about dbgapr, start with the vignettes:
#' `browseVignettes(package = "dbgapr")`
#'
#' The functions of the package belong to \code{\linkS4class{Commons}} and \code{\linkS4class{Study}} two classes. The public functions of the classes are listed below.
#'
#' \emph{Commons class}
#'
#' \itemize{
#' \item \code{\link{prjConfig}}
#' \item \code{\link{getPrjDir}}
#' \item \code{\link{prepareData}}
#' \item \code{\link{searchCopyPhenoFiles}}
#' \item \code{\link{ftpDownload}}
#' \item \code{\link{recordPrjFileInfo}}
#' \item \code{\link{mergeDatasetConsent}}
#' \item \code{\link{accInfo}}
#' \item \code{\link{getAllStudyInfo}}
#' \item \code{\link{getAllDatasetInfo}}
#' }
#'
#' \emph{Study class}
#'
#' \itemize{
#' \item \code{\link{getStudyDatasetInfo}}
#' \item \code{\link{getStudyVariableInfo}}
#' \item \code{\link{getStudyVariableInfoByTerms}}
#' \item \code{\link{getStudyVariableData}}
#' \item \code{\link{getIdInfo}}
#' \item \code{\link{variableSummary}}
#' \item \code{\link{variableBoxplot}}
#' \item \code{\link{variableScatterplot}}
#' \item \code{\link{variableHistogram}}
#' \item \code{\link{variablePiechart}}
#' \item \code{\link{variableVenndiagram}}
#' \item \code{\link{variableCorrelHeatmap}}
#' }
#'
"_PACKAGE"



