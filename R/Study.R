# ++++++++++++++
# Class: Study 
# ++++++++++++++
#' @include Commons.R 
NULL

#' Class Study 
#'
#' The class contains the methods that handle the data of a specified study. The functions of this class include viewing and retrieving the data and meta-data of the specified study by accessions, visulizing variable data through a variety of plotting methods, etc. The data (datasets, variables) and meta-data (data ditionary) files of the study need to be available under the user project directory before calling the functions under this class. A dbGaP study accession is rerquired for initializing the class.
#'
#' @import methods
#' @slot phsAcc a character string. A dbGaP study accession.
#' @export Study 
#' @import plyr
#' @importFrom reshape2 melt 
#' @import VennDiagram
#' @import futile.logger
#' @import ggplot2
#' @examples 
#' \dontrun{
#'
#' s <- new("Study", phsAcc = 'phs000001.v1.p1')
#' s <- Study(phsAcc = 'phs000001.v1.p1')
#'}

Study <- setClass("Study",
                  slots = list(
                               phsAcc = 'character'
                               ),

                  # Set the default values for the slots
                  prototype = list(
                                   phsAcc = ''
                                   ),

                  validity = function(object) {
                      errors <- character()
                      phsAcc <- object@phsAcc

                      # Validate accession
                      phsAcc <- cleanObjAcc(object, acc = phsAcc, type = 'phs')


                      # Accession format check
                      phsAcc.rex <- "^phs\\d{6}.v\\d+"
                      phsAcc.match <- grepl(phsAcc.rex, object@phsAcc, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)

                  },

                  contains = "Commons"

                  )

# ----------------------
# Method: initialize 
# ----------------------

#' Initialize Study class 
#'
#' The method initializes the Study class. Once the class is initialized, the input study accession and all varibles associated with the parent class Commons become available to all classs methods through the class object. 
#'
#' @param .Object private arugment used in class init 
#' @param ... There are optional arguments. 
#' @param phsAcc a character string. A dbGaP study accession.
#' @examples 
#' \dontrun{
#'
#' s <- new("Study", phsAcc = 'phs000001.v1.p1')
#' s <- Study(phsAcc = 'phs000001.v1.p1')
#'}

#'
setMethod("initialize",
          "Study",
          function(.Object, ..., phsAcc = .Object@phsAcc) {

              .Object <- callNextMethod()

              # Validate accession
              phsAcc <- cleanObjAcc(.Object, acc = phsAcc, type = 'phs')

              msg <- paste("Initializing class Study with ", phsAcc, "\n", sep="")
              cat(msg)

              .Object@phsAcc <- phsAcc

              .Object
          })


# ----------------
# Method: show 
# ----------------

#' Display class methods 
#'
#' The method displays all the available methods in the Study class.
#'
#' @param object Study class object. 
#' @export show
#' @examples
#' \dontrun{
#'
#' s <- Study(phsAcc = 'phs000001.v1.p1')
#' show(s) 
#'}
#
setMethod(
          f="show", 
          signature="Study",
          definition=function(object) {
              cat("\n")
              cat("Class Methods:\n")

              # Note: set where=search() to show only S4 method 
              s4Methods <- classMethods <- showMethods(classes="Study", where = search(), showEmpty = FALSE, inherited = TRUE, printTo = FALSE)
              s4MethodsCombo = paste(s4Methods, collapse="\n")
              cat("\n")
              cat(s4MethodsCombo)
              cat("\n")


          })






####################################
# List of functions of this Class 
####################################
# initialize
# show
# getVariableDataByPhvAccAndSubjId 
# getVariableDataByPhvAcc 
# viewStudyVariableInfo 
# getPhvAccListByTerms
# getStudyVariableInfo 
# getVariableInfoByPhvAcc
# getDatasetDataByPhtAcc
# checkObjStudyByAcc
# checkPhvAccList 
# variableDataSummary 
# variableHistogram
# variableScatterplot
# variableBoxplot
# variablePiechart
# variableSubjVenndiagram
# variableCorrelgram
