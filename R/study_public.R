#######################
# study_public.R
#######################

# The methods of Study class that are intended to be used by users.
# ATTN! The documentation rd file of this function is manually created: man/study_public.rd


# ---------------------------------- 
# Method: getStudyVariableInfo 
# ---------------------------------- 

#' Get meta-info of study variables
#'
#' The method returns the meta-info of variables under the study. It takes either a list of variable accessions or a dataset accession and returns the meta-info of the respective variables. When neither a list of variable accessions nor a dataset accession is provided, it returns the meta-info of all variable of the class study. When the data type is provided, only the variables of the respective data type are returned.
#'
#' @param object Study class object.
#' @param ... There are optional arguments.
#' @param phtAcc a character string. The dbGaP dataset accession.
#' @param phvAccList a character vector. The dbGaP phenotype variable accessions.
#' @param dataType a character string. Specifies the data type of returned variables. The possible value is either 'num' (for numerical variable) or 'cat' (for categorical variable).
#' @param showAs a character string. (optional) When the value is 'table', displays the data as a table through a platform specific table viewer; When it is 'json', displays the json text through a plain text editor; When it is 'text', displays in a brief left-justified text format.
#' @param editor a character string. (optional) The name of your favorite plain text editor. It should be executable from a command-line prompt of the respective platform. For example, notepad (Windows), vim, emacs (Unix), gedit (Ubuntu), nedit (CentOS), etc.
#' @return a data frame. The meta-info of input variables. 
#' @export getStudyVariableInfo
#' @examples
#' \dontrun{
#'
#' s <- Study(phsAcc = 'phs000001.v3.p1')
#' getStudyVariableInfo(s)
#' # or
#' getStudyVariableInfo(s, phtAcc = 'pht000370.v2.p1')
#' or
#' getStudyVariableInfo(s, phtAcc = 'pht000370.v2.p1', dataType = 'num')
#' # or
#' accList = c('phv00054119.v1.p1.c2', 'phv00054118.v1.p1', 'phv00053733.v2')
#' getStudyVariableInfo(s, phvAccList = accList) 
#' # or
#' getStudyVariableInfo(s, phvAccList = accList, showAs = 'table')) 
#'}

# getStudyVariableInfo(s, phvAccList = c('phv00054119.v1.p1.c2', 'phv00054118.v1.p1', 'phv00053733.v2'))
#
# getStudyVariableInfo(s, phvAccList = c('phv00251310.v1', 'phv00251320.v1', 'phv00251339.v1'), dataType='num') # phs000007.v29
# df <- getStudyVariableInfo(s, phtAcc='pht004815.v1', dataType = 'num')  # phs000007.v29
# df <- getStudyVariableInfo(s, phtAcc='pht004815.v1', dataType = 'cat')  # phs000007.v29


setGeneric(
           name = "getStudyVariableInfo",
           def = function(object, ...) {
               standardGeneric("getStudyVariableInfo")
           })

#' @describeIn getStudyVariableInfo of class Study 
setMethod(
          f = "getStudyVariableInfo",
          signature = c("Study"),
          definition = function(object, ..., phtAcc = "", phvAccList = vector(), dataType = "", showAs = "", editor = "") {

              phsAcc = object@phsAcc
              extDataDir = object@extDataDir 
              prjTempDir = object@prjTempDir


              ##### clean Validate PhvAccList ####
              cleanPhvAccList = vector() 
              if (length(phvAccList) > 0) {
                  cleanPhvAccList <- checkPhvAccList(object, phvAccList = phvAccList) 
              }

              # Get dataDic data
              dataDicComboDF <- getDataDicByStudy(object, phsAcc) 


              # process phvAccList
              if (length(cleanPhvAccList > 0)) {

                  phvInfoDF = dataDicComboDF[dataDicComboDF$variable_accession %in% cleanPhvAccList,]

                  finalDF = data.frame()
                  if (dataType == 'num' | dataType == 'cat') {
                      finalDF <- getStudyVariableInfoByDataType(object, dataType = dataType, dataDicDF = phvInfoDF)
                  }
                  else {
                      finalDF <- phvInfoDF 
                  }

                  if (showAs != "") {
                      finalJson <- toJSON(finalDF, pretty=T)

                      # Write json to a temp file 
                      tempJsonFile <- file.path(prjTempDir, 'temp_phtVarInfoJson.json')
                      write(finalJson, file = tempJsonFile, ncolumns = if(is.character(finalJson)) 1 else 5, append = F, sep = "\n")
                      displayFile = tempJsonFile

                      if (showAs == 'table') {
                          displayTable(object, file = displayFile)
                      }
                      else if (showAs == 'json') {
                          displayTextFile(object, file = displayFile, editor = editor) 
                      }
                  }

                  if (showAs == "") {
                      return(finalDF)
                  }
                  else {
                      return(invisible(finalDF))
                  }

              }
              # process phtAcc 
              else {

                  #if (length(phvAccList) == 0) {

                      ##### clean phtAcc ####
                      inputPhtAcc = phtAcc

                      if (inputPhtAcc != "") {
                          phtAcc <- cleanObjAcc(object, acc = phtAcc, type = 'pht') 
                      }

                      validPht = FALSE
                      if (inputPhtAcc != "") {
                          if (phtAcc != "") {
                              validPht = TRUE
                          }
                      }

                      if (validPht) {

                          phtVarInfoDF <- subset(dataDicComboDF, dataDicComboDF$dataset_accession == phtAcc &  dataDicComboDF$study_accession == phsAcc)

                          if (nrow(phtVarInfoDF) == 0) {
                              type = 'process'
                              level = 'error'
                              show = T
                              mesg = paste("There is no variable meta-info found for dataset ", phtAcc, " within the class study ", phsAcc, ". You may want to initialize the study class with the study corresponding to the dataset or vice versa.\n", sep="")
                              writeLog(object,  type = type, level = level, message = mesg, show = show) 
                          }
                          else {

                              finalDF = data.frame()
                              if (dataType == 'num' | dataType == 'cat') {
                                  finalDF <- getStudyVariableInfoByDataType(object, dataType = dataType, dataDicDF = phtVarInfoDF)
                              }
                              else {
                                  finalDF <- phtVarInfoDF 
                              }

                              if (showAs != "") {
                                  finalJson <- toJSON(finalDF, pretty=T)

                                  # Write json to a temp file 
                                  tempJsonFile <- file.path(prjTempDir, 'temp_phtVarInfoJson.json')
                                  write(finalJson, file = tempJsonFile, ncolumns = if(is.character(finalJson)) 1 else 5, append = F, sep = "\n")
                                  displayFile = tempJsonFile

                                  if (showAs == 'table') {
                                      displayTable(object, file = displayFile)
                                  }
                                  else if (showAs == 'json') {
                                      displayTextFile(object, file = displayFile, editor = editor) 
                                  }
                              }

                              if (showAs != "") {
                                  return(finalDF)
                              }
                              else {
                                  return(invisible(finalDF))
                              }
                          }
                      }
                      # No valid pht
                      else {

                          # No pht input and not phvAccList provided
                          if (inputPhtAcc == "") {

                              phvInfoDF <- dataDicComboDF

                              finalDF = data.frame()
                              if (dataType == 'num' | dataType == 'cat') {
                                  finalDF <- getStudyVariableInfoByDataType(object, dataType = dataType, dataDicDF = phvInfoDF)
                              }
                              else {
                                  finalDF <- phvInfoDF 
                              }

                              if (showAs != "") {
                                  finalJson <- toJSON(finalDF, pretty=T)

                                  # Write json to a temp file 
                                  tempJsonFile <- file.path(prjTempDir, 'temp_phtVarInfoJson.json')
                                  write(finalJson, file = tempJsonFile, ncolumns = if(is.character(finalJson)) 1 else 5, append = F, sep = "\n")
                                  displayFile = tempJsonFile

                                  if (showAs == 'table') {
                                      displayTable(object, file = displayFile)
                                  }
                                  else if (showAs == 'json') {
                                      displayTextFile(object, file = displayFile, editor = editor) 
                                  }
                              }

                              if (showAs != "") {
                                  return(finalDF)
                              }
                              else {
                                  return(invisible(finalDF))
                              }
                          }
                      }


                      if (inputPhtAcc != "") {

                          if (phtAcc != "") {

                          }
                      }
                      # inputAcc == ""
                      else {

                      }
                  #} #  length(phvAccList) > 0


              } # process PhtAcc

          })


# ------------------------------- 
# Method: getPhvAccListByTerms 
# ------------------------------- 

#' Search variables by terms
#' 
#' The method searches for variables related to given terms. It looks for text matches in the descriptions or the names of all variables or a specified set of variables within the class study. The search terms can be provided through three separate arguments of term-set that allow to either combine or narrow down the search results. The searches between the terms in each of the input term-set are in an 'OR' logic, which combines the search results of each term in the set. The resulting variables of the terms_1 search are used as the input of the terms_2 search, and the resulting variables of the terms_2 search are used as the input of the further term_3 search. The searches between the term-sets are in a 'AND' logic, which means that only the overlapping results are returned.
#'
#' @param object Study class object.
#' @param terms_1 a character vector. A list of terms used to search and find matching variables in the study. The search between the terms within the list is in a 'OR' logic. The search between this and terms_2 lists is in a 'AND' logic.
#' @param ... There are optional arguments. 
#' @param terms_2 a character vector (optional) A list of search terms for the search against the variables resulting from the search of input terms_1. The terms in the list are searched in an 'OR' logic. The search between this and the term_1, as well as the terms_3 lists is in a 'AND' logic. 
#' @param terms_3 a character vector (optional) A list of search terms against the variables resulting from the second round of search of input terms_2. The terms in the list are searched in an 'OR' logic. The search between this and terms_2 lists is in a 'AND' logic.
#' @param searchIn a character string. (optional) A string with the possible value of either 'description' (default) or 'name'. If the value is 'description', the searches is against the variable descriptions. If the value is 'name', the search is against the variable names. 
#' @param showTable a logical value. (optional) If TRUE, displays the variable meta-info in a platform specific table viewer; Not display if FALSE (default).
#' @param searchInPhvAccList a character vector. (optional) list of variable accessions. If not provided, the search will be against the variable descriptions of all variables of the study. If provided, the search will be against only the variables specified in list.
#' @return a data frame. The meta-info of the variables that have the search terms in the variable name or description. 
#' @export getPhvAccListByTerms 
#' @examples
#' \dontrun{
#'
#' s <- Study(phsAcc = 'phs000001.v3.p1')
#' t1 = c('Diabetes Treatment', 'Smoking Status')
#' t2 = c('Year 10')
#' t3 = c('6 Months') 
#' df <- getPhvAccListByTerms(s, terms_1=t1, terms_2=t2, terms_3=t3, showTable = T)
#'}


# s <- Study(phsAcc = 'phs000001.v3.p1')
# getPhvAccListByTerms(s, terms_1 = c('Diabetes Treatment', 'Study Visit'), showJson = FALSE)
# test <- getPhvAccListByTerms(s, terms_1 = c('Diabetes Treatment', 'Smoking Status'), terms_2 = c('Year 10'), showTable = T)
# test <- getPhvAccListByTerms(s, terms_1 = c('Diabetes Treatment', 'Smoking Status'), terms_2 = c('Year 10'), terms_3 = c('6 Months'), showTable = T)
# test <- getPhvAccListByTerms(s, terms_1 = c('Diabetes Treatment', 'Smoking Status'), terms_2 = c('Year 10'), terms_3 = c('6 Months'), searchIn = 'description', showTable = T)
# test <- getPhvAccListByTerms(s, terms_1 = c('SYS', 'SMK', 'DIAS'), searchIn = 'name', showTable = T)
#  test <- getPhvAccListByTerms(s, terms_1 = c('SYS', 'SMK', 'DIAS'), terms_2 = c('T1'), searchIn = 'name', showTable = T)
# test <- getPhvAccListByTerms(s, terms_1 = c('SYS', 'SMK', 'DIAS'), terms_2 = c('T1'), terms_3 = c('13'), searchIn = 'name', showTable = T)
#
# test <- getPhvAccListByTerms(s, terms_1 = c('ECG', 'X-RAY'), terms_2 = c('ATRIAL'), terms_3 = c('EXAM 6'), showTable = T)   # phs000007.v29

setGeneric(
           name = "getPhvAccListByTerms",
           def = function(object, terms_1, ...) {
               standardGeneric("getPhvAccListByTerms")
           })

#' @describeIn getPhvAccListByTerms of class Study 
setMethod(
          f = "getPhvAccListByTerms",
          signature = c("Study", "character"),
          definition = function(object, terms_1, ..., terms_2 = vector(), terms_3 = vector() , searchIn = 'description', showTable = FALSE, searchInPhvAccList = vector()) {

              phsAcc = object@phsAcc

              parseIdsFromStAcc =  parseIdsFromStAcc(object, phsAcc = phsAcc)
              phsAccNoVer = parseIdsFromStAcc$phsAccNoVer

              if (length(terms_1) > 0) {

                  # phs000001.v3.p1_data_dic_combo.rds
                  dataDicComboDF <- getDataDicByStudy(object, phsAcc) 
                  searchDF <- dataDicComboDF

                  ###########################################
                  # Search against given phvAccList
                  ###########################################
                  if (length(searchInPhvAccList) > 0) {
                      # validateInput = F is much faster
                      searchDF <- getVariableInfoByPhvAcc(object, phvAccList = retPhvAccList_1, validateInput = F, showBrief = F) 
                  }

                  retDFList = lapply(terms_1, function(term) 
                                     {

                                         # Search desc or name
                                         if (searchIn == 'name') {
                                             matchVarDF <- subset(searchDF, grepl(tolower(term), tolower(searchDF$name)))
                                         }
                                         else {
                                             matchVarDF <- subset(searchDF, grepl(tolower(term), tolower(searchDF$description)))
                                         }
                                     })

                  mergedDF <- do.call('rbind', retDFList)


                  retPhvAccList_1 = vector() 
                  retPhvAccList_2 = vector() 
                  if (nrow(mergedDF) > 0) {
                      retPhvAccList_1 <- as.vector(mergedDF$variable_accession)

                      if (length(terms_2) > 0 ) {

                          if (length(retPhvAccList_1) > 0) {
                              # validateInput = F is much faster
                              searchDF <- getVariableInfoByPhvAcc(object, phvAccList = retPhvAccList_1, validateInput = F, showBrief = F) 

                              retDFList = lapply(terms_2, function(term) {

                                                     # Search desc or name
                                                     if (searchIn == 'name') {
                                                         matchVarDF <- subset(searchDF, grepl(tolower(term), tolower(searchDF$name)))
                                                     }
                                                     else {
                                                         matchVarDF <- subset(searchDF, grepl(tolower(term), tolower(searchDF$description)))
                                                     }

})
                              mergedDF <- do.call('rbind', retDFList)
                              retPhvAccList_2 <- as.vector(mergedDF$variable_accession)
                          }
                      }
                      else {
                          retPhvAccList_2 <- retPhvAccList_1 
                      }
                  }
                  else {
                      retPhvAccList_2 <- retPhvAccList_1 
                  }

                  retPhvAccList_3 = vector() 
                  if (length(terms_3) > 0 ) {

                      if (length(retPhvAccList_2) > 0) {
                          
                          # validateInput = F is much faster
                          searchDF <- getVariableInfoByPhvAcc(object, phvAccList = retPhvAccList_2, validateInput = F, showBrief = F) 

                          retDFList = lapply(terms_3, function(term) {
                                                 # Search desc or name
                                                 if (searchIn == 'name') {
                                                     matchVarDF <- subset(searchDF, grepl(tolower(term), tolower(searchDF$name)))
                                                 }
                                                 else {
                                                     matchVarDF <- subset(searchDF, grepl(tolower(term), tolower(searchDF$description)))
                                                 }
})
                          mergedDF <- do.call('rbind', retDFList)

                          if (nrow(mergedDF) > 0) {
                              retPhvAccList_3 <- as.vector(mergedDF$variable_accession)
                          }
                          else {
                              retPhvAccList_3 = vector() 
                          }
                      }
                      else {
                          retPhvAccList_3 <- retPhvAccList_2 
                      }

                  }
                  else {
                      retPhvAccList_3 <- retPhvAccList_2 
                  }


                  ######################
                  # Console display
                  ######################
                  if (nrow(mergedDF) > 0) {
                      if (showTable) {
                          displayTable(object, data = mergedDF) 
                      }

                      return (mergedDF)
                  }
                  else {
                      cat("[INFO] There is no matching variable found.\n")
                  }
              }
          })


# -----------------------------------
# Method: getStudyVariableData
# -----------------------------------

#' Get study variable data by accession 
#' 
#' The method retrieves the variable data given a list of dbGaP variable accessions or a dataset accession.  
#'
#' @param object Study class object.
#' @param ... There are optional arguments.
#' @param phtAcc a character string. (optional) The dbGaP phenotype dataset accession.
#' @param phvAccList a character vector. (optional) The dbGaP variable accessions.
#' @param subjIdsOrFile a character vector or a character string. (optional) This argument is either list of dbGaP subject ids (dbGaP_Subject_ID) of the variables or the path to a file that contains a list of the dbGaP subject ids of the variables. The file is a plain text file with one dbGaP subject id per line.
#' @param emptyToNa a logical value. (optional). If TRUE, converts the empty values to NA; If FALSE (default), not convert.
#' @param colNameWithAcc a logical value. (optional) If TRUE, the variable column name is concatenated with the respective variable accession (e.g. AGEPHOT_phv00000027.v2); If FALSE (default), keep the original column name unchanged (e.g. AGEPHOT).
#' @return  a data frame. Merged data of the input variables.
#' @export getStudyVariableData 
#' @examples
#' \dontrun{
#'
#' s <- Study(phsAcc = 'phs000001.v3.p1')
#' var_list <- c('phv00054119.v1.p1.c2', 'phv00054118.v1.p1', 'phv00053733.v2')
#' getStudyVariableData(s, phvAccList = var_list)
#' # or
#' getStudyVariableData(s, phtAcc = 'pht000370.v2.p1', colNameWithAcc = TRUE)
#'}

# s <- Study(phsAcc = 'phs000001.v3.p1')
# getStudyVariableData(s, phvAccList = c('phv00054119.v1.p1.c2', 'phv00054118.v1.p1', 'phv00053733.v2', 'phv00053735.v2', 'phv00053732.v2'))
#
# getStudyVariableData(s, phvAccList = c('phv00054119.v1.p1.c2', 'phv00053733.v2'))

# getStudyVariableData(s, phvAccList = c('phv00000027.v2', 'phv00053747.v2', 'phv00000006.v2', 'phv00054122.v1'))
#
# getStudyVariableData(s, phvAccList = c('phv00000027.v2', 'phv00053747.v2', 'phv00000006.v2', 'phv00054122.v1'), colNameWithAcc=T)   # colNamIsAcc is True (there are duplicated column name)
#
# getStudyVariableData(s, phtAcc = 'pht000370.v2.p1', colNameWithAcc = TRUE)

# Dataset of phs000007.v29, an example of large dataset, takes too long to finish
# getStudyVariableData(s, phtAcc = 'pht000009.v2', colNameWithAcc = TRUE)       
# df <- getStudyVariableData(s, phtAcc='pht004815.v1', colNameWithAcc=T)

setGeneric(
           name = "getStudyVariableData",
           def = function(object, ...) {
               standardGeneric("getStudyVariableData")
           })

#' @describeIn getStudyVariableData of class Study 
setMethod(
          f = "getStudyVariableData",
          signature = c("Study"),
          definition = function(object, ..., phtAcc = "", phvAccList = vector(), subjIdsOrFile = NA, emptyToNa = FALSE, colNameWithAcc = FALSE) {

              inputPhtAcc = phtAcc

              # S3 function getPhtData
              getPhtData <- function(inputPhtAcc, phtAcc, colNameWithAcc) {
                  if (inputPhtAcc != "") {
                      phtAcc <- cleanObjAcc(object, acc = phtAcc, type = 'pht')
                      if (phtAcc != "") {

                          # No longer need to show this
                          message("[WARN] For a dataset that has a large number of variables, this step may take quite a while to finish.\n")

                          cat("Retrieving the data of", phtAcc, "...\n")

                          varDF <- getDatasetDataByPhtAcc(object, phtAcc = phtAcc, subjIdsOrFile = subjIdsOrFile, colNameWithAcc = colNameWithAcc)
                          return (varDF)
                      }
                  }
              }
              

              # Allow not list and vector as input. Convert to vector of it is a list.
              
              if (length(phvAccList) == 0) {
                  varDF <- getPhtData(inputPhtAcc, phtAcc, colNameWithAcc)
                  return (varDF)
              }
              else {
                  cleanPhvAccList <- checkPhvAccList(object, phvAccList = phvAccList) 

                  if (length(cleanPhvAccList) == 0) {

                      varDF <- getPhtData(inputPhtAcc, phtAcc, colNameWithAcc)
                      return (varDF)
                  }
                  else {
                      if (is.na(subjIdsOrFile)) {
                          # Get variable data no furhter filter 
                          varDF <- getVariableDataByPhvAcc(object, phvAccList = cleanPhvAccList, colNameWithAcc = colNameWithAcc, checkList = F) 
                          return (varDF)
                      }
                      else {
                          # Get variable data by subjIdsOrFile
                          varDF <- getVariableDataByPhvAccAndSubjId(object, phvAccList = cleanPhvAccList, subjIdsOrFile = subjIdsOrFile)
                          return (varDF)
                      }
                  }
              }
          })

# ----------------------------- 
# Method: variableSummary
# ----------------------------- 

#' Variable data summary
#' 
#' 
#' The method returns the statistical summary of a given variable of the class study. The statistical summary is also displayed along with the variable meta-info. 
#'
#' @param object Study class object.
#' @param phvAcc a character string. The dbGaP variable accessions
#' @param ... There are optional arguments. 
#' @param emptyToNa a logical value. (optional) If TRUE,  converts the empty value to NA. Not convert if FALSE (default).
#' @return a data frame. The statistical summary data.
#' @export variableSummary 
#' @examples
#' \dontrun{
#'
#' s <- Study(phsAcc = 'phs000001.v3.p1')
#' variableSummary(s, phvAcc = 'phv00054119.v1.p1.c2')
#'}

# s <- Study(phsAcc = 'phs000001.v3.p1')
# variableSummary(s, phvAcc = 'phv00054119.v1.p1.c2')
# variableSummary(s, phvAcc = 'phv00000087.v2')		# numerical
# variableSummary(s, phvAcc = 'phv00053747.v2')     # numerical
# variableSummary(s, phvAcc = 'phv00000035.v2')		# categorical
# variableSummary(s, phvAcc = 'phv00053757.v2')     # categorical

setGeneric(
           name = "variableSummary",
           def = function(object, phvAcc, ...) {
               standardGeneric("variableSummary")
           })

#' @describeIn variableSummary of class Study 
setMethod(
          f = "variableSummary",
          signature = c("Study", "character"),
          definition = function(object, phvAcc, ..., emptyToNa = F) {

              phsAcc = object@phsAcc
              phvAcc <- cleanObjAcc(object, acc = phvAcc, type = 'phv')

              if (phvAcc != '') {

                  varDataDF <- getVariableDataByPhvAcc(object, phvAccList = c(phvAcc), emptyToNa = emptyToNa) 

                  if (!is.null(varDataDF)) {

                      ################
                      # Get datatype
                      ################
                      # possible types: decimal, enum_integer, integer, string, unknown
                      # type "unknown" should be ignored
                      studyDataDicDF <- getDataDicByStudy(object, phsAcc) 
                      varInfoDF <- subset(studyDataDicDF,  studyDataDicDF$variable_accession==phvAcc) 
                      varType <- as.character(varInfoDF$calculated_type)
                      varName <- as.character(varInfoDF$name)
                      varDesc <- as.character(varInfoDF$description)
                      varUnits <- as.character(varInfoDF$units)

                      varDataNoIdDF = subset(varDataDF, select = c(varName))
                      colName = as.character(colnames(varDataNoIdDF)[1])
                      varDataNoIdList <- as.list(varDataNoIdDF)[[1]]

                      df <- varDataNoIdDF

                      varna <- apply(df, 2, function(x) {
                                         unname(sum(length(which(is.na(x))))[1])
})
                      varna <- unname(varna)

                      varempty <- apply(df, 2, function(x) {
                                            sum(length(which(nchar(x)==0)))
})
                      varempty <- unname(varempty)
                      varn = apply(df, 2, length)

                      # na an empty are defined early	
                      # To show a brief version of varInfo
                      getVariableInfoByPhvAcc(object, phvAccList = c(phvAcc), showBrief=T)

                      # subset varInfoDF
                      varInfoDFSub <- subset(varInfoDF, select = c('variable_accession', 'name', 'calculated_type', 'units')) 

                      # Reset row names  (reset row index for subsetted DF)
                      rownames(varInfoDFSub) <- seq(length=nrow(varInfoDFSub))

                      # Numerical type
                      if (varType == 'integer' | varType == 'decimal') {

                          ##########################
                          # Compute summary
                          ##########################

                          varmean = apply(df, 2, mean, na.rm=TRUE) 
                          varsd = apply(df, 2, sd, na.rm=TRUE) 
                          varmedian = apply(df, 2, median, na.rm=TRUE)
                          varmin = apply(df, 2, min, na.rm=TRUE)
                          varmax = apply(df, 2, max, na.rm=TRUE)

                          # Append to varIndoDFSub
                          varInfoDFSub$max <- varmax 
                          varInfoDFSub$min <- varmin 
                          varInfoDFSub$median <- varmedian 
                          varInfoDFSub$mean <- varmean 
                          varInfoDFSub$na <- varna
                          varInfoDFSub$empty <- varempty 
                          varInfoDFSub$n <- varn 


                          #print(head(varInfoDFSub))
                          #              id      name    type units max min median     mean na empty    n
                          #			  2 phv00054119.v1 MATCHSPEC integer  <NA>   4   0      2 1.578682  0     0 3762

                          cat("Summary table:\n")
                          print(varInfoDFSub)
                          cat("\n")
                          return(invisible(varInfoDFSub))

                      }
                      # Categorical variable , na.rm=TRUE
                      else {

                          varInfoDFSub$na <- varna
                          varInfoDFSub$empty <- varempty 
                          varInfoDFSub$n <- varn 

                          #print(head(varInfoDFSub))
                          #               id   name               type units na empty    n
                          #			   52 phv00000035.v2 SCHOOL enumerated integer  <NA>  0     0 3762

                          ######################
                          # Display
                          ######################

                          cat("Summary table:\n")
                          print(varInfoDFSub)
                          cat("\n")

                          cat("Frequency table:\n")
                          freq <- plyr::count(df, varName)
                          print(freq)
                          cat("\n")

                          return(invisible(varInfoDFSub))
                      }
                  }
                  else {
                      type = 'process'
                      level = 'error'
                      show = T
                      mesg = paste("There is no data available for the input variable ", phvAcc, ".\n", sep="")
                      writeLog(object,  type = type, level = level, message = mesg, show = show) 
                  }

              }


          })


# ------------------------- 
# Method: variableBoxplot 
# ------------------------- 

#' Variable boxplot 
#'
#' The method draws the box-and-whisker plot given the accession of either a numerical variable alone or a numerical plus a categorical variable. The boxplot is drawn with the numerical variable as the function of each category of the categorical variable, respectively. When categorical variable accession is not provided, the boxplot is drawn with the numerical variable as the function of the respective subject indices. The resulting graph is saved as PDF and PNG files. 
#'
#' @param object Study class object.
#' @param numPhvAcc a character string. The dbGaP accession of a numerical variable. 
#' @param ... There are optional arguments. 
#' @param catPhvAcc a character string. (optional) The dbGaP accession of a categorical variable.
#' @param saveToDir a character string. (optional) The path to the directory where the plot PDF file is saved. If not provided, the file is saved in the 'temp' directory under the user project directory.
#' @param showPlot a logical value. (optional) If TRUE (default), shows the created graph; Not show if FALSE. The graph is always saved in PDF and PNG files regardless shown or not.
#' @return a data frame. The data used for plotting. 
#' @export variableBoxplot
#' @examples
#' \dontrun{
#'
#' s <- Study(phsAcc = 'phs000001.v3.p1')
#' variableBoxplot(s, numPhvAcc = 'phv00000027.v2', catPhvAcc = 'phv00000032.v2')
#'}

# variableBoxplot(s, numPhvAcc = 'phv00000027.v2', catPhvAcc = 'phv00000032.v2')		# AGEPHOT: integer, MARITAL: enumerated integer
#
# variableBoxplot(s, numPhvAcc = 'phv00053747.v2', catPhvAcc = 'phv00053764.v2')		# LNUCSCORE integer, LPSN: enumerated integer
#
# variableBoxplot(s, numPhvAcc = 'phv00000006.v2', catPhvAcc = 'phv00000035.v2')		# LNUCSCORE: decimal, SCHOOL: enumerated integer 
#
# variableBoxplot(s, numPhvAcc = 'phv00053747.v2', catPhvAcc = 'phv00053764.v2', saveToDir = '/panfs/sandpan1.be-md.ncbi.nlm.nih.gov/homes/hao/temp', showPlot=F)
#
# variableBoxplot(s, numPhvAcc = 'phv00251348.v1', catPhvAcc = 'phv00251769.v1')    # phs000007.v29

setGeneric(
           name = "variableBoxplot",
           def = function(object, numPhvAcc, ...) {
               standardGeneric("variableBoxplot")
           })

#' @describeIn variableBoxplot of class Study 
setMethod(
          f = "variableBoxplot",
          signature = c("Study", "character"),
          definition = function(object, numPhvAcc, ..., catPhvAcc = '', saveToDir = '', showPlot = T) {
              saveToDir <- checkInputPath(object, saveToDir)

              phsAcc = object@phsAcc
              prjTempDir = object@prjTempDir 

              numPhvAcc <- cleanObjAcc(object, acc = numPhvAcc, type = 'phv')

              if (catPhvAcc != '') {
                  catPhvAcc <- cleanObjAcc(object, acc = catPhvAcc, type = 'phv')
              }

              if (numPhvAcc != '') {

                  varDataDF = data.frame()

                  if (catPhvAcc != '') {
                      varDataDF <- getVariableDataByPhvAcc(object, phvAccList = c(numPhvAcc, catPhvAcc), emptyToNa = T) 
                  }
                  else {
                      varDataDF <- getVariableDataByPhvAcc(object, phvAccList = c(numPhvAcc), emptyToNa = T) 
                  }


                  if (!is.null(varDataDF)) {

                      if (nrow(varDataDF) > 0) {

                          ################
                          # Get datatype
                          ################
                          # possible types: decimal, enum_integer, integer, string, unknown
                          # type "unknown" should be ignored
                          studyDataDicDF <- getDataDicByStudy(object, phsAcc) 

                          numVarInfoDF <- subset(studyDataDicDF,  studyDataDicDF$variable_accession==numPhvAcc) 
                          numVarType <- as.character(numVarInfoDF$calculated_type)
                          numVarName <- as.character(numVarInfoDF$name)
                          numVarUnits <- as.character(numVarInfoDF$units)

                          # Get numVarColumn
                          numVarDataNoIdDF = subset(varDataDF, select = c(numVarName))			# separate out catVar column only


                          # Numerical type
                          # decimal
                          inputOk = '' 
                          if (numVarType == 'integer' | numVarType == 'decimal') {
                              inputOk = T 
                          }
                          else {
                              inputOk = F

                              type = 'process'
                              level = 'info'
                              show = T
                              mesg = paste("The data type ", type, ", of input variable, ", numVarName, " ( ", numPhvAcc, " ), is not numerical . The boxplot is not drawn.\n", sep="")
                              writeLog(object,  type = type, level = level, message = mesg, show = show) 
                          }

                          #########################
                          # Numericl  type 
                          #########################

                          if (nchar(catPhvAcc) > 0) {

                              catVarInfoDF <- subset(studyDataDicDF,  studyDataDicDF$variable_accession==catPhvAcc) 
                              catVarType <- as.character(catVarInfoDF$calculated_type)
                              catVarName <- as.character(catVarInfoDF$name)
                              catVarUnits <- as.character(catVarInfoDF$units)
                              catVarCodeValCombo <- as.character(catVarInfoDF$code_value_combo)

                              #################################
                              # Separate out the data columns 
                              #################################
                              catVarDataNoIdDF = subset(varDataDF, select = c(catVarName))			# separate out catVar column only

                              #########################
                              # Categorical type 
                              #########################
                              if (catVarType == 'enum_integer' | catVarType == 'string' | catVarType == 'enumerated integer') {

                                  ################################
                                  # With categorical variable 
                                  ################################
                                  #catVarDataNoIdDF <- CatVarDataForPlot(object, varDataDFInList = list(varDataDF), varInfoDFInList = list(catVarInfoDF))
                                  #catVarDataNoIdDF <- convertEnumVarColName(object, varDataDF = varDataDF, varInfoDF = catVarInfoDF)

                                  varCodeValDF <- getExtData(object, type = 'code', phsAcc = phsAcc)
                                  catVarDataNoIdDF <- convertEnumVarColName(object, varDataDF = varDataDF, varInfoDF = catVarInfoDF, varCodeValDF = varCodeValDF)

                                  finalDF <- cbind(numVarDataNoIdDF, catVarDataNoIdDF)


                                  if (inputOk == T) {

                                      #################
                                      # Labels
                                      #################
                                      xLabCombo = paste("\n", catVarName, "\n")
                                      yLabCombo = ''
                                      if (nchar(numVarUnits) > 0 & !is.na(numVarUnits)) {
                                          yLabCombo = paste("\n", numVarName, " (", numVarUnits, ")\n") 
                                      }
                                      else {
                                          yLabCombo = paste("\n", numVarName, "\n") 
                                      }
                                      title = paste("\n", numVarName, "(", numPhvAcc, ")\n vs\n", catVarName, "(", catPhvAcc, ")\n")

                                      ##############
                                      # Draw plot
                                      ##############

                                      bp <- ggplot(finalDF, aes_string(x=catVarName, y=numVarName, fill=catVarName, group=catVarName)) +
                                          geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2) +
                                          stat_summary(fun.y="mean", geom="point", shape=21, size=5, fill="white") +
                                          labs(y = yLabCombo, x = xLabCombo) +
                                          ggtitle(title) +
                                          #theme(plot.title = element_text(lineheight=.8, face="plain", size=11))
                                          theme(plot.title = element_text(hjust = 0.5, size = 10)) +    # center the title
                                          theme(aspect.ratio=4/4)   # graph height is larger than width if not set this 

                                          ###########################
                                          # Disply variable info
                                          ###########################
                                          getVariableInfoByPhvAcc(object, phvAccList = c(catPhvAcc, numPhvAcc), showBrief=T)

                                          ############################
                                          # Display and Save plot
                                          ############################
                                          plotObj = bp
                                          plotType = 'boxplot'
                                          phvAccNameCombo = paste(numPhvAcc, "_", numVarName, "_", catPhvAcc, "_", catVarName, sep="")
                                          savedPlotFiles <- saveGapPlot(object, plotObj = plotObj, plotType = plotType, phvAccNameCombo = phvAccNameCombo, saveToDir = saveToDir, showPlot = showPlot)

                                          return(invisible(finalDF))
                                  }

                              }
                              else {
                                  type = 'process'
                                  level = 'info'
                                  show = T
                                  mesg = paste("The data type ", type, ", of input variable, ", catVarName, " ( ", catPhvAcc, " ), is not categorical . The boxplot thus is not drawn.\n", sep="")
                                  writeLog(object,  type = type, level = level, message = mesg, show = show) 

                                  return ()
                              }

                          } # end of nchar(catPhvAcc) > 0
                          else {

                              #########################################################################
                              # Boxplot of numerical variable only (without categorical variable)
                              #########################################################################

                              # Get numVarColumn

                              if (inputOk == TRUE) {
                                  finalDF <- numVarDataNoIdDF
                                  # lnuCSCORE
                                  #  1      2.08
                                  #  2      3.39
                                  #  3      2.95
                                  #  4      4.09

                                  ###########################
                                  # Create a dummy column
                                  ###########################
                                  dummyCol <- rep('N', nrow(finalDF))
                                  finalDF$dummy <- dummyCol 
                                  # print(head(finalDF,4))
                                  #	  LNUCSCORE dummy
                                  #	  L  1      2.08     N
                                  #	  L  2      3.39     N
                                  #	  L  3      2.95     N
                                  #	  L  4      4.09     N


                                  #################
                                  # labels
                                  #################
                                  title = paste("\n", numVarName, " (", numPhvAcc, ")\n")

                                  xlabcombo = "" 
                                  ylabcombo = ''
                                  if (nchar(numVarUnits) > 0 & !is.na(numVarUnits)) {
                                      yLabCombo = paste("\n", numVarName, " (", numVarUnits, ")\n") 
                                  }
                                  else {
                                      yLabCombo = paste("\n", numVarName, "\n") 
                                  }

                                  ###########################
                                  # disply variable info
                                  ###########################
                                  getVariableInfoByPhvAcc(object, phvAccList = c(numPhvAcc), showBrief=T)


                                  ###########
                                  # plot
                                  ###########

                                  catVarName = 'dummy'
                                  xLabCombo = ''

                                  bp <- ggplot(finalDF, aes_string(x=catVarName, y=numVarName, fill=catVarName, group=catVarName)) +
                                      geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2) +
                                      stat_summary(fun.y="mean", geom="point", shape=21, size=5, fill="white") +
                                      labs(y = yLabCombo, x = xLabCombo) +
                                      ggtitle(title) +
                                      #theme(plot.title = element_text(lineheight=.8, face="plain", size=11))
                                      theme(plot.title = element_text(hjust = 0.5, size = 10)) +    # center the title
                                      theme(aspect.ratio=4/4)   # graph height is larger than width if not set this 

                                      ############################
                                      # Display and Save plot
                                      ############################
                                      plotObj = bp
                                      plotType = 'boxplot'
                                      phvAccNameCombo = paste(numPhvAcc, "_", numVarName, sep="")
                                      savedPlotFiles <- saveGapPlot(object, plotObj = plotObj, plotType = plotType, phvAccNameCombo = phvAccNameCombo, saveToDir = saveToDir, showPlot = showPlot)

                                      return(invisible(finalDF))

                              }
                          }



                      } # end (nrow(varDataDF) > 0)
                      
                  } # end varDataDF is not null

              }


          })



# ------------------------------- 
# Method: variableScatterplot
# ------------------------------- 

#' Variable scatter-plot
#' 
#' The method draws the scatterplot of two numerical variables for each category of a categorical variable. If only one numerical variable accession (numPhvAcc_1) is provided, it draws the numerical variable values as the function of the respective subject indices. If both numerical variables (numPhvAcc_1 and numPhvAcc_2) are provided, it draws the values of the first variable as the function of the second. All variables should belong to the same study and have overlapping subjects. The created graph is saved as PDF and PNG files.
#'
#' @param object Study class object.
#' @param numPhvAcc_1 a character string. A dbGaP variable accession of the numerical variable type. 
#' @param ... There are optional argument.
#' @param numPhvAcc_2 a character string. (optional) A dbGaP variable accession of the numerical variable type. 
#' @param catPhvAcc a character string. (optional) A dbGaP variable accession of the categorical variable type. This value is used only if the arguments of numPhvAcc_1 and numPhvAcc_2 are both provided.
#' @param saveToDir a character string. (optional) The path to the directory where the plot PDF file is saved. If not provided, the file is saved in the 'temp' directory under the user project directory.
#' @param showPlot a logical value. (optional) If TRUE (default), shows the created graph; Not show if FALSE. 
#' @return a data frame. The data used for plotting. 
#' @export variableScatterplot 
#' @examples
#' \dontrun{
#'
#' s <- Study(phsAcc = 'phs000001.v3.p1')
#' numv1 = 'phv00000027.v2'
#' numv2 = 'phv00053747.v2'
#' catv = 'phv00053757.v2'
#' variableScatterplot(s, numPhvAcc_1=numv1, numPhvAcc_2=numv2, catPhvAcc=catv)
#' }


# s <- Study(phsAcc = 'phs000001.v3.p1')
# variableScatterplot(s, numPhvAcc_1 = 'phv00000027.v2', numPhvAcc_2 = 'phv00053747.v2', catPhvAcc = 'phv00053757.v2')		# phv00053757.v2, RNUC: variable type string
# variableScatterplot(s, numPhvAcc_1 = 'phv00000027.v2')

# variableScatterplot(s, numPhvAcc_1 = 'phv00000027.v2', numPhvAcc_2 = 'phv00053747.v2', catPhvAcc = 'phv00053856.v2')		# phv00053757.v2, SCHOOL: variable type enumerated integer

setGeneric(
           name = "variableScatterplot",
           def = function(object, numPhvAcc_1, ...) {
               standardGeneric("variableScatterplot")
           })

#' @describeIn variableScatterplot of class Study 
setMethod(
          f = "variableScatterplot",
          signature = c("Study", "character"),
          definition = function(object, numPhvAcc_1, ..., numPhvAcc_2 = '', catPhvAcc = '', saveToDir = '', showPlot = TRUE) {
              saveToDir <- checkInputPath(object, saveToDir)

              phsAcc = object@phsAcc
              prjTempDir = object@prjTempDir 

              numPhvAcc_1 <- cleanObjAcc(object, acc = numPhvAcc_1, type = 'phv')

              if (numPhvAcc_2 != '') {
                  numPhvAcc_2 <- cleanObjAcc(object, acc = numPhvAcc_2, type = 'phv')
              }

              if (catPhvAcc != '') {
                  catPhvAcc <- cleanObjAcc(object, acc = catPhvAcc, type = 'phv')
              }

              if (numPhvAcc_1 != '') {

                  varDataDF = data.frame()
                  if ((numPhvAcc_2) != '') {

                      if ((catPhvAcc) != '') {
                          catPhvAcc <- cleanObjAcc(object, acc = catPhvAcc, type = 'phv')
                          varDataDF <- getVariableDataByPhvAcc(object, phvAccList = c(numPhvAcc_1, numPhvAcc_2, catPhvAcc), emptyToNa = T) 
                          #  dbGaP_Subject_ID Submitted_Subject_ID AGEPHOT LNUCSCORE  RNUC
                          #  1                1                 1379    74.2      2.08 NUC-C
                          #  2                2                 4861    69.6      3.39 NUC-C
                          #  3                3                 3642    73.4      2.95 NUC-D
                          #  4                4                 5400    79.1      4.09 NUC-A
                      }
                      else {
                          varDataDF <- getVariableDataByPhvAcc(object, phvAccList = c(numPhvAcc_1, numPhvAcc_2), emptyToNa = T) 
                      }
                  }
                  else {
                      varDataDF <- getVariableDataByPhvAcc(object, phvAccList = c(numPhvAcc_1), emptyToNa = T) 
                  }

                  if (nrow(varDataDF) > 0) {
                      ################
                      # Get datatype
                      ################
                      # possible types: decimal, enum_integer, integer, string, unknown
                      # type "unknown" should be ignored
                      studyDataDicDF <- getDataDicByStudy(object, phsAcc) 

                      numVarInfoDF_1 <- subset(studyDataDicDF,  studyDataDicDF$variable_accession==numPhvAcc_1) 
                      numVarType_1 <- as.character(numVarInfoDF_1$calculated_type)
                      numVarName_1 <- as.character(numVarInfoDF_1$name)
                      numVarUnits_1 <- as.character(numVarInfoDF_1$units)

                      # Get numVarColumn
                      numVarDataNoIdDF_1 = subset(varDataDF, select = c(numVarName_1))			# separate out catVar column only
                      #  AGEPHOT
                      #  1    74.2
                      #  2    69.6
                      #  3    73.4
                      #  4    79.1
                      #print(head(numVarDataNoIdDF_1, 4))


                      if (nchar(numPhvAcc_2) == 0) {
                          ################################
                          # Single variable scatter plot
                          ################################


                          #numVarDataNoIdList_1 <- as.list(numVarDataNoIdDF_1)[[1]]

                          dat = numVarDataNoIdDF_1 
                          dat$index <-seq.int(nrow(dat))	# add an index column for Subject index (instead of original Subject ID)
                          #  AGEPHOT index
                          #  1    74.2	1
                          #  2    69.6	2
                          #  3    73.4	3
                          #  4    79.1	4


                          #################
                          # Labels
                          #################
                          xlab = 'index'		
                          ylab = numVarName_1

                          yLabCombo = ''
                          if (nchar(numVarUnits_1) > 0 & !is.na(numVarUnits_1)) {
                              yLabCombo = paste("\n", numVarName_1, " (", numVarUnits_1, ")\n") 
                          }
                          else {
                              yLabCombo = paste("\n", numVarName_1, "\n") 
                          }

                          xLabCombo = paste("\n", " Subject Index", "\n") 
                          title = paste("\n", numVarName_1, " (", numPhvAcc_1, ")\n")


                          ###########################
                          # Disply variable info
                          ###########################
                          getVariableInfoByPhvAcc(object, phvAccList = c(numPhvAcc_1), showBrief=T)


                          ###############
                          # Plot 
                          ###############

                          # light blue color
                          sp <-ggplot(dat, aes_string(x=xlab, y=ylab)) + geom_point(color=rgb(0,0,1,0.2))  +  
                              # create label margin and multi-line label 
                              labs(y = yLabCombo, x = xLabCombo) +
                              ggtitle(title) + 
                              theme(plot.title = element_text(lineheight=.8, face="plain")) +
                      theme(aspect.ratio=4/4) +  # graph height is larger than width if not set this 
                  theme(plot.margin = unit(c(0,0,0,0), "cm"))

                          ############################
                          # Display and Save plot
                          ############################
                          plotObj = sp
                          plotType = 'scatterplot'
                          phvAccNameCombo = paste(numPhvAcc_1, "_", numVarName_1, sep="")
                          savedPlotFiles <- saveGapPlot(object, plotObj = plotObj, plotType = plotType, phvAccNameCombo = phvAccNameCombo, saveToDir = saveToDir, showPlot = showPlot)

                          return(invisible(dat))

                      } # end of nchar(numPhvAcc_2) == 0 
                      else {

                          ################################
                          # Double variable scatter plot
                          ################################
                          numVarInfoDF_2 <- subset(studyDataDicDF,  studyDataDicDF$variable_accession==numPhvAcc_2) 
                          numVarType_2 <- as.character(numVarInfoDF_2$calculated_type)
                          numVarName_2 <- as.character(numVarInfoDF_2$name)
                          numVarUnits_2 <- as.character(numVarInfoDF_2$units)

                          numVarDataNoIdDF_2 = subset(varDataDF, select = c(numVarName_2))			# separate out the column only

                          if (nchar(catPhvAcc) == 0) {
                              #############################################
                              # None categorical (numerical ) variable 
                              #############################################

                              finalDF <- cbind(numVarDataNoIdDF_1, numVarDataNoIdDF_2)

                              #################
                              # Labels
                              #################
                              xlab = numVarName_1 
                              ylab = numVarName_2

                              xLabCombo = ''
                              if (nchar(numVarUnits_1) > 0 & !is.na(numVarUnits_1)) {
                                  xLabCombo = paste("\n", numVarName_1, " (", numVarUnits_1, ")\n") 
                              }
                              else {
                                  xLabCombo = paste("\n", numVarName_1, "\n") 
                              }

                              yLabCombo = ''
                              if (nchar(numVarUnits_2) > 0 & !is.na(numVarUnits_2)) {
                                  yLabCombo = paste("\n", numVarName_2, " (", numVarUnits_2, ")\n") 
                              }
                              else {
                                  yLabCombo = paste("\n", numVarName_2, "\n") 
                              }
                              title = paste("\n", numVarName_1, " (", numPhvAcc_1, ") vs\n", numVarName_2, " (", numPhvAcc_2, ")\n")

                              ###########################
                              # Disply variable info
                              ###########################
                              getVariableInfoByPhvAcc(object, phvAccList = c(numPhvAcc_1, numPhvAcc_2), showBrief=T)

                              ################
                              # Plot 
                              #################
                              dat <- finalDF
                              sp <- ggplot(dat) +
                                  geom_point(aes_string(x = xlab, y = ylab), color=rgb(0,0,1,0.2)) +
                                  labs(y = yLabCombo, x = xLabCombo) +
                                  ggtitle(title) + 
                                  #theme(plot.title = element_text(lineheight=.8, face="plain", size=11))
                                  theme(plot.title = element_text(hjust = 0.5, size = 10)) +    # center the title
                              theme(aspect.ratio=4/4) +   # graph height is larger than width if not set this 
                              theme(plot.margin = unit(c(0,0,0,0), "cm"))


                              ############################
                              # Display and Save plot
                              ############################
                              plotObj = sp
                              plotType = 'scatterplot'
                              phvAccNameCombo = paste(numPhvAcc_1, "_", numVarName_1, "_", numPhvAcc_2, "_", numVarName_2, sep="")
                              savedPlotFiles <- saveGapPlot(object, plotObj = plotObj, plotType = plotType, phvAccNameCombo = phvAccNameCombo, saveToDir = saveToDir, showPlot = showPlot)

                              return(invisible(dat))

                          }
                          else {
                              ################################
                              # With categorical variable 
                              ################################
                              catVarInfoDF <- subset(studyDataDicDF,  studyDataDicDF$variable_accession==catPhvAcc) 

                              catVarType <- as.character(catVarInfoDF$calculated_type)
                              catVarName <- as.character(catVarInfoDF$name)
                              catVarUnits <- as.character(catVarInfoDF$units)
                              catVarCodeValCombo <- as.character(catVarInfoDF$code_value_combo)

                              #### Process categorical variable data  ####
                              #catVarDataNoIdDF <- prepareCatVarDataForPlot(object, varDataDFInList = list(varDataDF), varInfoDFInList = list(catVarInfoDF))
                              #catVarDataNoIdDF <- convertEnumVarColName(object, varDataDF = varDataDF, varInfoDF = catVarInfoDF) 

                              varCodeValDF <- getExtData(object, type = 'code', phsAcc = phsAcc)
                              catVarDataNoIdDF <- convertEnumVarColName(object, varDataDF = varDataDF, varInfoDF = catVarInfoDF, varCodeValDF = varCodeValDF)

                              finalDF <- cbind(numVarDataNoIdDF_1, numVarDataNoIdDF_2, catVarDataNoIdDF)
                              # 	AGEPHOT LNUCSCORE  SCHOOL
                              #	1    74.2      2.08 SCHOOL2
                              #	2    69.6      3.39 SCHOOL4
                              #	3    73.4      2.95 SCHOOL5
                              #	4    79.1      4.09 SCHOOL4
                              #	5    72.9      2.42 SCHOOL5
                              # 	6    75.5      2.64 SCHOOL1

                              #################
                              # Labels
                              #################
                              xlab = numVarName_1 
                              ylab = numVarName_2
                              clab = catVarName 

                              xLabCombo = ''
                              if (nchar(numVarUnits_1) > 0 & !is.na(numVarUnits_1)) {
                                  xLabCombo = paste("\n", numVarName_1, " (", numVarUnits_1, ")\n") 
                              }
                              else {
                                  xLabCombo = paste("\n", numVarName_1, "\n") 
                              }

                              yLabCombo = ''
                              if (nchar(numVarUnits_2) > 0 & !is.na(numVarUnits_2)) {
                                  yLabCombo = paste("\n", numVarName_2, " (", numVarUnits_2, ")\n") 
                              }
                              else {
                                  yLabCombo = paste("\n", numVarName_2, "\n") 
                              }
                              title = paste("\n", numVarName_1, " (", numPhvAcc_1, ") vs\n", numVarName_2, " (", numPhvAcc_2, ") vs\n", catVarName, " (", catPhvAcc, ")\n")

                              ###########################
                              # Disply variable info
                              ###########################
                              getVariableInfoByPhvAcc(object, phvAccList = c(numPhvAcc_1, numPhvAcc_2, catPhvAcc), showBrief=T)

                              #################
                              # Plot 
                              #################
                              dat <- finalDF
                              sp <- ggplot(dat) +
                                  geom_point(aes_string(x = xlab, y = ylab, color = catVarName)) +
                                  labs(y = yLabCombo, x = xLabCombo) +
                                  ggtitle(title) + 
                                  #theme(plot.title = element_text(lineheight=.8, face="plain", size=11))
                                  theme(plot.title = element_text(hjust = 0.5, size = 10))  +   # center the title
                              theme(aspect.ratio=4/4) +  # graph height is larger than width if not set this 
                              theme(plot.margin = unit(c(0,0,0,0), "cm"))

                              ############################
                              # Display and Save plot
                              ############################
                              plotObj = sp
                              plotType = 'scatterplot'
                              phvAccNameCombo = paste(numPhvAcc_1, "_", numVarName_1, "_", numPhvAcc_2, "_", numVarName_2, "_", catPhvAcc, "_", catVarName, sep="")
                              savedPlotFiles <- saveGapPlot(object, plotObj = plotObj, plotType = plotType, phvAccNameCombo = phvAccNameCombo, saveToDir = saveToDir, showPlot = showPlot)

                              return(invisible(dat))
                          }

                      }

                  } # end nrow(varDataDF) > 0

              } # end of numPhvAcc_1 != ''

          }) 



# --------------------------- 
# Method: variableHistogram 
# --------------------------- 

#' Variable histogram 
#'
#'
#' The method draws the histograms of a given dbGaP variable. 
#'
#' @param object Study class object.
#' @param phvAcc a character string. The dbGaP variable accession.
#' @param ... There are optional arguments. 
#' @param withDensity a logical value. (optional) If TRUE (default), draws the histograms with a kernel density plot; If FALSE, draws without a kernel density plot.
#' @param saveToDir a character string. (optional) The path to the directory where the plot PDF file is saved. If not provided, the file is saved in the 'temp' directory under the user project directory.
#' @param showPlot a logical value. (optional) If TRUE (default), shows the created graph; Not show if FALSE. The created graph is saved as files regardless.
#' @return a data frame. The data used for plotting. 
#' @export variableHistogram 
# @examples
# s <- Study(phsAcc = 'phs000001.v3.p1')
# variableHistogram(s, phvAcc = 'phv00053747.v2')		# numerical 
# variableHistogram(s, phvAcc = 'phv00000087.v2')		# numerical
# variableHistogram(s, phvAcc = 'phv00054119.v1.p1.c2') # numerical 
# variableHistogram(s, phvAcc = 'phv00053747.v2', withDensity=F)    # withDensity 
# variableHistogram(s, phvAcc = 'phv00053747.v2', withDensity=F, showPlot=F)   # not show image 
# variableHistogram(s, phvAcc = 'phv00000035.v2', withDensity=F, showPlot=T)   # categorical variable SCHOOL 
#
# variableHistogram(s, phvAcc = 'phv00251409.v1', withDensity=T, showPlot=T)   # numerical: phs000007.v29
# variableHistogram(s, phvAcc = 'phv00251798.v1', withDensity=T, showPlot=T)   # categorical phs00007.v29

setGeneric(
           name = "variableHistogram",
           def = function(object, phvAcc, ...) {
               standardGeneric("variableHistogram")
           })

#' @describeIn variableHistogram of class Study 
setMethod(
          f = "variableHistogram",
          signature = c("Study", "character"),
          definition = function(object, phvAcc, ..., withDensity = TRUE, saveToDir = '', showPlot = TRUE) {

              phsAcc = object@phsAcc
              prjTempDir = object@prjTempDir
              phvAcc <- cleanObjAcc(object, acc = phvAcc, type = 'phv')

              if (phvAcc != '') {

                  varDataDF <- getVariableDataByPhvAcc(object, phvAccList = c(phvAcc), emptyToNa = T) 

                  if (!is.null(varDataDF)) {

                      ################
                      # Get datatype
                      ################
                      # possible types: decimal, enum_integer, integer, string, unknown
                      # type "unknown" should be ignored
                      studyDataDicDF <- getDataDicByStudy(object, phsAcc) 

                      varInfoDF <- subset(studyDataDicDF,  studyDataDicDF$variable_accession==phvAcc) 
                      varType <- as.character(varInfoDF$calculated_type)
                      varName <- as.character(varInfoDF$name)
                      varDesc <- as.character(varInfoDF$description)
                      varUnits <- as.character(varInfoDF$units)

                      varDataNoIdDF = subset(varDataDF, select = c(varName))
                      colName = as.character(colnames(varDataNoIdDF)[1])
                      varDataNoIdList <- as.list(varDataNoIdDF)[[1]]
                      dat = varDataNoIdDF 

                      #  MATCHSPEC index
                      #  1         2     1
                      #  2         2     2
                      #  3         2     3
                      #  4         2     

                      ###############
                      # Histogram
                      ###############
                      #m<-mean(airquality$Temp);std<-sqrt(var(airquality$Temp))
                      #mean <-mean(varDataNoIdList, na.rm=T)  # Ignore NA values for mean
                      #std<-sqrt(var(varDataNoIdList))

                      #hist(varDataNoIdList, prob=T, main=colName)
                      #curve(dnorm(x, mean=mean, sd=std), col="darkblue", lwd=2, add=TRUE)


                      dat$index <-seq.int(nrow(dat))	# add an index column for Subject index (instead of original Subject ID)

                      #####################
                      # Get y-label Unit
                      #####################
                      if (nchar(varUnits) > 0 && !is.na(varUnits)) {
                          ylab = paste(colName, " ( ", varUnits, " ) ", "\n", "( ", phvAcc, " )", sep="")
                      }
                      else {
                          ylab = paste(colName, "\n", "( ", phvAcc, " )", sep="")
                      }

                      # Labels
                      ylab = 'index'		
                      xlab = colName

                      xLabCombo = ''
                      if (nchar(varUnits) > 0 & !is.na(varUnits)) {
                          xLabCombo = paste("\n", varName, " (", varUnits, ")\n") 
                      }
                      else {
                          xLabCombo = paste("\n", varName, "\n") 
                      }
                      title = paste("\n", colName, "(", phvAcc, ")\n") 

                      ######################
                      # Numerical type
                      ######################
                      if (varType == 'integer' | varType == 'decimal') {

                          ###########################
                          # Disply variable info
                          ###########################
                          getVariableInfoByPhvAcc(object, phvAccList = c(phvAcc), showBrief=T)

                          datMean = mean(dat[,1], na.rm=TRUE)

                          # y-axis: Density 
                          if (withDensity == T) {

                              yLabCombo = paste("\n", "Density", "\n") 

                              # Histogram overlaid with kernel density curve, and mean line
                              ..density.. <- ''  # mute CMD check note
                              histp <- ggplot(dat, aes_string(x=xlab)) +  
                                  geom_histogram(aes(y=..density..), colour="black", fill="white") +
                                  geom_density(alpha=.2, fill="#FF6666") + 
                                  geom_vline(aes(xintercept=datMean),  color="red", linetype="dashed", size=1) + 
                                  labs(x = xLabCombo, y = yLabCombo) +
                                  ggtitle(title) +
                                  #theme(plot.title = element_text(lineheight=.8, face="plain"))
                                  theme(plot.title = element_text(hjust = 0.5, size = 10)) +     # center the title
                                  theme(aspect.ratio=2/3)   # graph height is larger than width if not set this 
                          }
                          # y-axis: SubjCount 
                          else {

                              yLabCombo = paste("\n", "Subject Count", "\n") 

                              # Histogram overlaid with and mean line
                              ..count.. <- ''  # mute CMD check note

                              histp <- ggplot(dat, aes_string(x=xlab)) +  
                                  geom_histogram(aes(y=..count..), colour="black", fill="white") +
                                  geom_density(alpha=.2, fill="#FF6666") + 
                                  geom_vline(aes(xintercept=datMean),  color="red", linetype="dashed", size=1) + 
                                  #geom_vline(aes(xintercept=mean),  color="red", linetype="dashed", size=1) +
                                  labs(x = xLabCombo, y = yLabCombo) +
                                  ggtitle(title) + 
                                  #theme(plot.title = element_text(lineheight=.8, face="plain"))
                                  theme(plot.title = element_text(hjust = 0.5, size = 10))  +   # center the title
                                  theme(aspect.ratio=4/4)   # graph height is larger than width if not set this 

                          }

                          ############################
                          # Display and Save plot
                          ############################
                          plotObj = histp
                          plotType = 'histogram'
                          phvAccNameCombo = paste(phvAcc, "_", varName, sep="")
                          savedPlotFiles <- saveGapPlot(object, plotObj = plotObj, plotType = plotType, phvAccNameCombo = phvAccNameCombo, saveToDir = saveToDir, showPlot = showPlot)

                          return(invisible(dat))

                      }
                      ######################
                      # Categorical data
                      ######################
                      else if (varType == 'enumerated integer' | varType == 'enum_integer' | varType == 'string') {
                          catVarInfoDF = varInfoDF

                          index  <-seq.int(nrow(dat))

                          ###########################
                          # Disply variable info
                          ###########################
                          getVariableInfoByPhvAcc(object, phvAccList = c(phvAcc), showBrief=T)

                          #### Process categorical variable data  ####
                          #catVarDataNoIdDF <- convertEnumVarColName(object, varDataDF = varDataDF, varInfoDF = catVarInfoDF)

                          varCodeValDF <- getExtData(object, type = 'code', phsAcc = phsAcc)
                          catVarDataNoIdDF <- convertEnumVarColName(object, varDataDF = varDataDF, varInfoDF = catVarInfoDF, varCodeValDF = varCodeValDF)

                          finalDF <- cbind(index, catVarDataNoIdDF)
                          # index  SCHOOL
                          #  1     1 SCHOOL2
                          #  2     2 SCHOOL4
                          #  3     3 SCHOOL5
                          #  4     4 SCHOOL4

                          countDF <- count(finalDF, xlab)
                          #print(head(countDF,4))

                          # SCHOOL freq
                          #  1 SCHOOL1  313
                          #   2 SCHOOL2  956
                          #   3 SCHOOL3 1139
                          #   4 SCHOOL4  601

                          yLabCombo = paste("\n", "Subject Count", "\n") 

                          freq <- ''  # mute CMD check

                          barp <- ggplot(countDF, aes_string(x=xlab, y = 'freq', fill=xlab)) +
                              geom_bar(stat = "identity") +
                              geom_text(aes(label=freq), position=position_dodge(width=0.9), vjust=-0.25) +
                              labs(x = xLabCombo, y = yLabCombo) +
                              ggtitle(paste("\n", xlab, "(", phvAcc, ")")) + 
                              #theme(plot.title = element_text(lineheight=.8, face="plain", size=11))
                              theme(plot.title = element_text(hjust = 0.5, size = 10))  +   # center the title
                              theme(aspect.ratio=4/4)   # graph height is larger than width if not set this 


                              ############################
                              # Display and Save plot
                              ############################
                              plotObj = barp
                              plotType = 'barchart'
                              phvAccNameCombo = paste(phvAcc, "_", varName, sep="")
                              savedPlotFiles <- saveGapPlot(object, plotObj = plotObj, plotType = plotType, phvAccNameCombo = phvAccNameCombo, saveToDir = saveToDir, showPlot = showPlot)

                              return(invisible(countDF))

                      }
                      else {

                          type = 'process'
                          level = 'info'
                          show = T
                          mesg = paste("The data type ", type, ", of input variable, ", varName, " ( ", phvAcc, " ), is neither numerical nor categorical. The statistical summary thus is not generated.\n", sep="")
                          writeLog(object,  type = type, level = level, message = mesg, show = show) 
                      }
                  } # end !is.null(varDataDF)
              } # end phvAcc != ""

          })



# -------------------------
# Method: variablePiechart
# -------------------------

#' Categorical variable subject count pie-chart 
#' 
#' 
#' The method draws the pie-chart of subject counts among categories of a categorical variable. The resulting graph is saved as PDF and PNG files. 
#'
#' @param object Study class object.
#' @param catPhvAcc a character string. The dbGaP accession of a categorical variable.
#' @param ... There are optional arguments.
#' @param saveToDir a character string. (optional) The path to the directory where the plot PDF file is saved. If not provided, the file is saved in the 'temp' directory under the user project directory.
#' @param showPlot a logical value. (optional) If TRUE (default), shows the created graph; Not show if FALSE.
#' @return a data frame. The data used for plotting. 
#' @export variablePiechart 
#' @examples
#' \dontrun{
#' 
#' s <- Study(phsAcc = 'phs000001.v3.p1')
#' variablePiechart(s, catPhvAcc = 'phv00053764.v2')
#'}

# s <- Study(phsAcc = 'phs000001.v3.p1')
# variablePiechart(s, catPhvAcc = 'phv00053764.v2')		# LPSN: enumerated integer
# variablePiechart(s, catPhvAcc = 'phv00000035.v2')		# SCHOOL: enumerated integer 
# variablePiechart(s, catPhvAcc = 'phv00000032.v2')		# MARITAL: enumerated integer 
# variablePiechart(s, catPhvAcc = 'phv00053764.v2')		# LPSC: enumerated integer 
# variablePiechart(s, catPhvAcc = 'phv00053757.v2')		# NUC: enumerated integer 
# variablePiechart(s, catPhvAcc = 'phv00054126.v1') 	# DIABTRT01 enum
# variablePiechart(s, catPhvAcc = 'phv00053764.v2', saveToDir = '/panfs/sandpan1.be-md.ncbi.nlm.nih.gov/homes/hao/temp', showPlot=F)

setGeneric(
           name = "variablePiechart",
           def = function(object, catPhvAcc, ...) {
               standardGeneric("variablePiechart")
           })

#' @describeIn variablePiechart of class Study 
setMethod(
          f = "variablePiechart",
          signature = c("Study", "character"),
          definition = function(object, catPhvAcc = '', ..., saveToDir = '', showPlot = T) {
              saveToDir <- checkInputPath(object, saveToDir)


              phsAcc = object@phsAcc
              prjTempDir = object@prjTempDir 

              catPhvAcc <- cleanObjAcc(object, acc = catPhvAcc, type = 'phv')

              varDataDF = data.frame()
              if (nchar(catPhvAcc) > 0) {
                  catPhvAcc <- cleanObjAcc(object, acc = catPhvAcc, type = 'phv')
                  varDataDF <- getVariableDataByPhvAcc(object, phvAccList = c(catPhvAcc), emptyToNa = T) 
              }



              if (nrow(varDataDF) > 0) {

                  ################
                  # Get datatype
                  ################
                  # possible types: decimal, enum_integer, integer, string, unknown
                  # type "unknown" should be ignored
                  studyDataDicDF <- getDataDicByStudy(object, phsAcc) 

                  catVarInfoDF <- subset(studyDataDicDF,  studyDataDicDF$variable_accession==catPhvAcc) 
                  catVarType <- as.character(catVarInfoDF$calculated_type)
                  catVarName <- as.character(catVarInfoDF$name)
                  catVarUnits <- as.character(catVarInfoDF$units)
                  catVarCodeValCombo <- as.character(catVarInfoDF$code_value_combo)

                  # Numerical type
                  # decimal
                  inputOk = '' 
                  if (catVarType == 'integer' | catVarType == 'decimal') {

                      type = 'process'
                      level = 'info'
                      show = T
                      mesg = paste("The data type ", type, ", of input variable, ", catVarName, " ( ", catPhvAcc, " ), is not catgorical . The Piechart is not drawn.\n", sep="")
                      writeLog(object,  type = type, level = level, message = mesg, show = show) 
                  }
                  else {

                      #################################
                      # Separate out the data columns 
                      #################################
                      catVarDataNoIdDF = subset(varDataDF, select = c(catVarName))			# separate out catVar column only

                      #########################
                      # Categorical type 
                      #########################
                      if (catVarType == 'enum_integer' | catVarType == 'string' | catVarType == 'enumerated integer') {


                          ################################
                          # With categorical variable 
                          ################################
                          varCodeValDF <- getExtData(object, type = 'code', phsAcc = phsAcc)
                          #catVarDataNoIdDF <- convertEnumVarColName(object, varDataDF = varDataDF, varInfoDF = catVarInfoDF, varCodeValDF = varCodeValDF)

                          varCodeValDF <- getExtData(object, type = 'code', phsAcc = phsAcc)
                          catVarDataNoIdDF <- convertEnumVarColName(object, varDataDF = varDataDF, varInfoDF = catVarInfoDF, varCodeValDF = varCodeValDF)

                          dat = catVarDataNoIdDF 
                          dat$index <-seq.int(nrow(dat))	# add an index column for Subject index (instead of original Subject ID)

                          #print(head(dat, 4))
                          #   LPSC index
                          #   1 PSC-A     1
                          #   2 PSC-B     2
                          #   3 PSC-A     3
                          #   4 PSC-A     4

                          xlab = catVarName
                          ylab = 'perc'

                          # mute CMD check  note
                          freq <- '' 
                          perc <- '' 
                          label_pos <- ''
                          perc_text <- ''
                          n <- ''

                          statDF <- dat %>% dplyr::group_by_(xlab) %>% dplyr::summarize(freq = n()) %>%
                              mutate(perc = freq / sum(freq)) %>%
                              arrange(desc(perc)) %>%
                              #mutate(label_pos = cumsum(perc) - perc / 2, perc_text = paste(freq, " (", round(perc * 100), "%)", sep=""))
                              mutate(
                                     # Note: Need manually adjusted postion (xPosVec) as shown later 
                                     label_pos = perc / 2 + c(0, cumsum(perc)[-length(perc)]), 
                                     #label_pos = cumsum(perc) - perc / 2,
                                     #label_pos = perc / 2,
                                     perc_text = paste(freq, " (", round(perc * 100), "%)", sep="")
                                     )
                              #	y = percentage / 2 + c(0, cumsum(percentage)[-length(percentage)]),

                              #print(head(statDF,4))
                              #	# A tibble: 4 <U+00D7> 5
                              #	 A   LPSC  freq       perc label_pos  perc_text
                              #	 A     <chr> <int>      <dbl>     <dbl>      <chr>
                              #	 A	 1 PSC-A  3244 0.68194240 0.3409712 3244 (68%)
                              #	 A	 2 PSC-C  1061 0.22303973 0.7934623 1061 (22%)
                              #	 A	 3 PSC-B   317 0.06663864 0.9383015   317 (7%)
                              #	 A	 4 PSC-D   135 0.02837923 0.9858104   135 (3%)


                              ##### Adjustment for small slice ####
                              # make 2nd slice y pos lower, to make the 2nd and 3rd lable such as below less crowded

                              #print(head(statDF,4))
                              #  DIABTRT01  freq       perc label_pos  perc_text
                              #  DI    <chr> <int>      <dbl>     <dbl>      <chr>
                              #  DI	1      <NA>  3544 0.94205210 0.4710260 3544 (94%)
                              #  DI	2         P   118 0.03136629 0.9577352   118 (3%)
                              #  DI	3         D    58 0.01541733 0.9811271    58 (2%)
                              #  DI	4         I    42 0.01116427 0.9944179    42 (1%)

                              pos2 <- statDF[2, 'label_pos']
                              newPos2 <- pos2 - 0.03
                              statDF[2, 'label_pos'] <- newPos2


                              # Algorithm to position text based on row number
                              rowNum = nrow(statDF)

                              # Variable name label on each slice 
                              y.breaks <- cumsum(statDF$perc) - statDF$perc / 2
                              labels <-unlist(statDF[,1])

                              # x, y-axis label
                              xLabCombo = paste("\n", "Subject Count\n")
                              yLabCombo = ''
                              title = paste("\n", catVarName, "(", catPhvAcc, ")\n")

                              if (rowNum < 9) {

                                  pie <- ggplot(data=statDF, aes_string(x=1, y=ylab, fill=xlab)) +
                                      geom_bar(width=1, stat="identity") +
                                      coord_polar(theta="y") +
                                      theme_void() +
                                      scale_y_continuous( breaks=y.breaks, labels=labels) +
                                      ggtitle(title) +
                                      labs(x = yLabCombo, y = xLabCombo) +		# Note: filipped  x, y
                                      #theme(plot.title = element_text(lineheight=.8, face="plain", size=11))
                                      theme(plot.title = element_text(hjust = 0.5, size = 10)) +    # center the title
                                      theme(aspect.ratio=4/4) +  # graph height is larger than width if not set this 
                                      theme(plot.margin = unit(c(0,0,0,0), "cm"))


                                      ###########################
                                      # Disply variable info
                                      ###########################
                                      getVariableInfoByPhvAcc(object, phvAccList = c(catPhvAcc), showBrief=T)

                                      # Show piechart slice label 
                                      info <- sprintf("%-5s %-1s %s", catVarName, ":", 'subject (%)') 
                                      cat(info, "\n")
                                      ddply(statDF, c(catVarName), function(x) { 
                                                catVal <- x[c(catVarName)][[1]]
                                                perc_text <- x$perc_text
                                                # Display pie chart info
                                                info <- sprintf("%-5s %-1s %s", catVal, ":", perc_text) 
                                                cat(info, "\n")
                                     })

                                      cat("\n")

                                      ############################
                                      # Display and Save plot
                                      ############################
                                      plotObj = pie
                                      plotType = 'boxplot'
                                      phvAccNameCombo = paste(catPhvAcc, "_", catVarName, sep="")
                                      savedPlotFiles <- saveGapPlot(object, plotObj = plotObj, plotType = plotType, phvAccNameCombo = phvAccNameCombo, saveToDir = saveToDir, showPlot = showPlot)

                                      return(invisible(statDF))
                              }
                      }

                  } # end of nchar(catPhvAcc) > 0

              } # end (nrow(varDataDF) > 0)
              else {
                  type = 'process'
                  level = 'info'
                  show = T
                  mesg = paste("There is no data in the study datasets files for variable ", catPhvAcc, " The boxplot is not drawn.\n", sep="")
                  writeLog(object,  type = type, level = level, message = mesg, show = show) 
              }

          })

# ----------------------------------
# Method: variableVenndiagram
# ----------------------------------

#' Subject overlapping with Venndigram
#' 
#' The method shows the overlapping subjects of given dbGaP variables by Venndiagram. The number of input variables is limited to five.
#'
#' @param object Study class object
#' @param phvAccList a character vector. A list of dbGaP variable accessions. The maximum number of variable accessions accepted for the drawing is 5, so that only the first 5 variables in the input list are included in the Venndiagram.
#' @param ... There are optional argument.
#' @param saveToDir a character string. (optional) The path to the directory where the plot PDF file is saved. If not provided, the file is saved in the 'temp' directory under the user project directory.
#' @param showPlot a logical value. (optional) If TRUE (default), shows the created graph; Not show if FALSE.
#' @return a data frame. The data used for plotting. 
#' @export variableVenndiagram 
#' @examples
#' \dontrun{
#'
#' s <- Study(phsAcc = 'phs000001.v3.p1')
#' accList = c('phv00054139.v1', 'phv00053796.v2', 'phv00000089.v2')
#' variableVenndiagram(s, phvAccList = accList) 
#' }

# s <- Study(phsAcc = 'phs000001.v3.p1')
# variableVenndiagram(s, phvAccList = c('phv00054119.v1.p1.c2', 'phv00054118.v1.p1', 'phv00053733.v2', 'phv00053735.v2', 'phv00053732.v2'))
#  variableVenndiagram(s, phvAccList = c('phv00054139.v1', 'phv00053796.v2', 'phv00000089.v2'))
# variableVenndiagram(s, phvAccList = c('phv00054139.v1', 'phv00053796.v2', 'phv00000089.v2', 'phv00000072.v2'))
# variableVenndiagram(s, phvAccList = c('phv00054139.v1', 'phv00053796.v2', 'phv00000089.v2', 'phv00000072.v2', 'phv00000173.v2')) 
# variableVenndiagram(s, phvAccList = c('phv00054119.v1.p1.c2', 'phv00053735.v2'))
# variableVenndiagram(s, phvAccList = c('phv00054119.v1.p1.c2', 'phv00053735.v2', 'phv00053732.v2'))
#
# phs000007.v29
# variableVenndiagram(s, phvAccList = c("phv00251740.v1", "phv00251348.v1", "phv00251634.v1"))
# variableVenndiagram(s, phvAccList = c("phv00251740.v1", "phv00251348.v1", "phv00251634.v1", "phv00251689.v1"))


setGeneric(
           name = "variableVenndiagram",
           def = function(object, phvAccList, ...) {
               standardGeneric("variableVenndiagram")
           })

#' @describeIn variableVenndiagram of class Study 
setMethod(
          f = "variableVenndiagram",
          signature = c("Study", "character"),
          definition = function(object, phvAccList, ..., saveToDir = '', showPlot = T) {
              saveToDir <- checkInputPath(object, saveToDir)

              phsAcc = object@phsAcc
              prjDotDir = object@prjDotDir
              prjDir = object@prjDir

              # suppress VenDiagram log
              futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")

              # Allow bot list and vector as input. Convert to vector of it is a list.
              if (is.list(phvAccList)) {
                  phvAccList = unlist(phvAccList, use.names=FALSE)
              }


              if (length(phvAccList) > 0 & length(phvAccList) < 6) {

                  ###########################
                  # Validate PhvAccList
                  ###########################
                  cleanPhvAccList <- checkPhvAccList(object, phvAccList = phvAccList) 

                  if (length(cleanPhvAccList) > 0) {

                      # Get StudyDataDicDF
                      parseIdsFromStAcc =  parseIdsFromStAcc(object, phsAcc = phsAcc)
                      phsAccNoVer = parseIdsFromStAcc$phsAccNoVer
                      studyDataDicDF <- getDataDicByStudy(object, phsAcc) 

                      ##############################
                      # Loop through each phvAcc 
                      ##############################
                      # Remove all NA values for each phv column. Get subjIds of each in a format for feeding VennDiagram 
                      setupVenn <- function(phvAcc, phsAcc, mergedVarDF, dataDicDF) {

                          inputObjPhsAcc <- checkObjStudyByAcc(object, acc = phvAcc)

                          if (phsAcc == inputObjPhsAcc) {

                              # subset id and phv
                              # Look for matching phtAcc and phsAcc 
                              matchVarDF <- subset(dataDicDF,  dataDicDF$variable_accession==phvAcc) 
                              matchStAcc <- matchVarDF$study_accession
                              matchPhtAcc <- matchVarDF$dataset_accession
                              matchVarName <- matchVarDF$name
                              matchVarType <- matchVarDF$calculated_type

                              #########################################
                              # Subset dbGaP_Subject_Id with each phv
                              #########################################
                              gapIdVarDF <- subset(mergedVarDF, select = c('dbGaP_Subject_ID', matchVarName))

                              ######################################
                              # Remove na value by complete.cases  
                              ######################################
                              completeVarDF <- gapIdVarDF[complete.cases(gapIdVarDF),]

                              # id column 
                              gapIdDF <- completeVarDF["dbGaP_Subject_ID"]

                              # Turn df to list
                              gapIdList <- as.list(gapIdDF)[1]

                              #################################
                              # Flatten the list structure
                              #################################
                              # To make laaply result data fit VennDiagram, get the data without item name,
                              # then put entire list data into a new list 
                              gapIdListVal = gapIdList[[1]]

                              newGapIdList = c(gapIdListVal[1:length(gapIdListVal)])

                              return (newGapIdList)

                          }
                      }

                      ###############################
                      #     Get mergedVarD          #
                      ###############################

                      ########################
                      # ATTN!!! Empty --> NA
                      ########################
                      # Convert empty values to na so complete case can be applied.
                      mergedVarDF =  getVariableDataByPhvAcc(object, cleanPhvAccList, emptyToNa = T)

                      #########################################
                      # Complete case of all input variables 
                      #########################################
                      #completeVarDF = mergedVarDF[complete.cases(mergedVarDF),]
                      retList <- lapply(cleanPhvAccList, FUN=function(x) setupVenn(phvAcc = x, phsAcc = phsAcc, mergedVarDF = mergedVarDF, dataDicDF = studyDataDicDF)) 


                      # Get PhvNameList 
                      getPhvName <- function(phvAcc, dataDicDF) {

                          inputObjPhsAcc <- checkObjStudyByAcc(object, acc = phvAcc)

                          if (phsAcc == inputObjPhsAcc) {
                              matchVarDF <- subset(dataDicDF,  dataDicDF$variable_accession==phvAcc) 


                              matchStAcc <- matchVarDF$study_accession
                              matchPhtAcc <- matchVarDF$dataset_accession
                              matchVarName <- matchVarDF$name
                              matchVarType <- matchVarDF$calculated_type

                              return (matchVarName) 
                          }
                      }

                      phvNameList <- lapply(cleanPhvAccList, FUN=function(x) getPhvName(phvAcc = x, dataDicDF = studyDataDicDF))


                      ##############################
                      # Draw VennDiagram
                      ##############################
                      # Compose title
                      # Note: No title added because when margin is used, it makes labels paritally out-of-scope 

                      # Get image file name NameAcc combo
                      varNameAccUscoreList = getVarNameByPhvAcc(object, phvAccList = cleanPhvAccList, studyDataDicDF = studyDataDicDF, colNameWithAcc = T, underscore = T) 
                      varNameAccUscoreCombo = paste(varNameAccUscoreList, collapse="_") 

                      # Compose label make it from "MATCHSPEC" to "MATCHSPEC \n( phv00054119.v1 )"
                      varNameAccList = getVarNameByPhvAcc(object, phvAccList = cleanPhvAccList, studyDataDicDF = studyDataDicDF) 

                      varNameComboList <- lapply(cleanPhvAccList, function(phvAcc) 
                                                 {
                                                     varName = varNameAccList[phvAcc]
                                                     nameAccNewlineCombo = paste(varName, "\n(", phvAcc, ")")
                                                 })
                      varNameCombList = unlist(varNameComboList)

                      # Add list item name
                      # Use phvNameAccCombo as display name
                      names(retList) <- varNameCombList 

                      # Suppress VenDiagram log
                      futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
                      # Skip color #1 (grey)
                      colorCount = length(phvNameList) + 1

                      margin = 0.15
                      height = 3000
                      width = 3000
                      if (length(phvAccList) == 5) {
                          vp <- VennDiagram::venn.diagram(retList, fill = 2:colorCount, alpha = 0.3, filename = NULL, height = height, width = width, margin=margin, main=NULL, main.cex=1, main.col="black", sub = NULL, cex=0.9, cat.cex = 0.9);
                      }
                      else {
                          margin = 0.08
                          vp <- VennDiagram::venn.diagram(retList, fill = 2:colorCount, alpha = 0.3, filename = NULL, height = height, width = width,  margin=margin, main=NULL, main.cex=1, main.col="black", sub = NULL, cex=0.9, cat.cex = 0.9);
                      }


                      ###########################
                      # Disply variable info
                      ###########################
                      getVariableInfoByPhvAcc(object, phvAccList = cleanPhvAccList, showBrief=T)


                      if (showPlot == T) {
                          # devoff is needed for VennDiagram, but not for other ggplot
                          dev.off()
                          grid::grid.draw(vp) 
                      }

                      ############################
                      # Display and Save plot
                      ############################
                      plotObj = vp
                      plotType = 'VennDiagram'
                      phvAccNameCombo = varNameAccUscoreCombo 
                      savedPlotFiles <- saveGapPlot(object, plotObj = plotObj, plotType = plotType, phvAccNameCombo = phvAccNameCombo, saveToDir = saveToDir, showPlot = showPlot)


                      return (invisible(mergedVarDF))
                  }


              } # end of length(phvAccList) > 0 & < 6
              else {

                  type = 'process'
                  level = 'info'
                  show = T

                  if (length(phvAccList) == 0) {
                      mesg = paste("The number of input accession is zero. The Venndiagram is not drawn\n", sep="")
                  }
                  else {
                      mesg = paste("The number of input accession exceeds the maximum allowed 5. The Venndiagram is not drawn\n", sep="")
                  }
                  writeLog(object,  type = type, level = level, message = mesg, show = show) 
              }

          })




# -------------------------------- 
# Method: variableCorrelHeatmap
# -------------------------------- 

#' Variable correleation heatmap
#' 
#' 
#' The method draws the correlation graphs between different categories of a categorical variable. The resulting graph is saved as PDF and PNG files.
#'
#' @param object Study class object.
#' @param catPhvAcc a character string. The dbGaP accession of a categorical variable. 
#' @param ... There are optional arguments.
#' @param numPhvAccList a character vector. A list of the dbGaP accessions of numerical variables.
#' @param saveToDir a character string. (optional) The path to the directory where the plot PDF file is saved. If not provided, the file is saved in the 'temp' directory under the user project directory.
#' @param showPlot a logical value. (optional) If TRUE (default), shows the created graph; Not show if FALSE.
#' @return a data frame. The data used for plotting. 
#' @export variableCorrelHeatmap 
#' @examples
#' \dontrun{
#'
#' s <- Study(phsAcc = 'phs000001.v3.p1')
#' catAcc = 'phv00000031.v2'
#' numAccList = c('phv00000007.v2', 'phv00053794.v2', 'phv00053786.v2', 'phv00000106.v2') 
#' variableCorrelHeatmap(s, catPhvAcc = catAcc, numPhvAccList = numAccLlist, showPlot=T)
#'} 



# s <- Study(phsAcc = 'phs000001.v3.p1')
#
# variableCorrelHeatmap(s, catPhvAcc = 'phv00053764.v2', numPhvAccList = c('phv00000027.v2', 'phv00053747.v2', 'phv00000006.v2', 'phv00053747.v2'), showPlot=T)	# 1 ~ 1
# variableCorrelHeatmap(s, catPhvAcc = 'phv00000035.v2', numPhvAccList = c('phv00000007.v2', 'phv00053794.v2', 'phv00054139.v1', 'phv00000084.v2'), showPlot=T)	 # 1 ~ 0.99
# variableCorrelHeatmap(s, catPhvAcc = 'phv00000014.v2', numPhvAccList = c('phv00000007.v2', 'phv00053794.v2', 'phv00053786.v2', 'phv00000106.v2'), showPlot=T)		# 1 ~ 0,98
# variableCorrelHeatmap(s, catPhvAcc = 'phv00000030.v2', numPhvAccList = c('phv00000007.v2', 'phv00053794.v2', 'phv00053786.v2', 'phv00000106.v2'), showPlot=T)	 # 1 ~ 0.90
#  variableCorrelHeatmap(s, catPhvAcc = 'phv00000031.v2', numPhvAccList = c('phv00000007.v2', 'phv00053794.v2', 'phv00053786.v2', 'phv00000106.v2'), showPlot=T)		# 1 ~ 0.55
# variableCorrelHeatmap(s, catPhvAcc = 'phv00000035.v2', numPhvAccList = c('phv00000007.v2', 'phv00053794.v2', 'phv00053786.v2', 'phv00000106.v2'))    # 1 ~ 0.66
# variableCorrelHeatmap(s, catPhvAcc = 'phv00000034.v2', numPhvAccList = c('phv00000007.v2', 'phv00053794.v2', 'phv00053786.v2', 'phv00000106.v2'), showPlot=T)	 # 1 ~ -1
#  variableCorrelHeatmap(s, catPhvAcc = 'phv00053944.v2', numPhvAccList = c('phv00000007.v2', 'phv00053794.v2', 'phv00053786.v2', 'phv00000106.v2'), showPlot=T)     # 1 ~ -1
#  variableCorrelHeatmap(s, catPhvAcc = 'phv00000016.v2', numPhvAccList = c('phv00000007.v2', 'phv00053794.v2', 'phv00053786.v2', 'phv00000106.v2'), saveToDir = '/panfs/sandpan1.be-md.ncbi.nlm.nih.gov/homes/hao/temp', showPlot=T)		# 1 ~ 0.44
#  variableCorrelHeatmap(s, catPhvAcc = 'phv00000015.v2', numPhvAccList = c('phv00000007.v2', 'phv00053794.v2', 'phv00053786.v2', 'phv00000106.v2'), saveToDir = '/panfs/sandpan1.be-md.ncbi.nlm.nih.gov/homes/hao/temp', showPlot=T)		# 1 ~ 0.16
# variableCorrelHeatmap(s, catPhvAcc = 'phv00000026.v2', numPhvAccList = c('phv00000007.v2', 'phv00053794.v2', 'phv00053786.v2', 'phv00000106.v2'), saveToDir = '/panfs/sandpan1.be-md.ncbi.nlm.nih.gov/homes/hao/temp', showPlot=T)		# 1 ~ 0.1


# phs000007.v29
# variableCorrelHeatmap(s, catPhvAcc = 'phv00251689.v1', numPhvAccList = c('phv00251303.v1', 'phv00251621.v1', 'phv00251618.v1', 'phv00251698.v1'), showPlot=T)
#  variableCorrelHeatmap(s, catPhvAcc = 'phv00251798.v1', numPhvAccList = c('phv00251628.v1', 'phv00251621.v1', 'phv00251631.v1', 'phv00251673.v1'), showPlot=T)

# phs000007.v29 --- Minus correlation
# variableCorrelHeatmap(s, catPhvAcc = 'phv00251769.v1', numPhvAccList = c('phv00251303.v1', 'phv00251621.v1', 'phv00251618.v1', 'phv00251698.v1'), showPlot=T)
# variableCorrelHeatmap(s, catPhvAcc = 'phv00251769.v1', numPhvAccList = c('phv00251795.v1', 'phv00251689.v1', 'phv00251618.v1', 'phv00251698.v1'), showPlot=T)

setGeneric(
           name = "variableCorrelHeatmap",
           def = function(object, catPhvAcc, ...) {
               standardGeneric("variableCorrelHeatmap")
           })

#' @describeIn variableCorrelHeatmap of class Study 
setMethod(
          f = "variableCorrelHeatmap",
          signature = c("Study", "character"),
          definition = function(object, catPhvAcc = '', ..., numPhvAccList = vector(), saveToDir = '', showPlot = T) {
              saveToDir <- checkInputPath(object, saveToDir)

              phsAcc = object@phsAcc
              prjTempDir = object@prjTempDir 

              #########################
              # Get Study-DataDic
              #########################
              studyDataDicDF <- getDataDicByStudy(object, phsAcc) 


              ###########################
              # Validate PhvAccList
              ###########################
              cleanNumPhvAccList <- checkPhvAccList(object, phvAccList = numPhvAccList) 


              ################################
              # Process Categorical variable 
              ################################
              catPhvAcc <- cleanObjAcc(object, acc = catPhvAcc, type = 'phv')

              if (length(cleanNumPhvAccList) > 0 & nchar(catPhvAcc) > 0) {

                  ################
                  # Get datatype
                  ################
                  # possible types: decimal, enum_integer, integer, string, unknown
                  # type "unknown" should be ignored

                  catVarInfoDF <- subset(studyDataDicDF,  studyDataDicDF$variable_accession==catPhvAcc) 
                  catVarType <- as.character(catVarInfoDF$calculated_type)
                  catVarName <- as.character(catVarInfoDF$name)
                  catVarUnits <- as.character(catVarInfoDF$units)
                  catVarCodeValCombo <- as.character(catVarInfoDF$code_value_combo)

                  # Numerical type
                  # decimal
                  inputOk = '' 
                  if (catVarType == 'integer' | catVarType == 'decimal') {

                      type = 'process'
                      level = 'info'
                      show = T
                      mesg = paste("The data type ", type, ", of input variable, ", catVarName, " ( ", catPhvAcc, " ), is not catgorical . The correlation graph is not drawn.\n", sep="")
                      writeLog(object,  type = type, level = level, message = mesg, show = show) 
                  }


                  ##################################
                  # Process Numerical Variable 
                  ##################################


                  # Check numPhvList item data type 
                  wrongTypePhvList <- lapply(cleanNumPhvAccList, function(numPhvAcc, dataDicDF = studyDataDicDF) { 

                                                 # possible types: decimal, enum_integer, integer, string, unknown
                                                 # type "unknown" should be ignored
                                                 numVarInfoDF <- subset(studyDataDicDF,  studyDataDicDF$variable_accession==numPhvAcc) 
                                                 numVarType <- as.character(numVarInfoDF$calculated_type)

                                                 if (numVarType != 'decimal' & numVarType != 'integer') {
                                                     numPhvAcc
                                                 }
                                                 else {
                                                     NULL
                                                 }
                                                 })
                  # Remove null
                  wrongTypePhvList <- wrongTypePhvList[!sapply(wrongTypePhvList, is.null)] 

                  if (length(wrongTypePhvList) > 0) {
                      phvListCombo = paste(wrongTypePhvList, collapse = ' ') 

                      type = 'process'
                      level = 'info'
                      show = T
                      mesg = paste("There following accession(s) in the input numPhvAccList is not numerical type. It needs to be fixed. The corrlation plot is not drawn.\n", sep="")
                      writeLog(object,  type = type, level = level, message = mesg, show = show) 
                  }
                  else {

                      ####################################
                      # Combine catPhvAcc and numPhvAcc
                      ####################################
                      carAndNumPhvAccList = c(catPhvAcc, cleanNumPhvAccList)
                      # Get varNameList: a named list of phvACC-Name pairs
                      varNameList <- getVarNameByPhvAcc(object, phvAccList =  cleanNumPhvAccList, studyDataDicDF = studyDataDicDF)  

                      ####### Get mergedVarDF #######

                      ########################
                      # ATTN!!! Empty --> NA
                      ########################
                      # Convert empty values to na so complete case can be applied.
                      mergedVarDF =  getVariableDataByPhvAcc(object, carAndNumPhvAccList, emptyToNa = T)


                      #########################################
                      # Complete case of all input variables 
                      #########################################
                      completeVarDF = mergedVarDF[complete.cases(mergedVarDF),]
                      #print(head(completeVarDF,4))
                      #  dbGaP_Subject_ID Submitted_Subject_ID  LPSC AGEPHOT LNUCSCORE LNUCSCORE.1		LNUCSCORE.2
                      #  1                1                 1379 PSC-A    74.2      2.08        2.08	       2.08
                      #  2                2                 4861 PSC-B    69.6      3.39        3.39         3.39
                      #  3                3                 3642 PSC-A    73.4      2.95        2.95         2.95 
                      #  4                4                 5400 PSC-A    79.1      4.09        4.09	       4.09


                      # variableCorrelHeatmap(s, catPhvAcc = 'phv00000035.v2', numPhvAccList = c('phv00000007.v2', 'phv00053794.v2', 'phv00053786.v2', 'phv00000106.v2'))    # 1 ~ 0.66
                      #
                      #			   dbGaP_Subject_ID Submitted_Subject_ID SCHOOL RCORBASE  DT_LYC DT_VITE DIAS12
                      #			   61               16                 3554      4      1.1 2536.37    2.84     56
                      #			   62               16                 3554      4      1.1 1025.88    2.84     56
                      #			   63               16                 3554      4      1.1 2536.37    2.89     56
                      #			   64               16                 3554      4      1.1 1025.88    2.89     56
                      #

                      ##### Make sure it is not an empty dataframe ####

                      #############################################################
                      # Check to make sure the categorical variable value
                      # has more than one unique value 
                      #############################################################
                      catUniqueVal <- unique(completeVarDF[catVarName])
                      catUniqueValList <- catUniqueVal[,1]


                      if (nrow(completeVarDF) > 0 ) {

                          #### Make sure cat variable has more than one value ####
                          if (length(catUniqueValList) > 1) {

                              # At least two numberical columns except 2 id and 1 categorical variable columns
                              if (ncol(completeVarDF) > 5) {

                                  dat <- completeVarDF

                                  ########################################################
                                  # Get mean of each num-variable gropu_by cat-variable
                                  ########################################################
                                  # Note: somehow the dplyr way doesn't work due to the error: summarise_each_ ...  could not find function "funs"  (possible reason: dplyr version not up-to-date)
                                  meanDF <- ddply(dat, catVarName, function(x) colSums(x[varNameList]))
                                  #print(head(meanDF,4))

                                  #					  SCHOOL RCORBASE   DT_LYC DT_VITE DIAS12
                                  #					  1      1     56.8 142067.8  305.68   2400
                                  #					  2      2    841.5 670035.2 1441.05  11512
                                  #					  3      3    492.4 738097.0 2007.72  12488
                                  #					  4      4    498.8 474929.3 1064.85   7254
                                  #

                                  # Compute correlation
                                  corrDF <- cor(meanDF[,2:ncol(meanDF)]) 
                                  #print(head(corrDF,4))

                                  #					          RCORBASE    DT_LYC   DT_VITE    DIAS12
                                  #							  RCORBASE 1.0000000 0.7753344 0.6640918 0.8125297
                                  #							  DT_LYC   0.7753344 1.0000000 0.9859944 0.9936985
                                  #							  DT_VITE  0.6640918 0.9859944 1.0000000 0.9720241
                                  #							  DIAS12   0.8125297 0.9936985 0.9720241 1.0000000
                                  #

                                  # Round 
                                  corrDF <- round(as.matrix(corrDF),2) 
                                  #print(head(corrDF,4))

                                  #					         RCORBASE DT_LYC DT_VITE DIAS12
                                  #							 RCORBASE     1.00   0.78    0.66   0.81
                                  #							 DT_LYC       0.78   1.00    0.99   0.99
                                  #							 DT_VITE      0.66   0.99    1.00   0.97
                                  #							 DIAS12       0.81   0.99    0.97   1.00

                                  # Make upper half NA
                                  corrDF[upper.tri(corrDF ,diag=F)]<-NA #We only want to plot 1/2 the matrix 
                                  # print(head(corrDF,4))

                                  #					        RCORBASE DT_LYC DT_VITE DIAS12
                                  #							RCORBASE     1.00     NA      NA     NA
                                  #							DT_LYC       0.78   1.00      NA     NA
                                  #							DT_VITE      0.66   0.99    1.00     NA
                                  #							DIAS12       0.81   0.99    0.97      1
                                  #

                                  # Convert to ggplot dataframe

                                  pd <- reshape2::melt(t(corrDF),value.name='Correlation') #convert it to data.frame readiable by ggplot, transposing it 't()' helps it properly oriented 
                                  #print(pd)

                                  #					       Var1     Var2 Correlation
                                  #						   1  RCORBASE RCORBASE        1.00
                                  #						   2    DT_LYC RCORBASE          NA
                                  #						   3   DT_VITE RCORBASE          NA
                                  #						   4    DIAS12 RCORBASE          NA
                                  #						   5  RCORBASE   DT_LYC        0.78
                                  #						   6    DT_LYC   DT_LYC        1.00
                                  #						   7   DT_VITE   DT_LYC          NA
                                  #						   8    DIAS12   DT_LYC          NA
                                  #						   9  RCORBASE  DT_VITE        0.66
                                  #						   10   DT_LYC  DT_VITE        0.99
                                  #						   11  DT_VITE  DT_VITE        1.00
                                  #						   12   DIAS12  DT_VITE          NA
                                  #						   13 RCORBASE   DIAS12        0.81
                                  #						   14   DT_LYC   DIAS12        0.99
                                  #						   15  DT_VITE   DIAS12        0.97
                                  #						   16   DIAS12   DIAS12        1.00

                                  #########################
                                  # Plot
                                  #########################

                                  # Get varNameAcc combo
                                  numVarNameAccParenthList = getVarNameByPhvAcc(object, phvAccList = cleanNumPhvAccList, studyDataDicDF = studyDataDicDF, colNameWithAcc = T, underscore = F) 
                                  catVarNameAccParenthList = getVarNameByPhvAcc(object, phvAccList = c(catPhvAcc), studyDataDicDF = studyDataDicDF, colNameWithAcc = T, underscore = F) 
                                  varNameAccParenthCombo = paste(numVarNameAccParenthList, collapse="\n") 

                                  # For naming image files
                                  numVarNameAccUscoreList = getVarNameByPhvAcc(object, phvAccList = cleanNumPhvAccList, studyDataDicDF = studyDataDicDF, colNameWithAcc = T, underscore = T) 
                                  catVarNameAccUscoreList = getVarNameByPhvAcc(object, phvAccList = c(catPhvAcc), studyDataDicDF = studyDataDicDF, colNameWithAcc = T, underscore = T) 
                                  UscoreComboList = c(catVarNameAccUscoreList, numVarNameAccUscoreList)
                                  varNameAccUscoreCombo = paste(UscoreComboList, collapse="_") 

                                  xLabCombo = ''
                                  yLabCombo = ''
                                  title = paste(catVarNameAccParenthList[1], "\nvs\n", varNameAccParenthCombo, "\n", sep="")

                                  #############################################
                                  # Flip value orderr of Var2 at y-axis
                                  #############################################
                                  # Plot with current pd, the values in both x- and y-axis are large to small. 
                                  # To make it look better, flip it to small to large
                                  levs <- rev(sort(unique(pd$Var2)))

                                  # corr <-ggplot(data=pd,aes(x=Var1,y=Var2,fill=Correlation,label=Correlation))+geom_raster() +			# not flip 
                                  corr <-ggplot(data=pd,aes(pd$Var1, ordered(pd$Var2, levels= levs), fill=pd$Correlation, label=pd$Correlation)) +	# filp y value order  
                                      geom_raster() +
                                          geom_text() +
                                          theme_bw() +
                                          labs(title='The Raw Plot') +
                                          scale_fill_gradient2(name='Correlation', na.value='white') + # create a diverging color gradient 
                                          ggtitle(title) +
                                              labs(x = yLabCombo, y = xLabCombo) +		# Note: filipped  x, y
                                              #theme(plot.title = element_text(lineheight=.8, face="plain", size=11))
                                              theme(plot.title = element_text(hjust = 0.5, size = 10)) +     # center the title
                                              theme(aspect.ratio=4/4)

                                              ###########################
                                              # Disply variable info
                                              ###########################
                                              getVariableInfoByPhvAcc(object, phvAccList = c(catPhvAcc, cleanNumPhvAccList), showBrief=T)

                                              ############################
                                              # Display and Save plot
                                              ############################
                                              plotObj = corr
                                              plotType = 'correlmatrix'
                                              phvAccNameCombo = varNameAccUscoreCombo 
                                              savedPlotFiles <- saveGapPlot(object, plotObj = plotObj, plotType = plotType, phvAccNameCombo = phvAccNameCombo, saveToDir = saveToDir, showPlot = showPlot)

                                              return(invisible(dat))

                              }
                              else {
                                  type = 'process'
                                  level = 'info'
                                  show = T
                                  mesg = paste("Less than 2 input numerical variables have data. The correlation plot is not drawn.\n", sep="")
                                  writeLog(object,  type = type, level = level, message = mesg, show = show) 
                              }

                          }
                          else {
                              type = 'process'
                              level = 'info'
                              show = T
                              mesg = paste("The input categorical variable, ", catPhvAcc, " (", catVarName, "), has only one unique vaule, '", catUniqueValList[1], "'. The correlation plot is not drawn.\n", sep="")
                              writeLog(object,  type = type, level = level, message = mesg, show = show) 
                          }


                      }
                      else {
                          type = 'process'
                          level = 'info'
                          show = T
                          mesg = paste("The data table of combined input numerical variable data has zero row. The correlation plot is not drawn.\n", sep="")
                          writeLog(object,  type = type, level = level, message = mesg, show = show) 
                      }


                  }

              }
              else {

                  type = 'process'
                  level = 'info'
                  show = T

                  if (length(cleanNumPhvAccList) == 0) {
                      mesg = paste("There is no valid numerical variable accession in the input list.\n", sep="")
                  }
                  else {
                      mesg = paste("The input categorical variable accession is not valid.\n", sep="")
                  }
                  writeLog(object,  type = type, level = level, message = mesg, show = show) 

              }
          })


###############################
# List of functions
###############################

# getStudyVariableInfo
# getPhvAccListByTerms
# getStudyVariableData
# variableSummary
# variableBoxplot
# variableScatterplot
# variableHistogram
# variablePiechart
# variableVenndiagram
# variableCorrelHeatmap


