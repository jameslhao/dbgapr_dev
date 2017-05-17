#######################
# study_internal.R
#######################

# Internal Study class functions that are not to be called by the users.
# ATTN! The documentation rd file of this function is manually created: man/study_internal.rd

# ---------------------------------- 
# Method: viewStudyVariableInfo
# ---------------------------------- 

#' (internal) View study variable meta-info
#'
#' The method displays the meta-info of the variables of either a specified dataset or all variables under the study.
#'
#' @name viewStudyVariableInfo
#' @param object Study class object.
#' @param ... There are optional arguments.
#' @param phtAcc a character string. The dbGaP dataset accession.
#' @param showAs a character string. (optional) When the value is 'table', displays the data as a table through a platform specific table viewer; When it is 'json', displays the json text through a plain text editor; When it is 'text', displays in a brief left-justified text format.
#' @param editor a character string. (optional) The name of your favorite plain text editor. It should be executable from a command-line prompt of the respective platform. For example, notepad (Windows), vim, emacs (Unix), gedit (Ubuntu), nedit (CentOS), etc.
#' @export viewStudyVariableInfo 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' s <- Study(phsAcc = 'phs000001.v3.p1')
#' viewStudyVariableInfo(s, phtAcc='pht000373.v2', showAs='json')
#'}

# s <- Study(phsAcc = 'phs000001.v3.p1')
# viewStudyVariableInfo(s, phtAcc='pht000373.v2', editor='nedit')

setGeneric(
           name = "viewStudyVariableInfo",
           def = function(object, ...) {
               standardGeneric("viewStudyVariableInfo")
           })

#' @describeIn viewStudyVariableInfo A method of class Study 
setMethod(
          f = "viewStudyVariableInfo",
          signature = c("Study"),
          definition = function(object, ..., phtAcc = '', showAs = 'table', editor = '') {

              # Validate accession
              phsAcc = object@phsAcc

              phsAcc = object@phsAcc
              prjDotDir = object@prjDotDir
              prjDir = object@prjDir
              # /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data
              prjDataDir = object@prjDataDir
              configFile = object@configFile
              fileInfoFile = object@fileInfoFile
              fileInfoArchDir = object@fileInfoArchDir

              parseIdsFromStAcc =  parseIdsFromStAcc(object, phsAcc = phsAcc)
              phsAccNoVer = parseIdsFromStAcc$phsAccNoVer

              # Compose the file path 
              varInfoForBrowseFileName = '' 
              varInfoForBrowseFile = '' 

              displayFile = ''
              if (nchar(phtAcc) > 0) {
                  phtAcc <- cleanObjAcc(object, acc = phtAcc, type = 'pht')
                  inputObjPhsAcc <- checkObjStudyByAcc(object, acc = phtAcc)

                  if (phsAcc == inputObjPhsAcc) {

                      if (showAs == 'text') {
                          # /panfs/sandpan1.be-md.ncbi.nlm.nih.gov/homes/hao/dbgapr_proj/dbgapr_user_project4/gapwork/data/phs000001/phs000001.v3/data_dic/brief_text_dump
                          # phs000001.v3_pht000001.v2_data_dic_brief_ljustify.txt
                          varInfoForBrowseFilePath = file.path(prjDataDir, phsAccNoVer, phsAcc, 'data_dic', 'brief_text_dump')
                          varInfoForBrowseFileName = paste(phsAcc, '_', phtAcc, '_data_dic_brief_ljustify.txt', sep = '')
                          varInfoForBrowseFile = file.path(varInfoForBrowseFilePath, varInfoForBrowseFileName) 
                          displayFile = varInfoForBrowseFile 
                          displayTextFile(object, file = displayFile, editor = editor) 
                      }
                      else if (showAs == 'json') {
                          # /panfs/sandpan1.be-md.ncbi.nlm.nih.gov/homes/hao/dbgapr_proj/dbgapr_user_project4/gapwork/data/phs000001/phs000001.v3/data_dic/json_dump
                          # phs000001.v3_pht000001.v2_data_dic.json
                          varInfoForBrowseFilePath = file.path(prjDataDir, phsAccNoVer, phsAcc, 'data_dic', 'json_dump')
                          varInfoForBrowseFileName = paste(phsAcc, '_', phtAcc, '_data_dic.json', sep = '')
                          varInfoForBrowseFile = file.path(varInfoForBrowseFilePath, varInfoForBrowseFileName) 
                          displayFile = varInfoForBrowseFile 
                          displayTextFile(object, file = displayFile, editor = editor) 
                      }
                      else {
                          varInfoForBrowseFilePath = file.path(prjDataDir, phsAccNoVer, phsAcc, 'data_dic', 'json_dump')
                          varInfoForBrowseFileName = paste(phsAcc, '_', phtAcc, '_data_dic.json', sep = '')
                          varInfoForBrowseFile = file.path(varInfoForBrowseFilePath, varInfoForBrowseFileName) 
                          displayFile = varInfoForBrowseFile 
                          displayTable(object, file = displayFile)
                      }
                  }
              }
              else {

                  # /panfs/sandpan1.be-md.ncbi.nlm.nih.gov/homes/hao/dbgapr_proj/dbgapr_user_project4/gapwork/data/phs000001/phs000001.v3/data_dic/combo_dump
                  # phs000001.v3_data_dic_combo_brief_ljustify.txt
                  # phs000001.v3_data_dic_combo.json
                  varInfoForBrowseFilePath = file.path(prjDataDir, phsAccNoVer, phsAcc, 'data_dic', 'combo_dump')

                  displayFile = ''
                  if (showAs == 'text') {
                      varInfoForBrowseFileName = paste(phsAcc, '_data_dic_combo_brief_ljustify.txt', sep = '')
                      varInfoForBrowseFile = file.path(varInfoForBrowseFilePath, varInfoForBrowseFileName) 
                      displayFile = varInfoForBrowseFile 
                      displayTextFile(object, file = displayFile, editor = editor) 
                  }
                  else if (showAs == 'json') {
                      varInfoForBrowseFileName = paste(phsAcc, '_data_dic_combo.json', sep = '')
                      varInfoForBrowseFile = file.path(varInfoForBrowseFilePath, varInfoForBrowseFileName) 
                      displayFile = varInfoForBrowseFile 
                      displayTextFile(object, file = displayFile, editor = editor) 
                  }
                  else {
                      varInfoForBrowseFileName = paste(phsAcc, '_data_dic_combo.json', sep = '')
                      varInfoForBrowseFile = file.path(varInfoForBrowseFilePath, varInfoForBrowseFileName) 
                      displayFile = varInfoForBrowseFile 
                      displayTable(object, file = displayFile)
                  }
              }

          })

# ----------------------------------- 
# Method: getVariableInfoByPhvAcc 
# ----------------------------------- 

#' (internal) Get variable meta-info by accession
#'
#' The method returns the variable meta-info as a data frame given the variable accession.
#'
#' @name getVariableInfoByPhvAcc
#' @param object Study class object.
#' @param phvAccList a character vector. The dbGaP phenotype variable accessions.
#' @param ... There are optional arguments.
#' @param showTable (optional) a logical value. If TRUE, displays the variable meta-info in a platform specific table viewer; Not display if FALSE (default).
#' @param showBrief (optional) a logical value. If TRUE (default), console displays a brief version of the variable info. Not display if FALSE.
#' @param validateInput (optional). If TRUE, checks the input variable accessions to make sure they belong to downloaded data under the user project. No check if FALSE.
#' @return a data frame. (invisible) The meta-info of the input variables.
#' @export getVariableInfoByPhvAcc
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' s <- Study(phsAcc = 'phs000001.v3.p1')
#' accList = c('phv00054119.v1.p1', 'phv00054118.v1.p1', 'phv00000035.v2')
#' getVariableInfoByPhvAcc(s, phvAccList = accList) 
#' getVariableInfoByPhvAcc(s, phvAccList = accList, showBrief=T)
#' }
#
# getVariableInfoByPhvAcc(s, phvAccList = c('phv00054119.v1.p1', 'phv00054118.v1.p1', 'phv00054123.v1', 'phv00000035.v2'))

setGeneric(
           name = "getVariableInfoByPhvAcc",
           def = function(object, phvAccList, ...) {
               standardGeneric("getVariableInfoByPhvAcc")
           })

#' @describeIn getVariableInfoByPhvAcc of class Commons 
setMethod(
          f = "getVariableInfoByPhvAcc",
          signature = c("Study", "character"),
          definition = function(object, phvAccList, ..., showTable = F, showBrief = T, validateInput = T) {

              phsAcc = object@phsAcc
              #extDataDir = object@extDataDir

              ############################
              # Validate phvAcc list
              ############################

              if (length(phvAccList) > 0 ) {

                  # Validation check is slow 
                  if (validateInput == T) {
                      ###########################
                      # Validate PhvAccList
                      ###########################
                      cleanPhvAccList <- checkPhvAccList(object, phvAccList = phvAccList) 
                  }
                  # Skip check to make it run faster (used when calling from a different function where the phvAccList has been done.)
                  else {
                      cleanPhvAccList = lapply(phvAccList, function(x) { a <- cleanObjAcc(object, acc = x, type = 'phv') })
                  }
                  phvAccList = unlist(cleanPhvAccList, recursive=FALSE)

                  if (length(phvAccList) > 0) {

                      ####################
                      # Get DataDic
                      ####################
                      parseIdsFromStAcc =  parseIdsFromStAcc(object, phsAcc = phsAcc)
                      phsAccNoVer = parseIdsFromStAcc$phsAccNoVer
                      dataDicComboDF <- getDataDicByStudy(object, phsAcc) 

                      ########################################
                      # Get all rows that match PhvAccList
                      ########################################
                      phvInfoDF = dataDicComboDF[dataDicComboDF$variable_accession %in% phvAccList,]


                      
                      varCodeValDF <- getExtData(object, type = 'code', phsAcc = phsAcc)

                      #############################
                      # Console Display Json
                      #############################
                      if (showTable == TRUE) {
                          #dfInList = list(phvInfoDF)
                          #saveTempAndDisplayJson(object, dfInList = dfInList, editor=editor)
                          displayTable(object, data = phvInfoDF) 
                      }
                      else {
                          if (showBrief == TRUE) {

                              # Process each row of the phvInfoDF and display in plain-text
                              cat("\n")
                              cat("Variable Info:\n")
                              lapply(split(phvInfoDF,1:nrow(phvInfoDF)), function(rowDF) {

                                         varAcc <- unlist(rowDF$variable_accession)
                                         varName <- unlist(rowDF$name)
                                         varType <- rowDF$calculated_type
                                         varUnit <- rowDF$units
                                         #varCodeVal <- rowDF$code_value_combo
                                         varDesc <- trim(paste(rowDF$description, collapse=""))


                                         ###############################
                                         # Build variable CodeValCombo
                                         ###############################
                                         codeValCombo <- buildVarCodeValCombo(object, catVarAcc = varAcc, varCodeValDF = varCodeValDF)

                                         # Replace '|' with ' | ' for better display
                                         codeValCombo <- gsub("\\|", " \\| ", codeValCombo)

                                         ##########################
                                         # Left justified dsiplay
                                         ##########################

                                         if (is.na(codeValCombo)) {
                                             str1 <- paste(varName, " (", varAcc, ") [", varType, "]  :\n",  sep="")
                                             display <- sprintf("  %2s", c(str1, varDesc)) # left justified
                                             cat(display)
                                         }
                                         else {
                                             str1 <- paste(varName, " (", varAcc, ") [", varType, "]  :\n",  sep="")
                                             display <- sprintf("  %2s", c(str1, varDesc)) # left justified
                                             cat(display)
                                             cat("\n")

                                             # display CodeValCombo
                                             str2 <- paste("code value  :\n", sep="")
                                             display <- sprintf("  %2s", c(str2, codeValCombo)) # left justified
                                             cat(display)
                                         }
                                         cat("\n\n")
})
                          } 
                      }

                      ##### Return Dataframe #####
                      return (invisible(phvInfoDF)) 
                  }
                  else {
                      cat("\n")
                      type = 'process'
                      level = 'error'
                      show = T
                      mesg = paste("The input PhvAccList is empty, possibly due to invalid accession provided. \n", sep="")
                      writeLog(object,  type = type, level = level, message = mesg, show = show) 
                  }
              }
              else {

                  cat("\n")
                  type = 'process'
                  level = 'error'
                  show = T
                  mesg = paste("The input PhvAccList is empty. \n", sep="")
                  writeLog(object,  type = type, level = level, message = mesg, show = show) 
              }
          })


# ----------------------------------------- 
# Method: getStudyVariableInfoByDataType 
# ----------------------------------------- 

#' (internal) Get meta-info of study numerical or categorical variables  
#' 
#' The method returns the variable meta-info of either numerical or categorical variables of the study. 
#'
#' @name getStudyVariableInfoByDataType
#' @param object Study class object.
#' @param dataType a character string. The variable datatype. The possible value is either 'num' (for numerical variable) or 'cat' (for categorical variable).
#' @param ... There are optional arguments.
#' @param dataDicDF a data frame. (optional) Dataset data dictionary data.
#' @return  a data frame. The variable meta-info. 
#' @export getStudyVariableInfoByDataType 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' getStudyVariableInfoByDataType(s, dataType = 'num', dataDicDF = dataDicDF)
#' getStudyVariableInfoByDataType(s, dataType = 'cat', dataDicDF = dataDicDF)
#'}


# s <- Study(phsAcc = 'phs000001.v3.p1')
#
# getStudyVariableInfoByDataType(s, dataType = 'num')
# or
# getStudyVariableInfoByDataType(s, dataType = 'cat')
#
setGeneric(
           name = "getStudyVariableInfoByDataType",
           def = function(object, dataType, ...) {
               standardGeneric("getStudyVariableInfoByDataType")
           })

#' @describeIn getStudyVariableInfoByDataType A method of class Study 
setMethod(
          f = "getStudyVariableInfoByDataType",
          signature = c("Study", "character"),
          definition = function(object, dataType, ..., dataDicDF = data.frame()) {

              phsAcc = object@phsAcc
              extDataDir = object@extDataDir 

              # Compose file path 
              # phs000001.v3.p1_var_report_combo.rds
              #dataDicComboDF <- getDataDicByStudy(object, phsAcc) 

              if (nrow(dataDicDF) == 0) {
                  dataDicDF <- getDataDicByStudy(object, phsAcc) 
              }

              #############################
              # Validate datatype input 
              #############################
              # possible types: integer, decimal, enum_integer, string, unknown
              # type "unknown" should be ignored

              dataType.rex <- "^num"
              dataType.match <- grepl(dataType.rex, dataType, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)

              if (grepl("^num", dataType) == TRUE) {
                  subDF <- subset(dataDicDF, dataDicDF$calculated_type == 'decimal' | dataDicDF$calculated_type =='integer') 
                  return(subDF)
              }
              else if (grepl("^cat", dataType) == TRUE) {
                  subDF <- subset(dataDicDF, dataDicDF$calculated_type == 'enum_integer' | dataDicDF$calculated_type =='string' | dataDicDF$calculated_type == 'enumerated integer') 
                  return(subDF)
              }
              else {
                  cat("\n")
                  type = 'process'
                  level = 'info'
                  show = T
                  mesg = paste("The input dataType ", dataType, " is neither 'num' nor 'cat'. Please re-run the command with the correct value.", "\n", sep="")
                  writeLog(object,  type = type, level = level, message = mesg, show = show) 
              }
          })

# ------------------------- 
# Method: checkPhvAccList 
# ------------------------- 

#' (internal) Validate variable accessions 
#' 
#' The method validates a given list of the dbGaP variable accessions to make sure each of them is in a right format and belongs to the class study. A new list that have invalid accessions removed is returned.
#'
#' @name checkPhvAccList
#' @param object Study class object.
#' @param phvAccList a character vector. A list of the dbGaP variable accessions. 
#' @return a character vector. A validated list of the variable accessions.
#' @export checkPhvAccList 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' s <- Study(phsAcc = 'phs000001.v3.p1')
#' accList = c('phv00054119.v1.p1.c2', 'phv00053735.v2', 'phv00053732.v2')
#' checkPhvAccList(s, phvAccList = accList)
#'}

# s <- Study(phsAcc = 'phs000001.v3.p1')
#
# checkPhvAccList(s, phvAccList = c('phv00054119.v1.p1.c2', 'phv00053735.v2', 'phv00053732.v2'))
#
setGeneric(
           name = "checkPhvAccList",
           def = function(object, phvAccList) {
               standardGeneric("checkPhvAccList")
           })

#' @describeIn checkPhvAccList A method of class Study 
setMethod(
          f = "checkPhvAccList",
          signature = c("Study", "character"),
          definition = function(object, phvAccList) {

              phsAcc = object@phsAcc

              ###########################
              # Get all vairable list
              ###########################
              studyDataDicInfo <- getExtData(object, type = 'variable', phsAcc = phsAcc)
              subDF <- unique(studyDataDicInfo['variable_accession'])
              allVarList <- subDF[['variable_accession']] 

              ##################################################################
              # Fiind out the items in phvAccList but not in allVarList
              ##################################################################
              notInAllList <- setdiff(phvAccList, allVarList) 
              cleanPhvAccList <- setdiff(phvAccList, notInAllList)

              if (length(cleanPhvAccList) > 0 ) {
                  validPhvCombo = paste(cleanPhvAccList, collapse = ", ")
                  errmsg = paste("[INFO] Valid variables: ", validPhvCombo, "\n", sep="")
                  #cat(errmsg)
              }
              else {
                  errmsg = paste("[ERROR] No valid variable found in the input variable accession list. Check to make sure the input variables belong to the class study.", sep="")
                  message(errmsg)
              }
              if (length(notInAllList) > 0) {
                  inValidPhvCombo = paste(notInAllList, collapse = ", ")
                  errmsg = paste("[INFO] Invalid variables: ", inValidPhvCombo, sep="")
                  cat(errmsg)
                  cat("\n")
              }

              return(cleanPhvAccList)
          })


# ---------------------------- 
# Method: checkObjStudyByAcc 
# ---------------------------- 

#' (internal) Get study of a dataset or variable 
#' 
#' The method checks a given dataset or variable accession to see if it belongs to the class study or not.
#'
#' @param object Study class object.
#' @param acc a character string. A dataset or variable accession.
#' @return a character string. (invisible) The study accession to which the input dataset or variable belongs.
#' @export checkObjStudyByAcc 
#' @keywords internal
#' @examples
#' \dontrun{ 
#'
#' s <- Study(phsAcc = 'phs000001.v3.p1')
#' phsAcc <- checkObjStudyByAcc(s, acc = 'pht000370.v2.p1')
#' phsAcc <-checkObjStudyByAcc(s, acc = 'phv00054119.v1.p1')
#'}

# s <- Study(phsAcc = 'phs000001.v3.p1')
# checkObjStudyByAcc(s, acc = 'pht000370.v2.p1')
# or
# checkObjStudyByAcc(s, acc = 'phv00054119.v1.p1')
#
# s <- Study(phsAcc = 'phs000651.v9.p10')
# checkObjStudyByAcc(s, acc = 'pht0003794.v1')

setGeneric(
           name = "checkObjStudyByAcc",
           def = function(object, acc) {
               standardGeneric("checkObjStudyByAcc")
           })

#' @describeIn checkObjStudyByAcc A method of class Study 
setMethod(
          f = "checkObjStudyByAcc",
          signature = c("Study", "character"),
          definition = function(object, acc) {

              phsAcc = object@phsAcc
              prjDataDir = object@prjDataDir

              # Check accession general format
              pattns = stringr::str_match(acc, "^(ph)(\\w)\\d+")
              typePatt = pattns[3]    

              type = ''
              if (nchar(typePatt) > 0 && !is.na(typePatt)) {
                  if (typePatt == 't') {
                      type = 'pht'
                      acc <- cleanObjAcc(object, acc = acc, type = 'pht')
                  }
                  else if (typePatt == 'v') {
                      type = 'phv'
                      acc <- cleanObjAcc(object, acc = acc, type = 'phv')
                  }
                  else {
                      errmsg = paste(
                                     "[ERROR] Only dataset or variable accession should be given. The input accession ", acc, " is either incorrect or in a wrong format.\n", sep=""  
                                     )
                      cat(errmsg)
                  }
              }
              else {
                  errmsg = paste(
                                 "[ERROR] Only dataset or variable accession should be given. The input accession ", acc, " is either incorrect or in a wrong format.\n", sep=""  
                                 )
                  cat(errmsg)
              }

              if (acc != "") {

                  if (type == 'pht' | type == 'phv') {

                      ####################################################
                      # Get match studyAcc of the input acc (pht or phv)
                      ####################################################
                      # New!
                      studyDataDicInfo <- getExtData(object, type = 'variable', phsAcc = phsAcc)

                      # Dataset
                      if (type == 'pht') {
                          matchInfoDF <- subset(studyDataDicInfo, studyDataDicInfo$study_accession== phsAcc & studyDataDicInfo$dataset_accession == acc, select = c('study_accession')) 
                      }
                      # Variable
                      else {
                          matchInfoDF <- subset(studyDataDicInfo, studyDataDicInfo$study_accession == phsAcc & studyDataDicInfo$variable_accession == acc, select = c('study_accession'))
                      }
                      # Get unique value 
                      matchStudyAcc <- toString(matchInfoDF[!duplicated(matchInfoDF), ])

                      if (nchar(matchStudyAcc) == 0) {
                          type = 'process'
                          level = 'error'
                          show = T
                          mesg = paste("The input accession ", acc, " does not belong to the class study ", phsAcc, sep="")
                          writeLog(object,  type = type, level = level, message = mesg, show = show) 
                      }
                      else {
                          return(invisible(matchStudyAcc))
                      }

                  } # end type ==
              } # end acc != ''

          })

# -------------------------------- 
# Method: getDatasetDataByPhtAcc 
# -------------------------------- 

#' (internal) Get data by dataset accession
#'
#' The method returns the data of the given dataset. For large dataset, it may take quite a while to retrieve the data. 
#'
#' @param object Study class object.
#' @param phtAcc a character string. The dbGaP phenotype dataset accession.
#' @param ... There are optional arguments.
#' @param subjIdsOrFile a character vector or a character string. (optional) This argument is either list of subject ids (dbGaP_Subject_ID) shared among the input variables or the path to a file that contains a list of the subject ids shared among the input variables. The file is a plain text file with one dbGaP_Subject_ID per line.
#' @param colNameWithAcc logical value. (optional). If TRUE, includes the variable accessions in the column names (e.g. AGEPHOT_phv00000027.v2); If FALSE, not include (e.g. AGEPHOT).
#' @return a data frame. Data of the dataset. 
#' @export getDatasetDataByPhtAcc 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' s <- Study(phsAcc = 'phs000001.v3.p1')
#' getDatasetDataByPhtAcc(s, phtAcc = 'pht000370.v2.p1')
#' # or
#' getDatasetDataByPhtAcc(s, phtAcc = 'pht000370.v2.p1', colNameWithAcc = TRUE)
#'}

# s <- Study(phsAcc = 'phs000001.v3.p1')
# getDatasetDataByPhtAcc(s, phtAcc = 'pht000370.v2.p1')
# getDatasetDataByPhtAcc(s, phtAcc = 'pht000371.v2')    # belongs phs000001.v3.p1
# getDatasetDataByPhtAcc(s, phtAcc = 'pht000371.v2', subjIdsOrFile = c("219", "220", "221"))

#
# s <- Study(phsAcc = 'phs000651.v7')
# getDatasetDataByPhtAcc(s, phtAcc = 'pht003525.v4') 	
# getDatasetDataByPhtAcc(s, phtAcc = 'pht003794.v1') 	
#
# s <- Study(phsAcc = 'phs000680.v1.p1')
# getDatasetDataByPhtAcc(s, phtAcc = 'pht003719.v1.p1') 	# contains categorical variables (SEASON, CASAVA_VERSIO)
# getDatasetDataByPhtAcc(s, phtAcc = 'pht003718.v1.p1') 	# contains numeric (age) and categorical (sex) variables   
#
# Dataset of phs000007.v29, an example of large dataset, takes too long to finish
# getDatasetDataByPhtAcc(s, phtAcc = 'pht000009.v2') 	# phs000007.v29 

# phs000572.v2.
# df <- getStudyVariableData(s4, phtAcc="pht003454.v4", colNameWithAcc=F)

# phs000001.v3
# df <- getDatasetDataByPhtAcc(s3, phtAcc = 'pht000371.v2', colNameWithAcc=F)

# phs000429.v1
# df <- getDatasetDataByPhtAcc(s2, phtAcc = 'pht002481.v1', colNameWithAcc=F)
setGeneric(
           name = "getDatasetDataByPhtAcc",
           def = function(object, phtAcc, ...) {
               standardGeneric("getDatasetDataByPhtAcc")
           })

#' @describeIn getDatasetDataByPhtAcc A method of class Study 
setMethod(
          f = "getDatasetDataByPhtAcc",
          signature = c("Study", "character"),
          definition = function(object, phtAcc, ..., subjIdsOrFile = NULL, colNameWithAcc = FALSE) {

              phsAcc = object@phsAcc
              prjDataDir = object@prjDataDir

              # Validate accession
              phtAcc <- cleanObjAcc(object, acc = phtAcc, type = 'pht')


              # Get data-dic info
              studyDataDicDF <- getDataDicByStudy(object, phsAcc) 

              if (phtAcc != "") {

                  inputObjPhsAcc <- checkObjStudyByAcc(object, acc = phtAcc)

                  if (!is.null(inputObjPhsAcc)) {

                      if (phsAcc == inputObjPhsAcc) {


                          phsAcc = object@phsAcc
                          prjDir = object@prjDir
                          prjDataDir = object@prjDataDir
                          studyDir = file.path(prjDir, "gapwork", "data", phsAcc)
                          prjDotDir = object@prjDotDir
                          fileInfoFile = object@fileInfoFile
                          extDataDir = object@extDataDir 

                          # Get Ids from accesssion 
                          parseIdsFromStAcc =  parseIdsFromStAcc(object, phsAcc = phsAcc)
                          phsAccNoVer = parseIdsFromStAcc$phsAccNoVer
                          phtAccIds = parseIdsFromPhtAcc(object, phtAcc = phtAcc)
                          phtAccId <- phtAccIds$phtAccId
                          phtAccVer <- phtAccIds$phtAccVer

                          ######################
                          # Get sharedIdVar
                          ######################
                          #extPhenoSharedIdNamesFile = object@extPhenoSharedIdNamesFile 
                          #phenoSharedIdNames <- read.table(extPhenoSharedIdNamesFile, header = T, fill = TRUE, quote = "", sep ='\t', stringsAsFactors = FALSE, encoding="UTF-8") 

                          # Get sharedVariableName
                          #specialVarDF <- phenoSharedIdNames   # ATTN!! phenoSharedIdNames rda under the data/ directory


                          # ExtData sharedIdNames
                          # New!
                          specialVarDF <- getExtData(object, type = 'id', phsAcc = phsAcc)

                          sharedVarSet <- subset(specialVarDF, specialVarDF$dataset_id == phtAccId & specialVarDF$dataset_version == phtAccVer & specialVarDF$subject_or_sample == 'subject') 

                          sharedVarSubset = head(sharedVarSet, 1)
                          #specialVarName = subset(sharedVarSubset, select = c("variable_name"))$variable_name
                          #specialVarId = subset(sharedVarSubset, select = c("variable_id"))$variable_id
                          specialVarName = sharedVarSubset[["variable_name"]]
                          specialVarId = sharedVarSubset[["variable_id"]]


                          ###############################################
                          # Compose path to study pht combo info json 
                          ###############################################
                          # Study pht info file
                          # /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/phs000651/phs000651.v7/combined/log/phs000651.v7_study_pht_combo_info.json
                          phtComboInfoFileName = paste(phsAcc, "_study_pht_combo_info.json", sep="")
                          phtComboInfoFile = file.path(prjDataDir, phsAccNoVer, phsAcc, 'combined', 'log', phtComboInfoFileName) 

                          if (file.exists(phtComboInfoFile)) {

                              phtComboInfoDF <- fromJSON(phtComboInfoFile, flatten=TRUE)
                              matchPhtComoInfoDF = subset(phtComboInfoDF, phtComboInfoDF$fPhtAcc == phtAcc)

                              if (nrow(matchPhtComoInfoDF) > 0) {
                                  matchPathToComboFile = toString(matchPhtComoInfoDF$pathToFile)		# needs to convert to string

                                  if (file.exists(matchPathToComboFile)) {

                                      phtComboDataDF <- read.table(matchPathToComboFile, header = T, fill = TRUE, sep ='\t', stringsAsFactors = FALSE, encoding="UTF-8") 
                                      ######################
                                      # Rename ID columns
                                      ######################
                                      firstColName = colnames(phtComboDataDF)[1]

                                      # Test if the 1st column is not dbGaP subject id such as pht002481.v1 of phs000429.v1
                                      matchGapSubjId <- grepl("dbgap.*subj.*$", firstColName, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)

                                      ###### Deal with the file that has dbgap_sample_id as the 1st column ######
                                      # such as: phs000651.v7_pht003525.v4_union_c1-c2.txt 
                                      # The 1st two columns are "dbGaP_Sample_ID" "SAMPID"

                                      ###### Check 1st column name to see if it is a sample-id column #####
                                      matchColName <- grep("subj", firstColName ,ignore.case=TRUE,value=TRUE)

                                      if(identical(matchColName, character(0))) {
                                          cat("\n")
                                          type = 'process'
                                          level = 'warn'
                                          show = F
                                          mesg = paste("The 1st column of the dataset file is not dbgap_subject_id column ", phtAcc, "\n", sep="")
                                          writeLog(object,  type = type, level = level, message = mesg, show = show) 
                                      }
                                      else {

                                          #################################
                                          # ATTN! Special case 
                                          #################################
                                          # When 1st column is not dbGaP subject id such as pht002481.v1 of phs000429.v1
                                          firstColName = colnames(phtComboDataDF)[1]

                                          finalVarDF <- data.frame()

                                          # If dbGaP and submitted subj id columns both exist
                                          if (matchGapSubjId) {

                                              # Note: When 1st col is dbGaP subject id column, the submitted id column may not be the 2nd col. 
                                              # It could be somewhere in the middle.

                                              # Set aprt of first, specialVar, and rest columns
                                              firstCol = subset(phtComboDataDF, select=c(firstColName))
                                              specialVarCol = subset(phtComboDataDF, select=c(specialVarName))
                                              restColDF <- phtComboDataDF[, -c(1:1)] # delete columns 1 through 1

                                              # Move existing specialVarCol in restColDF to first position 
                                              # Example: pht003554.v4 of phs000572.v7
                                              # "SUBJID" "FamID" "Sex" "AD" ... "Race" "Ethnicity" "dataset_consent" 
                                              df <- restColDF
                                              specialVarAndRestColDF <- df[,c(which(colnames(df)==specialVarName),which(colnames(df)!=specialVarName))]


                                              # Append Submitted_Subject_ID col to it
                                              # Example:
                                              # "Submitted_Subject_ID" "SUBJID" "FamID" ... "Race" "Ethnicity" "dataset_consent"
                                              specialVarAndRestColDF['Submitted_Subject_ID'] <-specialVarCol 
                                              # Move it to first position
                                              df <- specialVarAndRestColDF 
                                              specialVarAndRestColDF <- df[,c(which(colnames(df)=="Submitted_Subject_ID"),which(colnames(df)!="Submitted_Subject_ID"))]


                                              # Further append the firstCol and rename it to "dbGaP_Subject_ID"
                                              # "dbGaP_Subject_ID" "Submitted_Subject_ID" "SUBJID" "FamID" "Sex" ..."Race" "Ethnicity" "dataset_consent"
                                              specialVarAndRestColDF['dbGaP_Subject_ID'] <- firstCol 
                                              df <- specialVarAndRestColDF 
                                              finalVarDF <- df[,c(which(colnames(df)=="dbGaP_Subject_ID"),which(colnames(df)!="dbGaP_Subject_ID"))]


                                              # Final output example: ( pht000371.v2, phs000001.v3)
                                              # dbGaP_Subject_ID Submitted_Subject_ID  ID2 HASGENSP AMDSTAT CATARACT COR LPSCBASE
                                              # 1            52465                 G380 G380        Y      12        9   2        0
                                              # 2            52618                 G543 G543        Y       6        1   0        0

                                          }
                                          # If only submitted subj id column exists
                                          else {
                                              firstCol = subset(phtComboDataDF, select=c(firstColName))

                                              # Append Submitted_Subject_ID col to the comboDF 
                                              phtComboDataDF['Submitted_Subject_ID'] <-firstCol 
                                              # Move it to first position
                                              df <- phtComboDataDF 
                                              finalVarDF <- df[,c(which(colnames(df)=="Submitted_Subject_ID"),which(colnames(df)!="Submitted_Subject_ID"))]

                                              # Final output example: (pht002481.v1 of phs000429.v1)
                                              # Submitted_Subject_ID SUBJID case_control SEX SCHOOL ENROLLAGE WHITE axisrr axisrl
                                              # 1                 G004   G004            0   2      5        61     1      0    165
                                              # 2                 G005   G005            0   2      1        70     1     90     85

                                          }

                                          # Continue only if fltered data.frame is not empty


                                          if (nrow(finalVarDF) > 0) {

                                              #####################################
                                              # Make column name a NameAcc Combo
                                              # By further processing finalVarDF
                                              #####################################

                                              ##########################
                                              # Get Variable-Meta Info 
                                              ##########################
                                              studyDataDicInfo <- getExtData(object, type = 'variable', phsAcc = phsAcc)

                                              if (nrow(studyDataDicInfo) > 0) {

                                                  ################################################################
                                                  # Parse out phvAccList from info-header of Indiv dataset file
                                                  ################################################################
                                                  fileInfoDF <- fromJSON(fileInfoFile, flatten=TRUE)
                                                  thisFileInfoDF <- dplyr::filter(fileInfoDF, fileInfoDF$fPhtAcc == phtAcc & fileInfoDF$consentType == 'Indiv')
                                                  pathToFile = thisFileInfoDF$pathToFile[1]
                                                  multiType = 'Not'
                                                  headerInfoSets = parsePhtHeaderFromFile(object, phtFile = pathToFile, phtFileType = multiType) 
                                                  fieldPhvs = headerInfoSets[1]

                                                  # Remove p#.c# of each item in the list 
                                                  # phv00195355.v4.p4.c1 to phv00195355.v4
                                                  cleanFieldPhvs <- sapply(fieldPhvs, function(x) gsub("\\.p\\d+.*$", "", x), simplify=F)

                                                  #######################################################################
                                                  # Move specialVarPhv to the front of the list of cleanFieldPhvs
                                                  #######################################################################
                                                  # Note: The speicalVar column in a dataset (pht) table may not be the first column
                                                  # after the ID columns. 

                                                  # Get vector item match specialVarId
                                                  # Return logical vector
                                                  # [1] FALSE FALSE FALSE FALSE  TRUE FALSE ...
                                                  pattn = paste0("phv0*", specialVarId)
                                                  logicPhvs <- sapply(cleanFieldPhvs, function(x) grepl(pattn, x), simplify=F)[[1]]
                                                  # Get TRUE index of the logical vector 
                                                  svIndex <- which(ll <- logicPhvs) 
                                                  # Example: phv00053742.v2  (ID2)
                                                  svPhvAcc <- cleanFieldPhvs[[1]][svIndex]

                                                  # Get specialVar item only
                                                  x <-  cleanFieldPhvs[[1]]
                                                  # Remove specialVar item
                                                  x<-x[-which(x==svPhvAcc)]
                                                  # Combine two vector and now speicalVarPhvAcc is the 1st item
                                                  finalFieldPhvs <- c(svPhvAcc, x)


                                                  ################################
                                                  # Compose varNameAccCombo list
                                                  ################################
                                                  # Important! To speed up, make sure checkList is FALSE.
                                                  varNameAccComboList <- getVarNameAccCombos(object, phvAccList = finalFieldPhvs, studyDataDicDF = studyDataDicDF, checkList = FALSE)

                                                  # Display column names as combo
                                                  newColName = vector()

                                                  # Note: newVarNameList already has specialVarName, so only additional fileds needed are
                                                  # 'dbGaP_Subject_ID', 'Submitted_Subject_ID' at the begining, and "dataset_consent" at the end.
                                                  if (colNameWithAcc == T) {

                                                      if(matchGapSubjId) {

                                                          # Add starting id columns and ending dataset_consent columns 
                                                          newColNames = c('dbGaP_Subject_ID', 'Submitted_Subject_ID', varNameAccComboList, "dataset_consent")
                                                          #newColNames = c('dbGaP_Subject_ID', 'Submitted_Subject_ID', varNameAccComboList, "dataset_consent")

                                                      }
                                                      else {
                                                          #newColNames = c('Submitted_Subject_ID', varNameAccComboList, "dataset_consent")
                                                          newColNames = c('Submitted_Subject_ID', varNameAccComboList, "dataset_consent")
                                                      }
                                                      colnames(finalVarDF) <- newColNames

                                                  }
                                                  else {
                                                      # Strip Acc from varNameAccCombo and add .num extension for duplicate items
                                                      # From 
                                                      # 'AGEPHOT_phv00000027.v2', 'LNUCSCORE_phv00053747.v2', 'LNUCSCORE_phv00000006.v2', 'DIABAGE_phv00054122.v1'
                                                      # to
                                                      # "AGEPHOT"     "LNUCSCORE"   "LNUCSCORE.1" "DIABAGE"
                                                      newVarNameList <- stripColNameAcc(object, colNameAccList = unlist(varNameAccComboList)) 

                                                      if(matchGapSubjId) {
                                                          newColNames = c('dbGaP_Subject_ID', 'Submitted_Subject_ID', newVarNameList, "dataset_consent")
                                                      }
                                                      else {
                                                          newColNames = c('Submitted_Subject_ID', newVarNameList, "dataset_consent")
                                                      }
                                                      colnames(finalVarDF) <- newColNames

                                                  }

                                                  return(finalVarDF)

                                              }

                                          } # end colNameWithAcc == TRUE 

                                          return(finalVarDF)


                                      } # end of identical(matchColName)

                                  }
                                  else {
                                      cat("\n")
                                      type = 'process'
                                      level = 'error'
                                      show = T
                                      mesg = paste("The dataset data combo file is not found. Check ?searchMergeDatasetConsents to see how to create it. --- ", matchPathToComboFile, "\n", sep="")
                                      writeLog(object,  type = type, level = level, message = mesg, show = show) 
                                  }

                              }
                              else {
                                  cat("\n")
                                  type = 'process'
                                  level = 'error'
                                  show = T
                                  mesg = paste("The Study class is instantiated with the study accsssion ", phsAcc, ". The input dataset accession ", phtAcc, " however does not match any datasets available for the study.\n", sep="")
                                  writeLog(object,  type = type, level = level, message = mesg, show = show) 
                              }
                          }
                          else {
                              cat("\n")
                              type = 'process'
                              level = 'error'
                              show = T
                              mesg = paste("The dataset data combo info file is not found. Check ?searchMergeDatasetConsents to see how to create it. --- ", phtComboInfoFile, "\n", sep="")
                              writeLog(object,  type = type, level = level, message = mesg, show = show) 
                          }

                      } # end ObjPhsAcc == phsAcc
                  }

              } # end phtAcc != ""

          })

# -----------------------------------
# Method: getVariableDataByPhvAcc
# -----------------------------------

#' (internal) Get variable data by accession 
#' 
#' The method returns the data frame of merged data of given dbGaP variable accessions. The variables need to belong to the same study.
#'
#' @param object Study class object.
#' @param phvAccList a character vector. The dbGaP variable accessions.
#' @param ... There are optional arguments.
#' @param emptyToNa a logical value. (optional). If TRUE, converts the empty values to NA; If FALSE (default), not convert.
#' @param colNameWithAcc a logical value. (optional). If TRUE, includes the variable accessions in the column names (e.g. AGEPHOT_phv00000027.v2); If FALSE, not include (e.g. AGEPHOT).
#' @param checkList a logical value. (optional). If TRUE, validate the input variable accessions. Skip the check if FALSE. 
#' @return  a data frame. Merged data of input variables.
#' @export getVariableDataByPhvAcc 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' s <- Study(phsAcc = 'phs000001.v3.p1')
#' var_list <- c('phv00054119.v1.p1.c2', 'phv00054118.v1.p1', 'phv00053733.v2')
#' getVariableDataByPhvAcc(s, phvAccList = var_list)
#'}

# s <- Study(phsAcc = 'phs000001.v3.p1')
# getVariableDataByPhvAcc(s, phvAccList = c('phv00054119.v1.p1.c2', 'phv00054118.v1.p1', 'phv00053733.v2', 'phv00053735.v2', 'phv00053732.v2'))
#
# getVariableDataByPhvAcc(s, phvAccList = c('phv00054119.v1.p1.c2', 'phv00053733.v2'))

# getVariableDataByPhvAcc(s, phvAccList = c('phv00000027.v2', 'phv00053747.v2', 'phv00000006.v2', 'phv00054122.v1'))
#
# getVariableDataByPhvAcc(s, phvAccList = c('phv00000027.v2', 'phv00053747.v2', 'phv00000006.v2', 'phv00054122.v1'), colNameWithAcc=T)   # colNamIsAcc is True (there are duplicated column name)
#
# df <- getStudyVariableData(s, phvAccList=c('phv00000622.v1', 'phv00000674.v1'), colNameWithAcc=TRUE)   # phs000007.v29
#
# phs000429.v1  (no dbGaP subject id column)
# df <- getStudyVariableData(s2, phvAccList = c("phv00164691.v1", "phv00164704.v1", "phv00164700.v1")) # phs000429.v1

setGeneric(
           name = "getVariableDataByPhvAcc",
           def = function(object, phvAccList, ...) {
               standardGeneric("getVariableDataByPhvAcc")
           })

#' @describeIn getVariableDataByPhvAcc returns a dataframe of the merged variable data.  
setMethod(
          f = "getVariableDataByPhvAcc",
          signature = c("Study", "character"),
          definition = function(object, phvAccList, ..., emptyToNa = FALSE, colNameWithAcc = FALSE, checkList = T) {

              # Allow not list and vector as input. Convert to vector of it is a list.
              if (is.list(phvAccList)) {
                  phvAccList = unlist(phvAccList, use.names=FALSE)
              }

              ###########################
              # Validate PhvAccList
              ###########################
              cleanPhvAccList = vector()
              if (checkList) {
                  cleanPhvAccList <- checkPhvAccList(object, phvAccList = phvAccList) 
              }
              else {
                  cleanPhvAccList <- phvAccList 
              }

              if (length(cleanPhvAccList) > 0) {

                  phsAcc = object@phsAcc
                  prjDataDir = object@prjDataDir
                  #fileInfoFile = object@fileInfoFile

                  # ExtData sharedIdNames
                  # New!
                  specialVarDF <- getExtData(object, type = 'id', phsAcc = phsAcc)

                  # Get StudyDataDicDF
                  parseIdsFromStAcc =  parseIdsFromStAcc(object, phsAcc = phsAcc)
                  phsAccNoVer = parseIdsFromStAcc$phsAccNoVer
                  studyDataDicDF <- getDataDicByStudy(object, phsAcc) 

                  ##############################
                  # Loop through each phvAcc 
                  ##############################
                  # to get respective dataset files and read merge the files 
                  mergePhtFile <- function(phvAcc, phsAcc, dataDicDF, specialVarName){

                      #inputObjPhsAcc <- checkObjStudyByAcc(object, acc = phvAcc)
                      inputObjPhsAcc <- phsAcc 

                      ###########################################
                      # Look for matching phtAcc and phsAcc 
                      ###########################################
                      # Note: This could return multiple rows of different dataset_versions
                      matchVarDF <- subset(dataDicDF,  dataDicDF$variable_accession==phvAcc) 

                      #######################
                      # Get phtAcc
                      #######################
                      # ATTN! Multiple rows may be returned due to multiple dataset version for a variable.
                      # Randomly select the first phtAcc and use it to find out the avaiable one
                      availPhtAcc = ""
                      if (nrow(matchVarDF) > 1) {
                          randPhtAcc <- matchVarDF$dataset_accession[1]
                          # Get avaiable phtAcc
                          availPhtAcc <- getAvailPhtVer(object, randPhtAcc=randPhtAcc, phsAcc=phsAcc)
                      }
                      else {
                          availPhtAcc <- toString(matchVarDF$dataset_accession[1])
                      }

                      ###########################
                      # Process column names  
                      ############################
                      if (!is.null(availPhtAcc)) {

                          # Rest matchVarDF to the available dataset_version
                          matchVarDF <- subset(matchVarDF,  matchVarDF$dataset_accession==availPhtAcc) 
                          matchPhtAcc <- availPhtAcc 
                          matchStAcc <- phsAcc

                          matchVarName <- matchVarDF$name
                          matchVarType <- matchVarDF$calculated_type

                          # Get specialVarName given phtAcc, studyAcc
                          # Parse ids from phsAcc
                          parseIdsFromStAcc =  parseIdsFromStAcc(object, phsAcc = matchStAcc)
                          matchStAccNoVer = parseIdsFromStAcc$phsAccNoVer
                          matchStId = parseIdsFromStAcc$phsAccId
                          matchStVer = parseIdsFromStAcc$phsAccVer

                          # Parse ids from phtAcc
                          phtAccIds = parseIdsFromPhtAcc(object, phtAcc = matchPhtAcc)
                          matchPhtId = phtAccIds$phtAccId
                          matchPhtVer = phtAccIds$phtAccVer

                          # Compose path to pht file
                          # Example location of pht info file:
                          # /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/phs000001/phs000001.v3.p1/combined/log/phs000001.v3.p1_study_pht_combo_info.json


                          ###########################################
                          # Get Pht Data from Consent-Combined file
                          ###########################################
                          phtInfoFileName = paste(matchStAcc, '_study_pht_combo_info.json', sep="")
                          phtInfoFile = file.path(prjDataDir, matchStAccNoVer, matchStAcc, 'combined', 'log', phtInfoFileName) 


                          phtInfoFileDF <- fromJSON(phtInfoFile, flatten=TRUE)
                          fileInfoMatchDF <- subset(phtInfoFileDF, phtInfoFileDF$fPhtAccNoP==matchPhtAcc) 
                          pathToFile = unlist(fileInfoMatchDF$pathToFile)

                          ##########################################
                          # Decide whether convert empty to na
                          ##########################################
                          phtDataDF = ''

                          if (emptyToNa == T) {
                              ##########################
                              # ATTN!!! Empty --> NA
                              ##########################
                              # ATTN!!! Read data with empty space value converted to NA

                              ###### The combined csv data file is not .gz file, so read.csv works #####
                              phtDataDF <- read.csv(file=pathToFile, header=TRUE, sep="\t", encoding="UTF-8", stringsAsFactors=FALSE, na.strings=c(""," ","NA"))
                          }
                          else {
                              phtDataDF <- read.csv(file=pathToFile, header=TRUE, sep="\t", encoding="UTF-8", stringsAsFactors=FALSE)
                          }

                          firstColName = colnames(phtDataDF)[1] 

                          ###### Check 1st column name to make sure it is a subj-id column #####
                          subjIdColName <- grep("subj", firstColName ,ignore.case=TRUE,value=TRUE)


                          # Report error if the 1st column does not contain 'subj'
                          # No match
                          if (length(subjIdColName) == 0) {
                              type = 'process'
                              level = 'error'
                              show = F
                              mesg = paste("The dataset first column name does not match string 'subj'. PhtAcc: ", matchPhtAcc, " FilePath: ", pathToFile, sep="")
                              writeLog(object,  type = type, level = level, message = mesg, show = show) 

                              return()
                          }
                          else {

                              # subset subjId and this variable columns
                              # such as colnames: "SUBJID"   "recylnum" 

                              #######################
                              # ATTN! Speical case
                              #######################
                              # Note: input phv could be itself a SubjID variable

                              if (firstColName != matchVarName) {

                                  if (firstColName != specialVarName) {
                                      subjIdThisVarDF = subset(phtDataDF, select = c(firstColName, specialVarName, matchVarName))  
                                  }
                                  else {
                                      subjIdThisVarDF = subset(phtDataDF, select = c(firstColName, matchVarName))  
                                  }
                              }
                              # When firstColName is matchVarName
                              else {
                                  subjIdThisVarDF = subset(phtDataDF, select = c(firstColName))  
                              }

                              return (subjIdThisVarDF)
                          }
                      }
                      else {
                          type = 'process'
                          level = 'error'
                          show = F
                          mesg = paste("The input phvAcc ", phvAcc, " doesn't belong the defined study ", phsAcc, sep="")
                          writeLog(object,  type = type, level = level, message = mesg, show = show) 

                          return(NULL)
                      }

                  } # end of mergePhtFile

                  ####################################
                  # Get root_study given this phsAcc 
                  ####################################
                  # ATTN! The specialIdName table is only available from parent study e.g phs000007 but not phs000282 or phs000401
                  #extStudyInfoFile = object@extStudyInfoFile
                  #rootAndThisStudyInfo <- read.table(extStudyInfoFile, header = T, fill = TRUE, quote = "", sep ='\t', stringsAsFactors = FALSE, encoding="UTF-8") 

                  # New!
                  allStudyInfo <- getExtData(object, type = 'study')
                  thisStudyInfo <- subset(allStudyInfo, allStudyInfo$this_study_accession==phsAcc) 
                  rootStAcc = thisStudyInfo$root_study_accession
                  thisStudyId = thisStudyInfo$this_study_id
                  thisStudyVer = thisStudyInfo$this_study_version

                  ######################################
                  # Get SpecialVarName (sharedId) 
                  ######################################
                  parseIdsFromRootStAcc =  parseIdsFromStAcc(object, phsAcc = rootStAcc)
                  rootStAccNoVer = parseIdsFromRootStAcc$phsAccNoVer
                  rootStId = parseIdsFromRootStAcc$phsAccId
                  rootStVer = parseIdsFromRootStAcc$phsAccVer
                  # Get match SpecialVarName
                  specialVarDF <- getExtData(object, type = 'id', phsAcc = phsAcc)

                  # ATTN: the subject_or_smaple needs to be 'subject'
                  matchspecialVarDF <- dplyr::filter(specialVarDF, specialVarDF$study_id==thisStudyId & specialVarDF$study_version==thisStudyVer & specialVarDF$subject_or_sample =='subject') 

                  # Choose the 1st row from the multiple rows returned in matchspecialVarDF 
                  # Since they are all should be the same
                  specialVarName = head(matchspecialVarDF, 1)$variable_name

                  ###############################################
                  # Calling mergePhtFile
                  ###############################################
                  # Return a list of data frames
                  retList <- lapply(cleanPhvAccList, FUN=function(x) mergePhtFile(phvAcc = x, phsAcc = phsAcc, dataDicDF = studyDataDicDF, specialVarName = specialVarName)) 

                  #################################################


                  ##########################################################
                  # Merge all dataframes in the list by dbGaP_Subject_ID 
                  ##########################################################
                  # Reduce() func repeats the mergeTwo process 

                  # S3 function
                  mergeTwo <- function(x, y) {

                      ##############################################
                      # Decide to merge with compound ids or not
                      ##############################################
                      # This is necessary to avoid duplicated second column after merge. 
                      firstColName = colnames(x)[1]
                      matchGapSubjId <- grepl("dbgap.*subj.*$", firstColName, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
                      # If dbGaP and submitted subj id columns both exist
                      # merge by both columns
                      if (matchGapSubjId) {

                          #########################################################
                          # Note: SpecialVar col may not be the second column
                          #########################################################
                          #secondColName = colnames(x)[2]
                          merge(x, y, all=TRUE, by=c(firstColName, specialVarName))
                      }
                      # If submitted subj id column is the 1st (and only) id column 
                      # merge by both 1st column 
                      else {
                          merge(x, y, all=TRUE, by=c(firstColName))
                      }
                  }

                  # Remove null from the list
                  retListNoNull <- retList[!sapply(retList, is.null)]

                  if (length(retListNoNull) > 0) {

                      # Reduce: applies mergeTwo func to each item of retList
                      mergedVarDF <- Reduce(mergeTwo, retList)


                      #######################
                      # ATTN! Speical case
                      #######################
                      # ATTN: Sometimes the pht data file has dbGaP_Subject_ID column missing and 
                      # starts with SpecialVarName. 
                      # In this case, add a Submitted_Subject_ID as the 1st column. 
                      #
                      # Example: for phs000429.v1 dataset data file phs000429.v1.pht002481.v1.p1.c1.areds_data_final_11.EDO.txt.gz,
                      # The 1st column is not dbGaP_Subject_Id, instead it starts with the speicialVarName
                      # SUBJID case_control SEX SCHOOL ENROLLAGE WHITE axisrr axisrl resphnum lesphnum recylnum lecylnum RE_ERR_R LE_ERR_R AVG_ERR_R

                      ### Check if the 1st column is a dbGaP_Subject_Id column
                      firstColName = colnames(mergedVarDF)[1]
                      matchGapSubjId <- grepl("dbgap.*subj.*$", firstColName, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)

                      finalVarDF = data.frame() 
                      # Normal case
                      if(matchGapSubjId) {
                          # Subset dbgap_subj_id and special_id 
                          #gapIdSpecialVarIdDF = subset(mergedVarDF, select = c(colnames(datasetDataDF)[1], specialVarName))

                          ######################################
                          # ATTN! Normalize the Id column names
                          ######################################
                          # Rename the 1st and 2nd columns

                          # Example mergedVarDF at this point
                          # "dbGaP.SubjID" "ID2" "AGEPHOT" "LNUCSCORE.x" "LNUCSCORE.y"  "DIABAGE"

                          # The goal is to make it (duplicated submited subject id one with normalized name and another the original name)
                          # "dbGaP_Subject_ID" "Submitted_Sujcect_ID" "ID2" "AGEPHOT" "LNUCSCORE.x" "LNUCSCORE.y"  "DIABAGE"

                          # Break 1st, specialVarName, and rest of columns apart 
                          firstColName = colnames(mergedVarDF)[1]
                          secondColName = colnames(mergedVarDF)[2]

                          firstCol = subset(mergedVarDF, select=c(firstColName))
                          secondCol = subset(mergedVarDF, select=c(secondColName))
                          restColDF <- mergedVarDF[, -c(1:1)] # delete columns 1 through 1

                          # For restColDF, append a new "Submitted_Subject_ID" column that is a duplicate of the second (specialVarName) column
                          restColDF['Submitted_Subject_ID'] <- secondCol 
                          # Move the new column from the last to first
                          df <- restColDF
                          secondAndRestColDF <- df[,c(which(colnames(df)=="Submitted_Subject_ID"),which(colnames(df)!="Submitted_Subject_ID"))]

                          # Further append the firstCol and rename it to "dbGaP_Subject_ID"
                          secondAndRestColDF['dbGaP_Subject_ID'] <- firstCol 
                          # Move the new column from the last to first
                          df <- secondAndRestColDF 
                          finalVarDF <- df[,c(which(colnames(df)=="dbGaP_Subject_ID"),which(colnames(df)!="dbGaP_Subject_ID"))]

                      }
                      # Speical case where 1st column is submitted subject id
                      else {
                          ######################################
                          # ATTN! Normalize the Id column names
                          ######################################
                          # Append a new column that is duplicate of the first column (it should be specialVarName) but named as 'Submitted_Subject_ID' 
                          mergedVarDF["Submitted_Subject_ID"] <- mergedVarDF[[firstColName]] 

                          # Move the new column from the last to first
                          df <- mergedVarDF
                          finalVarDF <- df[,c(which(colnames(df)=="Submitted_Subject_ID"),which(colnames(df)!="Submitted_Subject_ID"))]
                      }


                      ######################################
                      # Include phvAcc in the column names
                      ######################################
                      # This is necessary to avoid the different phv with identical name. For example: 
                      # phvAccList = c('phv00000027.v2', 'phv00053747.v2', 'phv00000006.v2', 'phv00054122.v1')
                      # returns the following where phv53747 and phv6 has the same name LUNCSCORE.
                      #
                      #	  dbGaP_Subject_ID Submitted_Subject_ID AGEPHOT LNUCSCORE.x LNUCSCORE.y DIABAGE
                      #	  d  1                1                 1379    74.2        2.08        2.08      NA
                      #	  d  2                2                 4861    69.6        3.39        3.39      NA
                      #	  d  3                3                 3642    73.4        2.95        2.95      61
                      #	  d  4                4                 5400    79.1        4.09        4.09      N
                      #
                      # In this case, the column names are changed autumatically to LUNCSCORE.x and LUNCSOCRE.y,
                      # that causes trouble when, as an example, trying to subset the data based on the original column name LUNCSCORE.
                      # It is there for safe to use phvAcc as column names when working with the data of list of phv acc.

                      ################################
                      # Compose varNameAccCombo
                      ################################
                      # varNameAcccCommbo example: 'AGEPHOT_phv00000027.v2', 'LNUCSCORE_phv00053747.v2', 'LNUCSCORE_phv00000006.v2', 'DIABAGE_phv00054122.v1'
                      #cleanPhvAccList <- as.list(cleanPhvAccList)
                      cleanPhvAccList <- as.vector(cleanPhvAccList)

                      # Make sure checkList = FALSE
                      varNameAccComboList <- getVarNameAccCombos(object, phvAccList = cleanPhvAccList, studyDataDicDf = studyDataDicDF, checkList = FALSE)

                      firstColName = colnames(finalVarDF)[1]
                      secondColName = colnames(finalVarDF)[2]

                      # Display column names as combo
                      if (colNameWithAcc == T) {

                          # Normal case where 1st column is dbGaP_Subject_Id
                          if (matchGapSubjId) {
                              # Break 1st, specialVarName, and rest of columns apart 
                              thirdColName = colnames(finalVarDF)[3]

                              newColNames = c(firstColName, secondColName, thirdColName, varNameAccComboList)
                          }
                          # Speical case where 1st column is Submitted_Subject_ID 
                          else {
                              #newColNames = c('Submitted_Subject_ID', varNameAccComboList)
                              newColNames = c(firstColName, varNameAccComboList)
                          }
                          colnames(finalVarDF) <- newColNames
                      }
                      else {

                          # Strip Acc from varNameAccCombo and add .num extension for duplicate items
                          # From 
                          # 'AGEPHOT_phv00000027.v2', 'LNUCSCORE_phv00053747.v2', 'LNUCSCORE_phv00000006.v2', 'DIABAGE_phv00054122.v1'
                          # to
                          # "AGEPHOT"     "LNUCSCORE"   "LNUCSCORE.1" "DIABAGE"
                          newVarNameList <- stripColNameAcc(object, colNameAccList = unlist(varNameAccComboList)) 

                          # Normal case where 1st column is dbGaP_Subject_Id
                          if (matchGapSubjId) {
                              thirdColName = colnames(finalVarDF)[3]

                              #newColNames = c('dbGaP_Subject_ID', 'Submitted_Subject_ID', varNameAccComboList)
                              newColNames = c(firstColName, secondColName, thirdColName, newVarNameList)
                          }
                          # Speical case where 1st column is Submitted_Subject_ID 
                          else {
                              print("HHHHH22")
                              #newColNames = c('Submitted_Subject_ID', varNameAccComboList)
                              newColNames = c(firstColName, newVarNameList)
                          }
                          colnames(finalVarDF) <- newColNames
                      }

                      return (finalVarDF)
                  }
                  else {

                      phvAccListCombo = paste(cleanPhvAccList, collapse = ", ") 

                      cat("\n")
                      type = 'process'
                      level = 'error'
                      show = T
                      mesg = paste("None of the variables in the input PhvAccList belong study ", phsAcc, " There is no data is returned. \n   ", " --- ", phvAccListCombo,  "\n", sep="")
                      writeLog(object,  type = type, level = level, message = mesg, show = show) 

                  }

              } # end length cleanPhvAccList == 0 

          })


# ------------------------------ 
# Method: getVarNameAccCombos 
# ------------------------------ 

#' (internal) Get concatenated variable name and accession combo strings  
#' 
#' The method returns a list of concatenated combo strings of variable name and accession given a data dictionary. When dealing with large phvAccList, it is important to set checkList FALSE. Otherwise the process takes will take too long to finish.
#'
#' @param object Study class object.
#' @param phvAccList a character vector. A list of the dbGaP variable accessions. 
#' @param ... There are optional arguments.
#' @param studyDataDicDF a data frame. (optional) Study variable meta-info (data dictionary).
#' @param checkList a logical value. (optional) If TRUE, check the input phvAccList. Not check if FALSE. 
#' @return a character vector. The concatenated combo strings of the variable name and accession.
#' @export getVarNameAccCombos 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' s <- Study(phsAcc = 'phs000001.v3.p1')
#' accList = c('phv00054119.v1.p1.c2', 'phv00053735.v2', 'phv00053732.v2')
#' getVarNameAccCombos(s, phvAccList = accList)
#'}

# s <- Study(phsAcc = 'phs000001.v3.p1')
# getVarNameAccCombos(s, phvAccList = c('phv00054119.v1.p1.c2', 'phv00053735.v2', 'phv00053732.v2'))
#
setGeneric(
           name = "getVarNameAccCombos",
           def = function(object, phvAccList, ...) {
               standardGeneric("getVarNameAccCombos")
           })

#' @describeIn getVarNameAccCombos A method of class Study 
setMethod(
          f = "getVarNameAccCombos",
          signature = c("Study", "character"),
          definition = function(object, phvAccList, ..., studyDataDicDF = data.frame(), checkList = TRUE) {

              phsAcc = object@phsAcc

              if (nrow(studyDataDicDF) == 0) {
                  studyDataDicDF <- getDataDicByStudy(object, phsAcc) 
              }

              # Allow not list and vector as input. Convert to vector of it is a list.
              if (is.list(phvAccList)) {
                  phvAccList = unlist(phvAccList, use.names=FALSE)
              }

              ###########################
              # Validate PhvAccList
              ###########################
              # Important! Make sure to skip the check for large phvAccList 
              cleanPhvAccList = list()
              if (checkList) {
                  # ATTN! This step is very time consuming
                  cleanPhvAccList <- checkPhvAccList(object, phvAccList = phvAccList) 
              }
              else {
                  cleanPhvAccList <- phvAccList 
              }

              ################################
              # Compose varNameAccCombo
              ################################
              # varNameAcccCommbo example: 'AGEPHOT_phv00000027.v2', 'LNUCSCORE_phv00053747.v2', 'LNUCSCORE_phv00000006.v2', 'DIABAGE_phv00054122.v1'
              varNameAccComboList <- lapply(cleanPhvAccList, function(phvAcc, dataDicDF = studyDataDicDF) 
                                            {

                                                # Look for matching phtAcc and phsAcc 
                                                matchVarDF <- subset(dataDicDF,  dataDicDF$variable_accession==phvAcc & dataDicDF$study_accession==phsAcc) 

                                                ######################################
                                                # Sort out available dataset version
                                                ######################################

                                                availPhtAcc = ""
                                                if (nrow(matchVarDF) > 1) {
                                                    randPhtAcc <- matchVarDF$dataset_accession[1]
                                                    # Get avaiable phtAcc
                                                    availPhtAcc <- getAvailPhtVer(object, randPhtAcc=randPhtAcc, phsAcc=phsAcc)
                                                }
                                                else {
                                                    availPhtAcc <- toString(matchVarDF$dataset_accession[1])
                                                }
                                                matchVarDF <- subset(matchVarDF,  matchVarDF$dataset_accession==availPhtAcc) 

                                                if (nrow(matchVarDF) > 0) {
                                                    # Remove duplicate rows due to different phtAcc versions
                                                    matchVarName <- unique(matchVarDF[c('name')])$name
                                                    matchStAcc <- unique(matchVarDF[c('study_accession')])$study_accesssion

                                                    varNameAccCombo = paste(matchVarName, '_', phvAcc, sep="") 

                                                    return (varNameAccCombo)
                                                }
                                                else {
                                                    cat("\n")
                                                    type = 'process'
                                                    level = 'error'
                                                    show = T
                                                    mesg = paste("Data variable accession, ", phvAcc, " of study ", phsAcc, " has no match in the data dictionary file.\n", sep="")
                                                    writeLog(object,  type = type, level = level, message = mesg, show = show) 

                                                    return (phvAcc)
                                                }
                                            })

              return(varNameAccComboList)

          })

# ------------------------------------------- 
# Method: getVariableDataByPhvAccAndSubjId
# ------------------------------------------- 

#' (internal) Get variable data subset by subject ids
#' 
#' The method returns the data of a given list of variables subset by a list of subject ids.
#'
#' @param object Study class object
#' @param phvAccList a character vector. A list of dbGaP variable accessions.
#' @param ... There are optional arguments.
#' @param subjIdsOrFile a character vector or a character string. (optional) This argument can be a list of subject ids shared among the input variables. It can also be the path to a file that contains a list of of the subject ids shared among the input variables.
#' @return a data frame. The variable data of given variables subset by the given subjects.
#' @export getVariableDataByPhvAccAndSubjId
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' s <- Study(phsAcc = 'phs000001.v3.p1')
#' ids <- c("219", "220", "221") 
#' accList <- c('phv00054119.v1.p1.c2', 'phv00053735.v2')
#' getVariableDataByPhvAccAndSubjId(s, phvAccList = accList, subjIdsOrFile = ids)
#' idFile = '/home/user/temp/subj_ids.txt'
#' getVariableDataByPhvAccAndSubjId(s, phvAccList = accList, subjIdsOrFile = idFile)
#'}

# s <- Study(phsAcc = 'phs000001.v3.p1')
# getVariableDataByPhvAccAndSubjId(s, phvAccList = c('phv00054119.v1.p1.c2', 'phv00053735.v2'), subjIdsOrFile = '/netmnt/sandtraces04/dbgap-release04/dbgapr_test/test_user_data/other_files/selected_subj_ids_phs000001.v3.txt')
#
# s <- Study(phsAcc = 'phs00065`.v7')
# getVariableDataByPhvAccAndSubjId(s, phvAccList = c('phv00054119.v1.p1.c2', 'phv00053735.v2'), subjIdsOrFile = '/netmnt/sandtraces04/dbgap-release04/dbgapr_test/test_user_data/other_files/selected_subj_ids_phs000657.v7.txt')

setGeneric(
           name = "getVariableDataByPhvAccAndSubjId",
           def = function(object, phvAccList, ...) {
               standardGeneric("getVariableDataByPhvAccAndSubjId")
           })

#' @describeIn getVariableDataByPhvAccAndSubjId returns a dataframe of the variable data 
setMethod(
          f = "getVariableDataByPhvAccAndSubjId",
          signature = c("Study", "character"),
          definition = function(object, phvAccList, ...,  subjIdsOrFile = NULL) {

              phsAcc = object@phsAcc
              prjDotDir = object@prjDotDir
              prjDir = object@prjDir

              ########################
              # Validate PhvAccList
              ########################
              cleanPhvAccList <- checkPhvAccList(object, phvAccList = phvAccList) 

              if (length(cleanPhvAccList) > 0) {

                  # Get StudyDataDicDF
                  parseIdsFromStAcc =  parseIdsFromStAcc(object, phsAcc = phsAcc)
                  phsAccNoVer = parseIdsFromStAcc$phsAccNoVer
                  studyDataDicDF <- getDataDicByStudy(object, phsAcc) 

                  ####### Get mergedVarDF #######
                  # No emptyToNa conversion 
                  mergedVarDF =  getVariableDataByPhvAcc(object, cleanPhvAccList, emptyToNa = F)

                  if (!is.null(mergedVarDF)) {
                      #print(mergedVarDF)
                      ############################
                      # Further filter by subjIds
                      ############################
                      finalVarDF <- filterBySubjIds(object, varDF = mergedVarDF, subjIdsOrFile = subjIdsOrFile)

                      return (finalVarDF)

                  }
              }

          })




########################
# List of functions
########################

# viewStudyVariableInfo
# getVariableInfoByPhvAcc
# getStudyVariableInfoByDataType
# checkPhvAccList
# checkObjStudyByAcc
# getDatasetDataByPhtAcc
# getVariableDataByPhvAcc
# getVarNameAccCombos
# getVariableDataByPhvAccAndSubjId
