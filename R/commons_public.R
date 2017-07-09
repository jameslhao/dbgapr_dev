#######################
# commons_public.R
#######################

# The methods of Commons class that are intended to be used by users.
# ATTN! The documentation rd file of this function is manually created: man/commons_public.rd


# ----------------------
# Method: prjConfig
# ----------------------

#' Create user project directory
#'
#' The method creates a project directory and saves the path information of the prject directory in a project config file (~/ncbi/dbgapr_conf/prject_config.json). The project directory is where all the data and metadata files are located. Multiple project directories can be created, but only one is defined as the current. An old project directory can be reinstated to become current again.
#' @param object Commons class object.
#' @param prjDir a character string. The path to designated project directory.
#' @return a logical value. TRUE if the project directory is created and FALSE if not.
#' @export prjConfig 
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' prjConfig(c, prjDir = "/home/user/my_project")
#' }

setGeneric(name = "prjConfig",
           def = function(object, prjDir) {
               standardGeneric("prjConfig")
           })

#' @describeIn prjConfig A method of class Commons
setMethod(
          f = "prjConfig",
          signature = c("Commons","character"),
          definition = function(object, prjDir) {
              prjDir <- checkInputPath(object, prjDir)

              curTime <- Sys.time()
              confFile = object@configFile
              prjDotDir = object@prjDotDir


              if (dir.exists(prjDotDir)) {

                  if (is.null(confFile) == T) {
                      confFile = ""
                  }

                  # Create dbgapr project dir
                  createOk = createPrjDir(object, prjDir)


                  if (createOk == TRUE) {

                      ######################################
                      # Create initial config dataframe
                      ######################################
                      # ATTN!
                      # Only the initial creation of the configFile is done through this function.
                      # All the further updates is carried out by creatPrjDir.

                      if (!file.exists(confFile)) {


                          id <- c(1)
                          prj_dir <- I(c(prjDir))
                          created <- as.POSIXlt(c(Sys.time()))
                          updated <- as.POSIXlt(c(Sys.time()))
                          current <- c('yes')
                          confDF <- data.frame(id, prj_dir, current, created, updated)
                          confJson <- toJSON(confDF, pretty=T)

                          # Append json content
                          write(confJson, file = confFile, ncolumns = if(is.character(confJson)) 1 else 5, append = F, sep = "\n")
                          cat("\nThe dbgapr user project configuration file is created --- ", confFile, "\n")

                          # Important reminder to users 
                          message("\nBefore going forward, please run the command such as below to make the project directory change in effect.\nc <- Commons()\n") 

                          # Example json content

                          # {
                              #  "id": "4",
                              #  "prj_dir": "/netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4",
                              #  "current": "yes",
                              #  "created": "2016-06-15 12:05:54",
                              #  "updated": "2016-06-16 16:16:58"
                              # },

                          }
                      }

                      return(createOk)
              }
              else {

                  # 
                  message("\nThe project config directory (~/ncbi/dbgapr_conf) is missing. It needs to be created through initializing the class Commons.\nExample command: c <- Commons()\n") 
              }

          })


# ----------------------
# Method: getPrjDir
# ----------------------

#' Get user project directory
#'
#' The method looks for the current project directory information and associates the directory and its sub-directories to the class object. It returns a list of paths to the current project directory and the sub-directories. The method is called every time when a class object is initialized, which makes the current project directory and the sub-directory information readily accessible through the class object.
#'
#' @param object Commons class object.
#' @param ... There are optional arguments. 
#' @param showErr a logical value. (optional) If TRUE, display the error message; If FALSE (default), not display. 
#' @return a character list. Current user project directory and the sub-directories.
#' @export getPrjDir
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' getPrjDir(c, showErr = TRUE)
#'}

setGeneric(name="getPrjDir",
           def = function(object, ...) {
               standardGeneric("getPrjDir")
           })

#' @describeIn getPrjDir of Class Commons
setMethod(f ="getPrjDir",
          signature = c("Commons"),
          definition = function(object, ..., showErr = as.logical(F)) {

              # Look up prj_dir from ~/ncbi/dbgapr_conf/project_config.json
              confFile = object@configFile
              if (is.null(confFile) == T) {
                  confFile = ""
              }

              currPrjDir = '' 
              prjDataDir = '' 
              prjTempDir = '' 
              procLogArchDir = ''
              dataProcLog = ''
              prjSetupLog = ''

              if(nchar(confFile) != 0) {


                  if (file.exists(confFile)) {

                      # Get current prjDir from confDF 
                      confDF <- fromJSON(confFile, flatten=TRUE)

                      currPrjDir <- (subset(confDF, confDF$current == 'yes'))$prj_dir
                      

                      #################################
                      # Create prjSetupLog file 
                      #################################
                      # Get project dot dir 
                      prjDotDir = object@prjDotDir
                      prjSetupLog = file.path(prjDotDir, "project_setup.log")

                      ############################################################
                      # Check currPrjDir and associate dirs with class object
                      ############################################################
                      if (!is.null(currPrjDir)) {

                          # currPrjDir configured but physical dir not found 
                          if (!dir.exists(currPrjDir)) {

                              mesg = paste("The previously configured prject directory no longer exists ---", currPrjDir, ". Checkout ?projConfig to see how to re-create it.", sep="") 
                              message(mesg)
                          }
                          else {

                              #######################################################
                              # ATTN! This function is called by init of Commons.R
                              #######################################################
                              # So the following values are passed to the Commons()
                              # class object when the class is initialized. 

                              #########################################
                              # Associate directory with class object
                              #########################################
                              object@prjDir <- currPrjDir

                              # Save global paths 
                              prjDataDir = file.path(currPrjDir, 'gapwork', 'data')

                              # /net/snowman/vol/gap/gapr_prj/metadata
                              prjMetaDir = file.path(currPrjDir, 'metadata')

                              prjTempDir = file.path(currPrjDir, 'gapwork', 'temp')
                              procLogDir = file.path(currPrjDir, "gapwork", "log")
                              procLogArchDir = file.path(procLogDir, "archived_log")
                              dataProcLog = file.path(procLogDir, 'data_process.log')

                              ################################################################
                              # Create sub-directoires if they don't exist by some reasons
                              ################################################################

                              # Create dir recursively 
                              if (!dir.exists(prjDataDir)) {
                                  dir.create(prjDataDir, showWarnings = TRUE, recursive = T, mode = "0777")
                              }
                              if (!dir.exists(prjTempDir)) {
                                  dir.create(prjTempDir, showWarnings = TRUE, recursive = T, mode = "0777")
                              }
                              if (!dir.exists(procLogDir)) {
                                  dir.create(procLogDir, showWarnings = TRUE, recursive = T, mode = "0777")
                              }
                              if (!dir.exists(procLogArchDir)) {
                                  dir.create(procLogArchDir, showWarnings = TRUE, recursive = T, mode = "0777")
                              }

                              # Create file
                              if (!file.exists(dataProcLog)) {

                                  # Initilize log file
                                  currTime = toString(as.POSIXlt(Sys.time()))
                                  logFile = dataProcLog
                                  logData = paste("[", currTime, "] ", "Initial creationg of this log file.", "\n", sep="")
                                  write(logData, file = logFile, ncolumns = 1, append = T, sep = "\n")
                              }

                              # make it availabe to class
                              object@prjDataDir <- prjDataDir
                              object@prjMetaDir <- prjMetaDir
                              object@prjTempDir <- prjTempDir
                              object@dataProcLog <- dataProcLog 
                              object@procLogArchDir <- procLogArchDir 
                              object@prjSetupLog <- prjSetupLog 


                              ####################
                              # check PrjDir
                              ####################

                              # check prjDir saved in object
                              prjDir = object@prjDir

                              if (is.null(prjDir) == T) {
                                  prjDir = ""
                              }

                              # prjDir has no value (not provided)
                              if (nchar(prjDir) == 0) {
                                  mesg = paste("There is no project directory info associated with the class object. Please re-initialized the class object.", "\n", sep="")
                                  cat(mesg)
                              }
                              # prjDir has value (provided)
                              else {

                                  # input prjDir differs from currPrjDir 
                                  if (prjDir != currPrjDir) {

                                      type = 'setup'
                                      level = 'error'
                                      show = T
                                      mesg = paste(
                                          "The currently  configured prject directory is --- ", 
                                          currPrjDir, ".\n",
                                          "   It is different from the one associated with the class object. --- ", 
                                          prjDir, ".\n",
                                          "   Please re-initialize the class object Commons to make the two consistent.", "\n", sep=""
                                      )
                                      cat(mesg)

                                  }
                              }


                              if (prjDir != "") {
                                  if (!dir.exists(prjDir)) {
                                      message("\nPreviously configured current project directory no longer exists. Checkout ?prjConfig() to see how to re-configure it. --- ", prjDir, "\n") 
                                  }
                              }


                              ##########################
                              # List of dirs as output
                              ##########################
                              prjDirFiles = list('prjDir' = prjDir, 'prjDataDir' = prjDataDir, 'prjMetaDir' = prjMetaDir, 'prjTempDir' = prjTempDir, 'procLogArchDir' = procLogArchDir, 'dataProcLog' = dataProcLog, 'prjSetupLog' = prjSetupLog)

                              return(prjDirFiles)
                          }


                      }
                      # currPrjDir has no value
                      else {
                          mesg = paste("Previously configured prject directory no longer exists --- ", currPrjDir, ". Checkout ?prjConfig to see how to re-create it.", "\n", sep="")
                          cat(mesg)
                      }
                  }
                  # confFile not exist 
                  else {
                      mesg = paste("The project config file is not found. Checkout ?prjConfig to see how to create it.", "\n", sep="")
                      cat(mesg)
                  }
              }
              # confFile has no value
              else {
                  mesg = paste("The project config file is not found . Checkout ?prjConfig to see how to create it.", "\n", sep="")
                  cat(mesg)
              }


          } )

# ----------------------
# Method: prepareData
# ----------------------

#' Copy and preprocess data files 
#'
#' The method wraps several function calls related to the data preparation into one call. It copies user data files to the project directory ( \code{\link{searchCopyPhenoFiles}} ), downloads supplemental metadata files from the dbGaP ftp site ( \code{\link{ftpDownload}} ), extracts and saves the file meta-info ( \code{\link{recordPrjFileInfo}} ), and finally merges the data files of different consents ( \code{\link{mergeDatasetConsent}} ). If any of above steps hasn't gone through successfully (often due to temporary glitches), rerun the failed and rest of steps individually to make sure all steps are finished before going forward.
#'
#' @name prepareData 
#' @param object Commons class object.
#' @param userDataDir a character string. The path to the top level directory of decrypted dbGaP phenotype data files.
#' @param ... There are optional arguments. 
#' @param phsAcc a character string. The dbGaP study accession.
#' @param dataStudyOnly a logical value. When TRUE (default), only downloads the dataset and variable metadata of the stdudies that have data files in the project directory.  When FALSE, downloads the dataset and variable metadata of all dbGaP released studies, regardless the actual phenotype data files of the studies are downloaded or not. 
#' @return a data frame. (invisible) The meta-info of merged data files.
#' @export prepareData 
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' prepareData(c, userDataDir = '/home/user/data')
#' # or
#' prepareData(c, userDataDir = '/home/user/data', phsAcc = 'phs000001.v3.p1')
#' # or to include the data meta-info of all the studies released in dbGaP.
#' # Note: This step may take serveral hours.
#' prepareData(c, userDataDir='/home/user/data', dataStudyOnly = FALSE)
#'}


setGeneric(name = "prepareData",
           def = function(object, userDataDir, ...) {
               standardGeneric("prepareData")
           })

#' @describeIn prepareData A method of class Commons 
setMethod(
          f = "prepareData",
          signature = c("Commons", "character"),
          definition = function(object, userDataDir, ..., phsAcc = "", dataStudyOnly = TRUE) {

              # reload object
              object <- Commons()
              prjDotDir <- object@prjDotDir
              prjDir <- object@prjDir

              if (dir.exists(prjDotDir) & dir.exists(prjDir)) {
                  userDataDir <- checkInputPath(object, userDataDir)


                  # Validate accession 
                  inputPhsAcc = phsAcc
                  if (phsAcc != "") {
                      phsAcc <- cleanObjAcc(object, acc = phsAcc, type = 'phs') 
                  }

                  if (inputPhsAcc == "" | phsAcc != "") {

                      # Search and copy user files
                      cat("\nSearch and copy user data files ...\n")
                      parsedFileInfoDF <- searchCopyPhenoFiles(object, userDataDir = userDataDir, phsAcc = phsAcc)

                      if (!is.null(parsedFileInfoDF)) {

                          cat("\nDownload supplemental meta-data from ftp ...\n")
                          cat("\n")
                          downloadOk <- ftpDownload(object, phsAcc = phsAcc, dataStudyOnly = dataStudyOnly)

                          if (downloadOk) {

                              cat("\nExtract and save file info ...\n")
                              savedFileInfoDF <- recordPrjFileInfo(object, phsAcc = phsAcc)

                              if (!is.null(savedFileInfoDF)) {

                                  cat("\nMerge data of different consents ...\n")
                                  mergedFileInfoDF <- mergeDatasetConsent(object, phsAcc = phsAcc)

                                  return(invisible(mergedFileInfoDF))
                              }
                          }
                      }
                  }
              }
              else {
                  if (dir.exists(prjDotDir)) {
                      message("ERROR> The ~/.dbgapr directory does not exists. Re-init the Commons class should help. Checkout ?Commons for details.\n") 
                  }
                  if (dir.exists(prjDir)) {
                      message("ERROR> The user project directory does not exists. Re-config the project and re-init the Commons class should help. Checkout ?prjConfig for details.\n") 
                  }
              }

          })


# -------------------------------
# Method: searchCopyPhenoFiles
# -------------------------------

#' Copy phenotype files to project directory 
#'
#' The method searches for the decrypted dbGaP phenotype files in a given directory and copies (or moves) the files to the configured project directory. When a study accession is provided, only the files of specified study are copied.
#'
#' @name searchCopyPhenoFiles 
#' @param object Commons class object.
#' @param userDataDir a character string. The path to the top level directory of decrypted dbGaP phenotype files. The files can be from different studies and located in different sub-directories.
#' @param ... There are optional arguments.
#' @param phsAcc a character string. The dbGaP study accession.
#' @param copyOrMove a character string. (optional) If the string value is 'copy' (default), copies the files to the project directory. If the string value is 'move', movies the files from the user data directory to the project directory. 
#' @param showErr a logical value. (optional) If TRUE, display the error message; If FALSE (default), not display. 
#' @return (invisible) data frame. The meta-info of all copied files. 
#' @export searchCopyPhenoFiles
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' searchCopyPhenoFiles(c, userDataDir = '/home/user/data')
#' # or
#' searchCopyPhenoFiles(c, userDataDir = '/home/user/data', phsAcc="phs000001.v3.p1")
#'}

# searchCopyPhenoFiles(c, userDataDir = 'C:/Users/mars/Documents/R_Dev/decrypted_test_data', phsAcc="phs000001.v3.p1")
# searchCopyPhenoFiles(c, userDataDir = '/netmnt/sandtraces04/dbgap-release04/dbgapr_test/test_user_data/NHLBI/Framingham/Framingham_V27')
# searchCopyPhenoFiles(c, userDataDir = '/netmnt/sandtraces04/dbgap-release04/dbgapr_test/test_user_data/NEI')
# searchCopyPhenoFiles(c, userDataDir = ' /netmnt/sandtraces04/dbgap-release04/dbgapr_test/test_user_data/NHLBI/CHS_CARe')
# searchCopyPhenoFiles(c, userDataDir = ' /netmnt/sandtraces04/dbgap-release04/dbgapr_test/test_user_data/NHLBI/Hutterites/Hutterites_V3')

# searchCopyPhenoFiles(c, userDataDir = 'C:\\Users\\mars\\Documents\\dbgap_test_data\\temp')
# searchCopyPhenoFiles(c, userDataDir = 'C:\\Users\\mars\\Documents\\dbgap_test_data\\mixed_studies_decrypted')

setGeneric(name = "searchCopyPhenoFiles",
           def = function(object, userDataDir, ...) {
               standardGeneric("searchCopyPhenoFiles")
           })

#' @describeIn searchCopyPhenoFiles A method of class Commons 
setMethod(
          f = "searchCopyPhenoFiles",
          signature = c("Commons","character"),
          definition = function(object, userDataDir, ..., phsAcc = "", copyOrMove = 'copy', showErr = TRUE) {
              userDataDir <- checkInputPath(object, userDataDir)

              # Trim head/tail spaces
              userDataDir = stringr::str_trim(userDataDir)
              prjDotDir = object@prjDotDir
              prjDir = object@prjDir

              inputPhsAcc <- phsAcc

              if (phsAcc != "") {
                  phsAcc <- cleanObjAcc(object, acc = phsAcc, type = 'phs') 
              }

              if (dir.exists(userDataDir)) {

                  # Find files that ends with ncbi_enc 
                  encFiles <- list.files(path=userDataDir, pattern="phs\\d+.*ncbi_enc$",  recursive = TRUE) 

                  # Make sure no encrypted file under the whole directory
                  if (length(encFiles) == 0) {


                      ##################################################################
                      # These patterns effectively rule out encrypted ncbi_enc files 
                      ##################################################################

                      getFnames <- function(userDataDir, phsStr) {

                          # Search for phenotype .gz or .txt(ungzip) file 
                          # Example: phs000001.v3.pht000001.v2.p1.c1.genspecphenotype.EDO.txt.gz
                          #pattn = paste("phs[0-9]+", ".*pht[0-9]+.*.[gz|txt]$", sep="")
                          pattn = paste(phsStr, ".*pht[0-9]+.*.[gz|txt]$", sep="")
                          fnames1 <- list.files(path = userDataDir ,recursive = T, pattern = pattn)

                          # Example: phs000001.v3.p1.data_dictionary.MULTI.tar, phs000001.v3.p1.variable_report.MULTI.tar.gz
                          #pattn = paste("phs[0-9]+", ".*data_dictionary.*.tar.gz$", sep="")
                          pattn = paste(phsStr, ".*data_dictionary.*.tar.gz$", sep="")
                          fnames2 <- list.files(path = userDataDir ,recursive = T, pattern = pattn)

                          #pattn = paste("phs[0-9]+", ".*variable_report.*.tar.gz$", sep="")
                          pattn = paste(phsStr, ".*variable_report.*.tar.gz$", sep="")
                          fnames3 <- list.files(path = userDataDir ,recursive = T, pattern = pattn)

                          # Example: phs000007.v27.pht005164.v1.t_ctvasclng_2011_m_0952s.data_dict.xml, phs000007.v27.pht005165.v1.p10.t_mrtbss_2014_m_0961s.var_report.xml
                          # ATTN: There could be something between 'data_dic' and 'xml', such as below:
                          # phs000429.v1.pht002481.v1.areds_data_final_11.data_dict_2012_02_24.xml
                          #pattn = paste("phs[0-9]+", ".*data_dict.*.xml$", sep="")
                          pattn = paste(phsStr, ".*data_dict.*.xml$", sep="")
                          fnames4 <- list.files(path = userDataDir ,recursive = TRUE, pattern = pattn)


                          # Example: phs000007.v27.pht005164.v1.t_ctvasclng_2011_m_0952s.data_dict.xml, phs000007.v27.pht005165.v1.p10.t_mrtbss_2014_m_0961s.var_report.xml
                          #pattn = paste("phs[0-9]+", ".*var_report.*.xml$", sep="")
                          pattn = paste(phsStr, ".*var_report.*.xml$", sep="")
                          fnames5 <- list.files(path = userDataDir ,recursive = T, pattern = pattn)

                          # Combine all three lists
                          fnames = c(fnames1, fnames2, fnames3, fnames4, fnames5)

                          return(fnames)
                      }

                      phsStr = ""
                      if (inputPhsAcc == "") {
                          phsStr = "phs[0-9]+"
                      }
                      else {
                          if (phsAcc != "") {
                              phsStr = phsAcc
                          }
                      }

                      # It indicates an input error if phsStr == ""


                      if (phsStr != "") {

                          fnames <- getFnames(userDataDir, phsStr)


                          if (length(fnames) == 0) {
                              type = 'process'
                              level = 'info'
                              show = T
                              mesg = paste("There is no phenotype data file, data dictionary, or variable report file found in the input directory. No file is copied. --- ", userDataDir, sep="")
                              writeLog(object,  type = type, level = level, message = mesg, show = show)
                          }
                          else {

                              # Process files under userDataDir
                              message("\nCopying and processing files. It may take a while ...\n")

                              ############################
                              # Calling copyUserFiles()
                              ############################
                              # Loop through each file under userDataDir and copy the files to the default project diretory
                              # Note: x represents each item in fnames
                              fileInfoSets <- lapply(fnames, FUN=function(x) copyUserFile(object, userFile = file.path(userDataDir, x), copyOrMove = copyOrMove))
                              mergedFileInfoDF <- do.call('rbind', fileInfoSets)

                              cat("\n")
                              type = 'process'
                              level = 'info'
                              show = showErr 
                              if (copyOrMove == 'move') {
                                  mesg = paste("Total ", length(fileInfoSets), " files moved to the project direcotry.", sep="")
                              }
                              else {
                                  mesg = paste("Total ", length(fileInfoSets), " files copied to the project directory.", sep="")
                              }
                              writeLog(object,  type = type, level = level, message = mesg, show = show)
                              cat("\n")

                              return (invisible(mergedFileInfoDF))
                          }
                      } # end of phsStr == ""

                  }
                  else {
                      type = 'process'
                      level = 'error'
                      show = T
                      mesg = paste("There is at least one encrypted file (ending with ncbi_enc) found in the input directory. All files under the directory need to be decrypted first. It can be done by running vdb-decrypt of the sra-toolkit against the same directory. --- ", encFiles[1], sep="")
                      writeLog(object,  type = type, level = level, message = mesg, show = show)

                  }
              }
              else {
                  
                  type = 'process'
                  level = 'error'
                  show = T
                  mesg = paste("The input path of userDataDir doesn't not exist. Check to make sure its a valid real path and try it again. --- ", userDataDir, ". No file is copied.", sep="") 
                  writeLog(object,  type = type, level = level, message = mesg, show = show)
              }

          })

# ----------------------------------
# Method: ftpDownload
# ----------------------------------

#' Download supplemental data files from dbGaP ftp
#'
#' The method downloads the supplemental metadata files from the dbGaP ftp site. It is called by the \code{\link{prepareData}} function.
#'
#' @name ftpDownload 
#' @param object Commons class object
#' @param ... There are optional arguments.
#' @param phsAcc a character string. The dbGaP study accession. Only the files of the given study are downloaded when a study accession is provided.
#' @param dataStudyOnly a logical value. When TRUE (default), only downloads the dataset and variable metadata of the stdudies that have data files in the project directory.  When FALSE, downloads the dataset and variable metadata of all dbGaP released studies, regardless the actual phenotype data files of the studies are downloaded or not. 
#' @param overwrite a logical value. When TRUE, downloads the supplemental metadata files if they aren't already downloaded. When FALSE (default, downloads the metadata files even the they already exist. 
#' @return a logical value. If TRUE, all files are downloaded OK. If FALSE, there is at least one failed download.
#' @export ftpDownload
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' ftpDownload(c)
#' # or
#' ftpDownload(c, phsAcc = 'phs000001.v3.p1')
#' ftpDownload(c, dataStudyOnly=FALSE)
#' # or to include the data meta-info of all the studies released in dbGaP.
#' # Note: This step may take serveral hours.
#' ftpDownload(c, dataStudyOnly = FALSE)
#'}

setGeneric(name = "ftpDownload",
           def = function(object, ...) {
               standardGeneric("ftpDownload")
           })

#' @describeIn ftpDownload A method of class Commons 
setMethod(
          f = "ftpDownload",
          signature = c("Commons"),
          definition = function(object, ..., phsAcc = "", dataStudyOnly=TRUE, overwrite=FALSE ) {



              prjDataDir = object@prjDataDir
              prjMetaDir = object@prjMetaDir
              dataProcLog = object@dataProcLog 
              prjDir = object@prjDir


              inputPhsAcc = phsAcc;

              if (phsAcc != "") {
                  phsAcc <- cleanObjAcc(object, acc = phsAcc, type = 'phs') 
              }

              ########################
              # dbGaP ftp base URL
              ########################
              baseUrl = 'ftp://ftp.ncbi.nlm.nih.gov/dbgap/r-tool/studies'



              ################
              # S3 Function
              ################
              # Download to destDir
              downloadFile <- function(url, destFile) {

                  # This_study_version level
                  # ftp://ftp.ncbi.nlm.nih.gov/dbgap/r-tool/studies/phs000429/phs000429.v1/
                  # phs000429.v1_study_id_variable_name.txt.gz
                  # phs000429.v1_study_info.txt.gz
                  # phs000429.v1_study_dataset_info.txt.gz
                  # phs000429.v1_study_variable_info.txt.gz
                  # phs000429.v1_study_variable_code_value.txt.gz

                  ##############################
                  # TryCatch internet connect
                  ##############################
                  out <- tryCatch({
                      download.file(url=url, destfile=destFile, method= 'auto', quiet=T)
                  },
                  error=function(cond) {
                      mesg = (paste("\nThe ftp URL cannot be accessed. ---", url, "\n", cond))
                      message(mesg)

                      #################
                      # Log as error
                      #################
                      type = 'process'
                      level = 'error'
                      show = F
                      mesg = mesg 
                      writeLog(object,  type = type, level = level, message = mesg, show = show)


                      # Return 1 if error
                      return(1)
                  },
                  warning=function(cond) {
                      mesg = (paste("\nThe connection to the ftp file URL cannot be established. ---", url, "\n", cond))
                      message(mesg)


                      #################
                      # Log as error
                      #################
                      type = 'process'
                      level = 'error'
                      show = F
                      mesg = mesg 
                      writeLog(object,  type = type, level = level, message = mesg, show = show)

                      # Return 2 if warn
                      return(2)
                  },
                  finally={
                      mesg = paste0("Processed URL --- ", url, "\n")
                      cat(mesg)

                      #################
                      # Log as error
                      #################
                      type = 'process'
                      level = 'info'
                      show = F
                      mesg = mesg 
                      writeLog(object,  type = type, level = level, message = mesg, show = show)
                  }
                  )

                  # Return 0 if successds
                  return (out)
              }



















              ##################################
              # Download all released studies
              ##################################

              # Note: download.file() returns 0 if successds, 1 if failed
              loadOk_0 = 1
              loadOk_1 = 1
              loadOk_2 = 1
              loadOk_3 = 1
              loadOk_4 = 1
              loadOk_5 = 1
              loadOk_6 = 1
              #dload_method = 'auto'

              cat("\nDownloading supplemental metadata of studies ... ( overwrite = ", overwrite, " dataOnlyStudy = ", dataStudyOnly,  " )\n\n")

              ###########################
              # Download all ID info 
              ###########################
              # Including study, dataset, variable id info file ##
              extAllIdInfoFile <- object@extAllIdInfoFile 

              destFile = extAllIdInfoFile 
              url = paste(baseUrl, "/", basename(destFile), sep = "")

              #loadOk_0 <- downloadFile(url, destFile)
              loadOk_0 <- download.file(url=url, destfile=destFile, method= 'auto', quiet=T)


              ###########################
              # Download AllStudyhInfo 
              ###########################
              # Download AllStudyInfoFile to ncbi/dbgapr_conf dir

              # Example ftp files
              # URL  : ftp://ftp.ncbi.nlm.nih.gov/dbgap/r-tool/studies/all_released_study_info.txt.gz
              # File : /Users/hao/Documents/ncbi/dbgapr_conf/supplemental_data/all_released_study_info.txt.gz"
              extAllStudyInfoFile <- object@extAllStudyInfoFile

              destFile = extAllStudyInfoFile
              url = paste(baseUrl, "/", basename(destFile), sep = "")
              #loadOk_0 <- downloadFile(url, destFile)
              loadOk_0 <- download.file(url=url, destfile=destFile, method= 'auto', quiet=T)



              ##################################
              ## Download individual study_info 
              ##################################

              ###### S3 function ftp download by study ######
              loadOk <- downloadByStudy <- function(studyDir) {

                  # Match studyDir 
                  # "C:/Users/mars/Documents/myprj/gapwork/data/phs000724/phs000724.v6"

                  ###########################################
                  # Build ftp Url of supplemental files 
                  ###########################################

                  # phs000724.v6
                  dirStudyIdVer = basename(studyDir)
                  # phs000724
                  dirStudyId = basename(dirname(studyDir))
                  # ftp://ftp.ncbi.nlm.nih.gov/dbgap/r-tool/studies/phs000724/phs000724.v6
                  studyVerUrl = paste0(baseUrl, "/", dirStudyId, "/", dirStudyIdVer);


                  # ftp files 
                  # phs000429.v1_study_info.txt.gz
                  # phs000429.v1_study_dataset_info.txt.gz
                  # phs000429.v1_study_variable_info.txt.gz
                  # phs000429.v1_study_variable_code_value.txt.gz
                  # phs000429.v1_study_file_manifest.txt.gz

                  studyInfoFile = paste0(dirStudyIdVer, "_study_info.txt.gz")
                  studyDatasetInfoFile = paste0(dirStudyIdVer, "_study_dataset_info.txt.gz")
                  studyVariableInfoFile = paste0(dirStudyIdVer, "_study_variable_info.txt.gz")
                  studyVarCodeValFile = paste0(dirStudyIdVer, "_study_variable_code_value.txt.gz")
                  studyIdVarNameFile = paste0(dirStudyIdVer, "_study_id_variable_name.txt.gz")
                  studyManifestFile = paste0(dirStudyIdVer, "_study_file_manifest.txt.gz")

                  #####################
                  # Create destDir
                  #####################

                  destDir = file.path(studyDir, "supplemental_data")
                  if (!file.exists(destDir)){
                      dir.create(destDir)
                  }

                  #### study specific study_info and their datasets  ####

                  ##############
                  # Download
                  ##############
                  ##### Note: these are plain-text file, use auto for http web-download is fine. Do not use 'curl'. ####

                  # study info 
                  file = studyInfoFile
                  url = paste0(studyVerUrl, "/", file)
                  destFile = file.path(destDir, file)

                  if (overwrite) {
                      loadOk_1 <- downloadFile(url, destFile)
                  }
                  else {
                      if (!file.exists(destFile)) {
                          loadOk_1 <- downloadFile(url, destFile)
                      }
                      else {
                          loadOk_1 = 0
                      }
                  }

                  # dataset info 
                  file = studyDatasetInfoFile 
                  url = paste0(studyVerUrl, "/", file)
                  destFile = file.path(destDir, file)

                  if (overwrite) {
                      loadOk_2 <- downloadFile(url, destFile)
                  }
                  else {
                      if (!file.exists(destFile)) {
                          loadOk_2 <- downloadFile(url, destFile)
                      }
                      else {
                          loadOk_2 = 0
                      }
                  }

                  # variable info 
                  file = studyVariableInfoFile 
                  url = paste0(studyVerUrl, "/", file)
                  destFile = file.path(destDir, file)

                  if (overwrite) {
                      loadOk_3 <- downloadFile(url, destFile)
                  }
                  else {
                      if (!file.exists(destFile)) {
                          loadOk_3 <- downloadFile(url, destFile)
                      }
                      else {
                          loadOk_3 = 0
                      }
                  }

                  # variable code value
                  file = studyVarCodeValFile 
                  url = paste0(studyVerUrl, "/", file)
                  destFile = file.path(destDir, file)

                  if (overwrite) {
                      loadOk_4 <- downloadFile(url, destFile)
                  }
                  else {
                      if (!file.exists(destFile)) {
                          loadOk_4 <- downloadFile(url, destFile)
                      }
                      else {
                          loadOk_4 = 0
                      }
                  }

                  # variable code value
                  file = studyIdVarNameFile
                  url = paste0(studyVerUrl, "/", file)
                  destFile = file.path(destDir, file)

                  if (overwrite) {
                      loadOk_5 <- downloadFile(url, destFile)
                  }
                  else {
                      if (!file.exists(destFile)) {
                          loadOk_5 <- downloadFile(url, destFile)
                      }
                      else {
                          loadOk_5 = 0
                      }
                  }

                  # manifest 
                  file = studyManifestFile 
                  url = paste0(studyVerUrl, "/", file)
                  destFile = file.path(destDir, file)

                  if (overwrite) {
                      loadOk_6 <- downloadFile(url, destFile)
                  }
                  else {
                      loadOk_6 = 0
                  }

                  loadOk = F 
                  if (loadOk_0 == 0 & loadOk_1 == 0 & loadOk_2 == 0 & loadOk_3 == 0 & loadOk_4 == 0 & loadOk_5 == 0 & loadOk_6 == 0) {
                      loadOk = T
                  }

                  return (loadOk)

              } # End S3 function




              #####################################################
              # Download study metadata of noDataStudy 
              #####################################################
              # Under prjMetaDir
              # Including the studies that don't have data under the project directory


              # Create all_released_study_info dir
              dataStudyOnlyMetaLoadOk = TRUE 

              if (!dataStudyOnly) {

                  cat("\nDownloading supplemental metadata of all released studies ... ( overwrite =", overwrite, ")\n")
                  message("\nThis step may take quite a while (up to several hours) to finish.\n")

                  #dataStudyOnlyDir = file.path(prjDir, "metadata")

                  if (!dir.exists(prjMetaDir)) {
                      dir.create(prjMetaDir)
                  }


                  # Get the external studyInfo data
                  allStudyInfo <- getExtData(object, type = 'study')

                  if (!is.null(allStudyInfo)) {

                      studyAccVec <- allStudyInfo[['this_study_accession']]

                      retList <- lapply(studyAccVec, function(thisPhsAcc) {

                          ####################
                          # Compose studyDir
                          ####################
                          parseIdsFromPhsAcc =  parseIdsFromStAcc(object, phsAcc = thisPhsAcc)
                          phsAccNoVer = parseIdsFromPhsAcc$phsAccNoVer

                          # Example:
                          # W://gapr_prj/metadata/phs001362/phs001362.v1
                          metaStudyDir <- file.path(prjMetaDir, phsAccNoVer, thisPhsAcc)
                          metaStudySupplDir <- file.path(metaStudyDir, 'supplemental_data')
                          dataStudyDir <- file.path(prjDataDir, phsAccNoVer, thisPhsAcc)
                          dataStudySupplDir <- file.path(dataStudyDir, "supplemental_data")


                          ################
                          # S3 function 
                          ################
                          # Copy or Download metaData files
                          handleMetaFiles <- function(metaStudyDir, dataStudySupplDir, metaStudySupplDir) {

                              #### study specific study_info and their datasets  ####

                              # Copy study data supplementao_data dir exists and not empty 
                              if (dir.exists(dataStudySupplDir)) {

                                  if (length(dir(dataStudyDir, all.files=FALSE)) > 0) {
                                      file.copy(dataStudySupplDir, metaStudyDir, recursive=TRUE)

                                      if (dir.exists(metaStudySupplDir)) {

                                          if (length(dir(metaStudyDir, all.files=FALSE)) > 0) {
                                              cat("Supplmental data files of study", thisPhsAcc, "are copied from\n---", dataStudySupplDir, "to ---", metaStudySupplDir, "\n")  
                                          }
                                      }
                                  }

                              }
                              else {
                                  pattn = paste0(phsAcc, "$")
                                  match <- grepl(pattn, metaStudyDir)
                                  if (match == TRUE) {
                                      loadOk_0 <- downloadByStudy(metaStudyDir)
                                  }
                              }


                              return(loadOk)
                          }  # end of S3



                          if (inputPhsAcc != "") {

                              ##########################
                              # For specified studies 
                              ##########################

                              if (phsAcc != "") {

                                  ######################
                                  # Check match study
                                  ######################
                                  if (thisPhsAcc == phsAcc) {

                                      if (!dir.exists(metaStudyDir)) {
                                          dir.create(metaStudyDir, recursive = TRUE)
                                      }

                                      # Overwrite or not 
                                      if(overwrite) {
                                          loadOk <- handleMetaFiles(metaStudyDir, dataStudySupplDir, metaStudySupplDir)
                                      }
                                      else {

                                          # Empty dir 
                                          if(length(dir(metaStudyDir, all.files=FALSE)) == 0){
                                              loadOk <- handleMetaFiles(metaStudyDir, dataStudySupplDir, metaStudySupplDir)
                                          }
                                      }

                                  }
                              }
                          }
                          else {

                              #####################
                              # For all studies 
                              #####################


                              if (!dir.exists(metaStudyDir)) {
                                  dir.create(metaStudyDir, recursive = TRUE)
                              }

                              if (length(dir(metaStudyDir, all.files=FALSE)) == 0) {
                                  loadOk <- handleMetaFiles(metaStudyDir, dataStudySupplDir, metaStudySupplDir)
                              }
                              else {
                                  if(overwrite) {
                                      loadOk <- handleMetaFiles(metaStudyDir, dataStudySupplDir, metaStudySupplDir)
                                  }
                              }

                          }
                      })
                  }
              } # end of dataStudyOnly






              ############################################
              # Loop through all files under prjDataDir
              ############################################

              existDataMetaLoadOk = FALSE

              # If all_released_study_info file downloaded successfully
              if (file.exists(extAllStudyInfoFile)) {



                  ##################################################
                  # Download study metadata of existing studies
                  ##################################################
                  # The data files exist under the project directory
                  if (dir.exists(prjDataDir)) {
                      # /c/Users/mars/Documents/myprj/gapwork/data/phs000001/phs000001.v3/data_dic

                      # Get all study-version dirs in the project directory
                      allDirs <- list.dirs(path = prjDataDir, full.names = TRUE, recursive = T)

                      # Keep only the item with study-acc match 
                      X <- allDirs
                      studyDirs <- X[grepl("phs\\d+\\.v\\d+$", X)]

                      if (length(studyDirs) > 0) {

                          retList <- lapply(studyDirs, function(studyDir) 
                          {


                              if (dir.exists(studyDir) & studyDir != prjDataDir ) {
                                  # Check match study
                                  if (inputPhsAcc != "") {

                                      if (phsAcc != "") {
                                          pattn = paste0(phsAcc, "$")
                                          match <- grepl(pattn, studyDir)
                                          if (match == TRUE) {
                                              loadOk <- downloadByStudy(studyDir)
                                              return(loadOk)
                                          }
                                      }
                                  }
                                  else {

                                      #####################################
                                      # Select only study-version dir
                                      #####################################
                                      # Example value of studyDir:
                                      #  ~/Documents/R_Dev/my_dbgapr_project21/gapwork/data/phs000572/phs000572.v7
                                      # "C:\\Users\\mars\\Documents\\R_Dev\\my_dbgapr_project21/gapwork/data/phs000001/phs000001.v3"

                                      # Get study-version directroies such as
                                      # "C:\\Users\\mars\\Documents\\R_Dev\\my_dbgapr_project21/gapwork/data/phs000001/phs000001.v3"
                                      # Match study-version 
                                      match <- grepl("phs\\d+\\.v\\d+$", studyDir)
                                      if (match == TRUE) {
                                          loadOk <- downloadByStudy(studyDir)
                                          return(loadOk)
                                      }
                                  }
                              }
                          })

                          # remove null value
                          retList <- unlist(retList[!sapply(retList, is.null)])
                          allTrue <- all(retList)
                          existDataMetaLoadOk <- allTrue

                          if (allTrue == F & overwrite == T) {
                              mesg = paste0("\nThe ftp download of at least one supplemental file failed. For more details, lookup the ERROR messages in the log file ", dataProcLog, " .\nThe problem could simply be a temporary glitch of the internet connection. To complete the download, run ftpDownload() again with the study of the file specified. See ?ftoDowload() for details. \nIf the problem persists, write to dbgap-help@ncbi.nlm.nih.gov for help.\n")
                              message(mesg)

                              type = 'process'
                              level = 'error'
                              show = F
                              writeLog(object,  type = type, level = level, message = mesg, show = show)
                          }



                      }
                      else {
                          mesg = paste0("\nThere no study directory found under the project data directory. Checkout ?prepareData() to see how to move data files to the directory. --- ",  prjDataDir, "\n") 
                          message(mesg)

                          type = 'process'
                          level = 'error'
                          show = F
                          writeLog(object,  type = type, level = level, message = mesg, show = show)
                      }

                  }


              }

              return (existDataMetaLoadOk)


          })


# ---------------------------
# Method: recordPrjFileInfo
# ---------------------------

#' Extract and save phenotype file meta-info
#'
#' The method looks for phenotype files in the project directory, extracts, and saves the file attribute information to a file. The phenotype data files should be available under the project directory before calling this function. Checkout \code{\link{searchCopyPhenoFiles}} for more information if not. The resulting project file information is saved in ~/ncbi/dbgap_conf/pht_file_info_indiv.json. When a study accession is provided, it processes only the files of specified study.
#'
#' @name recordPrjFileInfo 
#' @param object Commons class object
#' @param ... There are optional arguments. 
#' @param phsAcc a character string. The dbGaP study accession.
#' @export recordPrjFileInfo
#' @return (invisible) a data frame. Meta-info of the dataset files under the prject directory.
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' recordPrjFileInfo(c)
#' # or 
#' recordPrjFileInfo(c, phsAcc="phs000001.v3.p1")
#'}


setGeneric(name = "recordPrjFileInfo",
           def = function(object, ...) {
               standardGeneric("recordPrjFileInfo")
           })

#' @describeIn recordPrjFileInfo 
setMethod(
          f = "recordPrjFileInfo",
          signature = c("Commons"),
          definition = function(object, ..., phsAcc = "") {

              prjDotDir = object@prjDotDir
              prjDir = object@prjDir
              prjDataDir = object@prjDataDir
              configFile = object@configFile

              # Example: "C:/Users/mars/Documents/.dbgapr/pht_file_info_indiv.json"
              fileInfoFile = object@fileInfoFile
              # Example: "C:/Users/mars/Documents/.dbgapr/archived_log"
              fileInfoArchDir = object@fileInfoArchDir

              inputPhsAcc = phsAcc
              if (phsAcc != "") {
                  phsAcc <- cleanObjAcc(object, acc = phsAcc, type = 'phs') 
              }

              ###################################################
              # ATTN! Remove existing pht_file_info_indiv.json
              ###################################################

              #### Determine move file_info file to archive or not #### 
              # Rules for archive management
              # 1. If size reach default 0MB (not size limit) and the file_info file is older than ond day,  rename it with dateInfo and move to file_info_archive directory. 
              # 2. Remove archived file that is older than default 90 days.
              manageArchive(object, workFile = fileInfoFile, archiveDir = fileInfoArchDir, type = 'infojson', maxSizeInMb = 0, keptDay = 90, minFileAge = 1)

              # Loop through all files under prjDataDir
              if (dir.exists(prjDataDir)) {


                  # S3 function
                  processFnames <- function(prjDataDir, pattn, inputPhsAcc, phsAcc) {

                      fnames <- list.files(path = prjDataDir ,recursive = T, pattern = pattn)


                      # Process files under userDataDir
                      message("\nProcessing and parsing meta-info of the data files. It may take quite a while ...\n")

                      ##########################################
                      # Parse each pht file info and record it 
                      ##########################################

                      ##########################################
                      # ATTN! Removee existing info json
                      ##########################################
                      # Important! Remove existing pht_file_info_indiv.json and recreate it every time.

                      startRowCount = 0
                      removedRowCount = 0
                      finalRowCount = 0

                      if (file.exists(fileInfoFile)) {

                          # deal with zero size file
                          if (file.info(fileInfoFile)$size == 0) {
                              file.remove(fileInfoFile)
                          }
                          else {
                              ####### Process all Studies if phsAcc not provided ######

                              #################################################
                              # Remove fileInfoFile if process all studies
                              #################################################
                              if (inputPhsAcc == "") {
                                  file.remove(fileInfoFile)
                              }
                              else {
                                  if (phsAcc != "") {
                                      ###### Process specified study if phsAcc provided ######

                                      ################################################
                                      # Remove the rows of the study 
                                      ################################################
                                      # Read fileInfoFile json into a dataframe
                                      savedDF <- fromJSON(fileInfoFile, flatten=TRUE)
                                      startRowCount = nrow(savedDF) 


                                      # Remove all rows match the study
                                      studyRemovedDF <- savedDF[savedDF$fStAcc != phsAcc, ]
                                      removedRowCount = nrow(studyRemovedDF) 


                                      # Remove the file if no row left after study match removed 
                                      if (nrow(studyRemovedDF) == 0) {
                                          file.remove(fileInfoFile)
                                      }
                                      else {
                                          # Re-write fileInfoFile 
                                          studyRemovedJson <- toJSON(studyRemovedDF, pretty=T)
                                          write(studyRemovedJson, file = fileInfoFile, ncolumns = if(is.character(studyRemovedJson)) 1 else 5, append = F, sep = "\n")
                                      }
                                  }
                              }
                          }
                      }

                      # parse and copy files 
                      lapply(fnames, FUN=function(x) writeFileInfoJson(object, dbgapFile = file.path(prjDataDir, x)))

                      # Read fileInfoFile json into a dataframe
                      fileInfoDF <- fromJSON(fileInfoFile, flatten=TRUE)
                      finalRowCount = nrow(fileInfoDF)

                      # Added / processed row count 
                      addRowCount = finalRowCount - removedRowCount 

                      type = 'setup'
                      level = 'info'
                      show = T
                      mesg = paste("Total ", addRowCount, " files are processed.", sep="")
                      writeLog(object,  type = type, level = level, message = mesg, show = show)


                      return (invisible(fileInfoDF))
                  }

                  # Search for phenotype .gz or .txt(ungzip) file 
                  pattn = ""
                  if (inputPhsAcc == "") {

                      ###################################################
                      # Process all Studies if phsAcc not provided
                      ###################################################
                      pattn = paste("phs[0-9]+", ".*pht[0-9]+.*.[gz|txt]$", sep="")
                      processFnames(prjDataDir, pattn, inputPhsAcc, phsAcc)

                      # Find files that ends with ncbi_enc 
                      #studyDirs <- list.files(path=prjDataDir, pattern="phs\\d+\\.v\\d+$",  recursive = TRUE) 
                      #print(studyDirs)
                  }
                  else {

                      if (phsAcc != "") {
                          pattn = paste(phsAcc, ".*pht[0-9]+.*.[gz|txt]$", sep="")

                          ###################################################
                          # Process specified study if phsAcc provided
                          ###################################################
                          processFnames(prjDataDir, pattn, inputPhsAcc, phsAcc)
                      }
                  }
              }
              else {
                  type = 'setup'
                  level = 'error'
                  show = T
                  mesg = paste("The input path of userDataDir doesn't not exist. Check to make sure its a valid real path and try it again. --- ", prjDataDir, " No file is copied.", sep="")
                  writeLog(object,  type = type, level = level, message = mesg, show = show)
              }


          })


# ----------------------------- 
# Method: mergeDatasetConsent
# ----------------------------- 

#' Merge dataset data of different consents
#'
#' The method searches for all dataset files under the project directory and merges the files that have the same dataset accession but belong to different consents.
#'
#' @param object Commons class object.
#' @param ... There are optional arguments. 
#' @param phsAcc a character string. The dbGaP study accession.
#' @param overwrite a logical value. (optional) If TRUE (default), overwrites existing files. If FALSE, skips the merge for entire study if the merged file directory of the respective study is not empty.
#' @return a data frame. (invisible) The meta-info of merged data files.
#' @export mergeDatasetConsent
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' mergeDatasetConsent(c)
#' mergeDatasetConsent(c, phsAcc = "phs000001.v3.p1")
#'}

setGeneric(
           name = "mergeDatasetConsent",
           def = function(object, ...) {
               standardGeneric("mergeDatasetConsent")
           })

#' @describeIn mergeDatasetConsent A method of class Commons 
setMethod(
          f = "mergeDatasetConsent",
          signature = c("Commons"),
          definition = function(object, ..., phsAcc = "", overwrite = TRUE) {

              inputPhsAcc = phsAcc
              if (phsAcc != "") {
                  phsAcc <- cleanObjAcc(object, acc = phsAcc, type = 'phs') 
              }

              prjDotDir = object@prjDotDir
              prjDir = object@prjDir
              prjDataDir = object@prjDataDir 

              fileInfoFile = object@fileInfoFile
              #comboInfoFile = object@comboInfoFile
              lastComboInfoFile = object@lastComboInfoFile

              ########################################
              # Loop through all available studies
              ########################################
              # Get the list of all available study accessions
              fileInfoDF <- fromJSON(fileInfoFile, flatten=TRUE)
              studyAccDF <- unique(fileInfoDF[c("fStAcc")])
              studyAccList = studyAccDF$fStAcc

              ################################################
              # Check file existance to decide merge or not
              ################################################
              # S3 function
              mergeStudyData <- function(thisPhsAcc, overwrite) {

                  #####################################
                  # Build study 'original' file path 
                  #####################################
                  # original file dir: /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/phs000001/phs000001.v3/original
                  # merged file dir: /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/phs000001/phs000001.v3/combined
                  parseIdsFromStAcc =  parseIdsFromStAcc(object, phsAcc = thisPhsAcc)
                  thisPhsAccNoVer = parseIdsFromStAcc$phsAccNoVer
                  origDir <- file.path(prjDataDir, thisPhsAccNoVer, thisPhsAcc, 'original') 
                  combDir <- file.path(prjDataDir, thisPhsAccNoVer, thisPhsAcc, 'combined') 

                  ####################
                  # S3 function
                  ####################
                  checkAndMerge <- function(origDir, thisPhsAcc) {
                      # Check if the study 'orginal' dir is empty 

                      origDirFileList <- list.files(origDir, all.files = TRUE, include.dirs = TRUE, no.. = TRUE)
                      if (length(origDirFileList) == 0) {
                          cat("\n")
                          type = 'process'
                          level = 'error'
                          show = T
                          mesg = paste(
                                       "The study dataset files are not found in the following directory. To merge the dataset data, the dataet files should be in place. Check ?searchCopyPhenoFiles for more information about how to copy the dataset file\n   ", " --- ", origDir, "\n", sep=""
                                       ) 
                          writeLog(object,  type = type, level = level, message = mesg, show = show)
                          cat("\n")
                      }
                      else {
                          comboInfoDF <- mergeDatasetConsentByStudy(object, phsAcc = thisPhsAcc)
                          return(invisible(comboInfoDF))
                      }

                  }

                  # overwrite switch
                  if (overwrite == TRUE) {

                      #######################
                      # Merge regardless
                      #######################
                      comboInfoDF <- checkAndMerge(origDir, thisPhsAcc)
                      return(invisible(comboInfoDF))
                  }
                  else {

                      # Check if the study 'combined' dir is empty 
                      combDirFileList <- list.files(combDir, all.files = TRUE, include.dirs = TRUE, no.. = TRUE)
                      if (length(combDirFileList) == 0) {
                          comboInfoDF <- checkAndMerge(origDir, thisPhsAcc)
                          return(invisible(comboInfoDF))
                      }
                      else {
                          cat("\n")
                          type = 'process'
                          level = 'error'
                          show = T
                          mesg = paste("Skipped consent file merge for the study since the respective merged file directory is not empty. Call the functioin with 'overwrite = TRUE' to overwrite the existing merged files.,\n   ", " --- ", combDir, "\n", sep="") 
                          writeLog(object,  type = type, level = level, message = mesg, show = show)
                          cat("\n")
                      }

                  } # end overwrite == TRUE

              } # end S3 function mergeStudyData



              #####################################################
              # Process specified study when phsAcc provided
              #####################################################
              if (inputPhsAcc != "") {

                  # If input phsAcc valid
                  if (phsAcc != "") {
                      comboInfoDF <- mergeStudyData(phsAcc, overwrite)
                      return(invisible(comboInfoDF))
                  }
              }
              # Process all studies when no phsAcc provided
              else {

                  comboInfoDFList <- lapply(studyAccList, function(phsAcc) {
                                              comboInfoDF <- mergeStudyData(phsAcc, overwrite)
                                              return(invisible(comboInfoDF))
                                   })

                                   # Remove null from list
                                   nullRemovedList <- comboInfoDFList[!sapply(comboInfoDFList, is.null)]

                                   mergedInfoDF <- do.call("rbind", nullRemovedList)
                                   return(invisible(mergedInfoDF))



              }

          })





# --------------------------- 
# Method: accInfo 
# --------------------------- 

#' Display meta-info of dbGaP objects 
#'
#' The method displays the meta-info of the dbGaP objects given an object accession. The object can be a dbGaP study, dataset, or variable. 
#'
#' @param object Commons class object.
#' @param acc a character string. The dbGaP study, dataset, or variable accession. 
#' @param display a logical value. (optional) If TRUE (default), console displays the meta-info. If FALSE, not display. 
#' @param ... There are optional arguments.
#' @return a data frame. (invisible) The meta-info of the input object.
#' @export accInfo
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' df <- accInfo(c, acc = 'phs000001.v3.p1')
#' # or
#' df <- accInfo(c, acc = 'pht000824.v5')
#' # or
#' df <- accInfo(c, acc = 'phv00000090.v2')     # numeric with units
#' # or
#' df <- accInfo(c, acc = 'phv00000122.v2')     # categorical with code-value 
#'}

# or
# accInfo(c, acc = 'phs000651.v7.p10')		# no match study 
# or
# accInfo(c, acc = 'phs000724.v4.p10')		# has parent study
# or
# accInfo(c, acc = 'pht000824.v5')
# or 
# accInfo(c, acc = 'phv00073737.v2')
# or
# accInfo(c, acc = 'pht000373.v2')
# or
# accInfo(c, acc = 'phv00000622.v1')        # phs000007.v29
#
# Study that has no data, only metadata 
# accInfo(c, acc = 'phv00273621.v1')        # phs001255.v1
# 

setGeneric(
           name = "accInfo",
           def = function(object, acc, ...) {
               standardGeneric("accInfo")
           })

#' @describeIn accInfo returns dataset accession
setMethod(
          f = "accInfo",
          signature = c("Commons"),
          definition = function(object, acc, ..., display = TRUE) {

              prjMetaDir <- object@prjMetaDir

              # Trim heading and trailing space
              acc <- trimws(acc)
              type = ''


              # Check accession general format
              pattns = stringr::str_match(acc, "^(ph)(\\w)\\d+")
              typePatt = pattns[3]

              typeOk = FALSE 
              if (nchar(typePatt) > 0 && !is.na(typePatt)) {
                  if (typePatt == 's') {
                      typeOk = TRUE 
                      type = 'phs'
                  }
                  else if (typePatt == 't') {
                      typeOk = TRUE 
                      type = 'pht'
                  }
                  else if (typePatt == 'v') {
                      typeOk = TRUE 
                      type = 'phv'
                  }
              }

              if (typeOk == TRUE) {

                  # Final check accession of each type 
                  if (nchar(type) > 0) {
                      acc <- cleanObjAcc(object, acc = acc, type = type)  
                  }

                  if (acc != "") {

                      prjDataDir = object@prjDataDir 
                      prjDotDir = object@prjDotDir   
                      prjDir = object@prjDir  
                      configFile = object@configFile  
                      fileInfoFile = object@fileInfoFile 
                      fileInfoArchDir = object@fileInfoArchDir  

                      # Get the external studyInfo data
                      allStudyInfo <- getExtData(object, type = 'study')

                      if (!is.null(allStudyInfo)) {

                          # Example location of study (no-version) data dictionary
                          # /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/general_info
                          dataGenInfoDir = file.path(prjDataDir,'general_info') 
                          if (!dir.exists(dataGenInfoDir)) {
                              dir.create(dataGenInfoDir, showWarnings = TRUE, recursive = T, mode = "0777")
                          }

                          # Make sure fileInfoFile is created
                          if (!file.exists(fileInfoFile)) {
                              type = 'process'
                              level = 'error'
                              show = T
                              mesg = paste("The phenotype file info file is not found. It needs to be created first. Checkout ?recordPrjFileInfo for more information.  --- ", fileInfoFile, "  ", sep="")
                              writeLog(object,  type = type, level = level, message = mesg, show = show) 
                          }
                          else {

                              fileInfoDF <- fromJSON(fileInfoFile, flatten=TRUE)

                              studyAccDF <- unique(fileInfoDF[c("fStAcc", "fStAccNoP")])
                              studyAccList = studyAccDF$fStAccNoP

                              #################################
                              # Process study accession
                              #################################

                              if (type == 'phs') {

                                  # Go through all available StudyAcc
                                  availStudyInfoList <- lapply(studyAccList, function(x, inputPhsAcc = acc) {
                                      thisStudyAcc <- x

                                      if (thisStudyAcc == acc) {
                                          avalThisStudyInfo <- subset(allStudyInfo,allStudyInfo$this_study_accession == thisStudyAcc)
                                          return (avalThisStudyInfo)
                                      }
                                  })  

                                  # Subset allStudyInfo DF 
                                  matchStudyList <- availStudyInfoList[!sapply(availStudyInfoList, is.null)] 
                                  subDF <- dplyr::filter(allStudyInfo, allStudyInfo$this_study_accession == acc)
                                  
                                  hasDataInPrj = FALSE
                                  studyName = ''
                                  studyDiease = ''
                                  rootStudyAcc = ''

                                  finalStudyDF = data.frame()

                                  if (length(matchStudyList) > 0) {

                                      # Has data file under prj dir
                                      hasDataInPrj = TRUE 

                                      # Display Info
                                      studyName <- matchStudyList[[1]]$name
                                      studyDisease <- matchStudyList[[1]]$disease
                                      rootStudyAcc  <- matchStudyList[[1]]$root_study_accession

                                      finalStudyDF <- matchStudyList[[1]]
                                  }
                                  else {

                                      ###########################################
                                      # If the study not in project directory  
                                      # look for all released studies 
                                      ###########################################

                                      # Subset allStudyInfo DF 
                                      phsAcc = acc
                                      thisStudyInfo <- subset(allStudyInfo, allStudyInfo$this_study_accession == phsAcc)

                                      if (nrow(thisStudyInfo)) {
                                          finalStudyDF <- thisStudyInfo 

                                          studyName <- thisStudyInfo[['name']] 
                                          studyDisease <- thisStudyInfo[['disease']] 
                                          rootStudyAcc <- thisStudyInfo[['root_study_accession']] 
                                      }
                                  }

                                  
                                  if (!hasDataInPrj) {

                                      errmsg = paste(
                                          "\n[INFO] The meta-info of the input accession ", acc, " is available. The study ", phsAcc, " however doesn't have any data file under the project directory.\n", sep=""
                                      )
                                      cat(errmsg)
                                  }

                                  if (studyName != '') {

                                      if (display == TRUE) {
                                          cat("\n")
                                          info <- sprintf("%-13s %-3s %s", "Object Type", ":", "Study") 
                                          cat(info)
                                          cat("\n")
                                          info <- sprintf("%-13s %-3s %s", "Accession", ":", acc) 
                                          cat(info)
                                          cat("\n")
                                          info <- sprintf("%-13s %-3s %s", "Name", ":", studyName) 
                                          cat(info)
                                          cat("\n")
                                          info <- sprintf("%-13s %-3s %s", "Disease", ":", studyDisease) 
                                          cat(info)
                                          cat("\n")
                                      }

                                      # Is parent study
                                      if (rootStudyAcc == acc) {

                                          ##########################
                                          # Lookup child studies
                                          ##########################
                                          childStudyDF <- subset(allStudyInfo, allStudyInfo$root_study_accession == acc & allStudyInfo$this_study_accession != acc, select = c('root_study_accession', 'this_study_accession'))
                                          childStudyList <- as.list(childStudyDF$this_study_accession)
                                          childStudyAccCombo = paste(childStudyList, collapse = ', ')

                                          if (length(childStudyList) > 0) {
                                              info <- sprintf("%-13s %-3s %s", "Child Study", ":", childStudyAccCombo) 

                                              if (display == TRUE) {
                                                  cat(info)
                                                  cat("\n")
                                              }
                                          }
                                      }
                                      # Has parent study
                                      else {
                                          info <- sprintf("%-13s %-3s %s", "Parent Study", ":", rootStudyAcc) 
                                          if (display == TRUE) {
                                              cat(info)
                                              cat("\n")
                                          }
                                      }
                                      if (display == TRUE) {
                                          cat("\n")
                                      }


                                      return (invisible(finalStudyDF))
                                  }
                                  else {

                                      errmsg = paste(
                                          "\n[ERROR] The input dbGaP object accession ", acc, " doesn't match any study under the project metadata directory.\n", sep=""
                                      )
                                      cat(errmsg)

                                      return (NULL) 
                                  }


                              }

                              #################################
                              # Process dataset accession
                              #################################
                              hasDataInPrj = FALSE


                              if (type == 'pht') {

                                  # New! Get extData all_study_info 
                                  allDatasetInfoDF <- getAllDatasetInfo(object)
                                  subDF <- dplyr::filter(allDatasetInfoDF, allDatasetInfoDF$dataset_accession == acc)

                                  studyAcc = ''
                                  datasetAcc = ''
                                  datasetName = ''
                                  datasetDesc = ''
                                  finalPhtDF = data.frame()

                                  if (nrow(subDF) > 0) {
                                      hasDataInPrj = TRUE 

                                      studyAcc <- subDF$study_accession
                                      datasetAcc <- subDF$dataset_accession
                                      datasetName <- subDF$name
                                      datasetDesc <- subDF$description

                                      finalPhtDF = subDF 

                                  }
                                  else {

                                      ###############################################
                                      # Expand the search to all released studies 
                                      ###############################################

                                      finalPhtDF <- getMetaByObjAcc(object, acc = acc, type = 'dataset')

                                      studyAcc <- finalPhtDF$study_accession
                                      datasetAcc <- finalPhtDF$dataset_accession
                                      datasetName <- finalPhtDF$name
                                      datasetDesc <- finalPhtDF$description

                                      errmsg = paste(
                                          "\n[INFO] The meta-info of the input accession ", acc, " is available. The study ", studyAcc, " however doesn't have any data file under the project directory.\n", sep=""
                                      )
                                      cat(errmsg)

                                  }

                                  if (datasetName != "") {
                                      if (display == TRUE) {
                                          cat("\n")
                                          info <- sprintf("%-13s %-3s %s", "Object Type", ":", "Dataset") 
                                          cat(info)
                                          cat("\n")
                                          info <- sprintf("%-13s %-3s %s ( %s )", "Accession", ":", acc, studyAcc) 
                                          cat(info)
                                          cat("\n")
                                          info <- sprintf("%-13s %-3s %s", "Name", ":", datasetName) 
                                          cat(info)
                                          cat("\n")
                                          info <- sprintf("%-13s %-3s %s", "Description", ":", datasetDesc) 
                                          cat(info)
                                          cat("\n")
                                          cat("\n")
                                      }
                                  }
                                  else {
                                      errmsg = paste(
                                          "\n[ERROR] The input dbGaP object accession ", acc, " doesn't match any study dataset under the project metadata directory.\n", sep=""
                                      )
                                      cat(errmsg)

                                      return (NULL) 
                                  }

                                  return (invisible(finalPhtDF))
                              }

                              #################################
                              # Process variable accession
                              #################################

                              if (type == 'phv') {
                                  # Get the external studyInfo data
                                  #studyDatasetInfo <- read.table(extStudyDatasetInfoFile, header = T, fill = TRUE, quote = "", sep ='\t', stringsAsFactors = FALSE, encoding="UTF-8")  

                                  varAcc = ''
                                  varType = ''
                                  studyAcc = ''
                                  datasetAcc = ''
                                  datasetName = ''
                                  varUnit = ''
                                  varName = ''
                                  varDesc = ''
                                  codeValCombo = ''
                                  finalPhtDF = data.frame()

                                  availStudyDatasetVarInfoList <- lapply(studyAccList, function(thisPhsAcc, inputPhvAcc = acc) {

                                      varInfoDF <- getExtData(object, type = 'variable', phsAcc = thisPhsAcc)

                                      if (!is.null(varInfoDF)) {

                                          subDF <- dplyr::filter(varInfoDF, varInfoDF$variable_accession == inputPhvAcc)

                                          # Loop through all available studies.
                                          # Only match study has non-zero retura.n
                                          if (nrow(subDF) > 0) {

                                              # Multipe dataset versions could have the same phvAcc.
                                              # Furhter filter as below if that is the case.
                                              if (nrow(subDF) > 1) {
                                                  randPhtAcc <- subDF$dataset_accession[1]
                                                  # Get avaiable phtAcc
                                                  availPhtAcc <- getAvailPhtVer(object, randPhtAcc=randPhtAcc, phsAcc=thisPhsAcc)

                                                  subDF <- dplyr::filter(subDF, subDF$dataset_accession == availPhtAcc)
                                              }
                                              return(subDF)
                                          }
                                      }

                                  })  

                                  # Remove null items
                                  matchStudyDatasetVarList <- availStudyDatasetVarInfoList[!sapply(availStudyDatasetVarInfoList, is.null)] 

                                  codeValCombo = NA 
                                  if (length(matchStudyDatasetVarList) > 0) {

                                      # Display Info
                                      varAcc <- matchStudyDatasetVarList[[1]]$variable_accession
                                      varType <- matchStudyDatasetVarList[[1]]$calculated_type
                                      studyAcc <- matchStudyDatasetVarList[[1]]$study_accession
                                      datasetAcc <- matchStudyDatasetVarList[[1]]$dataset_accession
                                      datasetName <- matchStudyDatasetVarList[[1]]$dataset_name
                                      varUnit <- matchStudyDatasetVarList[[1]]$units
                                      varName <- matchStudyDatasetVarList[[1]]$name
                                      varDesc <- matchStudyDatasetVarList[[1]]$description

                                      ###############################
                                      # Build variable CodeValCombo
                                      ###############################
                                      codeValCombo <- buildVarCodeValCombo(object, catVarAcc = varAcc, phsAcc = studyAcc)


                                      # Append codeValCombo as a new column
                                      matchStudyDatasetVarDF <- matchStudyDatasetVarList[[1]]
                                      matchStudyDatasetVarDF['code_value_combo'] <- c(codeValCombo) 

                                  }
                                  else {


                                      finalPhvDF <- getMetaByObjAcc(object, acc = acc, type = 'variable')

                                      varAcc <- finalPhvDF$variable_accession
                                      varType <- finalPhvDF$dataset_type_category
                                      studyAcc <- finalPhvDF$study_accession
                                      datasetAcc <- finalPhvDF$dataset_accession
                                      datasetName <- finalPhvDF$dataset_name
                                      varUnit <- finalPhvDF$units
                                      varName <- finalPhvDF$name
                                      varDesc <- finalPhvDF$description
                                      codeValCombo <- finalPhvDF$code_value_combo
                                      maleCount <- finalPhvDF$male_count
                                      femaleCount <- finalPhvDF$female_count
                                      max <- finalPhvDF$max
                                      min <- finalPhvDF$min
                                      sd <- finalPhvDF$sd
                                      nonNullCount <- finalPhvDF$non_null_count
                                      nullCount <- finalPhvDF$null_count

                                      errmsg = paste(
                                          "\n[INFO] The meta-info of the input accession ", acc, " is available. The study ", studyAcc, " however doesn't have any data file under the project directory.\n", sep=""
                                      )
                                      cat(errmsg)

                                  }

                                  if (varName != "") {

                                      if (display == TRUE) {
                                          cat("\n")
                                          info <- sprintf("%-13s %-3s %s", "Object Type", ":", "Variable") 
                                          cat(info)
                                          cat("\n")
                                          info <- sprintf("%-13s %-3s %s ( %s, %s )", "Accession", ":", varAcc, studyAcc, datasetAcc)
                                          cat(info)
                                          cat("\n")
                                          info <- sprintf("%-13s %-3s %s ( %s )", "Name", ":", varName, varDesc) 
                                          cat(info)
                                          cat("\n")
                                          info <- sprintf("%-13s %-3s %s", "Value Type", ":", varType) 
                                          cat(info)
                                          cat("\n")
                                          if (!is.na(codeValCombo)) {
                                              info <- sprintf("%-13s %-3s %s", "Code Value", ":", codeValCombo) 
                                              cat(info)
                                              cat("\n")
                                          }
                                          if (!is.na(varUnit)) {
                                              info <- sprintf("%-13s %-3s %s", "Units", ":", varUnit) 
                                              cat(info)
                                              cat("\n")
                                          }
                                          info <- sprintf("%-13s %-3s %s ( %s )", "Dataset", ":", datasetAcc, datasetName) 
                                          cat(info)
                                          cat("\n")
                                          info <- sprintf("%-13s %-3s %s", "Study", ":", studyAcc) 
                                          cat(info)
                                          cat("\n")

                                          if (!is.na(maleCount)) {
                                              info <- sprintf("%-13s %-3s %s", "Male Count", ":", maleCount) 
                                              cat(info)
                                              cat("\n")
                                          }
                                          if (!is.na(femaleCount)) {
                                              info <- sprintf("%-13s %-3s %s", "Female Count", ":", femaleCount) 
                                              cat(info)
                                              cat("\n")
                                          }
                                          if (!is.na(max)) {
                                              info <- sprintf("%-13s %-3s %s", "Max", ":", max) 
                                              cat(info)
                                              cat("\n")
                                          }
                                          if (!is.na(min)) {
                                              info <- sprintf("%-13s %-3s %s", "Min", ":", min) 
                                              cat(info)
                                              cat("\n")
                                          }
                                          if (!is.na(sd)) {
                                              info <- sprintf("%-13s %-3s %s", "SD", ":", sd) 
                                              cat(info)
                                              cat("\n")
                                          }
                                          if (!is.na(nonNullCount)) {
                                              info <- sprintf("%-13s %-3s %s", "Non-null", ":", nonNullCount) 
                                              cat(info)
                                              cat("\n")
                                          }
                                          if (!is.na(nullCount)) {
                                              info <- sprintf("%-13s %-3s %s", "Null", ":", nullCount) 
                                              cat(info)
                                              cat("\n")
                                          }

                                      }

                                      return (invisible(finalPhtDF))
                                  }
                                  else {
                                      errmsg = paste(
                                          "\n[ERROR] The input dbGaP object accession ", acc, " doesn't match any study dataset under the project metadata directory.\n", sep=""
                                      )
                                      cat(errmsg)
                                  }


                              }
                          } # end if fileInfoFile not exist 


                      } # !is.null(allStudyInfo)


                  } # end acc != ""


              }
              else {
                  errmsg = paste(
                                 "\n[ERROR] The format of the input dbGaP object accession is wrong, ", 
                                 acc, 
                                 ". For study accession, the correct format is like phs000001.v1 (phs followed by zero padded 6 digits); for dataset it is like pht000001.v1 (phs followed by zero padded 6 digits); For variable it is like phv00000001.v1 (phv followed by zero padded 8 digits).\n", sep=""
                                 )
                  cat(errmsg)
              }
          })


# --------------------------- 
# Method: getAllStudyInfo 
# --------------------------- 

#' Get meta-info of all studies 
#'
#' The method returns meta-info of all studies available in the project directory. In case of cohort studies, both parent and child studies (also called root- and sub-studies) are included.
#'
#' @param object Commons class object.
#' @param ... There are optional arguments. 
#' @param dataStudyOnly a logical value. When TRUE (default), only downloads the dataset and variable metadata of the stdudies that have data files in the project directory.  When FALSE, downloads the dataset and variable metadata of all dbGaP released studies, regardless the actual phenotype data files of the studies are downloaded or not. 
#' @param showAs a character string. (optional) When the value is 'table', displays the data as a table through a platform specific table viewer; When it is 'json', displays the json text through a plain text editor; When it is 'text', displays in a brief left-justified text format.
#' @param editor a character string. (optional) The name of your favorite plain text editor. It should be executable from a command-line prompt of the respective platform. For example, notepad (Windows), vim, emacs (Unix), gedit (Ubuntu), nedit (CentOS), etc.
#' @return a data frame. The meta-info of all studies. For the table row of a non-cohort study or a parent study in a chort, the root_study and this_study are identical. For the table row of a child- (or sub-) study in a cohort, the this_study column represents the child study, and the root_study column represents the parent study.
#' @export getAllStudyInfo
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' getAllStudyInfo(c)
#' # or
#' getAllStudyInfo(c, showAs = 'json', editor = 'notepad')
#' # or
#' getAllStudyInfo(c, showAs = 'text', editor = 'gedit')
#'}


setGeneric(
           name = "getAllStudyInfo",
           def = function(object, ...) {
               standardGeneric("getAllStudyInfo")
           })

#' @describeIn getAllStudyInfo A method of class Commons 
setMethod(
          f = "getAllStudyInfo",
          signature = c("Commons"),
          definition = function(object, ..., dataStudyOnly = TRUE, showAs = "", editor = "") {

              prjDataDir = object@prjDataDir 
              fileInfoFile = object@fileInfoFile 

              # New!
              allStudyInfo <- getExtData(object, type = 'study')

              if (!is.null(allStudyInfo)) {

                  # Example location of study (no-version) data dictionary
                  # /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/general_info
                  allStudyInfoDir = file.path(prjDataDir,'all_study_info') 

                  if (!dir.exists(allStudyInfoDir)) {
                      dir.create(allStudyInfoDir, showWarnings = FALSE, recursive = TRUE, mode = "0777")
                  }


                  if (dataStudyOnly) {

                      if (!file.exists(fileInfoFile)) {
                          type = 'process'
                          level = 'error'
                          show = T
                          mesg = paste("The phenotype file info file is not found. It needs to be created first. Check ?recordPrjFileInfo for more information.\n   --- ", fileInfoFile, "  ", sep="")
                          writeLog(object,  type = type, level = level, message = mesg, show = show) 

                          return()
                      }
                      else {

                          fileInfoDF <- fromJSON(fileInfoFile, flatten=TRUE)
                          studyAccDF <- unique(fileInfoDF[c("fStAcc")])
                          studyAccList = studyAccDF$fStAcc

                          availStudyInfoList <- lapply(studyAccList, function(thisStudyAcc) {
                              avalThisStudyInfo <- subset(allStudyInfo, allStudyInfo$this_study_accession == thisStudyAcc)
                          })  

                          # Merge all var_report of all phts and save
                          availStudyInfoDF <- do.call('rbind', availStudyInfoList)
                      }
                  }
                  else {
                      availStudyInfoDF <- allStudyInfo 
                  }


                  ###################################################################
                  # Save the data fo json, csv, and text file for further viewing
                  ###################################################################
                  # Make sure fileInfoFile is created


                  # Save to rds 
                  availStudyInfoFileName = paste('all_study_info.rds', sep='')
                  availStudyInfo_rdsFile = file.path(allStudyInfoDir, availStudyInfoFileName)
                  saveRDS(availStudyInfoDF, file=availStudyInfo_rdsFile)

                  # Save to csv
                  availStudyInfoCsvFileName = paste('all_study_info.csv', sep='')
                  availStudyInfo_csvFile = file.path(allStudyInfoDir, availStudyInfoCsvFileName)
                  #write.csv(availStudyInfoDF, file = availStudyInfo_csvFile, row.names=FALSE)
                  write.table(availStudyInfoDF, file = availStudyInfo_csvFile, sep = "\t", row.names = FALSE)

                  # Save to json 
                  availStudyInfoJson <- toJSON(availStudyInfoDF, pretty=T)
                  availStudyInfoJsonFileName = paste('all_study_info.json', sep='')
                  availStudyInfo_jsonFile = file.path(allStudyInfoDir, availStudyInfoJsonFileName) 
                  write(availStudyInfoJson, file = availStudyInfo_jsonFile, ncolumns = if(is.character(availStudyInfoJson)) 1 else 5, append = F, sep = "\n")

                  # Save to left-justified text 
                  availStudyInfoSubDF = subset(availStudyInfoDF, select = c("root_study_accession", "this_study_accession", "disease", "name")) 
                  availStudyInfoTxtFileName = paste('all_study_info_brief_ljustify.txt', sep='')
                  availStudyInfo_txtFile = file.path(allStudyInfoDir, availStudyInfoTxtFileName)
                  gdata::write.fwf(x=availStudyInfoSubDF, file=availStudyInfo_txtFile, quote=F, justify="left", sep="    ")

                  if (showAs != "") {

                      viewAllStudyInfo(object, showAs = showAs, editor = editor)

                      # invisible return
                      return(invisible(availStudyInfoDF))
                  }
                  else {
                      return(availStudyInfoDF)
                  }

              }

          })


# -----------------------------
# Method: getAllDatasetInfo 
# -----------------------------

#' Get meta-info of all datasets
#'
#' The method returns meta-info of all datasets of the studies that have data files available under the project directory.
#'
#' @param object Commons class object.
#' @param ... There are optional arguments. 
#' @param showAs a character string. (optional) When the value is 'table', displays the data as a table through a platform specific table viewer; When it is 'json', displays the json text through a plain text editor; When it is 'text', displays in a brief left-justified text format.
#' @param editor a character string. (optional) The name of your favorite plain text editor. It should be executable from a command-line prompt of the respective platform. For example, notepad (Windows), vim, emacs (Unix), gedit (Ubuntu), nedit (CentOS), etc.
#' @return a data frame. The meta-info of all datasets available under the project directory.
#' @export getAllDatasetInfo
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' getAllDatasetInfo(c)
#' getAllDatasetInfo(c, showAs = 'json', editor='notepad')
#' getAllDatasetInfo(c, showAs = 'text', editor='gedit')
#'}


setGeneric(
           name = "getAllDatasetInfo",
           def = function(object, ...) {
               standardGeneric("getAllDatasetInfo")
           })

#' @describeIn getAllDatasetInfo A method of class Commons 
setMethod(
          f = "getAllDatasetInfo",
          signature = c("Commons"),
          definition = function(object, ..., showAs = "", editor = "") {

              prjDataDir = object@prjDataDir 
              fileInfoFile = object@fileInfoFile


              if (file.exists(fileInfoFile)) {

                  # Get the list of all available study accessions
                  fileInfoDF <- fromJSON(fileInfoFile, flatten=TRUE)
                  studyAccDF <- unique(fileInfoDF[c("fStAcc")])
                  studyAccList = studyAccDF$fStAcc

                  availStudyDatasetInfoList <- lapply(studyAccList, function(thisPhsAcc) {

                      # Compose file path
                      parseIdsFromPhsAcc =  parseIdsFromStAcc(object, phsAcc = thisPhsAcc)
                      thisPhsAccNoVer = parseIdsFromPhsAcc$phsAccNoVer
                      studyDatasetInfoDF <- getDatasetMetaByStudy(object, phsAcc = thisPhsAcc)


                      return (studyDatasetInfoDF)
                  })

                  # Remove null items
                  availStudyDatasetInfoList <- availStudyDatasetInfoList[!sapply(availStudyDatasetInfoList, is.null)] 


                  if (length(availStudyDatasetInfoList) > 0) {

                      

                      # Merge all var_report of all phts a#nd save
                      availStudyDatasetInfoDF <- do.call('rbind', availStudyDatasetInfoList)


                      # Example of saved dataset info files
                      # /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/phs000001/phs000001.v3/data_dic/combo_dump
                      # phs000001.v3_study_dataset_info_combo.rds
                      # phs000001.v3_study_dataset_info_combo.txt
                      # phs000001.v3_study_dataset_info_combo.json

                      #####################################
                      # Create all_study_info directory
                      #####################################
                      # Example location of study (no-version) data dictionary
                      # /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/all_study_info
                      allStudyInfoDir = file.path(prjDataDir,'all_study_info') 
                      if (!dir.exists(allStudyInfoDir)) {
                          dir.create(allStudyInfoDir, showWarnings = TRUE, recursive = T, mode = "0777")
                      }

                      # Save to rds 
                      availStudyDatasetInfoFileName = paste('all_study_dataset_info.rds', sep='')
                      availStudyDatasetInfo_rdsFile = file.path(allStudyInfoDir, availStudyDatasetInfoFileName)
                      saveRDS(availStudyDatasetInfoDF, file=availStudyDatasetInfo_rdsFile)

                      # Save to csv
                      availStudyDatasetInfoCsvFileName = paste('all_study_dataset_info.csv', sep='')
                      availStudyDatasetInfo_csvFile = file.path(allStudyInfoDir, availStudyDatasetInfoCsvFileName)
                      #write.csv(availStudyDatasetInfoDF, file = availStudyDatasetInfo_csvFile, row.names=FALSE)
                      write.table(availStudyDatasetInfoDF,, file = availStudyDatasetInfo_csvFile, sep = "\t", row.names = F)

                      # Convert to json and save 
                      availStudyDatasetInfoJson <- toJSON(availStudyDatasetInfoDF, pretty=T)
                      availStudyDatasetInfoJsonFileName = paste('all_study_dataset_info.json', sep='')
                      availStudyDatasetInfo_jsonFile = file.path(allStudyInfoDir, availStudyDatasetInfoJsonFileName) 
                      write(availStudyDatasetInfoJson, file = availStudyDatasetInfo_jsonFile, ncolumns = if(is.character(availStudyDatasetInfoJson)) 1 else 5, append = F, sep = "\n")

                      # Save to left-justified text 
                      availStudyDatasetInfoSubsetDF = subset(availStudyDatasetInfoDF, select = c("study_accession", "dataset_accession", "name")) 
                      availStudyDatasetInfoTxtFileName = paste('all_study_dataset_info_brief_ljustify.txt', sep='')
                      availStudyDatasetInfo_txtFile = file.path(allStudyInfoDir, availStudyDatasetInfoTxtFileName)
                      gdata::write.fwf(x=availStudyDatasetInfoSubsetDF, file=availStudyDatasetInfo_txtFile, quote=F, justify="left", sep="    ")

                      if (showAs != "") {
                          viewAllDatasetInfo(object, showAs = showAs, editor = editor)

                          # invisible return
                          return(invisible(availStudyDatasetInfoDF))
                      }
                      else {
                          # visible return
                          return (availStudyDatasetInfoDF)
                      }
                  }
                  else {
                      cat("[INFO] Only the dataset info of the studies available under the project directory is shown. Checkout ?getStudyDatasetInfo() to see how to retrive the dataset info for other studies.\n");
                  }
              }
              else {

                  type = 'setup'
                  level = 'error'
                  show = T
                  mesg = paste("The file-info-file is not found. Checkout ?prjConfig() and ?prepareData() to make sure the prject directory is setup and the data files are copied and processed. ---", fileInfoFile, sep="") 
                  writeLog(object,  type = type, level = level, message = mesg, show = show)
              }

          })



########################
# List of functions 
########################

# prjConfig
# getPrjDir
# prepareData
# searchCopyPhenoFiles
# ftpDownload
# recordPrjFileInfo
# mergeDatasetConsent
# accInfo
# getAllStudyInfo
# getAllDatasetInfo
