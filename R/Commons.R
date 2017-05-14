# +++++++++++++++++++
# Class: Commons 
# +++++++++++++++++++
NULL

#' Base class of dbgapr package 
#'
#' The class contains the methods commonly used by all classes and methods in the package. The related functions include setting up the user project directory, copying the decrypted dbGaP phenotype data (datasets) files to the project directory, ftp download of the meta-data (data dictionary) files, merging the datasets split into different consents groups, reviewing and retrieving the data and meta-data by accessions, etc.
#'
#' @export Commons 
#' @import stringr  
#' @import jsonlite 
#' @import gdata 
#' @import XML 
#' @import methods 
#' @import curl 
#' @import RCurl 
#' @importFrom dplyr group_by do distinct 
#' @importFrom grDevices dev.off rgb
#' @importFrom stats complete.cases cor median sd
#' @importFrom utils View download.file untar head read.csv read.table write.csv write.table 
#' @rdname Commons-class 
#' @examples 
#' \dontrun{
#'
#' c <- Commons()
#'}
Commons <- setClass(

                    # Set the name of the class
                    "Commons",

                    # Define the slots as empty 
                    #slots=character(0),

                    # The slots with an optional item of 'list' for args from class methods 
                    slots = list(
                                 prjDotDir = 'character',
                                 configFile = 'character',
                                 prjDir = 'character',

                                 fileInfoFile = 'character',
                                 fileInfoArchDir = 'character',
                                 lastFileInfoFile = 'character',
                                 prjSetupLog = 'character',

                                 prjDataDir = 'character', 
                                 prjTempDir = 'character', 
                                 procLogArchDir = 'character', 
                                 dataProcLog = 'character', 

                                 extDataDir = 'character',
                                 extStudyInfoFile = 'character',
                                 extStudyDatasetInfoFile = 'character',
                                 extPhenoSharedIdNamesFile = 'character',

                                 # New!
                                 extAllStudyInfoFile = 'character', 

                                 comboInfoFile = 'character',
                                 lastComboInfoFile = 'character',

                                 gapworkDir = 'character',
                                 dataDir = 'character',
                                 logDir = 'character',
                                 showErr = 'character'

                                 ),

                    # Set the default values for the slots
                    #prototype=list()

                    prototype = list(
                                     prjDotDir = '', 
                                     configFile = '', 
                                     prjDir = '',

                                     fileInfoFile = '',
                                     fileInfoArchDir = '',
                                     lastFileInfoFile = '',
                                     prjSetupLog = '',

                                     prjDataDir = '', 
                                     prjTempDir = '', 
                                     procLogArchDir = '', 
                                     dataProcLog = '', 
                                     prjSetupLog = '',

                                     extDataDir = '',
                                     extStudyInfoFile = '',
                                     extStudyDatasetInfoFile = '',
                                     extPhenoSharedIdNamesFile = '',

                                     extAllStudyInfoFile = '', 

                                     comboInfoFiel = '',
                                     lastComboInfoFiel = '',

                                     gapworkDir = '',
                                     dataDir = '',
                                     logDir = '',
                                     showErr = ''

                                     ), 

                    validity = function(object) {
                        errors <- character()

                    }, )

# ----------------------
# Method: initialize()
# ----------------------

#' Initialize class Commons
#'
#' The method initializes the Commons class, through which the package wide global variables are made avaiable through the class object.
#' @param .Object The class object. 
#' @param ... There arer optional arguments. 
#' @examples 
#' \dontrun{
#'
#' c <- Commons()
#'}

setMethod("initialize",
          "Commons",
          function(.Object, ...) {

              .Object <- callNextMethod()

              cat("Initializing class Commons ...\n")

              ##############################
              # Define home directory
              ##############################

              # Define project dot dir and config file
              
              # Changed from .dbgapr to ncbi/dbgapr_conf
              prjDotDir = file.path(Sys.getenv("HOME"), "ncbi", "dbgapr_conf")
              prjDotExtDataDir = file.path(prjDotDir, "supplemental_data")
              confFile = file.path(prjDotDir, "project_config.json")

              fileInfoArchDir = file.path(prjDotDir, "archived_log")
              fileInfoFile = file.path(prjDotDir, 'pht_file_info_indiv.json')
              lastFileInfoFile = file.path(fileInfoArchDir, 'last_pht_file_info_indiv.json')

              #comboInfoFile = file.path(prjDotDir, 'pht_file_info_combo.json')
              lastComboInfoFile = file.path(fileInfoArchDir, 'last_pht_file_info_combo.json')

              #extDataDir <- system.file("inst/extdata", package="dbgapr")
              #extStudyInfoFile <- file.path(extDataDir, 'root_and_this_study_info.csv')
              #extStudyDatasetInfoFile <- file.path(extDataDir, 'study_dataset_info.csv')
              #extPhenoSharedIdNamesFile <- file.path(extDataDir, 'phenogeno_shared_id_names.csv')

              # Ftp downloaded files
              #dotExtStudyInfoFile <- file.path(prjDotExtDataDir, 'root_and_this_study_info.csv')
              #dotExtStudyDatasetInfoFile <- file.path(prjDotExtDataDir, 'study_dataset_info.csv')
              #dotExtPhenoSharedIdNamesFile <- file.path(prjDotExtDataDir, 'phenogeno_shared_id_names.csv')

              # New! all-study-info file in dot dir
              extAllStudyInfoFile <- file.path(prjDotExtDataDir, 'all_released_study_info.txt.gz')
              


              .Object@prjDotDir <- prjDotDir
              .Object@configFile <- confFile 
              .Object@fileInfoArchDir <- fileInfoArchDir 
              .Object@fileInfoFile <- fileInfoFile 
              .Object@lastFileInfoFile <- lastFileInfoFile 

              ###############################################
              # Redir extDataDir to prjDotExtData dir
              ###############################################
              #.Object@extDataDir <- prjDotExtDataDir 
              #.Object@extStudyInfoFile <- dotExtStudyInfoFile 
              #.Object@extStudyDatasetInfoFile <- dotExtStudyDatasetInfoFile 
              #.Object@extPhenoSharedIdNamesFile <- dotExtPhenoSharedIdNamesFile 

              # New!
              .Object@extAllStudyInfoFile <- extAllStudyInfoFile 


              #############################
              # New! ftp downloaded file

              #.Object@comboInfoFile <- comboInfoFile 
              .Object@lastComboInfoFile <- lastComboInfoFile 


              if (!dir.exists(prjDotDir)) {
                  dir.create(prjDotDir, showWarnings = TRUE, recursive = T, mode = "0777")
              }
              if (!dir.exists(prjDotExtDataDir)) {
                  dir.create(prjDotExtDataDir, showWarnings = TRUE, recursive = T, mode = "0777")
              }
              if (!dir.exists(fileInfoArchDir)) {
                  dir.create(fileInfoArchDir, showWarnings = TRUE, recursive = T, mode = "0777")
              }

              #############################
              # Copy supplemental files 
              #############################
              # Only copy when the files are not found in the dotExtData dir
              # After that dotExtData files will be overwritten by ftp download 
              #if (!file.exists(dotExtStudyInfoFile)) {
              #    file.copy(extStudyInfoFile, dotExtStudyInfoFile)
              #}

              #if (!file.exists(dotExtStudyDatasetInfoFile)) {
              #    file.copy(extStudyDatasetInfoFile, dotExtStudyDatasetInfoFile)
              #}

              #if (!file.exists(dotExtPhenoSharedIdNamesFile)) {
              #    file.copy(extPhenoSharedIdNamesFile, dotExtPhenoSharedIdNamesFile)
              #}

              ###################################
              # Project directory setting up
              ###################################
              # Physical directory are created and assigned to object throught createPrjDir()
              prjDir = '' 
              prjDataDir = '' 
              prjTempDir = '' 
              procLogArchDir = '' 
              dataProcLog = '' 
              prjSetupLog = ''

              if (file.exists(confFile)) {


                  #########################################
                  # Get current prjDir
                  #########################################
                  # Look up prjDirFiles file  
                  # Get project related dirs/files and assign to the global object

                  # A list of all project directories 
                  ########################################################################
                  # The physical directories  are created by PrjConfig and creatPrjDir 
                  ########################################################################
                  prjDirFiles = getPrjDir(.Object, showErr = TRUE)

                  # Current dir is the 1st item of the list 
                  # Example "C:\\Users\\mars\\Documents\\R_Dev\\my_dbgapr_project2"
                  prjDir = prjDirFiles$prjDir

                  if (prjDir !=  "") {
                      prjDataDir = prjDirFiles$prjDataDir
                      prjTempDir = prjDirFiles$prjTempDir
                      procLogArchDir = prjDirFiles$procLogArchDir
                      dataProcLog = prjDirFiles$dataProcLog
                      prjSetupLog = prjDirFiles$prjSetupLog

                      # Pass to global
                      .Object@prjDir <- prjDir 
                      .Object@prjDataDir <- prjDataDir 
                      .Object@prjTempDir <- prjTempDir 
                      .Object@procLogArchDir <- procLogArchDir 
                      .Object@dataProcLog <- dataProcLog 
                      .Object@prjSetupLog <- prjSetupLog 

                      # Init the project_setup.log 
                      type = 'setup'
                      level = 'info'
                      show = F
                      mesg = paste("Initialize project_setup.log", sep="") 
                      writeLog(.Object,  type = type, level = level, message = mesg, show = show) 

                      if (file.exists(prjSetupLog)) {
                          ############################################
                          # Determine move log file to archive or not  
                          ############################################
                          # Rules for log file management
                          # 1. If size reach default 1MB and the log file is older than ond day,  rename it with dateInfo and move to log_archive directory. 
                          # 2. Remove archived file that is older than default 90 days.
                          logFile = prjSetupLog
                          logArchDir = procLogArchDir 
                          type = 'logfile'
                          manageArchive(.Object, workFile = logFile, archiveDir = logArchDir, type = type, maxSizeInMb = 1, keptDay = 90, minFileAge = 1)
                      }
                  }
                  else {
                      type = 'setup'
                      level = 'error'
                      show = T
                      mesg = paste("The current project directory info is missing form the project config file. Checkout ?prjConfig to see how to create it.", sep="")
                      writeLog(.Object,  type = type, level = level, message = mesg, show = show)
                  }

              } # end: if (is.null(prjDir))
              else {
                  type = 'setup'
                  level = 'error'
                  show = F
                  mesg = paste("The project config file is not found. Checkout ?prjConfig to see how to create it.", sep="")
                  writeLog(.Object,  type = type, level = level, message = mesg, show = show)
              }

              .Object
          })

# ----------------
# Method: show()
# ----------------

#' Display all class Commons methods 
#' 
#' The method displays all the methods available in the class Commons.
#'
#' @param object Commons class object.
#' @export show
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' show(c)
#'}

setMethod(
          f="show",
          signature="Commons",
          definition=function(object) {
              cat("\n")
              cat("Class Methods:\n")

              # Note: set where=search() to show only S4 method
              s4Methods <- classMethods <- showMethods(classes="Commons", where = search(), showEmpty = FALSE, inherited = TRUE, printTo = FALSE)
              s4MethodsCombo = paste(s4Methods, collapse="\n")
              cat("\n")
              cat(s4MethodsCombo)
              cat("\n")
          })



#######################################
# List of global variabls, functions
#######################################
# initialize 
# show
# getPrjDir
# createPrjDir
# prjConfig
# recordPrjFileInfo
# searchCopyPhenoFiles
# copyUserFile
# composeFileInfoJson
# writeFileInfoJson
# mergeDatasetConsent
# mergeDatasetConsentByStudy
# parsePhenoFileMeta
# writeLog
# mangeArchive
# parseAllStudyDataDic
# parseDataDicByStudy
# dataDicXmlParser

# getAllStudyInfo
# getAllDatasetInfo
# viewAllDatasetInfo
# viewAllStudyInfo
# accInfo
# getDataDicByStudy
# parsePhtHeaderFromFile
# parseIdsFromStAcc
# parseIdsFromPhtAcc
# cleanObjAcc
# displayTextFile 
# saveGapPlot
# convertEnumVarColName
# getVarNameByPhvAcc
# stripColNameAcc
# downloadDataDicFromFtp
# downloadExtDataFromFtp
# testFunc 



##########################
# Update history
##########################
# 2016-12-07: added code to process data-dic and var-report
# 2017-01-23: finished parseDataDicByStudy()
# 2017-01-27: added viewAllStudyInfo 
# 2017-02-08: done viewAllStudyInfo 
# 2017-03-01: added get functions
# 2017-03-02: dataDic parser improved 
# 2017-04-12: Finalized all functions 
