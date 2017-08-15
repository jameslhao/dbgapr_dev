#######################
# commons_internal.R
#######################

# Internal Commons class functions that are not to be called by the users.
# ATTN! The documentation rd file of this function is manually created: man/commons_internal.rd

# ----------------------
# Method: createPrjDir
# ----------------------

#' (internal) Create user project directory
#'
#' The method creates a project directory and updates the project config file. The project directory is where all the data and metadata files are located. The method should be called through the \code{\link{prjConfig}} function.
#'
#' @name createPrjDir
#' @param object Commons class object
#' @param prjDir user project directory
#' @return TRUE if the project directory is created and FALSE if not.
#' @export createPrjDir
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' createPrjDir(c, prjDir = "/home/user/dbgapr_project")
#' # or
#' createPrjDir(c, prjDir ="C:\\Users\\user\\dbgapr_project")       # for Windows
#'}

# createPrjDir(c, prjDir ="C:\\Users\\mars\\Documents\\R_Dev\\my_dbgapr_project2")

setGeneric(name = "createPrjDir",
           def = function(object, prjDir) {
               standardGeneric("createPrjDir")
           })

#' @describeIn createPrjDir of class Commons 
setMethod(
          f = "createPrjDir",
          signature = c("Commons","character"),
          definition = function(object, prjDir) {
              stringr::str_trim(prjDir)
              prjDir <- checkInputPath(object, prjDir)

              ################
              # S3 function
              ################
              realityCheck <- function(prjDir) {

                  #######################
                  # Reality check
                  #######################
                  if (dir.exists(prjDir)) {

                      # Assign to object
                      object@prjDir <- prjDir

                      #### Create project sub directories #### 

                      gapworkDir = file.path(prjDir, "gapwork")
                      if (!dir.exists(gapworkDir)) {
                          dir.create(gapworkDir, showWarnings = TRUE, recursive = T, mode = "0777")
                      }

                      dataDir = file.path(gapworkDir, "data")
                      if (!dir.exists(dataDir)) {
                          dir.create(dataDir, showWarnings = TRUE, recursive = T, mode = "0777")
                      }

                      # Assign to object
                      object@prjDataDir <- dataDir

                      logDir = file.path(gapworkDir, "log")
                      if (!dir.exists(logDir)) {
                          dir.create(logDir, showWarnings = TRUE, recursive = T, mode = "0777")
                      }

                      procLogArchDir = file.path(logDir, "archived_log")
                      if (!dir.exists(procLogArchDir)) {
                          dir.create(procLogArchDir, showWarnings = TRUE, recursive = T, mode = "0777")
                      }

                      # Assign to object
                      object@procLogArchDir <- procLogArchDir 

                      dataProcLog = file.path(logDir, 'data_process.log')
                      if (!dir.exists(procLogArchDir)) {
                          dir.create(procLogArchDir, showWarnings = TRUE, recursive = T, mode = "0777")
                      }
                      # Assign to object
                      object@dataProcLog <- dataProcLog 

                      tempDir = file.path(gapworkDir, "temp")
                      if (!dir.exists(tempDir)) {
                          dir.create(tempDir, showWarnings = TRUE, recursive = T, mode = "0777")
                      }

                      # Assign to object
                      object@prjTempDir <- tempDir

                      return(TRUE)
                  }
                  else {
                      mesg = paste("[INFO] The prject directory is not created. --- ", prjDir, "\n", sep="")
                      cat(mesg)
                      return(FALSE)
                  }
              } # end realityCheck()

              createOk = FALSE


              if (is.null(prjDir) == T | prjDir == "") {
                  cat("[INFO] The input project directory is empty. No change is made.\n")
              }
              else {

                  currPrjDir = ""

                  # Look up prj_dir from dbgapr_config file 
                  confFile = object@configFile

                  if (is.null(confFile) == T) {
                      mesg = paste("[INFO] No project directory registered. Checkout ?prjConfig to see how to register it.", "\n", sep="") 
                      cat(mesg)
                  }


                  
                  ##################################
                  # Check one-level up dir exists
                  ##################################
                  prjDirUpDir = dirname(prjDir)
                  
                  if (dir.exists(prjDirUpDir)) {

                      newCreation = FALSE

                      # check confFile
                      if (nchar(confFile) != 0) {

                          if (!file.exists(confFile)) {

                              ##############################
                              # Config file not exists
                              ##############################

                              #### Create a brand new prjDir #### 
                              if (!dir.exists(prjDir)) {


                                  dir.create(prjDir, showWarnings = TRUE, recursive = FALSE, mode = "0777")

                                  if (dir.exists(prjDir) ) {

                                      mesg = paste("[INFO] The new project directory is created. --- ", prjDir, sep="")
                                      cat(mesg)

                                      newCreation = TRUE 
                                  }
                              }

                              if (dir.exists(prjDir) ) {

                                  # Reality check
                                  createOk <- realityCheck(prjDir)
                              }
                              else {

                                  type = 'setup'
                                  level = 'error'
                                  show = T
                                  mesg = paste("[INFO] The creation of the prject directory is failed. The current project directory is not changed. --- ", currPrjDir, "\n", sep="")
                                  cat(mesg)
                              }

                          }
                          # confFile exists
                          else {

                              confDF <- fromJSON(confFile, flatten=TRUE)

                              # Get current prjDir from confDF 
                              currPrjDir <- (subset(confDF, confDF$current == 'yes'))$prj_dir 


                              # Check currPrjDir
                              if (is.null(currPrjDir)) {
                                  mesg = paste("[INFO] No current project directory registered. Checkout ?prjConfig() to see how to register it.", "\n",  sep="") 
                                  cat(mesg)
                              }

                              if (nchar(currPrjDir) != 0) {

                                  #### Create prjDir if it does not exist 
                                  if (!dir.exists(prjDir)) {
                                      dir.create(prjDir, showWarnings = TRUE, recursive = FALSE, mode = "0777")

                                      if (dir.exists(prjDir) ) {
                                          mesg = paste("[INFO] The input project directory is created. --- ", prjDir, "\n", sep="")
                                          cat(mesg)

                                          newCreation = TRUE 
                                      }
                                  }

                                  if (dir.exists(prjDir) ) {

                                      # Reality check
                                      createOk <- realityCheck(prjDir)

                                  }
                                  else {
                                      mesg = paste("[INFO] The creation of the prject directory is failed. The current project directory is not changed. --- ", currPrjDir, "\n", sep="")
                                      cat(mesg)
                                  }


                                  ######################################
                                  # Record to config file
                                  ######################################

                                  # Compare input prjDir with currPrjDir
                                  # prjDir not found in currPrjDir -- new prjDir

                                  if (prjDir %in% confDF$prj_dir == F) {


                                      # Prompt to confirm the change
                                      cat("\n")
                                      cat("[INFO] The input prorject directory is different from the current default.\n")  
                                      cat("       Input   : ", prjDir, "\n")  
                                      cat("       Default : ", currPrjDir, "\n")  
                                      cat("\n")
                                      cat("Are you sure you want to make the change? (yes/no)\n")
                                      yesOrNo <- readline(prompt = "   ")
                                      cat("\n")

                                      if (nchar(yesOrNo) == 0) {
                                          yesOrNo = 'no'
                                      }

                                      # Change current prjDir and update prjConfig
                                      if ( grepl('^yes$', yesOrNo, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE) == T | grepl('^y$', yesOrNo, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE) == T ) {

                                          #### Create prject directory ####


                                          ##############################
                                          # Updare project_config.json
                                          ##############################
                                          # The initial project_config.json file creation is done through prjConfig().
                                          # All the json file update is done here.

                                          # Get max id
                                          maxId <- max(confDF$id, na.rm = TRUE)
                                          newId = strtoi(maxId)+ 1

                                          # Reset value of column 'current' to 'no' for all rows
                                          current <- rep('no', nrow(confDF))
                                          confDF[['current']] <- current 

                                          # Append the new path as a new row to confDF with a new 'updated' date 
                                          newRow = c(newId, prjDir,  'yes', toString(as.POSIXlt(Sys.time())), toString(as.POSIXlt(Sys.time())))
                                          newConfDF = rbind(confDF, newRow)

                                          # Sort the dataframe by 'updated' date, the newest date on top 
                                          sortedNewConfDF <- newConfDF[order(newConfDF['updated'], decreasing=T),]

                                          # Convert to newConfJson
                                          newConfJson <- toJSON(sortedNewConfDF, pretty=T)

                                          # Update the prjConf json file
                                          write(newConfJson, file = confFile, ncolumns = if(is.character(newConfJson)) 1 else 5, append = F, sep = "\n")



                                          mesg = paste("[INFO] The new project directory is created and the project config file is updated. --- ", prjDir, "\n", sep="")
                                          cat(mesg)

                                          # Reality check
                                          createOk <- realityCheck(prjDir)

                                          # Important reminder to users 
                                          message("\nBefore going forward, please run the command such as below to make the project directory change in effect.\nc <- Commons()\n") 

                                      }
                                      else {
                                          cat("[INFO] No change is made. Curret default project directory is --- ", currPrjDir, "\n")
                                      }
                                  } # end prjDir not found in currPrjDir


                              }
                              # currPrjDir empty value 
                              else {
                                  mesg = paste("[INFO] No current project registered in the project config file. Checkout ?prjConfig to see how to register it.", "\n", sep="")
                                  cat(mesg)
                              }


                              #### Update 'current' status of existing row ####

                              # prjDir is previously configured confPrjDir 
                              if (prjDir %in% confDF$prj_dir == T) {


                                  # prjDir exists 
                                  if (dir.exists(prjDir)) {

                                      #### Change status to current if it is not already current #### 


                                      if (currPrjDir != prjDir) {

                                          # Prompt to confirm the change
                                          cat("\n")
                                          cat("[INFO] The input project directory was previously configured, but it is no longer current. \n")
                                          cat("       Current prjDir : ", currPrjDir, "\n")  
                                          cat("       Input prjDir   : ", prjDir, "\n")  
                                          cat("\n")

                                          cat("\n")
                                          cat("Are you sure you want to reinstate it as the current one? (yes/no)\n")
                                          yesOrNo <- readline(prompt = "   ")
                                          cat("\n")

                                          if (nchar(yesOrNo) == 0) {
                                              yesOrNo = 'no'
                                          }

                                          # Change current prjDir and update prjConfig
                                          if ( grepl('^yes$', yesOrNo, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE) == T | grepl('^y$', yesOrNo, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE) == T ) {

                                              # Reset value of column 'current' to 'no' for all rows
                                              current <- rep('no', nrow(confDF))
                                              confDF[['current']] <- current 

                                              # The row that matches input prjDir
                                              prj_dir <- NULL  	# necessary for passing CMD check 
                                              matchRow <- (subset(confDF, prj_dir == prjDir))

                                              # compose replace row 
                                              replaceRow <- c(matchRow$id, matchRow$prj_dir, 'yes', matchRow$created,  toString(as.POSIXlt(Sys.time())))

                                              # replace the match row
                                              matchRowIndex = which(confDF$prj_dir == prjDir)
                                              confDF[matchRowIndex, ] <- replaceRow
                                              updatedConfDF <- confDF

                                              # Sort the dataframe by 'updated' date, the newest date on top 
                                              sortedUpdatedConfDF <- updatedConfDF[order(updatedConfDF['updated'], decreasing=T),]
                                              #sortedNewConfDF <- newConfDF[order(newConfDF['updated'], decreasing=T),]

                                              ## Convert to newConfJson
                                              newConfJson <- toJSON(sortedUpdatedConfDF, pretty=T)

                                              ## Update the prjConf json file
                                              write(newConfJson, file = confFile, ncolumns = if(is.character(newConfJson)) 1 else 5, append = F, sep = "\n")

                                              mesg = paste("[INFO] The current project directory is reset and the project config file is updated. --- ", prjDir, "\n", sep="")
                                              cat(mesg)

                                              # Reality check
                                              createOk <- realityCheck(prjDir)

                                              # Important reminder to users 
                                              message("\nBefore going forward, please run the command such as below to make the project directory change in effect.\nc <- Commons()\n") 
                                          }
                                      }
                                      else {

                                          if (newCreation) {
                                              # Input dir is the same as PrjDir 
                                              mesg = paste("[INFO] The created prject directory is set to current.\n", sep="")
                                              cat(mesg)
                                          }
                                          else {
                                              # Input dir is the same as PrjDir 
                                              mesg = paste("[INFO] The input prject directory exists and is already current. --- ", prjDir, "\n", sep="")
                                              cat(mesg)
                                          }

                                          # Important reminder to users 
                                          message("\nBefore going forward, please run the command such as below to make the project directory change in effect.\nc <- Commons()\n") 

                                          createOk = TRUE 
                                      }

                                  }

                              }

                          } # end confile exists
                      }
                      # confFile empty value
                      else {
                          mesg = paste("[INFO] The user project directory info is not registered. Checkout ?projConf to see how to register it.", "\n", sep = "")
                          cat(mesg)
                      }

                  } # end dir.exists(prjDirUpDir) 
                  else {
                      mesg = paste("[INFO] The parent directory of the input path does not exist. It needs to be created first. --- ", prjDirUpDir, "\n", sep = "")
                      cat(mesg)
                  }


              } # end prjDir = ""

              return (createOk)


          })



# ----------------------
# Method: writeLog
# ----------------------

#' (internal) Error Logging
#'
#' The method writes and displaye logging message to the log files. 
#'
#' @name writeLog 
#' @param object Commons class object.
#' @param type a character string. The logging type of either 'setup' for project_setup.log or 'process' for data_process.log.
#' @param level a character string. The logging level of either 'error', 'warn', 'info', or 'debug'. The default value is 'info'.
#' @param message a character string. The logging message.
#' @param show a logical value. If TRUE, console display the log messaage; If FALSE (default), not display.
#' @param ... There are optional arguments. 
#' @param maxSizeInMb a integer. (optional) The maximum size (MB) of a log file. The log file is moved to archieved_log directory after reaching the maximum size (default 1MB).
#' @param keptDay an integer. (optional) The maximum days of an archieved log file is kept. The archived log file is deleted after the duration Day is past (default 90 days).
#' @export writeLog
#' @keywords internal
# @examples
# c <- Commons()
# writeLog(c, 
# className = 'Commons', methodName = 'getPrjDir', type = 'setup', level = 'error', message = 'this is an error', show = TRUE)

# writeLog(c, className = 'Commons', methodName = 'getPrjDir', type ='setup', level = 'error', message = 'this is an error', show = TRUE)
# or
# writeLog(c, className = 'Commons', methodName = 'getPrjDir', type ='setup', level = 'error', message = 'this is an error', show = TRUE, maxSizeInMb = 2, keptDay = 30)

setGeneric(name = "writeLog",
           #def = function(object, className, methodName, type, level, message, show, ...) {
           def = function(object, type, level, message, show, ...) {
               standardGeneric("writeLog")
           })


#' @describeIn writeLog of classs Commons
setMethod(
          f = "writeLog",
          signature = c("Commons", "character"),

          definition = function(object, type, level = 'info', message, show = FALSE, ...,  maxSizeInMb = 1, keptDay = 90) {

              # Allowed types 
              # 1 'setup' 	: 	project_setup.log 
              # 2 'process'	:  	data_process.log. 

              # Allowed levels
              # 1. info
              # 2. warn
              # 3. debug
              # 4. error

              currTime = toString(as.POSIXlt(Sys.time()))

              ####################################
              # Obtain caller function names
              ####################################
              # The items in final caller functin name combo are seperated by <-, e.g. func3<-func2<-funct1
              callNames <- lapply(sys.calls(), function(x) { x[[1]] })
              uniqueCallNames = unique(callNames)

              ##################################
              # Get className and methodName
              ##################################

              thisItemIndex <- which(uniqueCallNames == 'writeLog')				# find index of this writeLog() function
              displayCallerNames <- uniqueCallNames[1:thisItemIndex-1]			# remove all item wirteLog and all items after it 
              callerNameCombo = paste(displayCallerNames, collapse='->')		# make combo string separted by <-

              methodName = callerNameCombo
          className = class(object)[[1]]


          logLevel = '[INFO]'
          if (level == 'error') {
              logLevel = '[ERROR]'
          }
          if (level == 'warn') {
              logLevel = '[WARN]'
          }
          if (level == 'info') {
              logLevel = '[INFO]'
          }
          if (level == 'debug') {
              logLevel = '[DEBUG]'
          }

          prjDotDir = object@prjDotDir
          prjDir = object@prjDir
          configFile = object@configFile
          prjSetupLog = object@prjSetupLog
          procLogArchDir = object@procLogArchDir  
          dataProcLog = object@dataProcLog
          fileInfoArchDir = object@fileInfoArchDir


          # For project log from Commons() written to .dbgapr dir
          # Note: The logging type of either 'setup' for project_setup.log or 'process' for data_process.log. 
          logFile = ''

          dirCheck = FALSE


          if (type == 'process') {

              if (dir.exists(prjDir)) {
                  # The data process log from Study() is located at the project dir
                  # /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/log/data_process.log
                  logFile = dataProcLog 
                  logArchDir = procLogArchDir 

                  dirCheck = TRUE 
              }
              else {
                  message("The previously configured project directory no longer exists. Checout ?prjConfig() to reset it. --- ", prjDir, "\n");
              }

          }
          else if (type == 'setup') {
              if (dir.exists(prjDotDir)) {
                  # "C:/Users/mars/Documents/.dbgapr/project_setup.log"
                  logFile = prjSetupLog 
                  logArchDir = fileInfoArchDir 

                  dirCheck = TRUE 
              }
              else {
                  message("The ncbi/dbgapr_conf directory does not exist. Re-initialize the class Commons by the command such as below  to create it.\nc <- Commons()\n");
              }


          }


          if (dirCheck) {

              ############################################
              # Determine move log file to archive or not  
              ############################################
              # Rules for log file management
              # 1. If size reach default 1MB and the log file is older than ond day,  rename it with dateInfo and move to log_archive directory. 
              # 2. Remove archived file that is older than default 90 days.
              # manageArchive(object, workFile = logFile, archiveDir = logArchDir, type = type, show = FALSE, maxSizeInMb = 1, keptDay = 90)

              # Compsoe log content, such as 
              # [ERROR] [Commons::getPrjDir] 2017-02-20 11:49:09] This is an error.
              logTimeMethod = paste("[", className, "::", methodName, "]", " [", currTime, "]", sep="")
              logData = paste(logLevel, " ", logTimeMethod, " ", message, " ", sep="")
              infoData = paste(logLevel, " ", message, " ", sep="")

              if (logFile != "") {

                  ##### Write log #### 
                  logData <- gsub("\n", " ", logData)
                  write(logData, file = logFile, ncolumns = 1, append = T, sep = "\n")

                  # Show log 
                  if (show == TRUE) {
                      cat(infoData, "\n")
                  }
              }
              else {
                  message("\n[WARN] It appears that some required project logging files are missing. Please initialize the class Commons again by the command such as below.\nc <- Commons()\n"); 
              }
          }

          })

# ----------------------
# Method: copyUserFile
# ----------------------

#' (internal) Copy decrypted dbGaP files to user project directory 
#'
#' The method copies or moves the decrypted dbgap phenotype files to the project directory. The copied files are organized by study accessions. It is called by the \code{\link{searchCopyPhenoFiles}} function. 
#'
#' @name copyUserFile 
#' @param object Commons class object
#' @param userFile a character string. The path to decrypted dbGaP phenotype file.
#' @param ... There are optional arguments. 
#' @param copyOrMove a character string. (optional) If the input string is 'copy' (default), copies the file from a user directory to the project directory. Movis the file if the input string is 'move'.
#' @param overwrite a logical value. (optional) If TRUE, overwrites the file if it already exists in the prject directory; Skip copy if FALSE (default).
#' @return (invisible) a character list. The meta-info of copied files.
#' @export copyUserFile
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' file = '/home/foo/data/phs000429.v1.pht002481.v1.p1.c1.areds_data_final_11.EDO.txt.gz' 
#' copyUserFile(c, userFile = file, overwrite = TRUE) 
#'}

# c <- Commons()
# copyUserFile(c, userFile = '/home/user/data/phs000429.v1.pht002481.v1.p1.c1.areds_data_final_11.EDO.txt.gz')
# copyUserFile(c, userFile = "C:\\Users\\mars\\Documents\\dbgap_test_data\\temp\\phs000001.v3.pht000001.v2.p1.c1.genspecphenotype.EDO.txt.gz.ncbi_enc")
# copyUserFile(c, userFile = "C:\\Users\\mars\\Documents\\dbgap_test_data\\temp\\phs000001.v3.pht000001.v2.p1.c1.genspecphenotype.EDO.txt.gz.ncbi_enc", copyOrMove = 'move')

setGeneric(name = "copyUserFile",
           def = function(object, userFile, ...) {
               standardGeneric("copyUserFile")
           })

#' @describeIn copyUserFile of class Commons
setMethod(
          f = "copyUserFile",
          signature = c("Commons","character"),
          definition = function(object, userFile = '', ..., copyOrMove = 'copy', overwrite = F) {
              userFile <- checkInputPath(object, userFile)
              

              fileName = basename(userFile)
              filePath = normalizePath(userFile)

              if (nchar(userFile) != 0) {

                  if (file.exists(userFile)) {

                      #########################################
                      # Parse study accession from file name
                      #########################################
                      fileInfoSet = parsePhenoFileMeta(object, dbgapFile = userFile)
                      phsAcc <- fileInfoSet$fStAcc
                      multiType <- fileInfoSet$multiType
                      consentType <- fileInfoSet$consentType
                      phsAccNoVer <- fileInfoSet$fStAccNoVer 
                      isNcbiEnc <- fileInfoSet$isNcbiEnc

                      if (isNcbiEnc == FALSE) {

                          #multiType = 'DataDicXml'
                          #consentType = 'Multi'

                          ##### Copy user files under data/orginal subDir #####
                          #prjDir = getPrjDir(object, showErr = TRUE)

                          prjDirs = getPrjDir(object, showErr = TRUE)
                          prjDir = prjDirs$prjDir

                          #####################################
                          # Determine where to copy the file
                          #####################################
                          if (multiType == 'DataDicXml') {
                              destnDir = file.path(prjDir, "gapwork", "data", phsAccNoVer, phsAcc,  "data_dic")
                          }
                          else if (multiType == 'VarReportXml') {
                              destnDir = file.path(prjDir, "gapwork", "data", phsAccNoVer, phsAcc, "var_report")
                          }
                          else if (multiType == 'DataDicTarGz') {
                              destnDir = file.path(prjDir, "gapwork", "data", phsAccNoVer, phsAcc, "data_dic")
                          }
                          else if (multiType == 'VarReportTarGz') {
                              destnDir = file.path(prjDir, "gapwork", "data", phsAccNoVer, phsAcc, "var_report")
                          }
                          else {
                              destnDir = file.path(prjDir, "gapwork", "data", phsAccNoVer, phsAcc, "original")
                          }

                          # Example: /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/phs000001/phs000001.v3.p1/original
                          destnFile = file.path(destnDir, fileName) 

                          if(is.null(prjDir)) {
                              prjDir = ""
                          }

                          if (nchar(prjDir) != 0) {

                              if (dir.exists(prjDir)) {

                                  # Create the study directory
                                  if (!dir.exists(destnDir)) {
                                      dir.create(destnDir, showWarnings = TRUE, recursive = T, mode = "0777")
                                  }

                                  #######################################
                                  # Copy file (ATTN! with overwrite "T")
                                  #######################################
                                  destnFile = file.path(destnDir, fileName)

                                  ###### S3 function for file copying ######
                                  copyOrMoveFiles <- function(copyOrMove, userFile, destnFile) {

                                      if (copyOrMove == 'move') {
                                          file.rename(from = userFile, to = destnFile)
                                      }
                                      else {
                                          file.copy(userFile, destnDir, overwrite = T, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
                                      }
                                      if (file.exists(destnFile)) {
                                          type = 'process'
                                          level = 'info'
                                          show = T
                                          if (copyOrMove == 'move') {
                                              mesg = paste("File move is OK. --- ", destnFile, sep="")
                                          }
                                          else {
                                              mesg = paste("File copy is OK. --- ", destnFile, sep="")
                                          }
                                          writeLog(object, type = type, level = level, message = mesg, show = show) 
                                      }
                                  }

                                  ####### Determining copy / move or not #######

                                  if (file.exists(destnFile)) {
                                      if (overwrite == TRUE) {
                                          copyOrMoveFiles(copyOrMove, userFile, destnFile)
                                      }
                                      else {
                                          type = 'process'
                                          level = 'info'
                                          show = T
                                          mesg = paste("The file exists. Skipped copying. --- ", destnFile, sep="")
                                          writeLog(object, type = type, level = level, message = mesg, show = show) 
                                      }
                                  }
                                  # Copy or move if not exists
                                  else {
                                      copyOrMoveFiles(copyOrMove, userFile, destnFile)
                                  }

                                  ######################################################
                                  # UntarGz of data_dic and var_report in old studies 
                                  ######################################################

                                  copiedFileName = basename(userFile)
                                  copiedFileWithPath = file.path(destnDir, copiedFileName) 


                                  if (multiType == 'DataDicTarGz' || multiType == 'VarReportTarGz') {

                                      #############################
                                      # UntarGz to the same dir
                                      #############################
                                      untar(copiedFileWithPath, exdir = destnDir)

                                      #############################
                                      # Remove the original targz 
                                      #############################
                                      # such as /phs000001.v3.p1.data_dictionary.MULTI.tar.gz
                                      # so that only the XML files under tar data_dict xml such as below are left:
                                      # phs000001.v3.pht000001.v2.genspecphenotype.data_dict.xml
                                      file.remove(copiedFileWithPath)

                                  }
                                  # Example: /phs000007.v27.pht005145.v1.l_mtbtarg1_ex01_3_0955s.data_dict.xml
                                  else if (multiType == 'DataDicXml' || multiType == 'VarReportXml') {

                                      # Copy the file to destnDir
                                      if (copyOrMove == 'move') {
                                          file.rename(from = userFile, to = copiedFileWithPath)
                                      }
                                      else {
                                          file.copy(userFile, copiedFileWithPath)
                                      }
                                  }
                              }
                              else {
                                  type = 'process'
                                  level = 'error'
                                  show = T
                                  mesg = paste("The prjDir does not exist. Look up ?createPrjDir() to see how to set it up. --- ", prjDir, sep="")
                                  writeLog(object, type = type, level = level, message = mesg, show = show) 
                              }
                          }
                          else {
                              type = 'process'
                              level = 'error'
                              show = T
                              mesg = paste("The prjDir has no value. Look up ?createPrjDir() to see how to set it up. --- ", prjDir, sep="")
                              writeLog(object, type = type, level = level, message = mesg, show = show) 
                          }

                      } # end of isNcbiEnc
                      else {
                          type = 'process'
                          level = 'error'
                          show = T

                          mesg = ''
                          if (copyOrMove == 'move') {
                              mesg = paste("The file is not decrypted. It thus is not moved. --- ", userFile, sep="")
                          }
                          else {
                              mesg = paste("The file is not decrypted, thus is not copied. The file has to be decrypted first.--- ", userFile, sep="")
                          }
                          writeLog(object, type = type, level = level, message = mesg, show = show) 
                      }

                      return(invisible(fileInfoSet))
                  }
                  else {
                      type = 'process'
                      level = 'error'
                      show = T
                      mesg = paste("The input userFile does not exist. Check the path and try it again. --- ", userFile, sep="")
                      writeLog(object, type = type, level = level, message = mesg, show = show) 
                  }
              }
              else {
                  type = 'process'
                  level = 'error'
                  show = T
                  mesg = paste("The input userFile is empty. It should be the path to a user file. Check the path and try it again.", sep="")
                  writeLog(object, type = type, level = level, message = mesg, show = show) 
              }

          })

# ---------------------------
# Method: writeFileInfoJson
# ---------------------------

#' (internal) Write file info json
#'
#' The method takes the attribute set of a phenotype file, converts it to a data frame, and saves it a json file, which is composed through \code{\link{composeFileInfoJson}}. (Developer's notes: This function only appends a new item to the existing json file. It does not deal with replacing existing items. That is because the existing json file (pht_file_info_indiv.json) is removed and recreated every time when it is called by \code{\link{recordPrjFileInfo}}. 
#'
#' @name writeFileInfoJson
#' @param object Commons class object
#' @param dbgapFile a character string. Decrypted dbGaP phenotype file name with path.
#' @return TRUE a logical value. TRUE if json file is written successfully. FALSE if not. 
#' @export writeFileInfoJson 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' file = '/home/foo/data/phs000429.v1.pht002481.v1.p1.c1.areds_data_final_11.EDO.txt.gz' 
#' writeFileInfoJson(c, dbgapFile = file) 
#'}

# c <- Commons()
# writeFileInfoJson(c, dbgapFile = '/netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/phs000001/phs000001.v3/original/phs000001.v3.pht000370.v2.p1.c2.adverse.GRU.txt.gz')

setGeneric(name = "writeFileInfoJson",
           def = function(object, dbgapFile) {
               standardGeneric("writeFileInfoJson")
           })

#' @describeIn writeFileInfoJson of class Commons 
setMethod(
          f = "writeFileInfoJson",
          signature = c("Commons"),
          def = function(object, dbgapFile) {
              dbgapFile <- checkInputPath(object, dbgapFile)

              # Normalize path by converting / or \ to \\ on Windows (ingore for non-Windows)
              # "C:\\Users\\mars\\Documents\\R_Dev\\my_dbgapr_project21\\gapwork\\data\\phs000001\\phs000001.v3\\original\\phs000001.v3.pht000001.v2.p1.c1.genspecphenotype.EDO.txt.gz"
              dbgapFile <- checkInputPath(object, dbgapFile)

              prjDotDir = object@prjDotDir
              prjDir = object@prjDir

              # Example: "C:/Users/mars/Documents/.dbgapr/pht_file_info_indiv.json"
              fileInfoFile = object@fileInfoFile

              # Parse file info
              fileInfoSet = parsePhenoFileMeta(object, dbgapFile = dbgapFile)
              thisFileDF = composeFileInfoJson(object, fileInfoSet)

              ##########################################################################
              # Make sure only the pht files under a valid file path is processed
              ##########################################################################
              # The files under the irregular path such as .. /xxphs000001.v3.p1/... will be ignored 
              fileName <- fileInfoSet$fileName
              fStAcc <- fileInfoSet$fStAcc
              fStAccNoVer <- fileInfoSet$fStAccNoVer
              validFilePath = file.path(prjDir, 'gapwork', 'data', fStAccNoVer, fStAcc, 'original', fileName)

              if (validFilePath == dbgapFile) {

                  ###########################
                  # Construct dataframe
                  ###########################

                  if (!file.exists(fileInfoFile)) {

                      # Convert to json
                      thisFileJson <- toJSON(thisFileDF, pretty=T)


                      # Write 
                      write(thisFileJson, file = fileInfoFile, ncolumns = if(is.character(thisFileJson)) 1 else 5, append = F, sep = "\n")

                  }
                  else {

                      # Read fileInfoFile json into a dataframe
                      savedDF <- fromJSON(fileInfoFile, flatten=TRUE)

                      # Update thisFile count 
                      maxCount <- max(as.numeric(savedDF$count), na.rm = TRUE)
                      newCount = strtoi(maxCount)+ 1
                      thisFileDF$count <- newCount

                      # Append thisFileDF to savedDF
                      newDF <- rbind(savedDF, thisFileDF)

                      # Convert to json
                      newFileJson <- toJSON(newDF, pretty=T)

                      # Re-write fileInfoFile 
                      write(newFileJson, file = fileInfoFile, ncolumns = if(is.character(newFileJson)) 1 else 5, append = F, sep = "\n")
                  }

                  ##################
                  # Write log
                  ##################
                  type = 'setup'
                  level = 'info'
                  show = F
                  mesg = paste("File meta info Saved! --- from ", dbgapFile, sep="") 
                  writeLog(object, type = type, level = level, message = mesg, show = show) 

              }
          })


# ------------------------------
# Method: composeFileInfoJson
# ------------------------------

#' (internal) Construct a data-frame of the phenotype file info attributes 
#'
#' The method takes a set of phenotype file info attributes and converts it to a dataframe. It is called through the \code{\link{writeFileInfoJson}} function.
#'
#' @name composeFileInfoJson 
#' @param object Commons class object.
#' @param fileInfoSet a character list. The phenotype file info attribute sets.
#' @return a data frame.  Phenotype file info attributes. 
#' @export composeFileInfoJson 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' composeFileInfoJson(c, fileInfoSet)
#'}

setGeneric(name = "composeFileInfoJson",
           def = function(object, fileInfoSet) {
               standardGeneric("composeFileInfoJson")
           })

#' @describeIn composeFileInfoJson of class Commons 
setMethod(

          # Note: This method compose json for just one of pht file each time

          f = "composeFileInfoJson",
          signature = c("Commons"),

          def = function(object, fileInfoSet) {

              ###################
              # Compose Json
              ###################

              json = "{}"

              ##### Total 26 fields #####
              fileName <- I(c(fileInfoSet$fileName))
              isCurrent <- c(fileInfoSet$isCurrent)
              pathToFile <- I(c(fileInfoSet$pathToFile))
              sourceFiles <- I(c(fileInfoSet$sourceFiles))
              fStAcc <- I(c(fileInfoSet$fStAcc))
              fStAccNoP <- I(c(fileInfoSet$fStAccNoP))
              fStAccNoVer <- I(c(fileInfoSet$fStAccNoVer))
              fStId <- I(c(fileInfoSet$fStId))
              fStVerId <- I(c(fileInfoSet$fStVerId))
              fStPId <- I(c(fileInfoSet$fStPId))
              fPhtAcc <- I(c(fileInfoSet$fPhtAcc))
              fPhtAccNoP <- I(c(fileInfoSet$fPhtAccNoP))
              fPhtAccNoVer <- I(c(fileInfoSet$fPhtAccNoVer))
              fPhtId <- I(c(fileInfoSet$fPhtId))
              fPhtVerId <- I(c(fileInfoSet$fPhtVerId))
              fPhtAccConsent <- I(c(fileInfoSet$fPhtAccConsent))
              consentCode <- I(c(fileInfoSet$consentCode))
              consentShortName <- I(c(toString(fileInfoSet$consentShortName)))	# requires cast type from Vector string 
              consentType <- I(c(fileInfoSet$consentType))
              processType <- I(c(fileInfoSet$processType))
              isMulti <- c(fileInfoSet$isMulti)
              multiType <- I(c(fileInfoSet$multiType))
              isNcbiEnc <- c(fileInfoSet$isNcbiEnc)
              headerPhvs <- I(c(fileInfoSet$headerPhvs))
              headerColNames <- I(c(fileInfoSet$headerColNames))
              headerPhvColNamePairs <- I(c(fileInfoSet$headerPhvColNamePairs))
              ###################################################################################


              # Pickup CreatedDate and UpdateDate from the LastFileInfoFile
              created = ''

              lastFileInfoFile = object@lastFileInfoFile 

              if (file.exists(lastFileInfoFile)) {
                  savedLastDF <- fromJSON(lastFileInfoFile, flatten=TRUE)

                  # Check match row with the same fileName
                  lastInfoFileMatchRowIndex = which(savedLastDF$fileName == fileInfoSet$fileName)
                  lastInfoFileMatchCreateDate = savedLastDF[lastInfoFileMatchRowIndex, ]$created
                  lastInfoFileMatchUpdateDate = savedLastDF[lastInfoFileMatchRowIndex, ]$updated

                  if ( length(lastInfoFileMatchRowIndex) != 0) {
                      created <- I(c((toString(lastInfoFileMatchCreateDate)))) 
                  }
                  else {
                      created <- I(c((toString(Sys.time()))))
                  }
              }
              else {
                  created <- I(c((toString(Sys.time()))))
              }

              updated <- I(c((toString(Sys.time()))))

              # Construct dataframe

              count = I(1)   

              df <- data.frame(
                               count, 

                               #	##### Total 26 fields #####
                               fileName, 
                               isCurrent,
                               pathToFile,
                               sourceFiles,
                               fStAcc, 
                               fStAccNoP, 
                               fStAccNoVer, 
                               fStId,
                               fStVerId,
                               fStPId,
                               fPhtAcc,
                               fPhtAccNoP,
                               fPhtAccNoVer, 
                               fPhtId,
                               fPhtVerId, 
                               fPhtAccConsent,
                               consentCode, 
                               consentShortName, 
                               consentType, 
                               processType, 
                               isMulti, 
                               multiType, 
                               isNcbiEnc, 
                               headerPhvs, 
                               headerColNames, 
                               headerPhvColNamePairs, 
                               #	#############################

                               created, 
                               updated
                               )

              # Convert to json
              #json <- toJSON(df, pretty=T)
              return(df)

          })


# ------------------------------
# Method: parsePhenoFileMeta
# ------------------------------

#' (internal) Parse dataset file info 
#'
#' The method parses the meta-info from pthe henotype dataset file name and headers, and saves the file associated attributes to a json file. It is called from \code{\link{writeFileInfoJson}}.
#'
#' @name parsePhenoFileMeta 
#' @param object Commons class object.
#' @param dbgapFile a variable string. The path to a dbGaP phenotype file or a file name of the phenotype file. 
#' @param ... There are optional arguments. 
#' @param parseHeader a logical value. (optional) If TRUE, opens the file and parses out the info from the file header; If FALSE (default), only the file name is parsed. Note: Always leave it as default FALSE, since parsing data file header takes too long, and parsed header info is not used for anything in this package.  
#' @return Returns a character list.  The phenotype file attributes, such as fStAcc (file study accession), fPhtAcc (file phenotype accession) etc.  
#' @export parsePhenoFileMeta 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons() 
#' phtFile = '/home/foo/data/phs000001.AREDS.pht000001.v1.p1.c1.Eye_Disease_Only.txt.gz' 
#' parsePhenoFileMeta(c, dbgapFile = phtFile) 
#'}

#
# c <- Commons() 
# parsePhenoFileMeta(c, dbgapFile = "~/myprj/gapwork/data/phs000001/phs000001.v3/original/phs000001.v3.pht000001.v2.p1.c1.genspecphenotype.EDO.txt.gz")
# parsePhenoFileMeta(c, dbgapFile = 'phs000001v1/p1/phs000001.AREDS.pht000001.v1.p1.c1.Eye_Disease_Only.txt.gz')
# parsePhenoFileMeta(c, dbgapFile = 'phenotype/phs000363.v7.pht003333.v1.p8.c1.SABRe_Project_2_Immunoassays_l_mpimn05_2005_m_0758s.HMB-MDS-IRB.txt.gz')
#

setGeneric(name = "parsePhenoFileMeta",
           def = function(object, dbgapFile, ...) {
               standardGeneric("parsePhenoFileMeta")
           }
           )

#' @describeIn parsePhenoFileMeta Resturns fileInfoSet 
setMethod(
          f = "parsePhenoFileMeta",
          signature = c("Commons", "character"),
          definition = function(object, dbgapFile, ..., parseHeader = FALSE) {
              dbgapFile <- checkInputPath(object, dbgapFile)

              fileName = basename(dbgapFile)
              filePath = dirname(dbgapFile)
              isCurrent = 'yes'

              # For compose dataframe
              pathToFile = dbgapFile
              sourceFiles = dbgapFile

              # #### Parse ids of study accession ####
              # Examples : 
              # phs000461.v1.pht003684.v1.p1.GLAUGEN_POAG_Methylation_Subject.MULTI.txt.gz
              # phs000007.v20.pht000009.v2.p8.c1.ex0_7s.HMB-MDS-IRB.txt.gz 

              pattns = stringr::str_match(fileName, "^(phs0*)(\\d+)(\\.v(\\d+))*")
              fStAccNoP = pattns[1]	 		# phs000007.v20
              fPhsWithZero = pattns[2] 		# phs00000 
              fStId = pattns[3] 				# 7 
              fStVdotId = pattns[4]			# .v3
              fStVerId = pattns[5]			# 3  (NA when absent)
              fStAccNoVer = paste(pattns[2], pattns[3], sep="") # phs000007


              # For those w/o study-version  
              # phs000001.AREDS.pht000001.v1.p1.c1.Eye_Disease_Only.txt.gz 
              if (is.na(fStVerId)) {
                  fStVerId = 1
                  fStAccNoP = paste(fStAccNoP, '.v', fStVerId, sep="") 
              }

              # Parse p#
              pattns = stringr::str_match(fileName, "\\.p(\\d+)")
              fStPId = pattns[2]

              # Rconstitute file study accession
              ######################################
              # Get rid of p# in Study Accession
              ######################################
              fStAcc = fStAccNoP

              # #### Parse ids of phenotype accession ####
              pattns = stringr::str_match(fileName, "(pht0*)(\\d+)(\\.v(\\d+))")

              fPhtAccNoP = pattns[1]	 		# pht000371.v2
              fPhtWithZero = pattns[2] 		# pht000 
              fPhtId = pattns[3] 				# 371 
              fPhtVdotId = pattns[4]			# .v2
              fPhtVerId = pattns[5]			# 2
              fPhtAccNoVer = paste(pattns[2], pattns[3], sep="")
              fPhtAcc = fPhtAccNoP

              # Note: no longer include p# in accession 
              # fPhtAcc = paste(fPhtAccNoP, ".p", fStPId, sep="") 

              processType = 'copy-from-user-to-prj'

              # #### Parse multi type and consent code ####
              isMulti = F
              multiType = ''
              consentCode = '' 
              consentShortName = ''
              consentType = ''

              ###########################
              # Check files with MULTI
              ###########################
              if(length(grep("\\.MULTI\\.", fileName)) > 0) {

                  isMulti = T

                  if(length(grep("_Subject", fileName)) > 0) {
                      multiType = 'Subject'
                      consentType = 'Multi'
                  }
                  else if(length(grep("_Sample", fileName)) > 0) {
                      multiType = 'Sample'
                      consentType = 'Multi'
                  }
                  else if(length(grep("_Pedigree", fileName)) > 0) {
                      multiType = 'Pedigree'
                      consentType = 'Multi'
                  }
              }

              ###################################################
              # Check data_dic and variable_report 
              ###################################################
              # Example: 
              # phs000001.v3.p1.data_dictionary.MULTI.tar 
              # phs000007.v27.pht005164.v1.t_ctvasclng_2011_m_0952s.data_dict.xml 
              # phs000001.v3.pht002481.v1.areds_data_final_11.data_dict.xml
              # phs000429.v1.pht002481.v1.areds_data_final_11.data_dict_2012_02_27.xml       
              if (length(grep("\\.data_dictionary\\.MULTI\\.tar\\.gz", fileName)) > 0) {
                  multiType = 'DataDicTarGz'
                  consentType = 'Multi'
              }
              else if (length(grep("\\.data_dict.*\\.xml", fileName)) > 0) {            # ATTN!: allows something, such as date, between 'data_dic' and 'xml'
                  multiType = 'DataDicXml'
                  consentType = 'Multi'
              }
              # Example:
              # phs000001.v3.p1.variable_report.MULTI.tar.gz
              # phs000429.v1.p1.variable_report.MULTI.tar.gz
              # phs000007.v27.pht005165.v1.p10.t_mrtbss_2014_m_0961s.var_report.xml
              else if (length(grep("\\.variable_report\\.MULTI\\.tar\\.gz", fileName)) > 0) {
                  multiType = 'VarReportTarGz'
                  consentType = 'Multi'
              }
              else if (length(grep("\\.var_report.*\\.xml", fileName)) > 0) {
                  multiType = 'VarReportXml'
                  consentType = 'Multi'
              }


              ###############################
              # Individual level pht files
              ###############################
              if (multiType == '')   {

                  if (length(grep("\\.c\\d+\\.", fileName)) > 0) {
                      # phs000363.v7.pht003332.v1.p8.c2.SABRe_Project_2_Immunoassays_l_mpimn04_2005_m_0757s.HMB-NPU-MDS-IRB.txt.gz 
                      # phs000429.v1.pht002481.v1.p1.c1.areds_data_final_11.EDO.txt.gz
                      pattns = stringr::str_match(fileName, "\\.c(\\d+).*\\.(.+)\\.txt")
                      consentCode = paste("c", pattns[2], sep="")				# c1 
                      consentShortName = pattns[3]							# HMB-MDS-IRB
                      multiType = 'Not'
                      consentType = 'Indiv'
                  }
              }

              fPhtAccConsent = paste(fPhtAcc, ".", consentCode, sep="") 		# pht000379.v2.p1.c2

              # #### check suffix .ncbi_enc #### 
              headerPhvs = '' 
              headerColNames = '' 
              headerPhvColNamePairs = '' 
              headerPhvColNameComboSets = '' 

              # String combo delimited by ','
              headerPhvComboStr = '' 
              headerColNameComboStr = '' 
              headerPhvColNamePairComboStr = '' 

              isNcbiEnc = F
              if(length(grep("\\.ncbi_enc", fileName)) > 0) {
                  isNcbiEnc = T
              }
              else {

                  # Note: Always keep parseHeader FALSE since the process is too expensive and the same info can be retrieved from data_dic.
                  if (parseHeader == TRUE) {

                      if (file.exists(pathToFile)) {
                          # Parse header info into 3 different lists: phvHeader, colNames, phvColNameSet
                          headerInfoSets = parsePhtHeaderFromFile(object, phtFile = pathToFile, phtFileType = multiType) 

                          headerPhvs = headerInfoSets[1]
                          headerColNames = headerInfoSets[2]
                          headerPhvColNamePairs = headerInfoSets[3]
                          headerPhvColNameComboSets = headerInfoSets[4]

                          # String combo delimited by ','
                          headerPhvComboStr = paste(unlist(headerPhvs), collapse=', ' )
                          headerColNameComboStr = paste(unlist(headerColNames), collapse=', ' )
                          headerPhvColNamePairComboStr = paste(unlist(headerPhvColNameComboSets), collapse=', ' )
                      }
                      else {
                          # Write log
                          type = 'process'
                          level = 'error'
                          show = T 
                          mesg = paste("The phenotype data file is not found. The file header info cannot be parsed. --- ", pathToFile, sep="")
                          writeLog(object,  type = type, level = level, message = mesg, show = show) 
                      }
                  }
              }

              ############################################################################


              ###################
              # Total 26 fields
              ###################
              fileInfoSet = list()
              fileInfoSet['count'] <- '1' 
              fileInfoSet['fileName'] <- fileName 
              fileInfoSet['isCurrent'] <- isCurrent 
              fileInfoSet['pathToFile'] <- pathToFile 
              fileInfoSet['sourceFiles'] <- sourceFiles 
              fileInfoSet['fStAcc'] <- fStAcc
              fileInfoSet['fStAccNoP'] <- fStAccNoP 
              fileInfoSet['fStAccNoVer'] <- fStAccNoVer 
              fileInfoSet['fStId'] <- fStId 
              fileInfoSet['fStVerId'] <- fStVerId 
              fileInfoSet['fStPId'] <- fStPId 
              fileInfoSet['fPhtAcc'] <- fPhtAcc
              fileInfoSet['fPhtAccNoP'] <- fPhtAccNoP
              fileInfoSet['fPhtAccNoVer'] <- fPhtAccNoVer
              fileInfoSet['fPhtId'] <- fPhtId 
              fileInfoSet['fPhtVerId'] <- fPhtVerId
              fileInfoSet['fPhtAccConsent'] <- fPhtAccConsent
              fileInfoSet['consentCode'] <- consentCode 
              fileInfoSet['consentShortName'] <- consentShortName
              fileInfoSet['consentType'] <- consentType 
              fileInfoSet['processType'] <- processType 
              fileInfoSet['isMulti'] <- isMulti 
              fileInfoSet['multiType'] <- multiType 
              fileInfoSet['isNcbiEnc'] <- isNcbiEnc 
              fileInfoSet['headerPhvs'] <- headerPhvComboStr 
              fileInfoSet['headerColNames'] <- headerColNameComboStr 
              fileInfoSet['headerPhvColNamePairs'] <- headerPhvColNamePairComboStr 
              ##########################################################33###################

              return(fileInfoSet)

          })


# --------------------------------
# Method: parsePhtHeaderFromFile
# --------------------------------

#' (internal) Parse phenotype data file header 
#'
#' The method parses the header lines of a given phenotype data file and returns a list of file attribute sets. It is called through \code{\link{parsePhenoFileMeta}}.
#'
#' @name parsePhtHeaderFromFile 
#' @param object Commons class object.
#' @param phtFile Path to a phenotype file. 
#' @param phtFileType Type of the phenotype file with possible values of Subject, Sample, Pedigree, or Other. 
#' @return a list of the file attribute sets, including the sets of variable accessions, column names, variable accession and name pairs, variable accession and name combo strings.
#' @export parsePhtHeaderFromFile 
#' @keywords internal


# @examples
# \dontrun{
#
# c <- Commons()
# headerInfoSets = parsePhtHeaderFromFile(c, phtFile = pathToFile, phtFileType = multiType) 
#}


setGeneric(name = "parsePhtHeaderFromFile",
           def = function(object, phtFile, phtFileType) {
               standardGeneric("parsePhtHeaderFromFile")
           }
           )

#' @describeIn parsePhtHeaderFromFile A method of class Commons 
setMethod(
          f = "parsePhtHeaderFromFile",
          signature = c("Commons","character"),
          definition = function(object, phtFile, phtFileType) {
              phtFile <- checkInputPath(object, phtFile)

              # Get first 20 lines
              # SkipNul TRUE to get rid of embedded Nulls warnings
              lines <- paste(readLines(phtFile, n = 20, encoding="UTF-8", skipNul=TRUE))  

              # Example of the first a few lines  
              #
              #      # Study accession: phs000001.v3.p1
              #      # Table accession: pht002478.v1.p1
              #      # Consent group: All
              #      # Citation instructions: The study accession (phs000001.v3.p1) is used to cite the study and its data tables and documents. The data in this file should be cited using the accession pht002478.v1.p1.
              #      # To cite columns of data within this file, please use the variable (phv#) accessions below:
              #
              #      # 1) the table name and the variable (phv#) accessions below; or
              #      # 2) you may cite a variable as phv#.v1.p1
              #
              #      ##              phv00164677.v1.p1       phv00164678.v1.p1       phv00164679.v1.p1       phv00164680.v1.p1       phv00164681.v1.p1       phv00164682.v1.p1
              #      dbGaP SubjID    dbGaP SampID    ID2     specnum SAMP_SOURCE     SOURCE_SAMPID   STUDY   SAMPLE_USE
              #      52685   8323    1001    15865880        Coriell ED50129 phs000001.v3.p1
              #      52685   6511    1001    M6714073        Coriell ED32459 phs000001.v3.p1
              #      ...

              # Parse header lines (# xxx : xxx) 
              headerSets = list()
              parser <- function(x) {

                  line <- x
                  if ( grepl('^#\\s*(.+)(:)(.+)$', line, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE) == T) {

                      pattns = stringr::str_match(line, "^#\\s+(.+)(:)(.+)$")
                      key = pattns[2]    
                      val = pattns[4]     

                      if (nchar(key) > 0 & nchar(val) > 0) {
                          headerSets[key] <- val 
                          headerSets
                      }
                      else {
                          NULL
                      }
                  }
                  else {
                      NULL
                  }

              }
              result <- lapply(lines, function(x) parser(x))  

              # Remove NULL value from list
              removeNullRec <- function( x ){  
                  x <- x[ !sapply( x, is.null ) ]
                  if( is.list(x) ){
                      x <- lapply( x, removeNullRec)
                  }
                  return(x)
              }
              colHeaderSets <- removeNullRec(result)

              # Parse phvAcc line (## phvxxx, phvxxx, ... phvxxx)
              result2 <- lapply(lines, function(x) {
                                    line <- x
                                    if ( grepl('^##\\s*(phv.+)$', line, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE) == T) {
                                        pattns = stringr::str_match(line, "^##\\s*(phv.+)$")
                                        phvCombo = pattns[2]    
                                    }
                               })  
              colPhvComboSets <- removeNullRec(result2)

              if (length(colPhvComboSets) > 0) {

                  phvFields = strsplit(colPhvComboSets[[1]], '\t') [[1]]

                  colNames <- read.table(phtFile, nrows = 1, header = FALSE, sep =';', stringsAsFactors = FALSE)
                  colNameFields = strsplit(unlist(colNames), '\t') [[1]]

                  # Ceate a list of phvAcc-colName pair
                  phvColNameSets = list()
                  phvColNameComboSets = c()
                  for (i in 1:length(phvFields)) {

                      # Note: The phv and colName lists index may or may not match. For example: 
                      # _Sample file 1st, 2nd fields are always "dbGaP SubjID", "dbGaP SampleID", so that Starting from 3rd field, the field names match phv fields.
                      # _Subject file 1st field is always "dbGaP SubjID". Starting from 2nd field, so that the field names match phv fields.
                      # _Pedgree file 1st field is always "dbGaP SubjID". Starting from 2nd field, so that the field names match phv fields.
                      # Other type of pht file could have "dbGaP SubjID" as the 1st field in most of cases, but some others may not.

                      # Use the length difference of phvFields and colNameFields to make the match between the indices of the two lists 

                      # Get length difference of the two lists 
                      lenDiff = length(colNameFields) - length(phvFields)

                      # Matching index of colNameFields
                      mi = i + lenDiff
                      phvColNameSets[unlist(phvFields[i])] = unlist(colNameFields[mi])

                      #phvColNameComboSets[i]<-paste(phvFields[i], colNameFields[mi] , sep=":") 

                      # Make it like 
                      # "Sex:phv00195357.v4.p4.c1:Sex" "AD: phv00195358.v4.p4.c1:AD" "Age: phv00195359.v4.p4.c1"
                      phvColNameComboSets[i]<-paste(colNameFields[mi], phvFields[i], sep=":") 
                  }
                  return(list(phvFields, colNameFields, phvColNameSets, phvColNameComboSets))

              }
          })


# -------------------------
# Method: manageArchive
# -------------------------

#' (internal) Logging archive manager 
#'
#' The method moves expired log files into archive and deletes old archived files based on given conditions. 
#'
#' @name manageArchive 
#' @param object Commons class object.
#' @param workFile a character string. The path to the log file, such as project_setup.log.
#' @param archiveDir a character string. The path to the archive directory.
#' @param type a character string. The logging type of either 'setup' for project_setup.log or 'process' for data_process.log.
#' @param ... There are optional arguments. 
#' @param maxSizeInMb an integer. (optional) The maximum size (MB) of a log file. The log file is moved to the archived_log directory after reaching the maxSizeInDb (default 1MB).
#' @param keptDay an integer. (optional) The maximum days of an archived log file is kept. The archived log file is deleted after the durationDay (default 90 days). 
#' @param minFileAge an integer. (optional) The minimum age in days of a file can be archived.  
#' @export manageArchive
#' @keywords internal

# c <- Commons()
# manageArchive(c, workFile = '/home/hao/.dbgapr/project_setup.log', archiveDir = '/home/hao/.dbgapr/archived_log', maxSizeInMb = 1, keptDay = 90)
setGeneric(name = "manageArchive",
           def = function(object, workFile, archiveDir, type, ...) {
               standardGeneric("manageArchive")
           }
           )

#' @describeIn manageArchive A method of class Commons 
setMethod(
          f = "manageArchive",
          signature = c("Commons","character"),
          definition = function(object, workFile, archiveDir, type = 'infojson', ..., maxSizeInMb = 1, keptDay = 90,  minFileAge = 1) {
              workFile <- checkInputPath(object, workFile)
              archiveDir <- checkInputPath(object, archiveDir)

              # parse name from path
              workFileName = basename(workFile)

              currTime = toString(as.POSIXlt(Sys.time()))

              prjDotDir = object@prjDotDir
              prjDir = object@prjDir
              configFile = object@configFile

              lastFileInfoFile = object@lastFileInfoFile 

              #### Determine move log file to archive or not #### 
              # Rules for log file management
              # 1. If size reach default 1MB and the log file is older than ond day,  rename it with dateInfo and move to log_archive directory. 
              # 2. Remove archived file that is older than default 90 days.

              if (file.exists(workFile) == TRUE) {

                  # obtain log file timestamp and size
                  fileInfo = file.info(workFile)
                  fileSize = fileInfo$size 

                  # Check total size of archived log files  
                  if (dir.exists(archiveDir)) {
                      archFileNames = list.files(archiveDir)

                      # Remove archived log files that are older than 3 months
                      removeOldFile <- function(s1, s2){
                          archFileWithPath = file.path(s1, s2) 
                          archFileInfo = file.info(archFileWithPath)
                          timeDiff = as.numeric(difftime(currTime, archFileInfo$mtime, units="days"))

                          if (!is.na(timeDiff)) {
                              if ((timeDiff > keptDay) == TRUE) { 	# older than 3 months
                                  if (file.exists(archFileWithPath)) {
                                      file.remove(archFileWithPath)
                                      cat("\n[INFO] The log file larger than 1MB and older than 3 months is removed ---", archFileWithPath, sep="\n")
                                      return(archFileWithPath)
                                  }
                              }
                          }
                      }

                      # Loop through each archived file
                      oldFiles <- lapply(archFileNames, FUN=function(x, y) removeOldFile(s1 = archiveDir, s2=x))
                  }

                  maxSizeInByte = maxSizeInMb * 1000
                  if (fileSize > maxSizeInByte) {		# 1MB limit for log file 
                      timeDiff = as.numeric(difftime(fileInfo$mtime, currTime, units="days"))

                      if (type == 'infojson') {
                          # Move  workFile to lastWorkFile under prjDotDir 
                          lastWorkFile = lastFileInfoFile 
                          file.copy(from = workFile,  to = lastWorkFile, overwrite = TRUE, copy.mode = TRUE)
                      }

                      if (timeDiff > minFileAge) {  # to avoid split the log of the same run, make sure to only archive the log file at least one day old. 

                          # Create log_archive dir
                          if (!dir.exists(archiveDir)) {
                              dir.create(archiveDir, showWarnings = TRUE, recursive = T, mode = "0777")
                          }

                          # Parse from the 1st line of log file such as
                          # [2016-06-17 12:46:59 Commons::getPrjDir] [ERROR] this is an error
                          # to
                          # 2016-06-17 12:46:59
                          lines <- paste(readLines(workFile, encoding="UTF-8"))
                          firstLine = lines[1:0]
                          pattns = stringr::str_match(firstLine, "^\\[(.+:\\d+)\\s([a-z A-Z])")
                          firstLineTimeStamp = pattns[2]    
                          firstLineTimeStampConcat <- stringr::str_replace(firstLineTimeStamp, " ", "_")	

                          # Do the same for the last line
                          lastLine = lines[length(lines)]
                          pattns = stringr::str_match(lastLine, "^\\[(.+:\\d+)\\s([a-z A-Z])")
                          lastLineTimeStamp = pattns[2]    
                          lastLineTimeStampConcat <- stringr::str_replace(lastLineTimeStamp, " ", "_")	

                          if (is.na(lastLineTimeStampConcat)) {
                              currTime = toString(as.POSIXlt(Sys.time()))

                              # Example: "2016-10-20 14:55:57" to 2016-10-20-14:55:5
                              timeStamp = gsub(" ", "-", currTime)

                              archFileName = paste(timeStamp, "_", workFileName, sep="") 

                          }
                          else {
                              archFileName = paste(firstLineTimeStampConcat, "_to_", lastLineTimeStampConcat, "_", workFileName, sep="") 
                          }

                          # Compose archived log file name 
                          archFile = file.path(archiveDir, archFileName)
                          # Move log file to archive
                          file.rename(from = workFile,  to = archFile)
                      }
                  }
              }
          })


# ---------------------------
# Method: dataDicXmlParser
# ---------------------------

#' (internal, deprecated) XML parser of the data dictionary file  
#'
#' The method parses the field values of a given data dictionary XML file. It is called through the function "parseDataDicByStudy".
#'
#' @name dataDicXmlParser 
#' @param object Commons class object.
#' @param xmlFileName a character string. The name of the data dictionary XML file.
#' @param xmlFilePath a character string. The path to the directory where the data dictionary XML file is located.
#' @param phsAcc a character string. The dbGaP study accession.
#' @param ... There are optional arguments.
#' @param fileInfoDF a data frame. (optional) The copied file info read from the pht_file_info_indiv.json under the ncbi/dbgapr_conf directory. This is needed when computeType is TRUE.
#' @param computeType logical value. (optional) If TRUE, compute data type based on data value and include it in the meta-info file. If FALSE, not compute data type. Note: This step is computationally expensive. Do not set TRUE if it is a large study that have a large number of datasets. 
#' @return a character string. The path to the resulting rds file of the parsed data dictionary.
#' @export dataDicXmlParser 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' filename = 'phs000001.v3.pht000001.v2.genspecphenotype.data_dict.xml'
#' filepath = '/home/foo/my_project/gapwork/data/phs000001/phs000001.v3/data_dict'
#' dataDicXmlParser(c, xmlFileName=filename, xmlFilePath=filepath, phsAcc='phs000001.v3.p1')
#'}

# c <- Commons()
# dataDicXmlParser(c, xmlFileName = 'phs000001.v3.pht000001.v2.genspecphenotype.data_dict.xml', xmlFilePath = '/netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/phs000001/phs000001.v3/data_dic', 'phs000001.v3.p1')

# dataDicXmlParser(c, xmlFileName = 'phs000007.v27.pht000825.v4.vegf3_1s.data_dict.xml', xmlFilePath = '/netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/phs000007/phs000007.v27/data_dic', 'phs000007.v27.p10')

setGeneric(name = "dataDicXmlParser",
           def = function(object, xmlFileName, xmlFilePath, phsAcc, ...) {
               standardGeneric("dataDicXmlParser")
           })

#' @describeIn dataDicXmlParser A method of class Commons 
setMethod(
          f = "dataDicXmlParser",
          signature = c("Commons","character"),
          definition = function(object, xmlFileName, xmlFilePath, phsAcc, ..., fileInfoDF = data.frame(), computeType = FALSE) {
              xmlFilePath <- checkInputPath(object, xmlFilePath)

              # Validate accession 
              phsAcc <- cleanObjAcc(object, acc = phsAcc, type = 'phs') 

              xmlFile = file.path(xmlFilePath, xmlFileName)

              fileName = basename(xmlFile)
              filePath = dirname(xmlFile)

              prjDotDir = object@prjDotDir
              prjDir = object@prjDir
              prjDataDir = object@prjDataDir
              configFile = object@configFile
              fileInfoArchDir = object@fileInfoArchDir
              prjSetupLog = object@prjSetupLog

              # Parse file attrs from var_report XML file name 
              # Examples: 
              # phs000001.v3.pht000001.v2.p1.genspecphenotype.data_dict.xml
              # phs000001.v3.pht000374.v2.followup.data_dict.xml
              # phs000429.v1.pht002481.v1.areds_data_final_11.data_dict_2012_02_24.xml 
              pattns = stringr::str_match(fileName, "(phs0+\\d+\\.v\\d+)\\.(.+)")
              fileStAccNoP = pattns[2]

              pattns = stringr::str_match(fileName, "(pht0+\\d+\\.v\\d+)\\.(.+)\\.data_dict.*\\.xml$")
              filePhtAccNoP = pattns[2]		# "pht000001.v2.p1"
              filePhtAbbrev = pattns[3]	# example: "genspecphenotype"

              #	pattns = str_match(fileName, "(phs0+)(\\d+)\\.v(\\d+)\\.(pht0+)(\\d+)\\.v(\\d+)\\.p(\\d+)$")
              #	patphsWithZero = pattns[2]
              #	patphsAccId = pattns[3]
              #	patphsAccNoVer = paste(phsWithZero, phsAccId, sep='')
              #	patphsAccVer = pattns[4]
              #	patphsAccPnum = pattns[5]
              #	patphsAccIds <- list(phsAcc = phsAcc,  phsAccNoVer = phsAccNoVer, phsAccId = phsAccId, phsAccVer = phsAccVer,  phsAccPnum = phsAccPnum)

              ##################
              # XML Parsing
              ##################

              # Get XML doc
              doc <- xmlTreeParse(xmlFile, useInternalNodes = TRUE)
              root <- xmlRoot(doc)

              # Get dataset description
              # Note: Dataset description may contain HTML tag. No html gree version available.
              datasetDesc = xpathSApply(root, "./description", xmlValue)

              # Get all variable nodes
              var_nodes <- xpathSApply(doc, "//variable") 

              ##### Sometime the  data_dic file doesn't have description #####

              # Example 1:  (no descriptioin)
              # <data_table id="pht000001.v2" study_id="phs000001.v3" participant_set="1" date_created="Fri Feb 24 11:40:35 2012">
              #   <variable id="phv00054118.v1">
              #       <name>ID2</name>
              #       <description>DUMMY ID NUMBER (EYE DISEASE RESEARCH ONLY PARTICIPANTS HAVE A DIFFERENT DUMMY ID NUMBER)</description>
              #       <type>string</type>
              #   </variable>
              #
              # Example 2: (with description) 
              # <data_table id="pht000825.v4" study_id="phs000007.v27" participant_set="10" date_created="Tue Jan 12 16:45:10 2016">
              #  <description>Vascular Endothelial Growth Factor, Generation 3 Cohort Exam 1</description>
              #  <variable id="phv00073738.v3">
              #      <name>shareid</name>
              #      <description>SHARE ID Number</description>
              #      <type>integer</type>
              #  </variable>

              ########################################################
              # Lookup datasetInfoFile if descrption is not found 
              ########################################################

              if (length(datasetDesc) == 0) {
                  extStudyDatasetInfoFile = object@extStudyDatasetInfoFile
                  extStudyDatasetInfoDF <- read.table(extStudyDatasetInfoFile, header = T, fill = TRUE, quote = "", sep ='\t', stringsAsFactors = FALSE, encoding="UTF-8")  

                  # Get description  by phsAccId, phsAccVer, phtAcc 
                  parseIdsFromStAcc =  parseIdsFromStAcc(object, phsAcc = phsAcc)
                  phsAccId = parseIdsFromStAcc$phsAccId
                  phsAccVer = parseIdsFromStAcc$phsAccVer

                  matchStudyDatasetInfo <- subset(extStudyDatasetInfoDF, extStudyDatasetInfoDF$study_id == phsAccId & extStudyDatasetInfoDF$study_version == phsAccVer & extStudyDatasetInfoDF$dataset_accession_nop == filePhtAccNoP, select = c('study_accession', 'dataset_accession', 'name', 'description'))  

                  datasetDesc <- head(matchStudyDatasetInfo, 1)$description

              }

              #########################
              # Process XmL var_nodes
              #########################

              # Notes: 
              # 1. There are 9 columns in a data_dic table
              # variable_id, variable_name, description, type, units, logical_min, logical_max, values (multiple code values),  comment
              # 2. Sometimes, an unique key element such as below is included. It can be ignored. 
              # <unique_key phv="00054260" version="2">ID2</unique_key>

              buildVarComboFromXml <- function(comboDF, xmlNode, phsAcc, phtAcc, phtAbbrev) {

                  # Validate accession 
                  phtAcc <- cleanObjAcc(object, acc = phtAcc, type = 'pht') 

                  ##########################
                  # Parse variable node
                  ##########################
                  var_id <- xpathSApply(xmlNode, ".", xmlGetAttr, 'id')
                  var_name = xpathSApply(xmlNode, "./name", xmlValue)
                  var_desc = xpathSApply(xmlNode, "./description", xmlValue)


                  ####################################################################
                  # Note: Enumerated integer type field value is not uniformed
                  ####################################################################
                  # It is "enum_integer" in database and  "enumerated integer" in data_dic XML

                  var_type = xpathSApply(xmlNode, "./type", xmlValue)
                  if (length(var_type) == 0) {
                      var_type = NA
                  }
                  else {
                      if (nchar(var_type) == 0) {
                          var_type = NA
                      }
                  }
                  var_units = xpathSApply(xmlNode, "./unit", xmlValue)
                  if (length(var_units) == 0) {
                      var_units = NA
                  }
                  else {
                      if ( nchar(var_units) == 0) {
                          var_units = NA
                      }
                  }

                  var_logic_min = xpathSApply(xmlNode, "./logical_min", xmlValue)
                  if (length(var_logic_min) == 0) {
                      var_logic_min = NA
                  }
                  else {
                      if (nchar(var_logic_min) == 0) {
                          var_logic_min = NA
                      }

                  }
                  var_logic_max = xpathSApply(xmlNode, "./logical_max", xmlValue)
                  if (length(var_logic_max) == 0) {
                      var_logic_max = NA
                  }
                  else {
                      if (nchar(var_logic_max) == 0) {
                          var_logic_max = NA
                      }
                  }

                  # Multiple code values, make them a pipe delimited combo
                  var_code_values = xpathSApply(xmlNode, "./value", xmlValue) 
                  var_codes = xpathSApply(xmlNode, "./value", xmlAttrs) 


                  # Example <variable> node with code value 
                  # <variable id="phv00054261.v2">
                  # 	<name>REASON</name>
                  #	<description>REASON (ALL PARTICIPANTS)</description>
                  #	<type>enumerated integer</type>
                  #	<value code="1">Cardiovascular</value>
                  #	<value code="2">Respiratory</value>
                  #	<value code="3">Malignancy</value>
                  #	<value code="4">Accident</value>
                  #	<value code="5">Unknown</value>
                  #	<value code="6">Other</value>
                  # </variable>

                  # seq_along: flatens the vector
                  code_with_values <- lapply(seq_along(var_code_values), function(a, b, i) {

                                                 # a: list var_code_values 
                                                 # b: list var_codes
                                                 # i: the index num of the loop 
                                                 # code_with_value: such as  "3:Malignancy" 
                                                 code_with_value = paste(b[i], a[i], sep=":")

                                                 return (code_with_value)

                               }, a = var_code_values, b = var_codes)

                  if (length(var_code_values) == 0) {
                      var_code_value_combo = NA
                  }
                  else {
                      #  "1:Cardiovascular|2:Respiratory|3:Malignancy|4:Accident|5:Unknown|6:Other"
                      var_code_value_combo = paste(code_with_values, collapse="|")
                  }


                  #######################################
                  # Add calculated_type field
                  #######################################

                  calc_type = NA
                  if (computeType == TRUE) {
                      calc_type <- detectVariableDataType(object, phsAcc = phsAcc, phtAcc = filePhtAccNoP , phvAcc = var_id, varName = var_name, codeValCombo = var_code_value_combo, fileInfoDF = fileInfoDF)
                  }

                  ##################
                  # Skip comment
                  ##################
                  #var_comment = xpathSApply(xmlNode, "./comment", xmlValue) 
                  #if (length(var_comment) == 0) {
                  #var_comment = NA
                  #}

                  # Compose the combo data-frame
                  # Note: Comment is removed because it contains html tags that are hard to remove
                  id = I(c(var_id))
                  name = I(c(var_name))
                  description = I(c(var_desc))
                  type = I(c(var_type))
                  computed_type = I(c(calc_type))
                  units = I(c(var_units))
                  logic_min=I(c(var_logic_min))
                  logic_max = I(c(var_logic_max))
                  code_value_combo = I(c(var_code_value_combo))
                  #comment = I(c(var_comment))
                  study_accession = I(c(phsAcc))
                  dataset_accession = I(c(filePhtAccNoP))
                  dataset_name = I(c(filePhtAbbrev))
                  #dataset_description = I(c(datasetDesc))  # this gives error: arguments imply differing number of rows: 1, 0
                  dataset_description = toString(datasetDesc)
                  source_file = I(c(xmlFileName))

                  thisDF = data.frame(
                                      id,
                                      name,
                                      description,
                                      type,
                                      computed_type,
                                      units,
                                      logic_min,
                                      logic_max,
                                      code_value_combo,
                                      study_accession,
                                      dataset_accession,
                                      dataset_name,
                                      dataset_description,
                                      source_file
                                      )

                  return (thisDF)

              }


              ##### Loop through the XML var_nodes #####
              resultDF <- lapply(var_nodes, FUN=function(x) buildVarComboFromXml(xmlNode = x, phsAcc = phsAcc, phtAcc = filePhtAccNoP, phtAbbrev = filePhtAbbrev))

              # Resulting resultDF is a list of dataframe
              # Now! Combine the list items into a single dataframe 
              comboResultDF <- do.call("rbind", resultDF) 


              # Change description from all capital to 1st letter capital
              description = ''                # mute CMD check note: no global no visible binding ... 
              comboResultDF <- transform(comboResultDF, description = tools::toTitleCase(tolower(description)))


              ##################################
              # Get unique study-dataset row 
              ##################################
              # Select only dataset related rows 
              #datasetInfoRowsDF <- subset(comboResultDF, select = c('study_accession', 'dataset_accession', 'dataset_name', 'dataset_description')) 
              #distinctDatasetInfoDF <- unique(datasetInfoRowsDF[c("study_accession", "dataset_accession", 'dataset_name', 'dataset_description')])

              ############################################################
              # Get study-dataset-variable rows w/o dataset description 
              ############################################################
              # Revmoe 'dataset_description' column before save data_dic data to files
              #comboResultDF <- comboResultDF[,-grep("dataset_description", colnames(comboResultDF))]



              ######################################
              # Write each dataset data to files
              ######################################

              # Write to rds 
              rdsDumpDir = file.path(xmlFilePath, 'rds_dump')
              if (!dir.exists(rdsDumpDir)) {
                  dir.create(rdsDumpDir, showWarnings = TRUE, recursive = T, mode = "0777")
              }

              rdsFileName = paste(phsAcc, '_', filePhtAccNoP, "_data_dic.rds", sep='')
              dataDic_rdsFile = file.path(rdsDumpDir, rdsFileName) 
              saveRDS(comboResultDF, file=dataDic_rdsFile)

              # Write to csv
              csvDumpDir = file.path(xmlFilePath, 'csv_dump')
              if (!dir.exists(csvDumpDir)) {
                  dir.create(csvDumpDir, showWarnings = TRUE, recursive = T, mode = "0777")
              }
              csvFileName = paste(phsAcc, '_', filePhtAccNoP, "_data_dic.csv", sep='')
              dataDic_csvFile = file.path(csvDumpDir, csvFileName) 
              write.csv(comboResultDF, file = dataDic_csvFile, row.names=FALSE)

              # Convert to json and save 
              jsonDumpDir = file.path(xmlFilePath, 'json_dump')
              if (!dir.exists(jsonDumpDir)) {
                  dir.create(jsonDumpDir, showWarnings = TRUE, recursive = T, mode = "0777")
              }
              comboResultJson <- toJSON(comboResultDF, pretty=T)
              jsonFileName = paste(phsAcc, '_', filePhtAccNoP, "_data_dic.json", sep='')

              dataDic_jsonFile = file.path(jsonDumpDir, jsonFileName) 
              write(comboResultJson, file = dataDic_jsonFile, ncolumns = if(is.character(comboResultJson)) 1 else 5, append = F, sep = "\n")

              # Save to left-justified text 
              txtDumpDir = file.path(xmlFilePath, 'brief_text_dump')
              if (!dir.exists(txtDumpDir)) {
                  dir.create(txtDumpDir, showWarnings = TRUE, recursive = T, mode = "0777")
              }
              comboResultBriefDF <- subset(comboResultDF, select = c("id", "name", "type", "dataset_name", "description"))
              txtFileName = paste(phsAcc, '_', filePhtAccNoP, "_data_dic_brief_ljustify.txt", sep='')
              dataDic_txtFile = file.path(txtDumpDir, txtFileName) 
              gdata::write.fwf(x=comboResultBriefDF, file=dataDic_txtFile, quote=F, justify="left", sep="    ")

              # Rreturn path to rds file 
              return (invisible(dataDic_rdsFile))

          })

# ------------------------------
# Method: parseDataDicByStudy
# ------------------------------

#' (internal, deprecated) Parse data dictionary XML files of a specified study
#'
#' The method parses out the values from the data dictionary XML files of the given study. The parsed values are merges into a single data frame, which is further saved to files of various formats (rds, csv, and json). Before calling the function, the data dictionary files of the study should be made available in the data_dic sub-directory under the project directory. Checkout the \code{\link{searchCopyPhenoFiles}} function for how to move the files to the directory if not. In most of cases, this function is called through \code{\link{parseDataDic}}.
#'
#' @name parseDataDicByStudy 
#' @param object Commons class object.
#' @param phsAcc a character string. The dbGaP study accession.
#' @param ... There are optional arguments. 
#' @param computeType logical value. (optional) If TRUE, compute data type based on data value and include it in the meta-info file. If FALSE, not compute data type. Note: This step is computationally expensive. Do not use it if it is a large study such as Framingham.
#' @return (invisible) a data frame. The merged content of the parsed data dictionary files.
#' @export parseDataDicByStudy 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' parseDataDicByStudy(c, phsAcc = 'phs000001.v3.p1') 
#' parseDataDicByStudy(c, phsAcc = 'phs000007.v27') 
#'}

setGeneric(name = "parseDataDicByStudy",
           def = function(object, phsAcc, ...) {
               standardGeneric("parseDataDicByStudy")
           })

#' @describeIn parseDataDicByStudy A method of class Commons 
setMethod(
          f = "parseDataDicByStudy",
          signature = c("Commons","character"),
          definition = function(object, phsAcc, ..., computeType = TRUE) {

              # Validate accession 
              phsAcc <- cleanObjAcc(object, acc = phsAcc, type = 'phs') 

              prjDotDir = object@prjDotDir
              prjDir = object@prjDir
              prjDataDir = object@prjDataDir 

              # Get all copied file info
              fileInfoFile = object@fileInfoFile
              fileInfoDF <- fromJSON(fileInfoFile, flatten=TRUE)

              parseIdsFromStAcc =  parseIdsFromStAcc(object, phsAcc = phsAcc)
              phsAccNoVer = parseIdsFromStAcc$phsAccNoVer

              # NOT USED! 
              # Example location of study-version variable report  
              # /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/phs000001/phs000001.v3.p1/var_report
              #varReportDir = file.path(prjDataDir,phsAccNoVer, phsAcc, 'var_report') 
              #if (!dir.exists(varReportDir)) {
              #dir.create(varReportDir, showWarnings = TRUE, recursive = T, mode = "0777")
              #}

              # Example location of study (no-version) data dictionary
              # /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/phs000001/phs00000.v3.p1/data_dic
              dataDicDir = file.path(prjDataDir,phsAccNoVer, phsAcc, 'data_dic') 

              if (!dir.exists(dataDicDir)) {
                  dir.create(dataDicDir, showWarnings = TRUE, recursive = F, mode = "0777")
              }


              if (dir.exists(dataDicDir)) {

                  # Examples: 
                  # phs000001.v3.pht000001.v2.genspecphenotype.data_dict.xml 
                  # phs000429.v1.pht002481.v1.areds_data_final_11.data_dict_2012_02_24.xml
                  pattn = "phs[0-9]+.*pht[0-9]+.*data_dict.*.xml"
                  fnames <- list.files(path = dataDicDir, pattern = pattn, recursive=TRUE, full.names=F)

                  ############################
                  # Parse each file 
                  ############################
                  # Loop through and parse each XML file
                  if (length(fnames) == 0) {

                      cat("\n")
                      type = 'process'
                      level = 'error'
                      show = T
                      mesg = paste(
                                   "The data dictionary files are not found in the user porject study data_directory. The files need to be downloaded from dbGaP and made available in the directory by running 'copyUserFile' function.\n   ", 
                                   "Study accessison : ", phsAcc, "\n   ", 
                                   "DataDicDir : ", dataDicDir, "\n", 
                                   sep=""
                                   )
                      writeLog(object,  type = type, level = level, message = mesg, show = show) 
                  }
                  else {

                      msg = paste("Processing data dictionary files of study ", phsAcc, ". It may take quite a while   ...\n", sep="")
                      cat(msg)

                      #########################################
                      # Get parsed data of each pht file
                      #########################################
                      dataDic_rdsFiles <- lapply(fnames, FUN=function(x) dataDicXmlParser(object, xmlFileName = x, xmlFilePath = dataDicDir, phsAcc=phsAcc, fileInfoDF=fileInfoDF, computeType=computeType))

                      # Create data_dic_combo dir
                      dataDicComboDir = file.path(dataDicDir, 'combo_dump') 
                      if (!dir.exists(dataDicComboDir)) {
                          dir.create(dataDicComboDir, showWarnings = TRUE, recursive = T, mode = "0777")
                      }

                      #######################
                      # Merge each file
                      #######################
                      # Merge all var_report of all phts and save
                      mergeDataDicDF <- do.call('rbind', lapply(dataDic_rdsFiles, readRDS))

                      ############################################
                      # Write Combo Data of all datasets to files 
                      ############################################

                      #####################################################
                      # Write data_dic: study-dataset-variable info files
                      #####################################################

                      # Save to rds
                      mergeDataDicFileName = paste(phsAcc, '_data_dic_combo.rds', sep='')
                      mergeDataDic_rdsFile = file.path(dataDicComboDir, mergeDataDicFileName)
                      saveRDS(mergeDataDicDF, file=mergeDataDic_rdsFile)

                      # Save to csv
                      mergeDataDicCsvFileName = paste(phsAcc, '_data_dic_combo.csv', sep='')
                      mergeDataDic_csvFile = file.path(dataDicComboDir, mergeDataDicCsvFileName)
                      #write.csv(mergeDataDicDF, file = mergeDataDic_csvFile, row.names=FALSE)
                      write.table(mergeDataDicDF, file = mergeDataDic_csvFile, sep = "\t", row.names = FALSE)

                      # Convert to json and save 
                      mergeDataDicJson <- toJSON(mergeDataDicDF, pretty=T)
                      mergeDataDicJsonFileName = paste(phsAcc, '_data_dic_combo.json', sep='')
                      mergeDataDic_jsonFile = file.path(dataDicComboDir, mergeDataDicJsonFileName)
                      write(mergeDataDicJson, file = mergeDataDic_jsonFile, ncolumns = if(is.character(mergeDataDicJson)) 1 else 5, append = F, sep = "\n")

                      # Save to left-justified text 
                      mergeDataDicSubDF = subset(mergeDataDicDF, select = c("id", "name", "type", "dataset_accession", "dataset_name", "description")) 
                      mergedDataTxtFileName = paste(phsAcc, '_data_dic_combo_brief_ljustify.txt', sep='')
                      mergeDataDic_txtFile = file.path(dataDicComboDir, mergedDataTxtFileName)
                      gdata::write.fwf(x=mergeDataDicSubDF, file=mergeDataDic_txtFile, quote=F, justify="left", sep="    ")

                      #################################
                      # Write study-dataset info files
                      #################################

                      # Select only dataset related rows 
                      studyDatasetInfoRowsDF <- subset(mergeDataDicDF, select = c('study_accession', 'dataset_accession', 'dataset_name', 'dataset_description')) 
                      studyDatasetInfoDF <- unique(studyDatasetInfoRowsDF[c("study_accession", "dataset_accession", 'dataset_name', 'dataset_description')])

                      # Save to rds
                      studyDatasetInfoFileName = paste(phsAcc, '_study_dataset_info_combo.rds', sep='')
                      studyDatasetInfo_rdsFile = file.path(dataDicComboDir, studyDatasetInfoFileName)
                      saveRDS(studyDatasetInfoDF, file=studyDatasetInfo_rdsFile)

                      # Save to csv (no description)
                      studyDatasetInfoCsvFileName = paste(phsAcc, '_study_dataset_info_combo.csv', sep='')
                      studyDatasetInfo_csvFile = file.path(dataDicComboDir, studyDatasetInfoCsvFileName)
                      #write.csv(studyDatasetInfoDF, file = studyDatasetInfo_csvFile, row.names=FALSE)
                      write.table(studyDatasetInfoDF, file = studyDatasetInfo_csvFile, sep = "\t", row.names = FALSE)

                      # Convert to json and save (with description) 
                      studyDatasetInfoJson <- toJSON(studyDatasetInfoDF, pretty=T)
                      studyDatasetInfoJsonFileName = paste(phsAcc, '_study_dataset_info_combo.json', sep='')
                      studyDatasetInfo_jsonFile = file.path(dataDicComboDir, studyDatasetInfoJsonFileName)
                      write(studyDatasetInfoJson, file = studyDatasetInfo_jsonFile, ncolumns = if(is.character(studyDatasetInfoJson)) 1 else 5, append = F, sep = "\n")

                      # Save the subset to left-justified text 
                      studyDatasetInfoSubDF = subset(studyDatasetInfoDF, select = c("study_accession", "dataset_accession", "dataset_name")) 
                      studyDatasetInfoTxtFileName = paste(phsAcc, '_study_dataset_info_combo_brief_ljustify.txt', sep='')
                      studyDatasetInfo_txtFile = file.path(dataDicComboDir, studyDatasetInfoTxtFileName)
                      gdata::write.fwf(x=studyDatasetInfoSubDF, file=studyDatasetInfo_txtFile, quote=F, justify="left", sep="    ")


                      return (invisible(mergeDataDicDF))

                  } # end of if fnames == 0

              }
          })


# -------------------------------------- 
# Method: mergeDatasetConsentByStudy
# -------------------------------------- 

#' (internal) Merge data of different consent groups in a study
#'
#' The method merges the dataset files that have the same dataset accession but different consents into one file.
#'
#' @name mergeDatasetConsentByStudy
#' @param object Commons class object.
#' @param phsAcc a character string. The dbGaP study accession.
#' @return a data frame. Meta-info of the merged data files.  
#' @export mergeDatasetConsentByStudy
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' mergeDatasetConsentByStudy(c, phsAcc = 'phs000001.v3.p1')
#'}

#
# mergeDatasetConsentByStudy(c, phsAcc = 'phs000007.v27')
# mergeDatasetConsentByStudy(c, phsAcc = 'phs000651.v7')

# mergeDatasetConsentByStudy(c, phsAcc = 'phs000226.v4.p1')
# mergeDatasetConsentByStudy(c, phsAcc = 'phs000680.v1.p1')
# mergeDatasetConsentByStudy(c, phsAcc = 'phs000001.v3.p1')
#
setGeneric(
           name = "mergeDatasetConsentByStudy",
           def = function(object, phsAcc) {
               standardGeneric("mergeDatasetConsentByStudy")
           })

#' @describeIn mergeDatasetConsentByStudy returns study accession
setMethod(
          f = "mergeDatasetConsentByStudy",
          signature = c("Commons", "character"),
          definition = function(object, phsAcc) {

              # Validate accession 
              phsAcc <- cleanObjAcc(object, acc = phsAcc, type = 'phs') 

              # Introduce a linebreak for console display
              cat("\n")

              prjDotDir = object@prjDotDir
              prjDir = object@prjDir
              prjDataDir = object@prjDataDir 

              fileInfoFile = object@fileInfoFile

              # Parse phsAccNoVer from phsAcc
              #pattns = str_match(phsAcc, "^(phs0*)(\\d+)(\\.v(\\d+))*")
              #phsAccNoP = pattns[1]	 		# phs000007.v20
              #phsWithZero = pattns[2] 		# phs00000 
              #stId = pattns[3] 				# 7 
              #phsAccNoVer = paste(pattns[2], pattns[3], sep="") # phs000007

              parseIdsFromStAcc =  parseIdsFromStAcc(object, phsAcc = phsAcc)
              phsAccNoVer = parseIdsFromStAcc$phsAccNoVer

              # ATTN! TO DO!
              # Note: phsAcc should be parsed from prjDataDir	

              # Directory for pht files with combined consents
              # Example: 
              # /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/phs000001/phs000001.v3.p1/combined
              # /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/phs000007/phs000007.v27/combined
              destnDir = file.path(prjDataDir, phsAccNoVer, phsAcc, 'combined')
              destnLogDir = file.path(destnDir, 'log')

              if (!dir.exists(destnDir)) {
                  dir.create(destnDir, showWarnings = TRUE, recursive = T, mode = "0777")
              }

              if (!dir.exists(destnLogDir)) {
                  dir.create(destnLogDir, showWarnings = TRUE, recursive = T, mode = "0777")
              }

              #######################################
              # Define studyPhtComboInfoFile Json
              #######################################
              studyPhtComboInfoFileName = paste(phsAcc, '_study_pht_combo_info.json', sep='')
              studyPhtComboInfoFile = file.path(destnLogDir, studyPhtComboInfoFileName)

              lastStudyPhtComboInfoFileName = paste('last_', phsAcc, '_study_pht_combo_info.json', sep='')
              lastStudyPhtComboInfoFile = file.path(destnLogDir, lastStudyPhtComboInfoFileName)

              ##############################################
              # Revmoe all .txt files under the destnDir
              ##############################################
              # Note recursively !!  (Kepp the log files) 
              do.call(file.remove, list(list.files(destnDir, recursive = FALSE, pattern = "\\.txt", full.names = TRUE)))

              ###################################
              # Merging files
              ###################################
              type = 'setup'
              level = 'info'
              show = T
              mesg = paste("Merging dataset files of different consents in study ", phsAcc, ". ..." , sep="") 
              writeLog(object,  type = type, level = level, message = mesg, show = show) 

              # Example: 
              # /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/phs000001.v3.p1/original 
              #origDir = file.path(prjDir, "gapwork", "data", phsAcc, 'original')

              #####################
              # First time run
              #####################
              if (file.exists(studyPhtComboInfoFile)) {

                  # Keep a copy as lastStudyPhtComboInfoFile before writting new  
                  file.copy(from = studyPhtComboInfoFile,  to = lastStudyPhtComboInfoFile, overwrite = TRUE, copy.mode = TRUE)

                  # When running this for the first time 
                  # Important! Remove existing comboInfo 
                  file.remove(studyPhtComboInfoFile)
              }

              # From the original copied file directories, search for phenotype .gz or .txt(ungzip) file 
              if (file.exists(fileInfoFile)) {
                  # Read fileInfoFile json into a dataframe

                  ###########################################
                  # Process each pht-consent group
                  ###########################################
                  # Iterate dataframe row of each pht-consent group

                  # S3 function
                  unionPhtConsents <- function(df, queryStAcc) {

                      # Check group row count 
                      rowCount <- nrow(df)


                      # Union when more than one files in group  
                      # Get colNames info and Append an extra column 'dataset_consent'
                      colNameCombo = df$headerColNames

                      colNames = strsplit(colNameCombo, ',') [[1]] 
                      colNames <- append(colNames, 'dataset_consent')


                      ###################################################
                      # Init dataframe unionPhtData  (combined phtData)
                      ###################################################

                      # Define dataframe column names
                      union_phtData <- as.data.frame(matrix(,0,length(colNames))) 
                      colnames(union_phtData) <- colNames 

                      # Init outside for loop
                      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                      union_filePath = "" 
                      union_isCurrent = ""
                      union_sourceFiles = ""
                      union_fStAcc = "" 
                      union_fStAccNoP = "" 
                      union_fStAccNoVer = "" 
                      union_fStId = "" 
                      union_fStVerId = "" 
                      union_fStPId = "" 
                      union_fPhtAcc = "" 
                      union_fPhtAccNoP = "" 
                      union_fPhtAccNoVer = "" 
                      union_fPhtId = "" 
                      union_fPhtVerId = "" 
                      union_fPhtAccConsent = "" 
                      union_headerPhvComboStr = "" 
                      union_headerColNameComboStr = "" 
                      union_headerPhvColNamePairComboStr = "" 
                      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                      consentCodeCombo = "" 
                      consentCodeComboForFileName = ""
                      consentShortNameCombo = "" 
                      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                      createdDate = toString(Sys.time())
                      updatedDate = toString(Sys.time())
                      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


                      ######################################################
                      # Loop through each member (row) of the pht group
                      ######################################################
                      # Parse and copy files 

                      rowCounter = 0
                      for(i in 1:nrow(df)) { 

                          row <- df[i,]

                          ############################
                          # Get pht table colnames
                          ############################
                          pathToPhtFile <- toString(df$pathToFile[1])
                          phtDataDF <- read.table(pathToPhtFile, header = T, fill = TRUE, quote = "", sep ='\t', stringsAsFactors = FALSE, encoding="UTF-8")  
                          phtColNames <- colnames(phtDataDF)
                          phtColNameCombo <- paste(phtColNames, collapse = ',')

                          ########################################################
                          # Get indivdual (not combined) pht file attributes
                          ########################################################
                          indiv_count = row$count[1]

                          ###########################################################
                          # Total 26 items excluding count, created, updated date
                          ###########################################################

                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                          indiv_fileName = row$fileName[1]
                          indiv_isCurrent = row$fileName[1]
                          indiv_pathToFile = row$pathToFile[1]
                          indiv_sourceFiles = row$sourceFiles[1]
                          indiv_fStAcc = row$fStAcc[1]
                          indiv_fStAccNoP = row$fStAccNoP[1]
                          indiv_fStAccNoVer = row$fStAccNoVer[1]
                          indiv_fStId = row$fStId[1]
                          indiv_fStVerId = row$fStVerId[1]
                          indiv_fStPId = row$fStPId[1]
                          indiv_fPhtAcc = row$fPhtAcc[1]
                          indiv_fPhtAccNoP = row$fPhtAccNoP[1]
                          indiv_fPhtAccNoVer = row$fPhtAccNoVer[1]
                          indiv_fPhtId = row$fPhtId[1]
                          indiv_fPhtVerId = row$fPhtVerId[1]
                          indiv_fPhtAccConsent = row$fPhtAccConsent[1]
                          indiv_consentCode = row$consentCode[1]
                          indiv_consentShortName = row$consentShortName[1]
                          indiv_consentType = row$consentType[1]
                          indiv_processType = row$processType[1]
                          indiv_isMulti = row$isMulti[1]
                          indiv_multiType = row$multiType[1]
                          indiv_isNcbiEnc = row$isNcbiEnc[1] 
                          indiv_headerPhvs = row$headerPhvs[1]
                          #indiv_headerColNames = row$headerColNames[1]
                          # New!
                          indiv_headerColNames = phtColNameCombo 
                          indiv_headerPhvColNamePairs = row$headerPhvColNamePairs[1]
                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                          indiv_created = row$created[1] 
                          indiv_updated = row$updated[1] 
                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                          ##########################################
                          # Merge/Combine Indiv (non-Union)  rows
                          ##########################################
                          if (indiv_consentCode != 0 & indiv_consentType == 'Indiv') {
                              rowCounter = rowCounter + 1


                              #############################
                              # Compose combo/union  attr
                              #############################
                              union_isCurrent = "yes"

                              # First item (no combine on 1st file)
                              if (rowCounter == 1) {

                                  # Assign value to init variables 
                                  union_fStAcc = indiv_fStAcc
                                  union_fStAccNoP = indiv_fStAccNoP 
                                  union_fStAccNoVer = indiv_fStAccNoVer
                                  union_fStId = indiv_fStId
                                  union_fStVerId = indiv_fStVerId
                                  union_fStPId = indiv_fStPId
                                  union_fPhtAcc = indiv_fPhtAcc
                                  union_fPhtAccNoP = indiv_fPhtAccNoP
                                  union_fPhtAccNoVer = indiv_fPhtAccNoVer
                                  union_fPhtId = indiv_fPhtId
                                  union_fPhtVerId = indiv_fPhtVerId
                                  union_fPhtAccConsent = indiv_fPhtAccConsent
                                  union_headerPhvComboStr = indiv_headerPhvs 
                                  union_headerColNameComboStr = indiv_headerColNames 
                                  union_headerPhvColNamePairComboStr = indiv_headerPhvColNamePairs 

                                  sourceFilesCombo = indiv_sourceFiles

                                  consentCodeCombo = indiv_consentCode
                                  consentShortNameCombo = indiv_consentShortName
                                  consentCodeComboForFileName = indiv_consentCode

                                  #########################################
                                  # Create combined dir for the study
                                  #########################################
                                  # Example: /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/phs000001.v3.p1/combined
                                  union_filePath = file.path(prjDir, "gapwork", "data", indiv_fStAccNoVer, indiv_fStAcc, 'combined')
                                  if (!dir.exists(union_filePath)) {
                                      dir.create(union_filePath, showWarnings = TRUE, recursive = T, mode = "0777")
                                  }

                                  union_sourceFiles =indiv_pathToFile 
                              }
                              # Combine after the first item (more than one consent)
                              else {

                                  # Check if consent code is already in codeCombo
                                  haveMatchCode = grepl(indiv_consentCode, consentCodeCombo)

                                  # Comma delimited indiv_sourceFiles
                                  sourceFilesCombo = paste(sourceFilesCombo, ",", indiv_sourceFiles, sep = "")

                                  # Example, c1,c2,c3 or GRU,NPU
                                  consentCodeCombo = paste(consentCodeCombo, ",", indiv_consentCode, sep = "")
                                  consentShortNameCombo = paste(consentShortNameCombo, ",", indiv_consentShortName, sep = "")

                                  # The combo such as c1-c2-c3 is used in naming combo file  
                                  consentCodeComboForFileName = paste(consentCodeComboForFileName, "-", indiv_consentCode, sep = "")

                                  # Source file combo 
                                  union_sourceFiles = paste(union_sourceFiles, ",", indiv_pathToFile, sep = "")
                              }


                              ###########################
                              # Combine the indiv rows 
                              ###########################

                              # ConsetnCodeCombo
                              # Example: pht000001.v2.p1.c1-c2
                              union_fPhtAccConsent = paste(union_fPhtAcc, ".", consentCodeComboForFileName, sep="") 

                              if (file.exists(indiv_pathToFile)) {

                                  ###################
                                  # Skip empty file
                                  ###################
                                  # The special case where the pht file is empty
                                  # The only content file phs000007.v27.pht000094.v3.p10.c1.icd0_19s.HMB-IRB-MDS.txt.gz 
                                  # is the following:
                                  # This file is intentionally blank because this data table does not include subjects for the "Health/Medical/Biomedical (IRB, MDS)" consent group

                                  ##### Empty file does not contain phv accession ###### 
                                  pattnCnt <- grep('phv\\d+', readLines(indiv_pathToFile))
                                  if (length(pattnCnt) > 0) {

                                      # Make sure fill=T to deal with missing value error such as "line 3 did not have 13 element" 
                                      indiv_phtData <- read.table(indiv_pathToFile, header = T, fill = TRUE, quote = "", sep ='\t', check.names = T, stringsAsFactors = FALSE, encoding="UTF-8")  


                                      ############
                                      # Cbind
                                      ############
                                      # Add new consent column 'dataset_consent'
                                      dataset_consent = indiv_consentCode 
                                      cbind_phtData = cbind(indiv_phtData, dataset_consent)

                                      ##############
                                      # Rbind 
                                      ##############
                                      # Cmbine rows of the dataframe of the same phtAcc but different contents
                                      union_phtData <- rbind(union_phtData, cbind_phtData)

                                  }
                                  else {
                                      cat("\n")
                                      type = 'process'
                                      level = 'warn'
                                      show = T
                                      mesg = paste(
                                                   "This dataset file appears to be empty, thus is skipped. --- ", indiv_pathToFile, "\n", sep=""
                                                   ) 
                                      writeLog(object,  type = type, level = level, message = mesg, show = show)
                                  }
                              } 
                              else {
                                  type = 'process'
                                  level = 'error'
                                  show = T
                                  mesg = paste(
                                               "The phenotype file reocrded in the file Info file (pht_file_info_indiv.json) does not exists. The physical file may be removed. --- ", indiv_pathToFile, "\n", sep=""
                                               ) 
                                  writeLog(object,  type = type, level = level, message = mesg, show = show)
                              }

                              # Add the name of newly added dataset_consent to column title/name
                              union_headerColNameComboStr = paste(union_headerColNameComboStr, ",", "dataset_consent", sep = "")

                          } # end of if indiv_consentCode !=0 ...

                      }  # end of for(i in 1:nrow(df))  

                      ######################################################
                      # Write Union dataFile and dataInfoFile json file 
                      ######################################################

                      if (union_fStAcc != "") {
                          # File name of merged file
                          # Example: phs000001.v3.p1_pht000370.v2.p1_combo_c1-c2.txt 
                          union_fileName = paste(union_fStAcc, "_", union_fPhtAcc, "_union_", consentCodeComboForFileName, ".txt", sep = "")
                          union_pathToFile = file.path(union_filePath, union_fileName) 


                          # Write union data  to a tab delimited file with colname
                          # Remove if the file exists
                          if (file.exists(union_pathToFile)) {
                              file.remove(union_pathToFile)
                          }

                          ####################
                          # Merge pht data
                          ####################
                          # Write combined pht data file

                          ##### ATTN!: quote = FALSE  ####
                          write.table(union_phtData, union_pathToFile, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE, append=FALSE) 


                          ########################
                          # Get Update Date 
                          ########################
                          # Grab updated date from lastComboInfoFile
                          if (file.exists(lastStudyPhtComboInfoFile)) {

                              savedLastComboInfoDF = data.frame()
                              try(savedLastComboInfoDF <- fromJSON(lastStudyPhtComboInfoFile, flatten=TRUE))


                              # Check match row with the same fileName
                              lastComboInfoFileMatchRowIndex = which(savedLastComboInfoDF$fileName == union_fileName)

                              if ( length(lastComboInfoFileMatchRowIndex) != 0) {
                                  lastComboInfoFileMatchCreateDate = unlist(savedLastComboInfoDF[lastComboInfoFileMatchRowIndex, ]$created)
                                  lastCreated <- I(c((toString(lastComboInfoFileMatchCreateDate)))) 

                                  #unionFileInfoSet$created <- lastCreated 
                                  createdDate = lastCreated
                              }
                          }

                          ############################
                          # Write UninoFileInfoFile 
                          ############################
                          itemCount = 1

                          # Create UnionFileInfoSet data.frame 
                          unionFileInfoSet <- data.frame(
                                                         count = itemCount,

                                                         ###################################################################################
                                                         # Total 26 fields 
                                                         ###################################################################################

                                                         #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                         fileName = union_fileName, 
                                                         isCurrent = 'yes', 
                                                         pathToFile = union_pathToFile, 
                                                         sourceFiles = sourceFilesCombo,
                                                         fStAcc = union_fStAcc, 
                                                         fStAccNoP = union_fStAccNoP, 
                                                         fStAccNoVer = union_fStAccNoVer, 
                                                         fStId = union_fStId, 
                                                         fStVerId = union_fStVerId, 
                                                         fStPId  = union_fStPId, 
                                                         fPhtAcc = union_fPhtAcc, 
                                                         fPhtAccNoP  = union_fPhtAccNoP, 
                                                         fPhtAccNoVer = union_fPhtAccNoVer, 
                                                         fPhtId = union_fPhtId, 
                                                         fPhtVerId = union_fPhtVerId, 
                                                         fPhtAccConsent = union_fPhtAccConsent,
                                                         consentCode  = consentCodeCombo,
                                                         consentShortName = consentShortNameCombo,
                                                         consentType = 'Union', 
                                                         processType =  'combine-all-consents', 
                                                         isMulti = 'FALSE',
                                                         multiType = 'Not', 
                                                         isNcbiEnc = 'FALSE', 
                                                         headerPhvs = union_headerPhvComboStr, 
                                                         headerColNames = union_headerColNameComboStr, 
                                                         headerPhvColNamePairs  = union_headerPhvColNamePairComboStr, 
                                                         #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                                                         created = createdDate, 
                                                         updated = updatedDate 

                                                         )


                          ##################################
                          # Write studyPhtComboInfoFile 
                          ##################################
                          newComboFileJson = '{}'

                          if (file.exists(studyPhtComboInfoFile)) {

                              # Retrive saved comboInfo
                              savedComboInfoDF <- fromJSON(studyPhtComboInfoFile, flatten=TRUE)



                              # Important! Remove existing comboInfo at each subsequent consent group 
                              file.remove(studyPhtComboInfoFile)

                              ####################
                              # Update the count
                              ####################
                              maxCount <- max(as.numeric(savedComboInfoDF$count), na.rm = TRUE)
                              newCount = strtoi(maxCount)+ 1
                              unionFileInfoSet$count <- newCount

                              # Append the new row 
                              newComboInfoDF <- rbind(savedComboInfoDF, unionFileInfoSet)

                              # Convert to json
                              newComboFileJson <- toJSON(newComboInfoDF, pretty=T)
                          }
                          else {
                              # Convert to json
                              newComboFileJson <- toJSON(unionFileInfoSet, pretty=T)
                          }

                          ########################
                          # Write Combo-Json
                          ########################
                          # Write pht_file_info_combined.json 
                          write(newComboFileJson, file = studyPhtComboInfoFile, ncolumns = if(is.character(newComboFileJson)) 1 else 5, append = F, sep = "\n")


                          ####################################
                          # Confirm created file is correct 
                          ####################################

                          # Row count of dataframe before writting the file (excluding title line)
                          dfRowCount = nrow(union_phtData)


                          # Row count of dataframe after writting the file (including title line)
                          conn <- file(union_pathToFile) 
                          lineNum = length(readLines(conn, encoding="UTF-8"))
                          close(conn)

                          if (dfRowCount + 1 == lineNum) {

                              #### Write log ####
                              type = 'setup'
                              level = 'info'
                              show = F
                              mesg = paste("Write pht combined consent file OK ! --- ", union_pathToFile, sep="") 
                              writeLog(object, type = type, level = level, message = mesg, show = show) 
                          }
                          else {

                              type = 'setup'
                              level = 'error'
                              show = T
                              mesg = paste("Write pht combined consent file  FAILED?? Line number before (", dfRowCount, ") and after (", lineNum, ") the writting do not match --- ", union_pathToFile, "\n", sep="") 
                              writeLog(object, type = type, level = level, message = mesg, show = show) 
                          }
                          cat("\n")


                      } # end of if (union_fStAcc != "")

                      # Note: The function needs to return a datafram. 
                      # otherwise it gives "R Error: Results are not data frames at positions: 1, 2 ..." 
                      return(df)  

                  }  # end of call unionPhtConsent()


                  #######################################################################################
                  # ATTN! Groupping by pht 
                  # Group items in savedDF (read from pht_file_info_indiv.json) by the field name fPhtAcc
                  #######################################################################################

                  # Call pre-defined unionPhtConsent to process each group
                  fPhtAcc <- "" 			# mute CMD check note
                  fStAcc <- "" 			# mute CMD check note
                  . <- "Shut up"			
                  fileInfoDF <- fromJSON(fileInfoFile, flatten=TRUE)
                  # Subset by phsAcc
                  subFileInfoDF = fileInfoDF[fileInfoDF$fStAcc == phsAcc,]

                  # When the file meta info of the study found in the file info file
                  if (nrow(subFileInfoDF) != 0) {

                      ################################
                      # Call unionPhtConsents
                      ################################

                      ###### Groupping by fPhtAcc and fStAcc #####
                      allFileDF = subFileInfoDF %>% dplyr::group_by(fPhtAcc) %>% do(unionPhtConsents(., phsAcc))  

                      ########################################
                      # Exclude Subjec, Sample (multi) files
                      ########################################
                      # Note: used filter_, non-standard-eval to avoid CMD check no-visible binding error 
                      notMultiDF <- dplyr:: filter_(allFileDF, quote(multiType == 'Not'))   

                      # Retriev ComboInfo file
                      comboInfoDF <- fromJSON(studyPhtComboInfoFile)

                      type = 'process'
                      level = 'info'
                      show = T
                      mesg = paste(nrow(notMultiDF), " dataset files of different consents ('MULTI' excluded) are merged to ", nrow(comboInfoDF), " combined files.", sep = "")
                      writeLog(object,  type = type, level = level, message = mesg, show = show)

                      type = 'process'
                      level = 'info'
                      show = T
                      mesg = paste("The meta-info of the merged files is saved in --- ", studyPhtComboInfoFile,  sep = "")
                      writeLog(object,  type = type, level = level, message = mesg, show = show)

                      return(invisible(comboInfoDF))
                  }
                  else {
                      type = 'process'
                      level = 'error'
                      show = T
                      mesg = paste("No file merge occurs. The meta-info of the study ", phsAcc, " aren't found in the meta-info file. Checout ?recordPrjFileInfo to see how to include it. --- ", fileInfoFile, "\n", sep = "")
                      writeLog(object,  type = type, level = level, message = mesg, show = show)
                  }

              } # end of fileInfoFile exist
              else {
                  cat("\n")
                  type = 'process'
                  level = 'error'
                  show = T
                  mesg = paste("The path to pht_file_info_indiv.json does not exists --- ", fileInfoFile, sep = "")
                  writeLog(object,  type = type, level = level, message = mesg, show = show)
              }

              #} # end of file.exists(studyPhtComboInfoFile)


          }) # end of function 


# --------------------------- 
# Method: viewAllStudyInfo 
# --------------------------- 

#' (internal) Display meta-info of all studies 
#'
#' The method displays the meta-info of all studies available in the project directory. The display is through a separate a table viewer or a plain text editor intended for human to browse and search. This function is called through \code{\link{getAllStudyInfo}}.
#'
#' @name viewAllStudyInfo 
#' @param object Commons class object.
#' @param ... Optional arguments
#' @param showAs a character string. (optional) When the value is 'table', displays the data as a table through a platform specific table viewer; When it is 'json', displays the json text through a plain text editor; When it is 'text', displays in a brief left-justified text format.
#' @param editor a character string. (optional) The name of your favorite plain text editor. It should be executable from a command-line prompt of the respective platform. For example, notepad (Windows), vim, emacs (Unix), gedit (Ubuntu), nedit (CentOS), etc.
#' @export viewAllStudyInfo
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' viewAllStudyInfo(c)
#' viewAllStudyInfo(c, showAs = 'table', editor = 'notepad')
#' # or
#' viewAllStudyInfo(c, showAs = 'json', editor = 'notepad')
#' # or
#' viewAllStudyInfo(c, showAs = 'text', editor = 'gedit')
#'}

setGeneric(
           name = "viewAllStudyInfo",
           def = function(object, ...) {
               standardGeneric("viewAllStudyInfo")
           })

#' @describeIn viewAllStudyInfo A method of class Commons 
setMethod(
          f = "viewAllStudyInfo",
          signature = c("Commons"),
          definition = function(object, ..., showAs = 'table', editor = '') {

              prjDataDir = object@prjDataDir 

              # Example location of study (no-version) data dictionary
              # verbose: /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/all_study_info/all_study_dataset_info.json
              # brief: /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/all_study_info/all_study_info_brief_ljustify.txt
              allStudyInfoDir = file.path(prjDataDir,'all_study_info') 
              availStudyInfoJsonFileName = paste('all_study_info.json', sep='')
              availStudyInfo_jsonFile = file.path(allStudyInfoDir, availStudyInfoJsonFileName) 

              availStudyInfoTxtFileName = paste('all_study_info_brief_ljustify.txt', sep='')
              availStudyInfo_txtFile = file.path(allStudyInfoDir, availStudyInfoTxtFileName) 

              displayFile = ''
              if (showAs == 'text') {
                  displayFile = availStudyInfo_txtFile
                  displayTextFile(object, file = displayFile, editor = editor) 
              }
              else if (showAs == 'json') {
                  displayFile = availStudyInfo_jsonFile
                  displayTextFile(object, file = displayFile, editor = editor) 
              }
              else if (showAs == 'table') {
                  displayFile = availStudyInfo_jsonFile
                  displayTable(object, file = displayFile)
              }
              else {
                  cat("The value of argument 'showAs' is neither 'table', 'json', or 'text'. Fallback to 'table' as default\n") 
                  displayFile = availStudyInfo_jsonFile
                  displayTable(object, file = displayFile)
              }
          })


# -----------------------------
# Method: viewAllDatasetInfo 
# -----------------------------

#' (internal) Display meta-info of all datasets
#'
#' The method displays the meta-info of all datasets available in the project directory. The display is through a separate a table viewer or a plain text editor intended for human to browse and search. Before calling this function, the files under the 'combo_dump' sub-directory of the project directory should be made available through the function getAllDatasetInfo. 
#'
#' @name viewAllDatasetInfo
#' @param object Commons class object.
#' @param ... There are optional arguments. 
#' @param showAs a character string. (optional) When the value is 'table', displays the data as a table through a platform specific table viewer; When it is 'json', displays the json text through a plain text editor; When it is 'text', displays in a brief left-justified text format.
#' @param editor a character string. (optional) The name of your favorite plain text editor. It should be executable from a command-line prompt of the respective platform. For example, notepad (Windows), vim, emacs (Unix), gedit (Ubuntu), nedit (CentOS), etc.
#' @export viewAllDatasetInfo
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' viewAllDatasetInfo(c) 
#' viewAllDatasetInfo(c, showAs = 'json', editor='notepad')
#' viewAllDatasetInfo(c, showAs = 'text', editor='gedit')
#'}

setGeneric(
           name = "viewAllDatasetInfo",
           def = function(object, ...) {
               standardGeneric("viewAllDatasetInfo")
           })

#' @describeIn viewAllDatasetInfo A method of class Commons 
setMethod(
          f = "viewAllDatasetInfo",
          signature = c("Commons"),
          definition = function(object, ..., showAs = 'table', editor = '') {

              prjDataDir = object@prjDataDir 

              prjDotDir = object@prjDotDir   
              prjDir = object@prjDir  
              configFile = object@configFile  
              fileInfoFile = object@fileInfoFile 
              fileInfoArchDir = object@fileInfoArchDir  

              ###############################################
              # Get datasets info from already created file
              ###############################################
              # verbose: /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/all_study_info/all_study_dataset_info.json
              # brief: /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/all_study_info/all_study_dataset_info_brief_ljustify.txt
              allStudyInfoDir = file.path(prjDataDir,'all_study_info') 
              availStudyDatasetInfoJsonFileName = paste('all_study_dataset_info.json', sep='')
              availStudyDatasetInfoJsonFile = file.path(allStudyInfoDir, availStudyDatasetInfoJsonFileName) 

              availStudyDatasetInfoTxtFileName = paste('all_study_dataset_info_brief_ljustify.txt', sep='')
              availStudyDatasetInfoTxtFile = file.path(allStudyInfoDir, availStudyDatasetInfoTxtFileName) 

              displayFile = ''
              if (showAs == 'text') {
                  displayFile = availStudyDatasetInfoTxtFile 
                  displayTextFile(object, file = displayFile, editor = editor) 
              }
              else if (showAs == 'json') {
                  displayFile = availStudyDatasetInfoJsonFile 
                  displayTextFile(object, file = displayFile, editor = editor) 
              }
              else if (showAs == 'table') {
                  displayFile = availStudyDatasetInfoJsonFile 
                  displayTable(object, file = displayFile)
              }
              else {
                  cat("The value of argument 'showAs' is neither 'table', 'json', or 'text'. Fallback to 'table' as default\n") 
                  displayFile = availStudyDatasetInfoJsonFile 
                  displayTable(object, file = displayFile)
              }

          })



# ---------------------------
# Method: displayTextFile 
# ---------------------------

#' (Internal) Display text file content 
#'
#' The method displays the content of a given test file. 
#'
#' @name displayTextFile 
#' @param object Commons class object.
#' @param file a character string. The path to the text file to display. 
#' @param ... There are optional arguments.
#' @param editor a character string. (optional) The name of your favorite plain text editor. It should be executable from a command-line prompt of the respective platform. For example, notepad (Windows), vim, emacs (Unix), gedit (Ubuntu), nedit (CentOS), etc.
#' @export displayTextFile 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' file = 'phs000001.v3_pht000371.v2_data_dic.json'
#' displayTextFile(c, file = file)
#' }


# displayTextFile(c, file = '/panfs/sandpan1.be-md.ncbi.nlm.nih.gov/homes/hao/dbgapr_proj/dbgapr_user_project4/gapwork/data/phs000001/phs000001.v3/data_dic/json_dump/phs000001.v3_pht000001.v2_data_dic.json')
# displayTextFile(c, file = "C:\\Users\\mars\\Documents\\R_Dev\\my_dbgapr_project21\\gapwork\\data\\phs000001\\phs000001.v3\\data_dic\\json_dump\\phs000001.v3_pht000371.v2_data_dic.json")
# displayTextFile(c, file = ("C:\\Users\\mars\\Documents\\R_Dev\\my_dbgapr_project21\\gapwork\\data\\phs000001\\phs000001.v3\\data_dic\\json_dump\\phs000001.v3_pht000001.v2_data_dic.json"))
# displayTextFile(c, file = ("C:/Users/mars/Documents/R_Dev/my_dbgapr_project21/gapwork/data/phs000001/phs000001.v3/data_dic/json_dump/phs000001.v3_pht000371.v2_data_dic.json"))
# displayTextFile(c, file = ("C:\\Users\\mars\\Documents\\R_Dev\\my_dbgapr_project21\\gapwork\\data\\phs000001\\phs000001.v3\\data_dic\\json_dump\\phs000001.v3_pht000371.v2_data_dic.json"))

setGeneric(name = "displayTextFile",
           def = function(object, file, ...) {
               standardGeneric("displayTextFile")
           })

#' @describeIn displayTextFile A method of class Commons 
setMethod(
          f = "displayTextFile",
          signature = c("Commons", "character"),
          definition = function(object, file, ..., editor = '') {
              file <- checkInputPath(object, file)

              if (file.exists(file)) {

                  ####################
                  # Display file
                  ####################

                  if (editor != "" & nchar(editor) != 0) {
                      ####################
                      # Use input editor
                      ####################
                      options(editor=editor)
                      utils::file.edit(file = file, editor = getOption("editor"))

                      # Note: edit() as below gives exit error
                      # edit(file=file, editor=getOption('editor'))
                  }
                  else {
                      # Detect and deal with Mac OS
                      is_mac = ''  
                      sysinf <- Sys.info()
                      if (!is.null(sysinf)) {
                          os <- sysinf['sysname']
                          if (os == 'Darwin') {
                              is_mac <- "yes"
                          }
                          else {
                              is_mac <- "no"
                          }
                      } 
                      else { ## mystery machine
                          os <- .Platform$OS.type
                          if (grepl("^darwin", R.version$os)) {
                              is_mac <- "yes"
                          }
                          else {
                              is_mac <- "no"
                          }
                      }

                      if (is_mac == 'yes') {
                          # Open file with TextEditor through the system call
                          cmd = paste('open -e ', file, sep='')
                          try(system(cmd, intern = TRUE, ignore.stderr = TRUE))
                      }
                      else {
                          #######################
                          # Use default editor
                          #######################
                          options(editor = "internal")
                          utils::file.edit(file = file, editor = getOption("editor"))
                      }
                  }
              }
              else {
                  type = 'process'
                  level = 'error'
                  show = T
                  mesg = paste("The text file to display is not found. --- ", file, "\n", sep="")
                  writeLog(object,  type = type, level = level, message = mesg, show = show) 
              }
          })


# ---------------------------
# Method: displayTable 
# ---------------------------

#' (internal) Display text file content 
#'
#' The method displays the input data frame as a data table through a platform specific table viewer. The data frame is either input directly as a function argument or through an input data file of rds, csv, or json format.
#'
#' @name displayTable 
#' @param object Commons class object.
#' @param ... There are optional arguments.
#' @param file a character string. (optional) The path to the data file to display in rds, csv, or json format.
#' @param data a data frame. (optional) The data to display.
#' @return a data frame. The data for display. 
#' @export displayTable
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' df <- readRDS(data_dic.rds) 
#' displayTable(c, data = df)
#' displayTable(c, file = 'data_dic.json')
#' displayTable(c, file = 'data_dic.csv')
#' displayTable(c, file = 'data_dic.rds')
#' }


# displayTable(c, file = '/panfs/sandpan1.be-md.ncbi.nlm.nih.gov/homes/hao/dbgapr_proj/dbgapr_user_project4/gapwork/data/phs000001/phs000001.v3/data_dic/json_dump/phs000001.v3_pht000001.v2_data_dic.json')
# displayTable(c, file = "C:\\Users\\mars\\Documents\\R_Dev\\my_dbgapr_project21\\gapwork\\data\\phs000001\\phs000001.v3\\data_dic\\json_dump\\phs000001.v3_pht000371.v2_data_dic.json")
# displayTable(c, file = ("C:\\Users\\mars\\Documents\\R_Dev\\my_dbgapr_project21\\gapwork\\data\\phs000001\\phs000001.v3\\data_dic\\json_dump\\phs000001.v3_pht000001.v2_data_dic.json"))
# displayTable(c, file = ("C:/Users/mars/Documents/R_Dev/my_dbgapr_project21/gapwork/data/phs000001/phs000001.v3/data_dic/json_dump/phs000001.v3_pht000371.v2_data_dic.json"))
# displayTable(c, file = ("C:\\Users\\mars\\Documents\\R_Dev\\my_dbgapr_project21\\gapwork\\data\\phs000001\\phs000001.v3\\data_dic\\json_dump\\phs000001.v3_pht000371.v2_data_dic.json"))
# displayTable(c, file = "C:\\Users\\mars\\Documents\\R_Dev\\my_dbgapr_project21\\gapwork\\data\\phs000001\\phs000001.v3\\data_dic\\rds_dump\\phs000001.v3_pht000373.v2_data_dic.rds")
# displayTable(c, file = "C:\\Users\\mars\\Documents\\R_Dev\\my_dbgapr_project21\\gapwork\\data\\phs000001\\phs000001.v3\\data_dic\\csv_dump\\phs000001.v3_pht000374.v2_data_dic.csv")

setGeneric(name = "displayTable",
           def = function(object, ...) {
               standardGeneric("displayTable")
           })

#' @describeIn displayTable A method of class Commons 
setMethod(
          f = "displayTable",
          signature = c("Commons"),
          definition = function(object, ..., file = "", data = data.frame()) {

              file <- checkInputPath(object, file)

              # Detect CSV delimiter 
              # returns 'comma' or 'tab'
              checkCsvDelimitor <- function(csv = '') {
                  tempDF1 <- read.table(csv, header = T, nrows = 10, sep ='\t', stringsAsFactors = FALSE)
                  if (nrow(tempDF1) > 0) {
                      return("comma")
                  }
                  else {
                      return("tab")
                  }
              }

              if (file == "") {
                  # Check it is a dataframe

                  if (is.data.frame(data) == TRUE) {
                      # Check dataframe is not empty 
                      if (nrow(data) > 0) {

                          # Reset row names  (reset row index for subsetted DF)
                          rownames(data) <- seq(length=nrow(data))

                          # View data
                          # Note: Do not call utils:View(data). Otherwise, Rstudio table viewer won't work.  
                          View <- NULL # mute R CMD check note
                          View(data)
                      }
                      else {
                          type = 'process'
                          level = 'error'
                          show = T
                          mesg = paste("No data file or data frame is provided from the input.", sep="")
                          writeLog(object,  type = type, level = level, message = mesg, show = show) 
                      }
                  }
                  else {
                      type = 'process'
                      level = 'error'
                      show = T
                      mesg = paste("The data does not appear to be a data frame.", sep="")
                      writeLog(object,  type = type, level = level, message = mesg, show = show) 
                  }

              }
              else {

                  # Check file exist
                  if (file.exists(file)) {

                      # Check file extension
                      ext = tools::file_ext(file)

                      if (ext == 'json') {
                          data <- fromJSON(file, flatten = T)
                      }
                      else if (ext == 'csv') {
                          delim <- checkCsvDelimitor(csv = file)
                          if (delim == 'comma') {
                              data <- read.table(file, header = T, sep =',', stringsAsFactors = FALSE)
                          }
                          else {
                              data <- read.table(file, header = T, sep ='\t', stringsAsFactors = FALSE)
                          }
                      }
                      else if (ext == 'rds') {
                          data <- readRDS(file) 
                      }
                      else {
                          type = 'process'
                          level = 'error'
                          show = T
                          mesg = paste("The input file should be in either rds, csv, or json format. It appears to be none of them.", sep="")
                          writeLog(object,  type = type, level = level, message = mesg, show = show) 
                      }


                      if (nrow(data) > 0) {

                          rownames(data) <- seq(length=nrow(data))

                          # View data 
                          # Note: Do not call utils:View(data). Otherwise, Rstudio table viewer won't work.  
                          View <- NULL # mute R CMD check note
                          View(data)

                      }
                  }
                  else {
                      type = 'process'
                      level = 'error'
                      show = T
                      mesg = paste("The input file to display is not found. --- ", file, "\n", sep="")
                      writeLog(object,  type = type, level = level, message = mesg, show = show) 
                  }
              }
          })

# ---------------------------
# Method: getDatasetMetaByStudy 
# ---------------------------

#' (internal) Get study variable meta-info (data dictionary)
#'
#' The method returns the meta-info of all datasets of the given study.
#'
#' @name getDatasetMetaByStudy
#' @param object Commons class object.
#' @param phsAcc a character string. The dbGaP study accession.
#' @param ... There are optional arguments. 
#' @param dataStudyOnly a logical value. When TRUE (default), only downloads the dataset and variable metadata of the stdudies that have data files in the project directory.  When FALSE, downloads the dataset and variable metadata of all dbGaP released studies, regardless the actual phenotype data files of the studies are downloaded or not. 
#' @return a data frame. The dataset meta-info of the study.
#' @export getDatasetMetaByStudy
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' getDatasetMetaByStudy(c, phsAcc = 'phs000001.v3.p1')
#'}

setGeneric(
           name = "getDatasetMetaByStudy",
           def = function(object, phsAcc, ...) {
               standardGeneric("getDatasetMetaByStudy")
           })

#' @describeIn getDatasetMetaByStudy A method of class Commons 
setMethod(
          f = "getDatasetMetaByStudy",
          signature = c("Commons", "character"),
          definition = function(object, phsAcc) {

              datasetMetaDF <- getExtData(object, type = 'dataset', phsAcc = phsAcc, dataStudyOnly = TRUE)

              if (is.null(datasetMetaDF)) {
                  datasetMetaDF <- getExtData(object, type = 'dataset', phsAcc = phsAcc, dataStudyOnly = FALSE)
              }
              
              return(datasetMetaDF)

          })


# ---------------------------
# Method: getDataDicByStudy 
# ---------------------------

#' (internal) Get study variable meta-info (data dictionary)
#'
#' The method returns the meta-info of all variables of the given study.
#'
#' @name getDataDicByStudy
#' @param object Commons class object.
#' @param phsAcc a character string. The dbGaP study accession.
#' @param dataStudyOnly a logical value. When TRUE (default), only downloads the dataset and variable metadata of the stdudies that have data files in the project directory.  When FALSE, downloads the dataset and variable metadata of all dbGaP released studies, regardless the actual phenotype data files of the studies are downloaded or not. 
#' @export getDataDicByStudy
#' @return a data frame. The dataset meta-info of the study.
#' @export getDataDicByStudy
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' getDataDicByStudy(c, phsAcc = 'phs000001.v3.p1')
#'}

setGeneric(
           name = "getDataDicByStudy",
           def = function(object, phsAcc, ...) {
               standardGeneric("getDataDicByStudy")
           })

#' @describeIn getDataDicByStudy A method of class Commons 
setMethod(
          f = "getDataDicByStudy",
          signature = c("Commons", "character"),
          definition = function(object, phsAcc, ..., dataStudyOnly = TRUE) {
              
              varMetaDF <- getExtData(object, type = 'variable', phsAcc = phsAcc, dataStudyOnly = dataStudyOnly)

              return(varMetaDF)

          })


# -----------------------------
# Method: getVarNameByPhvAcc
# -----------------------------

#' (internal) Get variable names given accessions 
#'
#' The method takes a list of variable accessions and returns a list of variable names. The output variable name can be shown in three different formats: name only ( e.g. LNUCSCORE ), name and accession separated by underscore (e.g. LNUCSCORE_phv00000006.v2), name and accession separated with space and parenthesis (e.g. LNUCSCORE (phv00000006.v2)). There is no validation applied to the input accessions.
#'
#' @name getVarNameByPhvAcc
#' @param object Commons class object.
#' @param phvAccList a character vector. The dbGaP phenotype variable accessions. 
#' @param ... There are optional arguments.
#' @param colNameWithAcc a logical value. (optional). If FALSE (default), outputs variable name only; If TRUE, outputs concatenated variable name and variable.
#' @param underscore  a logical value. (optional) Used only when colNameWithAcc is TRUE. If TRUE (default), underscore is used between variable name and accession( e.g. LNUCSCORE_phv00000006.v2 ); If FALSE, the parenthesis with a space is used between variable name and accession ( e.g. LNUCSCORE (phv00000006.v2) ).
#' @param phsAcc a character string. (optional) The accession of the study where the variables belong. It is required if studyDataDicDF is not provided.
#' @param studyDataDicDF  a data frame. (optional) The data frame of the study data dictionary (variable meta-data). It is required if phsAcc is not provided.
#' @return a named list of variable names. The variable names of the input variable accession.
#' @export getVarNameByPhvAcc 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' accList = c('phv00000027.v2', 'phv00053747.v2')
#' phsAcc = 'phs000001.v3.p1'
#' getVarNameByPhvAcc(c, phvAccList=accList, phsAcc=phsAcc) 
#' # or
#' getVarNameByPhvAcc(c, phvAccList=accList, phsAcc=phsAcc, colNameWithAcc=T)
#' # or
#' getVarNameByPhvAcc(c, phvAccList=accList, phsAcc=phsAcc, colNameWithAcc=T, underscore=T)
#' }

# getVarNameByPhvAcc(c, phvAccList =  c('phv00000027.v2', 'phv00053747.v2', 'phv00000006.v2', 'phv00054122.v1', 'phv00053747.v2'), phsAcc = 'phs000001.v3.p1')
#  getVarNameByPhvAcc(c, phvAccList =  c('phv00000027.v2', 'phv00053747.v2', 'phv00000006.v2', 'phv00054122.v1', 'phv00053747.v2'), phsAcc = 'phs000001.v3.p1', colNameWithAcc = T) 
#  getVarNameByPhvAcc(c, phvAccList =  c('phv00000027.v2', 'phv00053747.v2', 'phv00000006.v2', 'phv00054122.v1', 'phv00053747.v2'), phsAcc = 'phs000001.v3.p1', colNameWithAcc = T, underscore = F) 


setGeneric(name = "getVarNameByPhvAcc",
           def = function(object, phvAccList, ...) {
               standardGeneric("getVarNameByPhvAcc")
           })

#' @describeIn getVarNameByPhvAcc 
setMethod(
          f = "getVarNameByPhvAcc",
          signature = c("Commons", "character"),
          definition = function(object, phvAccList, ..., colNameWithAcc = F, underscore = F, phsAcc, studyDataDicDF = data.frame()) {

              if (nrow(studyDataDicDF) == 0) {
                  studyDataDicDF <- getDataDicByStudy(object, phsAcc)  
              }

              newList <- lapply(phvAccList, function(phvAcc) {  
                                    matchVarDF <- subset(studyDataDicDF,  studyDataDicDF$variable_accession==phvAcc)  
                                    matchVarName <- matchVarDF$name 

                                    if (colNameWithAcc == T) {

                                        if (underscore == T) {
                                            paste(matchVarName , '_', phvAcc, sep='')
                                        }
                                        else {
                                            paste(matchVarName , ' (', phvAcc, ')', sep='')
                                        }
                                    }
                                    else {
                                        return (matchVarName)
                                    }
                                      })
              newList = unlist(newList)

              # Rename duplicated item
              # [1] "AGEPHOT"     "LNUCSCORE"   "LNUCSCORE" "DIABAGE"
              # [1] "AGEPHOT"     "LNUCSCORE"   "LNUCSCORE.1" "DIABAGE"
              varAccNameList = make.unique(newList)

              names(varAccNameList) = phvAccList

              return(varAccNameList)
          })



# ---------------------------
# Method: stripColNameAcc
# ---------------------------

#' (internal) Remove accession from concatenated string of variable name and accession
#'
#' The method takes a list of concatenated string of variable name and accession, removes the variable accession from each of the string (e.g. AGEPHOT_phv00000027.v2 --> AGEPHOT). After the removal, if there are duplicated items in the list, a .number is appended to each of the repeated items to make the ditinction. For example, the duplicated items LNUCSCORE_phv00053747.v2, LNUCSCORE_phv00000006.v2 become LNUCSCORE.1, LNUCSCORE.2 .
#'
#' @name stripColNameAcc
#' @param object Commons class object.
#' @param colNameAccList a character vector. A concatenated string of the variable name and accession, such as c('AGEPHOT_phv00000027.v2', 'LNUCSCORE_phv00053747.v2', 'LNUCSCORE_phv00000006.v2', 'DIABAGE_phv00054122.v1').
#' @return a character vector. Variable names with the accessions removed.
#' @export stripColNameAcc 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' names <- c('AGEPHOT_phv00000027.v2', 'LNUCSCORE_phv00053747.v2')
#' stripColNameAcc(c, colNameAccList =  names)) 
#' }

# @examples
# c <- Commons()
# stripColNameAcc(c, colNameAccList =  c('AGEPHOT_phv00000027.v2', 'LNUCSCORE_phv00053747.v2', 'LNUCSCORE_phv00000006.v2', 'DIABAGE_phv00054122.v1'))

setGeneric(name = "stripColNameAcc",
           def = function(object, colNameAccList) {
               standardGeneric("stripColNameAcc")
           })

#' @describeIn stripColNameAcc 
setMethod(
          f = "stripColNameAcc",
          signature = c("Commons", "character"),
          definition = function(object, colNameAccList) {

              newList <- lapply(colNameAccList, function(item) {  
                                    # Remove underscore and string after it 
                                    gsub("_phv.+", "", item)
                                      })
              newList = unlist(newList)
              # Rename duplicated item
              # [1] "AGEPHOT"     "LNUCSCORE"   "LNUCSCORE" "DIABAGE"
              # [1] "AGEPHOT"     "LNUCSCORE"   "LNUCSCORE.1" "DIABAGE"
              newList = make.unique(newList)

              return(newList)
          })


# ----------------------------------
# Method: convertEnumVarColName
# ----------------------------------

#' (internal) Convert enumerated integer type variable value
#'
#' The enumerated integer type variable values need to be converted to the respective string type values before plotting. The plotting program treats it as numeric values otherwise. This method takes the data frames of the data and meta-data of an enumerated integer type variable, and converts the variable values to the respective string type values based on the code meanings.
#'
#' @name convertEnumVarColName 
#' @param object Commons class object
#' @param varDataDF a data frames. The single variable row of the data table.
#' @param varInfoDF a data frames. The single variable row of the variable meta-data table.
#' @param varCodeValDF a data frames. The code and value table of the categorical vairables.  
#' @export convertEnumVarColName
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' object <- Commons()
#' convertEnumVarColName(c, varDataDF = catVarDataDF, varInfoDF = catVarInfoDF) 
#' }

# c <- Commons()
# convertEnumVarColName(object, varDataDFInList = list(catVarDataDF), varInfoDFInList = list(catVarInfoDF)) 


setGeneric(name = "convertEnumVarColName",
           def = function(object, varDataDF, varInfoDF, varCodeValDF) {
               standardGeneric("convertEnumVarColName")
           }
           )

#' @describeIn convertEnumVarColName 
setMethod(
          f = "convertEnumVarColName",
          signature = c("Commons", "data.frame"),
          definition = function(object, varDataDF, varInfoDF, varCodeValDF) {

              catVarDataDF = varDataDF 
              catVarInfoDF = varInfoDF

              catVarAcc <- as.character(catVarInfoDF$variable_accession)
              catVarType <- as.character(catVarInfoDF$calculated_type)
              catVarName <- as.character(catVarInfoDF$name)
              catVarUnits <- as.character(catVarInfoDF$units)
              #catVarCodeValCombo <- as.character(catVarInfoDF$code_value_combo)


              #matchVarCodeValDF <- dplyr::filter(varCodeValDF, varCodeValDF$variable_accession == catVarAcc)

              ############################
              ## Build code-value combo
              ############################
              #catVarCodeValCombo = NA 
              #dat <- matchVarCodeValDF
              #dat$comboCol <- do.call(paste, c(dat[c("code", "value")], sep = ":"))
              #comboList <- paste(dat$code, dat$value, sep=':') 

              #if (length(comboList) > 0) {
              #    catVarCodeValCombo <- paste(comboList, collapse = '|')
              #}
              #else {
              #    catVarCodeValCombo = NA
              #}

              #################################
              # Separate out the data columns 
              #################################

              if(catVarName %in% colnames(catVarDataDF)) {

                  #######################
                  # Build codeValCombo
                  #######################
                  catVarCodeValCombo <- buildVarCodeValCombo(object, catVarAcc = catVarAcc, varCodeValDF = varCodeValDF)

                  catVarDataNoIdDF = subset(catVarDataDF, select = c(catVarName))			# separate out catVar column only

                  # The string type example: (phv00053757.v2) 
                  # 	  RNUC
                  #   1 NUC-C
                  #   2 NUC-C
                  #   3 NUC-D
                  #   4 NUC-A

                  # The enumeated integer type example: (phv00053856.v2) 
                  #        SCHOOL
                  # 1      2
                  # 2      4
                  # 3      5
                  # 4      4


                  ########################
                  # Categorical type 
                  ########################
                  finalDF = data.frame()
                  if (catVarType == 'enum_integer' | catVarType == 'string' | catVarType == 'enumerated integer') {


                      ##################################
                      # ATTN! Reality check datatype
                      ##################################
                      # Sometime the variable is labelled as enum_integer in the data_dic, but actual type is string 
                      # For example phv00053764.v2 has an enum_integer type, but its value is a string
                      # LPSC
                      # 1 PSC-A
                      # 2 PSC-B
                      # 3 PSC-A
                      # 4 PSC-A

                      realTypeNamedChar <- sapply(catVarDataNoIdDF, class)
                      realTypeDF <- data.frame(as.list(realTypeNamedChar))				# convert named char to dataframe 
                      realType <- realTypeDF[,1]


                      ##### Reset if it is the case ######
                      if (realType == 'character') {
                          catVarType = 'string'											# reset cartVarType to the realType
                      }

                      #######################################
                      # Deal with enumerated integer type
                      #######################################
                      # Replace column value wiht a combo string of original colmn name and the respective enumerated value
                      if (catVarType == 'enum_integer' |  catVarType == 'enumerated integer') {

                          # Replace column names
                          # make
                          # h778 h778 h778 h778 h778 h778 
                          # to
                          # h778_ h778_ h778_ h778_ h778_ h778_ 

                          #############################################
                          # pre-pend catVarName to each column value 
                          #############################################
                          # such as MARITAL ---> MARITAL_2
                          newList <- lapply(catVarDataNoIdDF[[catVarName]], function(x)  paste0(catVarName, "_", x))

                          # Replace entire column value with the pre-pend values 
                          catVarDataNoIdDF[[catVarName]] <- unlist(newList) 


                          # Remove original SCHOOL column
                          #newData <- subset(dat, select = c('tempCol'))
                          # Rename tempCol to SCHOOL
                          #colnames(newData)[1] <- catVarName 
                          # SCHOOL
                          # 1 SCHOOL2
                          # 2 SCHOOL4
                          # 3 SCHOOL5
                          # 4 SCHOOL4

                          #finalDF <- newData

                          # Example: 
                          #    MARITAL
                          #    1 MARITAL_2
                          #    2 MARITAL_1
                          #    3 MARITAL_1
                          #    4 MARITAL_1
                          #    5 MARITAL_1
                          #    6 MARITAL_1

                          finalDF <- catVarDataNoIdDF
                      }
                      else {
                          #########################
                          # String type
                          #########################
                          finalDF <- catVarDataNoIdDF
                      }
                  } 
                  else {
                      finalDF <- catVarDataNoIdDF
                  }


              } # end catPhvVarName %in%
              else {
                  finalDF <- catVarDataNoIdDF
              }
              return (finalDF)
          })

# ----------------------
# Method: saveGapPlot
# ----------------------

#' (internal) Save dbGaP data plot to file 
#'
#' The method takes the ggplot graph object as an input, and save the graph to PDF and PNG files.  
#'
#' @name saveGapPlot 
#' @param object Commons class object.
#' @param ... There are optional arguments. 
#' @param plotObj a ggplot image object.  The resulting image object from a ggplot function all.
#' @param plotType a character string. Type of the plot with the possible value of 'boxplot', 'scatterplot', or 'histogram', etc.
#' @param phvAccNameCombo a character string. The concatenated string of a variable accession and name. For example, "phv00053747.v2_LNUCSCORE_phv00053764.v2_LPSC". It is used for naming the saved image files.
#' @param saveToDir a character string. (optional) The path to the directory where the plot PDF file is saved. If not provided, the file is saved in the 'temp' directory under the user project directory.
#' @param showPlot a logical value. (optional) If TRUE (default), shows the created graph; If FALSE, not show the created graph.  The created graph is saved as files regardless.
#' @return a character list. The full paths to the saved plot files  
#' @export saveGapPlot 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' accNameCombo = 'phv00053747.v2_LNUCSCORE_phv00053764.v2_LPSC'
#' saveGapPlot(c, plotObj, plotType = 'boxplot', phvAccNameCombo = accNameCombo) 
#'}

# c <- Commons()
# saveGapPlot(c, plotObj, plotType = 'boxplot', phvAccNameCombo = 'phv00053747.v2_LNUCSCORE_phv00053764.v2_LPSC')

setGeneric(name = "saveGapPlot",
           def = function(object, ...) {
               standardGeneric("saveGapPlot")
           })

#' @describeIn saveGapPlot 
setMethod(
          f = "saveGapPlot",
          signature = c("Commons"),
          # Note: The object type of plotObj could be either gList (VennDiagram) or gg (other ggplot).
          # Make it optional is a workaround so that the signature type doesn't have to be specified.
          definition = function(object, ..., plotObj, plotType, phvAccNameCombo, saveToDir = '', showPlot = T) {
              saveToDir <- checkInputPath(object, saveToDir)

              prjTempDir = object@prjTempDir

              #################
              # Display plot
              #################
              if (showPlot == T) {
                  print(plotObj)
              }

              ###############                                                                                                                                                           
              # Save plot                                                                                                                                                               
              ###############                                                                                                                                                           
              plotPdfFileName = paste(plotType, "_", phvAccNameCombo, ".pdf", sep="")
              plotPngFileName = paste(plotType, "_", phvAccNameCombo, ".png", sep="")
              plotFilePath = prjTempDir                                                                                                                                                 


              if (nchar(saveToDir) > 0) {                                                                                                                                               
                  if (dir.exists(saveToDir)) {                                                                                                                                          
                      plotFilePath = saveToDir                                                                                                                                          
                  }                                                                                                                                                                     
                  else {                                                                                                                                                                
                      type = 'process'                                                                                                                                                  
                      level = 'info'                                                                                                                                                    
                      show = T                                                                                                                                                          
                      mesg = paste("The input path of savToDir ", saveToDir, ", does not exist. The plot PDF is saved in the default 'temp' directory under the user proje\n", sep="")  
                      writeLog(object,  type = type, level = level, message = mesg, show = show)                                                                                        
                  }                                                                                                                                                                     
              }                                                                                                                                                                         
              plotPdfFile = file.path(plotFilePath, plotPdfFileName)                                                                                                                    
              plotPngFile = file.path(plotFilePath, plotPngFileName)                                                                                                                    

              ggsave(filename=plotPdfFile, plot=plotObj) # save PDF
              ggsave(filename=plotPngFile, plot=plotObj) # save PNG

              plotFiles = list(plotPdfFile, plotPngFile)


              # Display file paths
              cat("\nSaved Files: \n", plotPdfFile, "\n", plotPngFile, "\n\n")

              return (plotFiles)
          })


# ----------------------
# Method: isStudyAcc
# ----------------------

#' (internal) Validate the format of a dbGaP study accession
#'
#' The method checks the format of an input string to see if it is a dbGaP accession. The accessions with and without the p# (participant-set number) both are considered to be correct (e.g. phs000001.v3.p1 or phs000001.v3). 
#'
#' @name isStudyAcc 
#' @param object Commons class object.
#' @param phsAcc a character string. A dbGaP study accession.
#' @return a logical value. TRUE if the format matches a dbGaP accession, FALSE if it doesn't match.
#' @export isStudyAcc 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' isStudyAcc(c, phsAcc = 'phs000001.v3')
#'}


setGeneric(name = "isStudyAcc",
           def = function(object, phsAcc) {
               standardGeneric("isStudyAcc")
           }
           )

#' @describeIn isStudyAcc A method of class Commons 
setMethod(
          f = "isStudyAcc",
          signature = c("Commons", "character"),
          definition = function(object, phsAcc) {

              # Validate accession 
              phsAcc <- cleanObjAcc(object, acc = phsAcc, type = 'phs') 

              if (phsAcc == "") {
                  return (FALSE)
              }
              else {
                  return (TRUE)
              }
          })


# ----------------------------
# Method: composeObjAcc
# ----------------------------

#' Compose dbGaP object accession 
#'
#' The method composes the study, dataset, variable accession from the given object id and version. If object version is not provided, it returns the object accession without version. 
#'
#' @name composeObjAcc
#' @param object Commons class object.
#' @param objId an integer. A dbGaP object id. 
#' @param type an integer. The type of object, such as 'study', 'dataset', or 'variable'. 
#' @param ... There are optional arguments.
#' @param objVer an integer. (optional) The object version.
#' @return a character string. The object accession with or without version. 
#' @export composeObjAcc 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' composeObjAcc(c, objId = 1255, type = 'study', objVer = 1)
#'}

#
setGeneric(
    name = "composeObjAcc",
    def = function(object, objId, type, ...) {
        standardGeneric("composeObjAcc")
    })

    #' @describeIn composeObjAcc A method of class Commons 
    setMethod(
        f = "composeObjAcc",
        signature = c("Commons"),
        definition = function(object, objId, type, ..., objVer = NULL) {

            objAcc = ''
            phx = ''
            accWidth = ''
            

            typeOk = TRUE 
            if (type == 'study') {
                phx = 'phs'
                accWidth = 6 
            }
            else if (type == 'dataset') {
                phx = 'pht'
                accWidth = 6 
            }
            else if (type == 'variable') {
                phx = 'phv'
                accWidth = 8 
            }
            else {
                typeOk = FALSE 

                msg <- paste("[ERROR] The input type value ", type, " isn't correct. It needs to be either 'study', 'dataset', or 'variable'.\n", sep = "")
                cat(msg)
            }

            ################
            # Compose
            ################
            if (typeOk) {

                ######################
                # Make zero padding
                ######################
                # 1255 to 001255
                vec<- c(objId) 
                zeroPadObjId <- formatC(vec, width=accWidth, format="d", flag="0") 

                if (!is.null(objVer)) {

                    # returns "phs001255.v1"
                    objAcc <- paste0(phx, as.character(zeroPadObjId), ".v", as.character(objVer)) 
                }
                else {
                    # returns "phs001255"
                    objAcc <- paste0(phx, as.character(zeroPadObjId)) 
                }
            }

            return(objAcc)
        })

# ----------------------------
# Method: parseIdsFromPhvAcc
# ----------------------------

#' Parse ids from dbGaP variable accession
#'
#' The method parses out id strings from a dbGaP variable accession.
#'
#' @name parseIdsFromPhvAcc
#' @param object Commons class object.
#' @param phvAcc a character string. The dbGaP variable accession with or without the p# (participant-set number), e.g. phv00273621.v1.
#' @return a named character list. The list of parsed id strings including 'phvAcc' for dataset acession, 'phvAccNoVer' for the dataset accession with the v# (version number) removed, 'phvAccId' for dataset id, and 'phvAccVer' for variable version.
#' @export parseIdsFromPhvAcc
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' phvAccIds <- parseIdsFromPhvAcc(c, phvAcc = 'phv00273621.v1.p1')
#'}

# phvAccIds <- parseIdsFromPhvAcc(c, phvAcc = 'phv00273621.v1')
#
setGeneric(
    name = "parseIdsFromPhvAcc",
    def = function(object, phvAcc) {
        standardGeneric("parseIdsFromPhvAcc")
    })

    #' @describeIn parseIdsFromPhvAcc A method of class Commons 
    setMethod(
        f = "parseIdsFromPhvAcc",
        signature = c("Commons"),
        definition = function(object, phvAcc) {

            # Validate accession 
            phvAcc <- cleanObjAcc(object, acc = phvAcc, type = 'phv') 
            phvAccIds = list()

            if (phvAcc != "") {

                pattns = stringr::str_match(phvAcc, "^(phv0+)(\\d+)\\.v(\\d+)")
                phvAccIds <- list(phvAcc = pattns[1],  phvAccNoVer = pattns[2], phvAccId = pattns[3])

                phvWithZero = pattns[2]
                phvAccId = pattns[3]
                phvAccNoVer = paste(phvWithZero, phvAccId, sep='')
                phvAccVer = pattns[4]
                phvAccPnum =''

                phvAccIds <- list(phvAcc = phvAcc,  phvAccNoVer = phvAccNoVer, phvAccId = phvAccId, phvAccVer = phvAccVer)
            }

            return(phvAccIds)
        })


# ----------------------------
# Method: parseIdsFromPhtAcc
# ----------------------------

#' Parse ids from dbGaP dataset accession
#'
#' The method parses out id strings from a dbGaP dataset accession.
#'
#' @name parseIdsFromPhtAcc
#' @param object Commons class object.
#' @param phtAcc a character string. The dbGaP dataset accession with or without the p# (participant-set number), e.g. pht003718.v1.p1 or pht003718.v1.
#' @return a named character list. The list of parsed id strings including 'phtAcc' for dataset acession, 'phtAccNoVer' for the dataset accession with the v# (version number) removed, 'phtAccId' for dataset id, and 'phtAccVer' for dataset version.
#' @export parseIdsFromPhtAcc
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' parseIdsFromPhtAcc(c, phtAcc = 'pht003718.v1.p1')
#'}

# parseIdsFromPhtAcc(c, phtAcc = 'pht003718.v1')
#
setGeneric(
           name = "parseIdsFromPhtAcc",
           def = function(object, phtAcc) {
               standardGeneric("parseIdsFromPhtAcc")
           })

#' @describeIn parseIdsFromPhtAcc A method of class Commons 
setMethod(
          f = "parseIdsFromPhtAcc",
          signature = c("Commons"),
          definition = function(object, phtAcc) {

              # Validate accession 
              phtAcc <- cleanObjAcc(object, acc = phtAcc, type = 'pht') 
              phtAccIds = list()

              if (phtAcc != "") {

                  pattns = stringr::str_match(phtAcc, "^(pht0+)(\\d+)\\.v(\\d+)")
                  phtAccIds <- list(phtAcc = pattns[1],  phtAccNoVer = pattns[2], phtAccId = pattns[3])

                  phtWithZero = pattns[2]
                  phtAccId = pattns[3]
                  phtAccNoVer = paste(phtWithZero, phtAccId, sep='')
                  phtAccVer = pattns[4]
                  phtAccPnum =''

                  phtAccIds <- list(phtAcc = phtAcc,  phtAccNoVer = phtAccNoVer, phtAccId = phtAccId, phtAccVer = phtAccVer)
              }

              return(phtAccIds)
          })


# ----------------------------
# Method: parseIdsFromStAcc
# ----------------------------

#' Parse ids from dbGaP study accession
#'
#' The method parses out id strings from a dbGaP study accession.
#'
#' @name parseIdsFromStAcc
#' @param object Commons class object.
#' @param phsAcc a character string. A dbGaP study accession with or without the p# (participant-set number), e.g. phs000001.v3.p1 or phs000001.v3.
#' @return a named character list. The list of parsed id strings including 'phsAcc' for study accession, 'phsAccNoVer' for the study accession with the v# (version number) removed. 'phsAccId' for study id, and 'phsAccVer' for study version.
#' @export parseIdsFromStAcc
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons() 
#' parseIdsFromStAcc(c, phsAcc = 'phs000001.v3.p1')
#'}

setGeneric(
           name = "parseIdsFromStAcc",
           def = function(object, phsAcc) {
               standardGeneric("parseIdsFromStAcc")
           })

#' @describeIn parseIdsFromStAcc A method of class Commons 
setMethod(
          f = "parseIdsFromStAcc",
          signature = c("Commons"),
          definition = function(object, phsAcc) {

              # Validate accession 
              phsAcc <- cleanObjAcc(object, acc = phsAcc, type = 'phs') 
              phsAccIds = list()

              if (phsAcc != "") {
                  pattns = stringr::str_match(phsAcc, "^(phs0+)(\\d+)\\.v(\\d+)$")
                  phsWithZero = pattns[2]
                  phsAccId = pattns[3]
                  phsAccNoVer = paste(phsWithZero, phsAccId, sep='')
                  phsAccVer = pattns[4]
                  phsAccIds <- list(phsAcc = phsAcc,  phsAccNoVer = phsAccNoVer, phsAccId = phsAccId, phsAccVer = phsAccVer)

              }

              return(phsAccIds)
          })

# --------------------------- 
# Method: cleanObjAcc 
# --------------------------- 

#' (internal) Validate the format of dbGaP object 
#'
#' The method checks the format of a dbGaP object accession, including the study, dataset, and variable accessions.
#'
#' @name cleanObjAcc
#' @param object Commons class object.
#' @param acc a character string. The accession of a dbGaP object (study, dataset, or variable object) with or without the p# (participant-set number). The accession returned from the function has the p# removed if it exists in the input string. 
#' @param type a character string. The dbGaP objec type. The possible values are 'phs' for study, 'pht' for dataset, and 'phv' for variable.
#' @return a character string. The validated accession if the input is valid; An empty string if the input is invalid. The p# is removed from the returned accession if it exists in the input string.
#' @export cleanObjAcc
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' cleanObjAcc(c, acc = 'pht003718.v1.p1', type = 'pht')
#'}

# cleanObjAcc(c, acc = 'pht003718.v1', type = 'pht')
# cleanObjAcc(c, acc = 'phs000001.v1.p1', type = 'phs')
# cleanObjAcc(c, acc = 'phs000007.v12.p10', type = 'phs')
# cleanObjAcc(c, acc = 'phv00000135.v2.p1.c1', type = 'phv')

setGeneric(
           name = "cleanObjAcc",
           def = function(object, acc, type) {
               standardGeneric("cleanObjAcc")
           })

#' @describeIn cleanObjAcc 
setMethod(
          f = "cleanObjAcc",
          signature = c("Commons"),
          definition = function(object, acc, type) {

              # Remove p#, c# if exists
              acc <- gsub("(\\.p\\d+).*$", "", acc)

              # Trim heading and trailing space
              acc <- trimws(acc)

              cleanAcc = '' 

              typeName = ''
              if (type == 'phs') {
                  typeName = 'study'
              }
              else if (type == 'pht') {
                  typeName = 'dataset'
              }
              else if (type == 'phv') {
                  typeName = 'variable'
              }
              else {
                  msg <- paste("[ERROR] The input value for the argumentn 'type' is not correct. It needs to be either 'phs' for study, 'pht' for dataset, or 'phv' for variable.", sep = "")
                  cat(msg)
              }


              if (nchar(acc) == 0) { 
                  msg <- paste("[ERROR] The input value of dbGaP ", typeName, " accession appears to be empty. It should have a ", type, " followed by zero padded 6 digits and a suffix .v#, e.g. phs000001.v3 or phs000007.v27.p10 (the p# is optional).\n", sep = "")
                  cat(msg)
              }
              else {

                  if (type == 'phs') {
                      # Format check
                      acc.rex <- "^phs\\d{6}\\.v\\d+$"
                      acc.match <- grepl(acc.rex, acc, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)

                      if (acc.match == FALSE) {
                          msg <- paste("[ERROR] The input dbGaP ", typeName, " accession ",  acc, ", has an incorrect format. It should have a ", type,  " followed by zero padded 6 digits and a suffix .v#, e.g. phs000001.v3 or phs000007.v27.p10 (the p# is optional).\n", sep = "")
                          cat(msg)
                      }
                      else {
                          cleanAcc =acc 
                      }
                  }
                  if (type == 'pht') {
                      # Format check
                      acc.rex <- "^pht\\d{6}\\.v\\d+$"
                      acc.match <- grepl(acc.rex, acc, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)

                      if (acc.match == FALSE) {
                          msg <- paste("[ERROR] The input dbGaP ", typeName, " accession ",  acc, ", has an incorrect format. It should have a ", type,  " followed by zero padded 6 digits and a suffix .v#, e.g. pht003718.v1.\n", sep = "")
                          cat(msg)
                      }
                      else {
                          cleanAcc = acc
                      }
                  }
                  if (type == 'phv') {
                      # Format check
                      acc.rex <- "^phv\\d{8}\\.v\\d+$"
                      acc.match <- grepl(acc.rex, acc, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)

                      if (acc.match == FALSE) {
                          msg <- paste("[ERROR] The input dbGaP ", typeName, " accession ",  acc, ", has an incorrect format. It should have a ", type,  " followed by zero padded 8 digits and a suffix .v#, e.g. phv00000135.v2 or phv00000135.v2.p1 (the p# and c# are optional).\n", sep = "")
                          cat(msg)
                      }
                      else {
                          cleanAcc = acc
                      }
                  }
              }

              return(cleanAcc)
          })


# ----------------------
# Method: checkInputPath 
# ----------------------

#' (internal) Validate input directory or file path 
#'
#' The method checks the format of an input directory or file path to see if it is in a valid format. Normalized path is returned.  
#'
#' @name checkInputPath 
#' @param object Commons class object.
#' @param path a character string. A directory or file path. 
#' @return a character string. Normalized input path. 
#' @export checkInputPath 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' checkInputPath(c, path = '/home/foo/temp')
#'}

setGeneric(name = "checkInputPath",
           def = function(object, path) {
               standardGeneric("checkInputPath")
           }
           )

#' @describeIn checkInputPath A method of class Commons 
setMethod(
          f = "checkInputPath",
          signature = c("Commons", "character"),
          definition = function(object, path) {

              fileName = basename(path)
              filePath = dirname(path)  # normalized path
              path <- file.path(filePath, fileName) 

              # ATTN: When input path is an empty vale "", after above steps, the path becomes "/"
              # Replace "\" with "" here
              path <- gsub('^\\/$', '', path)

              return (path)

          })

# ---------------------------
# Method: filterBySubjIds 
# ---------------------------

#' (internal) Filter dataset data by subject ids 
#'
#' The method filters out the rows of a dataset by a given list of subject ids. The subject ids can be provided as a list or as a text file.
#'
#' @name filterBySubjIds 
#' @param object Commons class object.
#' @param varDF a data frame. The dataset data.
#' @param ... There are optional arguments.
#' @param dbgapIdsOrFile a character vector or a character string. (optional) This argument can be either a vector of ID list or a path to a file that contains a list of IDs. The IDs can be dbGaP_Subject_ID or dbGaP_Sample_ID denpending on type of the data. When the list of IDs is provided by a file, it should be  a plain text file with one ID per line. 
#' @return a data frame. Filtered dataset data. 
#' @export filterBySubjIds 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' filterBySubjIds(c, varDF = varDF, dbgapIdsOrFile = c("219", "220", "223"))
#'}

setGeneric(name = "filterBySubjIds",
           def = function(object, varDF, ...) {
               standardGeneric("filterBySubjIds")
           }
           )

#' @describeIn filterBySubjIds A method of class Commons 
setMethod(
          f = "filterBySubjIds",
          signature = c("Commons", "data.frame"),
          definition = function(object, varDF, ..., dbgapIdsOrFile = NULL) {


              ############################
              # Further process subjIds
              ############################

              if (!is.null(dbgapIdsOrFile)) {
                  ######## Get subjIdDF  #########

                  # Test it is a string and length is 1
                  # A single string or a string vector with a single element returns
                  inputFile = ''
                  subjIdList = list()

                  isFile = FALSE 
                  inputOk = T
                  if (is.character(dbgapIdsOrFile) & length(dbgapIdsOrFile) == 1) {

                      ###########################
                      # Check file existence
                      ###########################
                      inputFile <- checkInputPath(object, unlist(dbgapIdsOrFile))
                      if (file.exists(inputFile)) {
                          isFile = TRUE
                      }
                      else {
                          isFile = F 

                          checkDF = varDF[varDF$dbGaP_Subject_ID %in% dbgapIdsOrFile, ]  

                          #############################################
                          # The input is not an existing file.
                          # It is also not an valid dbGaP_Subject_ID
                          #############################################
                          if (nrow(checkDF) > 0) {
                              isFile = FALSE
                          }
                          else {
                              inputOk = F 

                              type = 'process'
                              level = 'error'
                              show = T
                              mesg = paste("The input suject id file is not found. --- ", dbgapIdsOrFile, "\n", sep="")
                              writeLog(object,  type = type, level = level, message = mesg, show = show) 
                          }

                      }
                  }

                  # Proceed only if input is Ok
                  if (inputOk) {

                      if (isFile) {
                          subjIdDF <- read.table(inputFile, header = F, fill = TRUE, quote = "", sep ='\t', stringsAsFactors = FALSE, encoding="UTF-8") 
                          subjIdList <- unlist(as.list(subjIdDF), use.names=FALSE) 
                      }
                      else {
                          subjIdList = dbgapIdsOrFile 
                      }

                      if (is.vector(subjIdList) & length(subjIdList) > 0) {

                          #################################
                          # Report unmatched input subjIds
                          #################################
                          # Subj id list from the merged data DF 

                          if (any(colnames(varDF) == 'dbGaP_Subject_ID')) {
                              dataSubjIdList <- as.list(subset(varDF, select = c('dbGaP_Subject_ID')))
                          }
                          else if (any(colnames(varDF) == 'dbGaP_Sample_ID')) {
                              dataSubjIdList <- as.list(subset(varDF, select = c('dbGaP_Sample_ID')))
                          }
                          else if (any(colnames(varDF) == 'Submitted_Subject_ID')) {
                              dataSubjIdList <- as.list(subset(varDF, select = c('Submitted_Subject_ID')))
                          }
                          else if (any(colnames(varDF) == 'Submitted_Sample_ID')) {
                              dataSubjIdList <- as.list(subset(varDF, select = c('Submitted_Sample_ID')))
                          }
                          else {
                              type = 'process'
                              level = 'error'
                              show = T
                              mesg = paste("The dataset does not have any dbGaP or Submitted subject or sample ID column. It appears to be an data error. Please report it to dbgap-help@ncbi.nlm.nih.gov.\n   --- ", falseIdCombo, "\n", sep="")
                              writeLog(object,  type = type, level = level, message = mesg, show = show) 
                          }


                          # Get indices of the input subjIdList items that have no match with any subjIds in mergedVarDf 
                          dt <- expand.grid(subjIdList, dataSubjIdList)
                          crossCheckLogicVec <- apply(dt,1, function(.v) all(.v[[1]] %in% .v[[2]]) )			# This returns a logic vector like: TRUE  TRUE FALSE FALSE

                          falseIndices <- which(!crossCheckLogicVec)											# Find out TRUE value indices of the reversed logic vector

                          if (length(falseIndices > 0)) {
                              falseIds <- sapply(falseIndices, function(x) 
                                                 {
                                                     unMatchId <- as.integer(subjIdList[x])

                                                 })

                              falseIdCombo <- paste(falseIds, collapse = ',')

                              type = 'process'
                              level = 'warn'
                              show = T
                              mesg = paste("These input IDs don't have any match wth the dbGaP IDs, or the Submitted IDs when the dbGaP ID column is absent, of the data table.\n   --- ", falseIdCombo, "\n", sep="")
                              writeLog(object,  type = type, level = level, message = mesg, show = show) 
                          }

                          ######################################
                          # Subset based on the subjIdList
                          ######################################
                          matchDataDF = varDF[varDF$dbGaP_Subject_ID %in% subjIdList, ]  

                          if (nrow(matchDataDF) > 0) {
                              finalVarDF = unique(matchDataDF)		# remove dupliate rows

                              if (nrow(varDF) == 0) {
                                  type = 'process'
                                  level = 'info'
                                  show = T
                                  mesg = paste("There is no data that match the input IDs.\n", sep="") 
                                  writeLog(object,  type = type, level = level, message = mesg, show = show) 
                              }
                              return (finalVarDF)
                          }

                      } # end length(subjIdList) > 0
                      else {
                          # Return empty data.frame
                          return (data.frame())
                      }
                  }
                  else {
                      return (data.frame())
                  }
              }
              else {
                  return (varDF)
              }


          })


# --------------------------------
# Method: detectVariableDataType 
# --------------------------------

#' (internal, deprecated) Detect variable data type 
#'
#' The method determines the data type of a given variable by looking into the data values. The funciont is called through \code{\link{dataDicXmlParser}}. Running this method is resource expensive. 
#'
#' @name detectVariableDataType 
#' @param object Commons class object.
#' @param phsAcc a character string. The dbGaP study accession of the given variable. 
#' @param phtAcc a character string. The dbGaP dataset accession of the given variable. 
#' @param phvAcc a character string. The dbGaP phenotype variable accession. 
#' @param varName a character string. The dbGaP phenotype variable name. 
#' @param ... There are optional arguments. 
#' @param codeValCombo a character string. The combo string of a categorical variable value code and the respective code meaning.
#' @param fileInfoDF a data frame. The file info data. 
#' @return a character string. The data types of the input variable accession.
#' @export detectVariableDataType 
#' @keywords internal

# @examples
# c <- Commons() 
# 
# detectVariableDataType(c, phsAcc = "phs000007.v29", phtAcc = "pht000027.v3", phvAcc = "phv00006623.v1", varName="FR397", codeValCombo="2:MULTIFOC|0:NO|5:R ON T|1:SIMPLE|.:UNKNOWN|3:PAIRS|4:RUN")      # combo 
# detectVariableDataType(c, phsAcc = "phs000007.v29", phtAcc = "pht000009.v2", phvAcc = "phv00000504.v1", varName="MF30", codeValCombo=NA)  # numeric, code_combo NA

# detectVariableDataType(s, phvAcc = "phv00065292.v2")      # NA
# detectVariableDataType(s, phvAcc = "phv00056514.v4")      # NA
# detectVariableDataType(s, phvAcc = "phv00036378.v8")      # NA
# detectVariableDataType(s, phvAcc = "phv00275923.v1")      # unknown
# detectVariableDataType(s, phvAcc = "phv00003598.v1")      # unknown
# detectVariableDataType(s, phvAcc = "phv00006616.v1")      # combo     (0, 1)
# detectVariableDataType(s, phvAcc = "phv00007669.v5")      # combo

setGeneric(name = "detectVariableDataType",
           def = function(object, phsAcc, phtAcc, phvAcc, varName, ...) {
               standardGeneric("detectVariableDataType")
           })

#' @describeIn detectVariableDataType A method of class Study 
setMethod(
          f = "detectVariableDataType",
          signature = c("Commons", "character"),
          definition = function(object, phsAcc, phtAcc, phvAcc, varName, ..., codeValCombo=NA, fileInfoDF = data.frame()) {

              prjDotDir = object@prjDotDir
              prjDataDir = object@prjDataDir
              prjDir = object@prjDir

              if (nrow(fileInfoDF) == 0) {
                  fileInfoFile = object@fileInfoFile
                  if (file.exists(fileInfoFile)) {
                      fileInfoDF <- fromJSON(fileInfoFile, flatten=TRUE)
                  }
              }



              thisFileInfoDF <- dplyr::filter(fileInfoDF, fileInfoDF$fPhtAcc == phtAcc & fileInfoDF$consentType == 'Indiv')

              # Only indiv is processed
              if (nrow(thisFileInfoDF) > 0) {

                  # Get the data file path
                  pathToFile = thisFileInfoDF$pathToFile[1]

                  ###########################################
                  # ATTN! This is the original txt.gz file
                  ###########################################
                  # Do not use read.csv. Use read.table to read gz File 
                  phtDataDF <- read.table(pathToFile, header=TRUE, fill = TRUE, sep="\t", encoding="UTF-8", stringsAsFactors=FALSE)

                  # Check if the column exists
                  if(varName %in% colnames(phtDataDF)) {

                      # Get unique column value as a list
                      # [1] NA  0  2  1  3
                      uniqueDataList <-unique(phtDataDF[,c(varName)])

                      # Remove NA from the list 
                      # [1] 0 2 1 3
                      listNoNa <- uniqueDataList[!is.na(uniqueDataList)]

                      ################################
                      # Calculated data type
                      ################################
                      varDF <- subset(phtDataDF, select=c(varName))
                      calcDataTypeDF <- sapply(varDF, class)

                      # Convert the varible column to a vector
                      calcDataType <- calcDataTypeDF[[varName]][1]

                      if (is.null(codeValCombo) | is.na(codeValCombo)) {
                          return(calcDataType)
                      }
                      else {

                          #########################################
                          # Determine type using code_value_combo
                          #########################################
                          # Example code_value_combo
                          # "2:MULTIFOC|0:NO|5:R ON T|1:SIMPLE|.:UNKNOWN|3:PAIRS|4:RUN"
                          # Split
                          codeValues <- strsplit(codeValCombo, "|", fixed = TRUE)[[1]]
                          codeListList <- lapply(codeValues, function(x) {
                                                     codeValPair <- strsplit(x, ":", fixed = TRUE)[[1]]
                                                     code <- codeValPair[1]

                                                     # Skip if code value is a dot "."
                                                     if (code != ".") {
                                                         return(code)
                                                     }
                                                 })

                          # Final list of code
                          # [1] "2" "0" "5" "1" "3" "4"
                          codeList <- unlist(codeListList)

                          # listNoNa: [1] 0 2 1 3
                          # codeList: [1] "2" "0" "5" "1" "3" "4"
                          # sharedList: [1] TRUE TRUE TRUE TRUE 
                          sharedList <- listNoNa %in% codeList 

                          # list of only TRUE value
                          sharedTrueCount <- summary(sharedList)["TRUE"]

                          ######################################################################
                          # Compute proportion of column unique values that have code.
                          ######################################################################
                          # Set 50% cutoff 
                          if (!is.na(sharedTrueCount)) {
                              # Call it categorical if at lease 1/2 of unique values found in code_val_combo
                              if (as.numeric(sharedTrueCount) / length(listNoNa) > 0.5) {
                                  return('enumerated_number')
                              }
                              else {
                                  return(calcDataType)
                              }
                          }
                          else {
                              return(calcDataType)
                          }
                      } # end of NA or NULL

                  } # end of check column exists 

              } # end zero row of thisFileInfoDF 
              else {
                  return(NA)
              }

          })

# ----------------------
# Method: getExtData
# ----------------------

#' (internal) Get the supplemental (external) data
#'
#' The method returns the supplemental data from the files downloaded from the dbGaP ftp site.   
#'
#' @name getExtData 
#' @param object Commons class object.
#' @param type a character string. The object type that is either 'study' (all_study_info), 'dataset' (study_dataset_info), 'variable' (study_variable_info), 'id' (study_id_variable_name), 'code' (study_variable_code_value), or 'manifest' (study_file_manifest).
#' @param ... There are optional arguments. 
#' @param phsAcc a character string. (optional) The study accession. It is required when the type argument value is not 'study'.
#' @param dataStudyOnly a logical value. When TRUE (default), only downloads the dataset and variable metadata of the stdudies that have data files in the project directory.  When FALSE, downloads the dataset and variable metadata of all dbGaP released studies, regardless the actual phenotype data files of the studies are downloaded or not. 
#' @return a data frame. The meta-data of respective type provided through the input. 
#' @export getExtData 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' getExtData(c, type = 'study')
#' # or
#' getExtData(c, type = 'dataset', phsAcc = 'phs000001.v3')
#' # or
#' getExtData(c, type = 'variable', phsAcc = 'phs000001.v3')
#' # or
#' getExtData(c, type = 'id', phsAcc = 'phs000001.v3')
#' # or
#' getExtData(c, type = 'code', phsAcc = 'phs000001.v3')
#' # or
#' getExtData(c, type = 'manifest', phsAcc = 'phs000001.v3')
#'}

setGeneric(name = "getExtData",
           def = function(object, type, ...) {
               standardGeneric("getExtData")
           })

#' @describeIn getExtData A method of class Commons 
setMethod(
          f = "getExtData",
          signature = c("Commons", "character"),
          definition = function(object, type, ..., phsAcc = "", dataStudyOnly = TRUE) {

              prjDataDir = object@prjDataDir 
              prjMetaDir <- object@prjMetaDir

              # This_study_version level
              # ftp://ftp.ncbi.nlm.nih.gov/dbgap/r-tool/studies/phs000429/phs000429.v1/
              # phs000429.v1_study_id_variable_name.txt.gz
              # phs000429.v1_study_info.txt.gz
              # phs000429.v1_study_dataset_info.txt.gz
              # phs000429.v1_study_variable_info.txt.gz
              # phs000429.v1_study_variable_code_value.txt.gz
              # phs000429.v1_study_file_manifest.txt.gz

              if (type == 'study') {

                  # New!
                  infoFile = object@extAllStudyInfoFile 


                  if (file.exists(infoFile)) {
                      infoDF <- read.table(infoFile, header=TRUE, fill = TRUE, sep="\t", encoding="UTF-8", stringsAsFactors=FALSE)

                      if (nrow(infoDF) > 1) {
                          return (infoDF)
                      }
                  }
                  else {
                      type = 'process'
                      level = 'error'
                      show = T
                      mesg = paste(
                          "The study-info file is not found. Checkout ?prjConfig() and ?prepareData() to make sure the project directory is setup and the data files are copied and processed.",
                          " --- ", infoFile, sep="" 
                      )
                      writeLog(object,  type = type, level = level, message = mesg, show = show) 

                  }
              }
              else {

                  # Validate accession 
                  inputPhsAcc = phsAcc





                  if (inputPhsAcc != "") {
                      phsAcc <- cleanObjAcc(object, acc = phsAcc, type = 'phs') 
                  }

                  if (inputPhsAcc != "" & phsAcc != "" & (type == 'dataset' | type == 'variable' | type == 'id' | type == 'code' | type == 'manifest')) {

                      # Parse study ids
                      parseIdsFromStAcc =  parseIdsFromStAcc(object, phsAcc = phsAcc) 
                      phsAccNoVer = parseIdsFromStAcc$phsAccNoVer 

                      studyExtDataDir = file.path(prjDataDir, phsAccNoVer, phsAcc, 'supplemental_data')
                      studyExtMetaDir = file.path(prjMetaDir, phsAccNoVer, phsAcc, 'supplemental_data')


                      if (type == 'dataset') {
                          # New!
                          # /c/Users/mars/Documents/myprj/gapwork/data/phs000429/phs000429.v1/supplemental_data/phs000429.v1_study_dataset_info.txt.gz
                          # /c/Users/mars/Documents/myprj/gapwork/data/phs000001/phs000001.v3/supplemental_data/phs000001.v3_study_dataset_info.txt.gz


                          infoFileName = paste0(phsAcc, "_study_dataset_info.txt.gz") 

                          ################################
                          # Get PhtInfoFile of DataStudy
                          ################################
                          infoFile = file.path(studyExtDataDir, infoFileName) 

                          if (file.exists(infoFile)) {
                              infoDF <- read.table(infoFile, header=TRUE, fill = TRUE, sep="\t", encoding="UTF-8", stringsAsFactors=FALSE)

                              if (nrow(infoDF) > 0) {
                                  return (infoDF)
                              }
                          }
                          else {


                              if (dataStudyOnly) {

                                  type = 'process'
                                  level = 'error'
                                  show = T
                                  mesg = paste(
                                      "The dataset-info file is not found for study ", phsAcc, ". To look for the dataset info of the study that has no data under the project, rerun the command with the argument dataStudyOnly=TRUE. Otherwise, checkout ?prjConfig() and ?prepareData() to make sure the project directory is setup and the data files are copied and processed.",
                                      " --- ", infoFile, sep="" 
                                  )
                                  #cat("\n")
                                  #writeLog(object,  type = type, level = level, message = mesg, show = show) 
                              }
                              else {
                                  #mesg = paste("Meta-info is available but there is no data file found under the project directory for this study, ", phsAcc, ".\n", sep="")
                                  #message(mesg)

                                  ###################################
                                  # Get PhtInfoFile of noDataStudy
                                  ###################################
                                  noDataInfoFile = file.path(studyExtMetaDir, infoFileName) 

                                  if (file.exists(noDataInfoFile)) {

                                      noDataInfoDF <- read.table(noDataInfoFile, header=TRUE, fill = TRUE, quote = "", sep="\t", encoding="UTF-8", stringsAsFactors=FALSE)

                                      if (nrow(noDataInfoDF) > 0) {
                                          return(noDataInfoDF)
                                      }
                                  }
                              }
                          }
                      }
                      else if (type == 'variable') {

                          # New!
                          # /c/Users/mars/Documents/myprj/gapwork/data/phs000429/phs000429.v1/supplemental_data/phs000429.v1_study_variable_info.txt.gz
                          infoFileName = paste0(phsAcc, "_study_variable_info.txt.gz") 

                          ################################
                          # Get PhtInfoFile of DataStudy
                          ################################
                          infoFile = file.path(studyExtDataDir, infoFileName) 


                          if (file.exists(infoFile)) {

                              infoDF <- read.table(infoFile, header=TRUE, fill = TRUE, quote = "", sep="\t", encoding="UTF-8", stringsAsFactors=FALSE)
                              if (nrow(infoDF) > 0) {
                                  return (infoDF)

                              }
                          }
                          else {

                              if (dataStudyOnly) {

                                  type = 'process'
                                  level = 'error'
                                  show = T
                                  mesg = paste(
                                      "The variable-info file is not found for study ", phsAcc, ". To look for the variable info of the study that has no data under the project, rerun the command with the argument dataStudyOnly=TRUE. Otherwise, checkout ?prjConfig() and ?prepareData() to make sure the project directory is setup and the data files are copied and processed.", " --- ", infoFile, sep="")
                                      #writeLog(object,  type = type, level = level, message = mesg, show = show) 
                                  
                              }
                              else {
                                  #mesg = paste("Meta-info is available but there is no data file found under the project directory for this study, ", phsAcc, ".\n", sep="")
                                  #message(mesg)

                                  ###################################
                                  # Get PhvInfoFile of noDataStudy
                                  ###################################
                                  noDataInfoFile = file.path(studyExtMetaDir, infoFileName) 

                                  if (file.exists(noDataInfoFile)) {
                                      noDataInfoDF <- read.table(noDataInfoFile, header=TRUE, fill = TRUE, quote = "", sep="\t", encoding="UTF-8", stringsAsFactors=FALSE)

                                      if (nrow(noDataInfoDF) > 0) {
                                          return(noDataInfoDF)
                                      }
                                  }
                              }
                          }

                      }
                      else if (type == 'id') {

                          ###############################
                          # Get study id variable data 
                          ###############################
                          # New!
                          # /c/Users/mars/Documents/myprj/gapwork/data/phs000429/phs000429.v1/supplemental_data/phs000429.v1_study_id_variable_name.txt.gz
                          infoFileName = paste0(phsAcc, "_study_id_variable_name.txt.gz") 
                          infoFile = file.path(studyExtDataDir, infoFileName) 

                          infoDF <- read.table(infoFile, header=TRUE, fill = TRUE, sep="\t", encoding="UTF-8", stringsAsFactors=FALSE)

                          if (file.exists(infoFile)) {
                              infoDF <- read.table(infoFile, header=TRUE, fill = TRUE, sep="\t", encoding="UTF-8", stringsAsFactors=FALSE)

                              if (nrow(infoDF) > 0) {
                                  return (infoDF)
                              }
                          }
                          else {

                              if (dataStudyOnly) {

                                  cat("\n")
                                  type = 'process'
                                  level = 'error'
                                  show = T
                                  mesg = paste(
                                      "The study id-variable-name info file is not found for study ", phsAcc, ". To look for the id-variable-name info file of the study that has no data under the project, rerun the command with the argument dataStudyOnly=TRUE. Otherwise, checkout ?prjConfig() and ?prepareData() to make sure the project directory is setup and the data files are copied and processed.",
                                      " --- ", infoFile, sep="" 
                                  )
                                  writeLog(object,  type = type, level = level, message = mesg, show = show) 
                              }
                              else {
                                  
                                  #mesg = paste("Meta-info is available but there is no data file found under the project directory for this study, ", phsAcc, ".\n", sep="")
                                  #message(mesg)

                                  ###################################
                                  # Get IdVarNameInfoFile of noDataStudy
                                  ###################################
                                  noDataInfoFile = file.path(studyExtMetaDir, infoFileName) 

                                  if (file.exists(noDataInfoFile)) {
                                      noDataInfoDF <- read.table(noDataInfoFile, header=TRUE, fill = TRUE, quote = "", sep="\t", encoding="UTF-8", stringsAsFactors=FALSE)

                                      if (nrow(noDataInfoDF) > 0) {
                                          return(noDataInfoDF)
                                      }
                                  }
                              }
                          }
                      }
                      else if (type == 'code') {
                          ###############################
                          # Get study code value data 
                          ###############################

                          # New!
                          # /c/Users/mars/Documents/myprj/gapwork/data/phs000429/phs000429.v1/supplemental_data/phs000429.v1_study_variable_code_value.txt.gz
                          infoFileName = paste0(phsAcc, "_study_variable_code_value.txt.gz") 
                          infoFile = file.path(studyExtDataDir, infoFileName) 

                          if (file.exists(infoFile)) {
                              infoDF <- read.table(infoFile, header=TRUE, fill = TRUE, sep="\t", encoding="UTF-8", stringsAsFactors=FALSE)

                              if (nrow(infoDF) > 0) {
                                  return (infoDF)
                              }
                          }
                          else {

                              if (dataStudyOnly) {
                                  #cat("\n")
                                  type = 'process'
                                  level = 'error'
                                  show = T
                                  mesg = paste(
                                      "The study variable-code-value-info file is not found for study ", phsAcc, ". To look for the variable-code-value-info file of the study that has no data under the project, rerun the command with the argument dataStudyOnly=TRUE. Otherwise, checkout ?prjConfig() and ?prepareData() to make sure the project directory is setup and the data files are copied and processed.",
                                      " --- ", infoFile, sep="" 
                                  )
                                  #writeLog(object,  type = type, level = level, message = mesg, show = show) 
                              }
                              else {

                                  #mesg = paste("Meta-info is available but there is no data file found under the project directory for this study, ", phsAcc, ".\n", sep="")
                                  #message(mesg)

                                  ########################################
                                  # Get VarCodValInfoFile of noDataStudy
                                  ########################################
                                  noDataInfoFile = file.path(studyExtMetaDir, infoFileName) 

                                  if (file.exists(noDataInfoFile)) {
                                      noDataInfoDF <- read.table(noDataInfoFile, header=TRUE, fill = TRUE, quote = "", sep="\t", encoding="UTF-8", stringsAsFactors=FALSE)

                                      if (nrow(noDataInfoDF) > 0) {
                                          return(noDataInfoDF)
                                      }
                                  }
                              }
                          }
                      }
                      else if (type == 'manifest') {

                          # New!
                          # /c/Users/mars/Documents/myprj/gapwork/data/phs000429/phs000429.v1/supplemental_data/phs000001.v3_study_file_manifest.txt.gz
                          infoFileName = paste0(phsAcc, "_study_file_manifest.txt.gz")
                          infoFile = file.path(studyExtDataDir, infoFileName) 

                          infoDF <- read.table(infoFile, header=TRUE, fill = TRUE, sep="\t", encoding="UTF-8", stringsAsFactors=FALSE)

                          if (file.exists(infoFile)) {
                              infoDF <- read.table(infoFile, header=TRUE, fill = TRUE, sep="\t", encoding="UTF-8", stringsAsFactors=FALSE)

                              if (nrow(infoDF) > 0) {
                                  return (infoDF)
                              }
                          }
                          else {
                              cat("\n")
                              type = 'process'
                              level = 'error'
                              show = T
                              mesg = paste(
                                           "The study manifest file is not found for study ", phsAcc, ". Checkout ?prjConfig() and ?prepareData() to make sure the project directory is setup and the data files are copied and processed.",
                                           " --- ", infoFile, sep="" 
                                           )
                              writeLog(object,  type = type, level = level, message = mesg, show = show) 
                          }
                      }
                      else {
                          type = 'process'
                          level = 'error'
                          show = T
                          mesg = paste("The 'type' argument ", type, " of etExtData function isn't a valid value\n", sep="")
                          writeLog(object,  type = type, level = level, message = mesg, show = show) 
                      }
                  }
                  else {
                      type = 'process'
                      level = 'error'
                      show = T
                      mesg = paste("When the input argument type is ", type, " a valide study accession should be provided\n", sep="")
                      writeLog(object,  type = type, level = level, message = mesg, show = show) 
                  }
              }

              return()

          })

# -------------------------------- 
# Method: parseDataDic 
# -------------------------------- 

#' (internal, deprecated) Parse data dictionary XML files of all studies.
#'
#' The method parses the data dictionary XML files of a specific or all studies available under the project directory. The parsed values from one study are merged and saved in files of three different formats (rds, csv, and json). Before calling the function, the data dictionary files of the study should be made available in the data_dic sub-directory under the project directory. Checkout the \code{\link{searchCopyPhenoFiles}} function for how to move the files to the directory if not. The method is no longer needed since the data dictionary XML files are replaced by the csv format of the data dictionary downloadable from the dbGaP ftp site.
#'
#' @name parseDataDic
#' @param object Commons class object.
#' @param ... There are optional arguments. 
#' @param phsAcc a character string. The dbGaP study accession.
#' @param computeType logical value. (optional) If TRUE, compute data type based on data value and include it in the meta-info file. If FALSE, not compute data type. Note: This step is computationally expensive. Do not use it if it is a large study such as Framingham.
#' @return (invisible) a data frame. The combined data of data dictionary files of all studies. 
#' @export parseDataDic
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' parseDataDic(c) 
#' # or
#' parseDataDic(c, phsAcc = 'phs000001.v3.p1') 
#'}

setGeneric(
           name = "parseDataDic",
           def = function(object, ...) {
               standardGeneric("parseDataDic")
           }
           )

#' @describeIn parseDataDic A method of class Commons 
setMethod(
          f = "parseDataDic",
          signature = c("Commons"),
          definition = function(object, ..., phsAcc = "", computeType = FALSE) {

              # Validate accession 
              inputPhsAcc = phsAcc
              if (phsAcc != "") {
                  phsAcc <- cleanObjAcc(object, acc = phsAcc, type = 'phs') 
              }

              prjDataDir = object@prjDataDir 
              prjDotDir = object@prjDotDir   
              prjDir = object@prjDir  
              configFile = object@configFile  
              fileInfoFile = object@fileInfoFile 
              fileInfoArchDir = object@fileInfoArchDir  

              extDataDir = object@extDataDir 
              extStudyInfoFile = object@extStudyInfoFile
              extStudyDatasetInfoFile = object@extStudyDatasetInfoFile
              extPhenoSharedIdNamesFile = object@extPhenoSharedIdNamesFile

              # Get the external studyInfo data
              studyDatasetInfo <- read.table(extStudyDatasetInfoFile, header = T, fill = TRUE, quote = "", sep ='\t', stringsAsFactors = FALSE, encoding="UTF-8")  

              # Example location of study (no-version) data dictionary
              # /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/general_info
              dataGenInfoDir = file.path(prjDataDir,'general_info') 
              if (!dir.exists(dataGenInfoDir)) {
                  dir.create(dataGenInfoDir, showWarnings = TRUE, recursive = T, mode = "0777")
              }

              # S3 function 
              mergeDataInfo <- function(fileInfoFile) {
                  # Get the list of all available study accessions
                  if (file.exists(fileInfoFile)) {
                      fileInfoDF <- fromJSON(fileInfoFile, flatten=TRUE)
                      studyAccDF <- unique(fileInfoDF[c("fStAcc")])
                      studyAccList = studyAccDF$fStAcc

                      availStudyDatasetInfoList <- lapply(studyAccList, function(x) 
                                                          {
                                                              thisPhsAcc <- x

                                                              # dataset info file location:
                                                              # /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/phs000001/phs000001.v3/data_dic/combo_dump
                                                              # phs000001.v3_study_dataset_info_combo.rds
                                                              # phs000001.v3_study_dataset_info_combo.txt
                                                              # phs000001.v3_study_dataset_info_combo.json
                                                              parseDataDicByStudy(object, phsAcc = thisPhsAcc) 

                                                          })  

                      mergeDataInfoDF <- do.call('rbind', availStudyDatasetInfoList)

                      return(invisible(mergeDataInfoDF))
                  }
                  else {
                      type = 'process'
                      level = 'error'
                      show = T
                      mesg = paste("\nThe data file info file is not found. --- ", fileInfoFile, "\n", sep="") 
                      writeLog(object,  type = type, level = level, message = mesg, show = show)
                  }
              }

              if (inputPhsAcc == "") {
                  mergeDataInfo(fileInfoFile)
              }
              else {
                  if (phsAcc != "") {
                      # dataset info file location:
                      # /netmnt/sandtraces04/dbgap-release04/dbgapr_test/dbgapr_user_project4/gapwork/data/phs000001/phs000001.v3/data_dic/combo_dump
                      # phs000001.v3_study_dataset_info_combo.rds
                      # phs000001.v3_study_dataset_info_combo.txt
                      # phs000001.v3_study_dataset_info_combo.json
                      parseDataDicByStudy(object, phsAcc = phsAcc, computeType = computeType) 
                  }
              }
          })


# ----------------------------
# Method: buildVarCodeValCombo
# ----------------------------

#' (internal) Compose variable code-value combo string. 
#'
#' The method parses id strings from a dbGaP study accession.
#'
#' @name buildVarCodeValCombo
#' @param object Commons class object.
#' @param catVarAcc a character string. The accession of a categorical vairable. 
#' @param ... There are optional arguments.
#' @param phsAcc a character string. (optional) The study accession to which the input variable belongs. This is varCodeVarDF is not provided.
#' @param varCodeValDF a data frame. (optional) The variable code value table. 
#' @return a named character list. The list of parsed id strings including 'phsAcc' for study acession, 'phsAccNoVer' for the study accession with the v# (version number) removed. 'phsAccId' for study id, and 'phsAccVer' for study version.
#' @export buildVarCodeValCombo
#' @keywords internal


# @examples
# \dontrun{
#
# c <- Commons() 
# combo <- buildVarCodeValCombo(c, catVarAcc = 'phv00000119.v2', phsAcc = 'phs000001.v3.p1')
#}

setGeneric(
           name = "buildVarCodeValCombo",
           def = function(object, catVarAcc, ...) {
               standardGeneric("buildVarCodeValCombo")
           })

#' @describeIn buildVarCodeValCombo A method of class Commons 
setMethod(
          f = "buildVarCodeValCombo",
          signature = c("Commons", "character"),
          definition = function(object, catVarAcc, ..., phsAcc = "", varCodeValDF = data.frame()) {


              if (nrow(varCodeValDF) == 0) {

                  if (phsAcc != "") {

                      tempDF <- getExtData(object, type = 'code', phsAcc = phsAcc)

                      if (!is.null(tempDF)) {
                          varCodeValDF <- tempDF 
                      }
                  }
              }

              if (nrow(varCodeValDF) > 0) {

                  #######################
                  # Build codeValCombo
                  #######################
                  matchVarCodeValDF <- dplyr::filter(varCodeValDF, varCodeValDF$variable_accession == catVarAcc)

                  ###########################
                  # Build code-value combo
                  ###########################
                  catVarCodeValCombo = NA 
                  dat <- matchVarCodeValDF
                  dat$comboCol <- do.call(paste, c(dat[c("code", "value")], sep = ":"))
                  comboList <- paste(dat$code, dat$value, sep=':') 

                  if (length(comboList) > 0) {
                      catVarCodeValCombo <- paste(comboList, collapse = '|')
                  }
                  else {
                      catVarCodeValCombo = NA
                  }

                  return(catVarCodeValCombo)

              }

          })


# --------------------------------------
# Method: getAvailPhtVer 
# --------------------------------------

#' (internal) Get available dataset accession by variable accession 
#'
#' When querying study_variable_info table with a variable accession, multiple dataset versions could be returned. This methods further queris the study_dataset table given any of the dataset accessions from the early query, and returns the correct dataset accession that matches the given study accession.
#'
#' @name getAvailPhtVer 
#' @param object Commons class object.
#' @param randPhtAcc a character string. One of the randomly chosen dataset accessions returned from queyring study_variable_info table by a variable accession.
#' @param ... There are optional arguments. 
#' @param phsAcc a character string. A study accession. It is required when studyPhtInfoDF is not provided. 
#' @param studyPhtInfoDF a data frame. The dataset info table of the study. 
#' @param dataStudyOnly a logical value. When TRUE (default), only downloads the dataset and variable metadata of the stdudies that have data files in the project directory.  When FALSE, downloads the dataset and variable metadata of all dbGaP released studies, regardless the actual phenotype data files of the studies are downloaded or not. 
#' @return a character string. The dataset accession that have a match with available studies. 
#' @export getAvailPhtVer 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' getAvailPhtVer(c, randPhtAcc='pht000009.v1', phsAcc='phs000007.v29')
#'}

setGeneric(name = "getAvailPhtVer",
           def = function(object, randPhtAcc, ...) {
               standardGeneric("getAvailPhtVer")
           }
           )

#' @describeIn getAvailPhtVer A method of class Commons 
setMethod(
          f = "getAvailPhtVer",
          signature = c("Commons", "character"),
          definition = function(object, randPhtAcc, ..., phsAcc = NA, studyPhtInfoDF = data.frame(), dataStudyOnly = TRUE) {

              # Parse ids from phtAcc
              phtAccIds = parseIdsFromPhtAcc(object, phtAcc = randPhtAcc)
              phtAccId = phtAccIds$phtAccId
              phtAccNoVer = phtAccIds$phtAccNoVer

              if (nrow(studyPhtInfoDF) == 0) {

                  if (!is.na(phsAcc)) {

                      tempDF <- getExtData(object, type = 'dataset', phsAcc = phsAcc, dataStudyOnly = dataStudyOnly)

                      if (!is.null(tempDF)) {
                          studyPhtInfoDF <- tempDF 
                      }
                  }
                  else {
                      type = 'setup'
                      level = 'error'
                      show = T
                      mesg = paste("Either studyPhtDF or phsAcc needs to be provided when calling getAvailPhtVer(). Neither is found.\n", sep="") 
                      writeLog(object,  type = type, level = level, message = mesg, show = show)
                  }
              }

              if (nrow(studyPhtInfoDF) > 0) {

                  matchDF <- dplyr::filter(studyPhtInfoDF, studyPhtInfoDF$study_accession == phsAcc & studyPhtInfoDF$dataset_id == phtAccId)
                  uniqueDF <- unique(matchDF[c('dataset_accession')])     # remove dupliate rows due to different consents

                  if (nrow(uniqueDF) > 0) {
                      if (nrow(uniqueDF) == 1) {
                          phtAcc = toString(uniqueDF) 
                          return(phtAcc)
                      }
                      else {
                          type = 'setup'
                          level = 'error'
                          show = T
                          mesg = paste("More than one dataset accession versions are found in the study_dataset_info file for study, ", phsAcc, " and dataset id, ", phtAccId, " (", phtAccNoVer, ")\n", sep="") 
                          writeLog(object,  type = type, level = level, message = mesg, show = show)
                      }
                  }
                  else {
                      type = 'setup'
                      level = 'error'
                      show = T
                      mesg = paste("No dataset found in the study_dataset_info file for study, ", phsAcc, " and dataset id, ", phtAccId, " (", phtAccNoVer, ")\n", sep="") 
                      writeLog(object,  type = type, level = level, message = mesg, show = show)
                  }
              }
              else {
                  type = 'setup'
                  level = 'error'
                  show = T
                  mesg = paste("No dataset found in the study_dataset_info file for study, ", phsAcc, "\n", sep="") 
                  writeLog(object,  type = type, level = level, message = mesg, show = show)
              }
          })


# --------------------------- 
# Method: getMetaByObjAcc 
# --------------------------- 

#' Get metadta of a dbGaP object 
#'
#' The method returns the metadat a given dataset or variable accession. 
#'
#' @param object Commons class object.
#' @param acc a character string. The dbGaP dataset, or variable accession. 
#' @param a character string. The type of the input accession that can be either 'dataseet' or 'variable'. 
#' @return a data frame. The metadata of the input object. 
#' @export getMetaByObjAcc
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' c <- Commons()
#' df <- getMetaByObjAcc(c, acc = 'pht000824.v5', type = 'dataset')
#' # or
#' df <- getMetaByObjAcc(c, acc = 'phv00000090.v2', type = 'variable')
#'}

setGeneric(
    name = "getMetaByObjAcc",
    def = function(object, acc, type) {
        standardGeneric("getMetaByObjAcc")
    })

    #' @describeIn getMetaByObjAcc returns dataset accession
    setMethod(
        f = "getMetaByObjAcc",
        signature = c("Commons"),
        definition = function(object, acc, type) {

            prjMetaDir <- object@prjMetaDir

            ###############################################
            # Expand the search to all released studies 
            ###############################################

            # Lookup respective study_id by the dataset_id
            extAllIdInfoFile <- object@extAllIdInfoFile
            extAllIdInfoDF <- read.table(extAllIdInfoFile, header = T, fill = TRUE, quote = "", sep ='\t', stringsAsFactors = FALSE, encoding="UTF-8")  

            if (type == 'dataset') {

                # Parse ids from phtAcc
                phtAccIds = parseIdsFromPhtAcc(object, phtAcc = acc)
                phtAccId = phtAccIds$phtAccId
                phtAccNoVer = phtAccIds$phtAccNoVer

                subIdInfoDF <- dplyr::filter(extAllIdInfoDF, extAllIdInfoDF$dataset_id == phtAccId)
                matchStudyId <- unique(subIdInfoDF[['study_id']])

                matchStudyAccNoVer <- composeObjAcc(object, objId=matchStudyId, type='study')

                # Look for studyPhsId under prjMetaData Dir 
                finalPhtDF = data.frame()
                if (dir.exists(prjMetaDir)) {

                    # Get all study-version dirs in the project directory
                    allDirs <- list.dirs(path = prjMetaDir, full.names = TRUE, recursive = F)

                    # Keep only the item with study-acc match 
                    # "W://gapr_prj/metadata/phs001255"

                    X <- allDirs
                    studyDir <- X[grepl(matchStudyAccNoVer, X)]

                    studyVerDirs <- list.dirs(path = studyDir, full.names = TRUE, recursive = F)

                    # Loop through each studyVerDirs, look for the dataset acc
                    # "W://gapr_prj/metadata/phs001255/phs001255.v1"
                    for (studyVerDir in studyVerDirs) {

                        # "W://gapr_prj/metadata/phs001255/phs001255.v1/supplemental_data/phs001255.v1_study_dataset_info.txt.gz"
                        phsAcc <- basename(studyVerDir)
                        phtInfoFileName = paste0(phsAcc, "_study_dataset_info.txt.gz")
                        phtInfoFile <- file.path(studyVerDir, 'supplemental_data', phtInfoFileName)

                        if (file.exists(phtInfoFile)) {

                             if (file.size(phtInfoFile) > 0){
                                 # Read the phtInfoFile
                                 phtInfoDF <- read.table(phtInfoFile, header = T, fill = TRUE, quote = "", sep ='\t', stringsAsFactors = FALSE, encoding="UTF-8")  

                                 subDF <- dplyr::filter(phtInfoDF, phtInfoDF$dataset_accession == acc)

                                 if (nrow(subDF) == 1) {

                                     finalPhtDF = subDF 

                                     # Jump out of for loop
                                     break
                                 } 
                             }
                        }
                    }
                }

                return(finalPhtDF)
            }

            if (type == 'variable') {

                # Parse ids 
                phvAccIds <- parseIdsFromPhvAcc(object, phvAcc = 'phv00273621.v1.p1')
                phvAccId = phvAccIds$phvAccId
                phvAccNoVer = phvAccIds$phvAccNoVer

                # Get match study_id 
                subIdInfoDF <- dplyr::filter(extAllIdInfoDF, extAllIdInfoDF$variable_id == phvAccId)
                matchStudyId <- unique(subIdInfoDF[['study_id']])
                matchStudyAccNoVer <- composeObjAcc(object, objId=matchStudyId, type='study')

                # Look for studyPhsId under prjMetaData Dir 
                finalPhtDF = data.frame()
                if (dir.exists(prjMetaDir)) {

                    # Get all study-version dirs in the project directory
                    allDirs <- list.dirs(path = prjMetaDir, full.names = TRUE, recursive = F)

                    # Keep only the item with study-acc match 
                    # "W://gapr_prj/metadata/phs001255"

                    X <- allDirs
                    studyDir <- X[grepl(matchStudyAccNoVer, X)]

                    studyVerDirs <- list.dirs(path = studyDir, full.names = TRUE, recursive = F)

                    # Loop through each studyVerDirs, look for the dataset acc
                    # "W://gapr_prj/metadata/phs001255/phs001255.v1"
                    for (studyVerDir in studyVerDirs) {

                        # Get study_variable_info file of the study
                        # "W://gapr_prj/metadata/phs001255/phs001255.v1/supplemental_data/phs001255.v1_study_variable_info.txt.gz"
                        phsAcc <- basename(studyVerDir)
                        phvInfoFileName = paste0(phsAcc, "_study_variable_info.txt.gz")
                        phvInfoFile <- file.path(studyVerDir, 'supplemental_data', phvInfoFileName)

                        phvCodeValFileName = paste0(phsAcc, "_study_variable_code_value.txt.gz")
                        phvCodeValFile <- file.path(studyVerDir, 'supplemental_data', phvCodeValFileName) 

                        codeValCombo = ''
                        if (file.exists(phvCodeValFile)) {
                            varCodeValDF <- read.table(phvCodeValFile, header = T, fill = TRUE, quote = "", sep ='\t', stringsAsFactors = FALSE, encoding="UTF-8")  
                            codeValCombo <- buildVarCodeValCombo(object, catVarAcc = acc, varCodeValDF = varCodeValDF)
                        }
                        #matchVarCodeValDF <- dplyr::filter(varCodeValDF, varCodeValDF$variable_accession == catVarAcc)

                        if (file.exists(phvInfoFile)) {

                            # Read the phtInfoFile
                            phvInfoDF <- read.table(phvInfoFile, header = T, fill = TRUE, quote = "", sep ='\t', stringsAsFactors = FALSE, encoding="UTF-8")  
                            subDF <- dplyr::filter(phvInfoDF, phvInfoDF$variable_accession == acc)


                            if (nrow(subDF) == 1) {

                                # Display Info
                                varAcc <- subDF$variable_accession
                                varType <- subDF$dataset_type_category
                                studyAcc <- subDF$study_accession
                                datasetAcc <- subDF$dataset_accession
                                datasetName <- subDF$dataset_name
                                varUnit <- subDF$units
                                varName <- subDF$name
                                varDesc <- subDF$description

                                subDF['code_value_combo'] <- c(codeValCombo) 

                                finalPhtDF = subDF 

                                # Jump out of for loop
                                break
                            } 
                        }

                    }
                }

                return (finalPhtDF)
            }
        })

#######################
# List of functions
#######################

# createPrjDir
# writeLog
# copyUserFile
# writeFileInfoJson
# composeFileInfoJson
# parsePhenoFileMeta
# parsePhtHeaderFromFile
# manageArchive
# dataDicXmlParser
# parseDataDicByStudy
# mergeDatasetConsentByStudy
# viewAllStudyInfo
# viewAllDatasetInfo
# displayTextFile
# displayTable
# getDatasetMetaByStudy
# getDataDicByStudy
# getVarNameByPhvAcc
# stripColNameAcc
# convertEnumVarColName
# saveGapPlot
# isStudyAcc
# composeObjAcc
# parseIdsFromPhtAcc
# parseIdsFromStAcc
# cleanObjAcc
# checkInputPath
# filterBySubjIds
# detectVariableDataType 
# getExtData
# parseDataDic
# buildVarCodeValCombo
# getAvailPhtVer 
# getMetaByObjAcc 
