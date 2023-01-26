

## load necessary packages 
loadPackage("deSolve") ## load this before compilation, otherwise you can get namespace errors

# ***********************************************************************
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PCModelWorkCaseSetup
#
#   Function to set up the folder structure for working with PCModel in R.
#     Folders are only made if they were not present before 
#     and files are only copied into the folders if none were present yet.
#     No files will be removed or overwritten by this function. 
#
#   Arguments are: 
#	  - dirSHELL = the directory in which the work_cases folder can be found
#   - dirCPP_ROOT = the PCLake directory that contains the latest version of the cpp files
#   - nameWORKCASE = the name of the work case folder you want to create
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ***********************************************************************

PCModelWorkCaseSetup <- function(dirSHELL, 
                                 dirCPP_ROOT,
                                 nameWORKCASE){
  
  ## debug
  # nameWORKCASE <- nameWorkCase
  # dirHOME   <- dirHome
  # dirSHELL <- file.path(dirHome, "PCModel1350", "PCModel", "3.00", "Models", "PCLake+", "6.13.16", "PCShell")
  # dirCPP_ROOT <- file.path(dirHOME, "PCModel1350", "PCModel", "3.00", "Frameworks", "Osiris", "3.01", "PCLake_plus")
  
  
  ## define directories of files
  dirWORKCASE <- file.path(dirSHELL, "work_cases", nameWORKCASE)
  
  ## if it did not exist yet: create main directory
  if(!dir.exists(dirWORKCASE)) dir.create(dirWORKCASE, recursive = T, showWarnings = T)
  
  ## if it did not exist yet: create directory for model output 
  if(!dir.exists(file.path(dirWORKCASE, "output"))) dir.create(file.path(dirWORKCASE, "output"), showWarnings = T)
  
  ## create a directory for cpp files containing model code and copy the cpp files to it
  ## cpp files = c++ code with the equations and initial settings
  ##   these are loaded in the script as the user may wish to create multiple different models (different cpp's) and compare them
  ##   In that case the user will have to compile multiple different DATM instances and save the cpp files to different folders,
  ##   the names of which can be looped through 
  if(!dir.exists(file.path(dirWORKCASE, "source_cpp"))) dir.create(file.path(dirWORKCASE, "source_cpp"), showWarnings = T)
 
  cpp_files <- list.files(dirCPP_ROOT, full.names = TRUE)[
    which((lapply(strsplit(x = list.files(dirCPP_ROOT, 
                                          full.names = TRUE), 
                           split="[/]"), 
                  function(x) which(x %in% c("pl61316ra.cpp","pl61316rc.cpp","pl61316rd.cpp","pl61316ri.cpp","pl61316rp.cpp","pl61316rs.cpp",
                                             "pl61316sa.cpp","pl61316sc.cpp","pl61316sd.cpp","pl61316si.cpp","pl61316sp.cpp","pl61316ss.cpp")))>0)==TRUE)]		
  
  if(length(list.files(file.path(dirWORKCASE, "source_cpp")))>0) print("The source_cpp folder was not empty. Therefore no changes were made to this folder.")
  if(length(list.files(file.path(dirWORKCASE, "source_cpp")))==0) file.copy(cpp_files, file.path(dirWORKCASE, "source_cpp"), overwrite = F)
  
  ## create a directory for the cpp files ['include files' of the model] to be adjusted in
  if(!dir.exists(file.path(dirWORKCASE, "source_cpp_adjusted"))) dir.create(file.path(dirWORKCASE, "source_cpp_adjusted"), showWarnings = T)
  
  ## create a directory for extra input you might want to use in your model
  ## think of time series etc. This folder will not be cleared if it already existed. I do not want to accidentally remove raw data or some such. 
  if(!dir.exists(file.path(dirWORKCASE, "input"))) dir.create(file.path(dirWORKCASE, "input"), showWarnings = T)
  
  ## create a directory for the model code, this folder will be used for compilation
  ## copy template code in here
  if(!dir.exists(file.path(dirWORKCASE, "model_code"))) dir.create(file.path(dirWORKCASE, "model_code"), showWarnings = T)
  copy_model_files <- list.files(file.path(dirSHELL, "scripts", "cpp2R"), full.names = TRUE)[
    which((lapply(strsplit(x = list.files(file.path(dirSHELL, "scripts", "cpp2R"), full.names = TRUE), split="[/]"), 
                  function(x) which(x %in% c("compile_model_cpp.cmd", "model_base.cpp")))>0)==TRUE)]		
  
  if(length(list.files(file.path(dirWORKCASE, "model_code")))>0) print("The model_code folder was not empty. Therefore no changes were made to this folder.")
  if(length(list.files(file.path(dirWORKCASE, "model_code")))==0)  file.copy(copy_model_files, file.path(dirWORKCASE, "model_code"), overwrite = F)
  
  print(paste0("Work case ", nameWORKCASE, " is ready for use."))
  
}




# ***********************************************************************
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PCModelReadDATMFile_PCLakePlus
#
#   Function to read run settings, states, parameters, auxiliary data, and forcings from PCLake files.
#     You can load the model code either from excel or from txt files. You can also load the forcings
#     either from excel or from txt files. You can select the source of the files via the arguments locDATM (model code)
#     and locFORCING (forcings). The location of the files / folder should be given via the arguments fileXLS or folderTXT.
#     
#   All column names from the excel sheets are hardcoded. 
#
#   Required: 
#     - R packages readxl and tidyr 
#
#    Arguments are:
#	   - fileXLS: the directory + filename + xls file extention of the PCLake excel file model you want to load
#	   - folderTXT: the directory of the PCLake txt files you want to load
#    - locDATM: either excel or txt
#    - locFORCING: either excel or txt 
#    - readAllForcings: switch to read all forcings from the file, regardless of them being switched on or not
#	
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ***********************************************************************


PCModelReadDATMFile_PCLakePlus <- function(fileXLS = NULL,
                                           folderTXT = NULL, 
                                           locDATM = c("excel", "txt"),
                                           locFORCING = c("excel", "txt"),
                                           readAllForcings = F){
  
  ## debug
  # dirHOME = dirHome
  # fileDATM = "PL613162PLUS_prepPackage.xls"
  # fileXLS = file.path(dirHOME, "PCModel1350", "PCModel", "3.00", "Models", "PCLake+", "6.13.16", fileDATM)
  # folderTXT = file.path(dirHOME, "PCModel1350", "PCModel", "3.00", "Models", "PCLake+", "6.13.16", "Txt")
  # locDATM = "txt"
  # locFORCING = "txt"
  
  if(is.null(fileXLS) & is.null(folderTXT) == TRUE){stop("Please enter either excel filename into the fileXls argument or the location of the txt folder into the folderTxt argument.")}
  ## NB: Would be good to add the error catching for locDATM / locFORCING and path entries. 
  
  ## Loading (and if missing, installing) packages 
  loadPackage("readxl")
  loadPackage("tidyr")

  ## Define path to DATM file (excel) and txt files
  # pathTxt  <- file.path(dirHOME, "PCModel1350", "PCModel", "3.00", "Models", "PCLake+", "6.13.16", "Txt")
  if(!is.null(fileXLS)){pathXls <- file.path(fileXLS)}else{pathXls <- ""}
  if(!is.null(folderTXT)){pathTxt <- file.path(folderTXT)}else{pathTxt <- ""}
  
  ifelse(locFORCING == "txt", pathForc <- pathTxt, pathForc <- pathXls)

  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Load sheets from DATM (excel) file 
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## Run settings ----
  
  if(locDATM == "txt"){
    dfRUN_SETTINGS_PREP <- read.table(file.path(pathTxt, "Control.txt"), skip = 29, fill = T)
    dfRUN_SETTINGS <- dfRUN_SETTINGS_PREP[-c(unique(which(is.na(dfRUN_SETTINGS_PREP), arr.ind = T)[,1])), ]
  }
  
  if(locDATM != "txt"){
    dfRUN_SETTINGS_PREP <- as.data.frame(read_excel(pathXls, sheet = "Control", skip = 29, col_names = T))
    rownames(dfRUN_SETTINGS_PREP) <- as.character(dfRUN_SETTINGS_PREP[, 1])
    dfRUN_SETTINGS <- dfRUN_SETTINGS_PREP[-c(unique(which(is.na(dfRUN_SETTINGS_PREP), arr.ind = T)[,1])), -1]
  }
  
  for(nCOL in 1:ncol(dfRUN_SETTINGS)){ dfRUN_SETTINGS[, nCOL]	<-	as.numeric(gsub(",", ".", as.character(unlist(dfRUN_SETTINGS[, nCOL])))) }
  
  
  ## Initial states ----
  if(locDATM == "txt"){
    dfSTATES_PRICE_RAW <- read.table(file.path(pathTxt, "states.txt"), sep = "\t", header = T)
  }
  if(locDATM != "txt"){
    dfSTATES_PRICE_RAW <- as.data.frame(read_excel(pathXls, sheet = "states"))
  }
  dfSTATES <- dfSTATES_PRICE_RAW[which(dfSTATES_PRICE_RAW[, 2]!=""), 
                                 which(colnames(dfSTATES_PRICE_RAW) %in% c("sStateName",
                                                                           "sInitialStateName",
                                                                           "iReportState",
                                                                           "sDefaultSetTurbid0",
                                                                           "sDefaultSetClear1",
                                                                           "sAltenativeSet2",
                                                                           "sTurbidSet15mDeep"))]  
  rownames(dfSTATES) <- as.character(unlist(dfSTATES[which(colnames(dfSTATES)=="sStateName")]))
  dfSTATES <- dfSTATES[, -which(colnames(dfSTATES)=="sStateName")]
  rownames(dfSTATES) <- gsub("_", "", rownames(dfSTATES))
  dfSTATES[which(colnames(dfSTATES)=="sInitialStateName")] <- gsub("_", "", as.character(unlist(dfSTATES[which(colnames(dfSTATES)=="sInitialStateName")])))
  for(nCOL in which(colnames(dfSTATES) %in% c("iReportState", 
                                              "sDefaultSetTurbid0",
                                              "sDefaultSetClear1",
                                              "sAltenativeSet2",
                                              "sTurbidSet15mDeep"))){ 
    dfSTATES[, nCOL] <-	as.numeric(as.character(unlist(dfSTATES[,nCOL])))
    }
  
  
  ## Parameters ----
  if(locDATM == "txt"){
    dfPARAMS_PRICE_RAW <- read.table(file.path(pathTxt, "parameters.txt"), sep = "\t", header = T)
  }
  if(locDATM != "txt"){
    dfPARAMS_PRICE_RAW <- as.data.frame(read_excel(pathXls, sheet = "parameters"))
  }
  dfPARAMS <- dfPARAMS_PRICE_RAW[which(dfPARAMS_PRICE_RAW[,2]!=""), 
                                 which(colnames(dfPARAMS_PRICE_RAW) %in% c("sName",
                                                                           "iReport",
                                                                           "sMinValue",
                                                                           "sMaxValue",
                                                                           "sDefault0",
                                                                           "sSet1",
                                                                           "sSet2",
                                                                           "sSet3"))] 
  rownames(dfPARAMS) <-	as.character(unlist(dfPARAMS[which(colnames(dfPARAMS)=="sName")]))
  dfPARAMS <- dfPARAMS[, -which(colnames(dfPARAMS)=="sName")]
  rownames(dfPARAMS) <-	gsub("_", "", rownames(dfPARAMS))
  dfINTERMEDIATE <- dfPARAMS ## preparation for if you'd want to use the .txt names for loading the params
  ## warnings are suppressed for the following action, because they should occur in case of a text file, and the ensuing behavior is always wanted
  for(nCOL in 1:ncol(dfPARAMS)){ dfPARAMS[, nCOL]	<- suppressWarnings(as.numeric(as.character(unlist(dfPARAMS[,nCOL])))) } #!# Here all parameter values are put to numeric. If txt was written in, this will become NA. For forcings see "Forcing" section.
  
  
  ## Forcings ----
  
  ## Get all the reads 
  if(readAllForcings == T){
    readOn <- which(is.na(dfINTERMEDIATE[grep("Read|InclStrat|InitMixDepth|calcMixDepth", rownames(dfINTERMEDIATE), value = F), ]) == FALSE, arr.ind = T)
    }else{
      readOn <- which(dfINTERMEDIATE[grep("Read|InclStrat|InitMixDepth|calcMixDepth", rownames(dfINTERMEDIATE), value = F), ] == 1, arr.ind = T)
  }
     
  if(length(unique(table(readOn[,2]))) != 1){ warning(paste0("You are trying to load an excelfile that has different forcings switched on per run scenario.",
                                                             " Please be aware of the fact that the compiled R model will expect all runs to have the same combination of forcings switched on.",
                                                             " The forcing values themselves may differ, but the ReadForcing values should be equal."))}
  realNamesOfForcings <- matchSwitchToForcing(unique(rownames(readOn)))
  dfINTERMEDIATE_sel <- dfINTERMEDIATE[which(rownames(dfINTERMEDIATE) %in% realNamesOfForcings),]
  for(nCOL in 1:ncol(dfINTERMEDIATE_sel)){ dfINTERMEDIATE_sel[, nCOL]	<- as.character(unlist(dfINTERMEDIATE_sel[,nCOL])) } 
  dfINTERMEDIATE_sel$param_names <- rownames(dfINTERMEDIATE_sel)
  dfINTERMEDIATE_sel$iReport <- NULL
  dfINTERMEDIATE_sel$sMinValue <- NULL
  dfINTERMEDIATE_sel$sMaxValue <- NULL
  dfINTERMEDIATE_sel2 <- dfINTERMEDIATE_sel[, c(length(colnames(dfINTERMEDIATE_sel)), 1:length(colnames(dfINTERMEDIATE_sel))-1)] 
  dfINTERMEDIATE_long <- pivot_longer(dfINTERMEDIATE_sel2, cols = c(2:length(colnames(dfINTERMEDIATE_sel2))), names_to = "run_name") 
  #dfINTERMEDIATE_long2 <- dfINTERMEDIATE_long[grep("txt", dfINTERMEDIATE_long$value, value = F),]
  dfINTERMEDIATE_long$value <- gsub("txt\\/|\\.txt", "", dfINTERMEDIATE_long$value)
  lsINTERMEDIATE <- split(as.data.frame(dfINTERMEDIATE_long), dfINTERMEDIATE_long$run_name, drop = T)
 
  dfMATCH_RUN_PARAM <- data.frame(run_set = c(0, 1, 2, 3),
                                  param_set = c("sDefault0", "sSet1", "sSet2", "sSet3"))
  
  lsFORCINGS <- list()
  ## load all defined forcings
  for(i in names(lsINTERMEDIATE)) {
    
    ## debug
    # i <- names(lsINTERMEDIATE)[1]

    ## get the real amount of timesteps for the forcings so they can be interpolated to their full extent
    runtime_years <- dfRUN_SETTINGS["dReady", 
                                    which(dfRUN_SETTINGS["iRuniD",] == dfMATCH_RUN_PARAM[dfMATCH_RUN_PARAM$param_set == i, "run_set"])]
    times_forcing  <- seq(0, 365*runtime_years)
    
    
    ## make a placeholder list for the forcings
    lsFORCINGS_prep <- list()
    lsFORCINGS_prep[[i]] <- vector("list", length(lsINTERMEDIATE[[i]]$value))
    names(lsFORCINGS_prep[[i]]) <- lsINTERMEDIATE[[i]]$param_names
    
    
    ## use function to load all forcings and interpolate them as well
    lsFORCINGS_prep[[i]] <- sapply(names(lsFORCINGS_prep[[i]][which(names(lsFORCINGS_prep[[i]])!="time")]), 
                                   getForcingAndInterpolate, 
                                   location = locFORCING, 
                                   pathLoc = pathForc,
                                   metadata = lsINTERMEDIATE[[i]],
                                   timesteps = times_forcing,
                                   simplify = F)
    
    lsFORCINGS[[i]] <- c(list(time = data.frame(time = times_forcing, value = times_forcing)), lsFORCINGS_prep[[i]])
  }
  

  ## Auxiliaries (output) ---- 
  if(locDATM == "txt"){
    dfAUXIL_PRICE_RAW <- read.table(file.path(pathTxt, "derivatives.txt"), sep = "\t", header = T)
  }
  if(locDATM != "txt"){
    dfAUXIL_PRICE_RAW <- as.data.frame(read_excel(pathXls, sheet = "derivatives"))
  }
  
  dfAUXIL <- dfAUXIL_PRICE_RAW[which(dfAUXIL_PRICE_RAW[, 2]!=""), which(colnames(dfAUXIL_PRICE_RAW) %in% c("sName",
                                                                                                           "iReport"))]
  rownames(dfAUXIL) <- as.character(unlist(dfAUXIL[which(colnames(dfAUXIL)=="sName")]))
  dfAUXIL <- dfAUXIL[, -which(colnames(dfAUXIL)=="sName"), drop = FALSE]
  rownames(dfAUXIL)	<- gsub("_", "", rownames(dfAUXIL))
  for(nCOL in 1:ncol(dfAUXIL)){ dfAUXIL[, nCOL] <- as.numeric(as.character(unlist(dfAUXIL[, nCOL]))) }
  
  
  ## Parameter adjustment ----
  ## Adjust parameter values so there are no NA values present in the df anymore
  ## This is necessary for the initialization of the states in PCModelAdjustCPPfile() 
  dfPARAMS_na_to_fill <- which(is.na(dfPARAMS), arr.ind = T) ## get the location of all NA values
  if(length(dfPARAMS_na_to_fill>0)){
    ## put to -99999 the ones that will not be used (the ones that are not part of the forcings)
    params_na_to_make_min99999 <- setdiff(unique(row.names(dfPARAMS_na_to_fill)), 
                                          names(lsFORCINGS[[1]])[which(!names(lsFORCINGS[[1]])%in% "time")])
    rows_to_put_to_zero <- which(rownames(dfPARAMS) %in% params_na_to_make_min99999)
    for(single_row in rows_to_put_to_zero){dfPARAMS[single_row, which(is.na(dfPARAMS[single_row, ]))] <- -99999}
    
    ## put the first value of the forcing in the NA slots that have forcings
    dfPARAMS_na_for_forc <- which(is.na(dfPARAMS), arr.ind = T)
    for(rownr in 1:nrow(dfPARAMS_na_for_forc)){
      ## rownr <- 1
      row_to_use <- dfPARAMS_na_for_forc[rownr, 1]
      col_to_use <- dfPARAMS_na_for_forc[rownr, 2]
      dfPARAMS[row_to_use, col_to_use] <- lsFORCINGS[[colnames(dfPARAMS)[col_to_use]]][[rownames(dfPARAMS)[row_to_use]]]$value[1]
    }
  }
  
  ## return 
  return(list(run_settings = dfRUN_SETTINGS, 
              states = dfSTATES,
              params = dfPARAMS,
              forcings = lsFORCINGS,
              auxils = dfAUXIL))
  
}



## ****************************************************************************************************************************************************
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Edit c++ files from PCLake/PCDitch (created by OSIRIS)
##
## Two parts are edited: 
##    - include files 
##    - the template for the model
##
## The include files are adjusted according to parameter settings, forcings and initial conditions
## The template model file is adjusted according to forcings, auxiliary output and model version name
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
## ****************************************************************************************************************************************************

PCModelAdjustCPPfiles <- function(dirSHELL,
                                  nameWORKCASE, 
                                  lDATM = NULL, 
                                  dfRUN_SETTINGS = lDATM$run_settings,
                                  nRUN_SET = c(0, 1, 2, 3), 
                                  dfPARAM = lDATM$params, 
                                  dfSTATES = lDATM$states,
                                  lFORCINGS = lDATM$forcings,
                                  dfAUXIL = lDATM$auxils){ 
  
  ## debug
  # dirSHELL <- dirShell
  # nameWORKCASE <- nameWorkCase
  # dfRUN_SETTINGS <- lDATM_SETTINGS$run_settings
  # nRUN_SET <- 0
  # dfPARAM <- lDATM_SETTINGS$params
  # dfSTATES <- lDATM_SETTINGS$states
  # lFORCINGS <- lDATM_SETTINGS$forcings
  # dfAUXIL <- lDATM_SETTINGS$auxils
  
  ## error catching
  if(length(nRUN_SET) > 1 | any(!(nRUN_SET %in% c(0, 1, 2, 3))) == TRUE){stop("Please enter one value between 0 and 3 for nRUN_SET")}
 
  
  ## define directories of files
  dirWORKCASE  =	file.path(dirSHELL, "work_cases", nameWORKCASE)     # location of work case
  dirCPP        = file.path(dirWORKCASE, "source_cpp")                    # location of cpp files
  dir_CPP_adj   = file.path(dirWORKCASE, "source_cpp_adjusted")           # location of output cpp files
  dir_MODEL_adj = file.path(dirWORKCASE, "model_code")                    # location of output model cpp file
  
  ## Get the right sets of data, based on the run
  # set_state <- dfRUN_SETTINGS["iStateSet", which(as.character(colnames(dfRUN_SETTINGS))==as.character(nRUN_SET))]
  set_state <- dfRUN_SETTINGS["iStateSet", colnames(dfRUN_SETTINGS)[which(dfRUN_SETTINGS["iRuniD",] == as.character(nRUN_SET))]]
  # set_param <- dfRUN_SETTINGS["iParamSet", which(as.character(colnames(dfRUN_SETTINGS))==as.character(nRUN_SET))]
  set_param <- dfRUN_SETTINGS["iParamSet", colnames(dfRUN_SETTINGS)[which(dfRUN_SETTINGS["iRuniD",] == as.character(nRUN_SET))]]
  set_forc  <- names(lFORCINGS)[which(colnames(dfPARAM)[4+set_param] == names(lFORCINGS))]
 
  
  ## Load forcings
  vFORCING_NAMES <- names(lFORCINGS[[set_forc]])
  ## note that we remove the time forcing, as it will lead to double definitions
  if('time' %in% vFORCING_NAMES){ vFORCING_NAMES	= vFORCING_NAMES[-which(vFORCING_NAMES=='time')]}
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## PART A: adjust CPP files that work as include files to the model.cpp
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  ## edit c++ files from PCLake/PCDitch (created by OSIRIS)
  ## 1. set parameters: 
  ##       - user-defined parameters
  ##       - forcing function parameters (switches, e.g. ReadTemp)
  ## 2. set (user-defined) initial conditions 
  ## 3. edit declaration files:
  ##       - remove forcing function parameters (e.g. mTemp) from declaration list to prevent double declarations (as both a parameter and a time series)
  ## 4. determine the length of the declaration arrays and store them
  ## 5. write all files: 
  ##       - underscores will be removed
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
  
  ## get names of cpp files to be adjusted
  cpp_files <- list.files(dirCPP, pattern = ".cpp")
  
  ## placeholder for information about the declaration files. This information is used in the model.cpp!
  arrays <- vector()
  
  for(cpp_file in cpp_files){
    
    ## cpp_file <- cpp_files[5]
    
    tmp_cpp_file <- readLines(file.path(dirCPP, cpp_file))
    
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## 1. Set parameters
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    if (grepl("sp", cpp_file)) {
      
      all_param <- dfPARAM[, 4 + set_param] ## get all model parameters
      names(all_param) <- row.names(dfPARAM) ## make them into a names vector so setValues() understands the format
      
      tmp_cpp_file <- setValues(tmp_cpp_file, all_param)  ## set user-defined parameters
      
      rm(list = c("all_param")) ## clean up
    }
    
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## 2. Set initial conditions 
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    if (grepl("sc", cpp_file)) { 
      
      all_inits <- dfSTATES[, 3 + set_state] ## get all model states
      names(all_inits) <- dfSTATES[, "sInitialStateName"] ## make them into a names vector so setValues() understands the format
      
      tmp_cpp_file <- setValues(tmp_cpp_file, all_inits) ## set user-defined initial conditions
      
      rm(list = c("all_inits")) ## clean up
    } 
    
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## 3. Remove forcing function parameters (e.g. mTemp) from parameter declaration list
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    if (grepl("rp", cpp_file)) { 
      
      ## Extra action: write a rp2.cpp file with all the parameters in there. 
      ## This file will be used in the initialization. 
      tmp_cpp_file2 <- tmp_cpp_file
      tmp_cpp_file2 <- gsub("_","", tmp_cpp_file2) ## remove underscores
      writeLines(tmp_cpp_file2, file.path(dir_CPP_adj, gsub("rp.cpp", "rp2.cpp", cpp_file))) ## write adjusted information to new file
      
      ## regular script
      i <- 0
      for (name in vFORCING_NAMES) {
        i   <- i + 1
        tmp_cpp_file <- gsub(paste("_", name, "_", sep = ""), paste("_dummy", i, "_", sep = ""), tmp_cpp_file) 
      }
    }
    
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## 4. Determine the length of the declaration arrays and store them
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    if ((grepl("ra", cpp_file) || grepl("rp", cpp_file) || grepl("rs", cpp_file) || grepl("ri", cpp_file))) {  
      
      array_name <- substring(tmp_cpp_file[1], regexpr("=", tmp_cpp_file[1])[1]+2, regexpr("\\[", tmp_cpp_file[1])[1]-1)
      array_length <- strsplit(tmp_cpp_file[length(tmp_cpp_file)]," ")[[1]][3]
      arrays <- c(arrays, paste("static double ", array_name, "[", array_length, "];", sep=""))
    }
    

    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## 5. Write output
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    ## remove underscores for easy R interpretation & write files
    tmp_cpp_file <- gsub("_", "", tmp_cpp_file) ## remove underscores
    writeLines(tmp_cpp_file, file.path(dir_CPP_adj, cpp_file)) ## write adjusted information to new file
  }
  
  writeLines(arrays, file.path(dir_CPP_adj, "arrays.cpp")) ## write length of declaration arrays to arrays.cpp file
  
  ## END of PART A
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## PART B: Adjust the model.cpp file
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## edit c++ model (scripts/cpp2R/model_base.cpp) for compilation:
  ## 1. define output auxiliaries
  ## 2. define forcing functions 
  ## 3. refer to the right model version name
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  model_base_cpp <- readLines(file.path(dir_MODEL_adj, "model_base.cpp")) # read the c++ model
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 1. set output for auxiliaries
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  id             <- grep(x = model_base_cpp, pattern = "output_auxiliaries")
  codelines      <- vector()
  aux_names      <- rownames(dfAUXIL[which(dfAUXIL[, 1] == 1), , drop = F])
  aux_number     <- length(aux_names)
  i              <- 0
  if (length(aux_names)>0) {
    for (aux_name in aux_names) { # define user-defined output auxiliaries as output_auxiliaries
      codelines <- c(codelines,paste("  yout[",i,"] = ", aux_name, ";", sep = "")) 
      i <- i + 1
    }
  } else { # if there are no output auxiliaries; make at least one 'dummy' output auxiliary, as desired by DeSolve
    codelines   <- "  yout[0]=0;"
    aux_number  <- 1
    aux_names   <- "dummy"
    aux_units   <- "-"
  }
  
  model_cpp <- c(model_base_cpp[1:(id-1)], codelines, model_base_cpp[(id+1):length(model_base_cpp)])
  
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 2. set forcing functions 
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  id        <- grep(x = model_cpp, pattern = "input_forcings")
  codelines <- paste("static double forc[", (1+length(vFORCING_NAMES)), "];", sep="")
  codelines <- c(codelines, "double &time = forc[0];") # define time as an external forcing
  i         <- 0
  for (name in vFORCING_NAMES) { # define user-defined forcings as external forcings
    i         <- i + 1
    codelines <- c(codelines, paste("double &", name, " = forc[",i,"];", sep=""))
  }
  codelines <- c(codelines, paste("#define MAXFORC ", (1+length(vFORCING_NAMES)), sep=""))
  model_cpp <- c(model_cpp[1:(id-1)], codelines, model_cpp[(id+1):length(model_cpp)])
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 3. refer to the right model version
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  cpp_files     <- list.files(file.path(dirWORKCASE, "source_cpp"), pattern = ".cpp") ## this is only to get the model version name!
  stop_id       <- regexpr(pattern = "...cpp", cpp_files[1])[[1]]-1
  model_version <- substr(cpp_files[1], start = 1, stop = stop_id) #get model version
  model_cpp     <- sub(pattern = "model_version", replacement = model_version, x = model_cpp) # insert model version into c++ file
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 4. write to file
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # writeLines(model_cpp, file.path(dir_SCHIL,"scripts", "cpp2R", "model.cpp")
  writeLines(model_cpp, file.path(dir_MODEL_adj, "model.cpp"))
  
}


## ****************************************************************************************************************************************************
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## Compilation of the model
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
## ****************************************************************************************************************************************************

PCModelCompileModelWorkCase <- function(dirSHELL, nameWORKCASE) { 
  
  ## define directory of model.cpp file
  dir_MODEL_adj <-	file.path(dirSHELL, "work_cases", nameWORKCASE, "model_code") 

  setwd(dir_MODEL_adj)
  # system("compile_model_cpp.cmd", show.output.on.console = T, invisible = FALSE) ## testing 
  file.remove("model.o", "model.dll")
  # file.remove("model.dll")
  # system("R CMD SHLIB model.cpp") 
  out <- system("R CMD SHLIB model.cpp", intern=TRUE)
  nISERROR <- grep("Error", out[length(out)])
  return(ifelse(length(nISERROR)==0, "Model compiled successfully", "Model compile error"))
  
  # system(paste("R --arch x64 CMD SHLIB ", dir_SCHIL,"scripts/cpp2R/","model.cpp",sep=""))
  ## NB. can throw error on architecture x64
  # system("R --arch x64 CMD SHLIB model.cpp") 
}


## ****************************************************************************************************************************************************
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## Initialization of the model
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
## ****************************************************************************************************************************************************

PCModelInitializeModel <- function(lDATM = NULL, 
                                   dfRUN_SETTINGS = lDATM$run_settings,
                                   dfSTATES = lDATM$states,
                                   dfPARAMS = lDATM$params, 
                                   dirSHELL,
                                   nameWORKCASE) {
  
  ## due to the way the function is written, dfSTATES/dfPARAMS will be made either based on lDATM (lDATM is given & dfSTATES is not, which will then default to lDATM$states)
  ## or on its own argument (when one is given) 
  ## using lDATM is the simple way to get to dfSTATES and dfPARAMS, lDATM itself is not used beyond the df's for states and params
  
  ## debug
  # dfRUN_SETTINGS <- lDATM_SETTINGS$run_settings
  # dfSTATES <- lDATM_SETTINGS$states
  # dfPARAMS <- lDATM_SETTINGS$params
  # dirSHELL <- dirShell
  # nameWORKCASE <- nameWorkCase
  
  ## error catching
  if(is.null(lDATM)==TRUE){
    if(is.null(dfRUN_SETTINGS)==TRUE){stop("Please add a data frame with states to the dfRUN_SETTINGS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(dfSTATES)==TRUE){stop("Please add a data frame with states to the dfSTATES argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(dfPARAMS)==TRUE){stop("Please add a data frame with states to the dfPARAMS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
  }
  
  ## define directories of files
  dir_CPP =	file.path(dirSHELL, "work_cases", nameWORKCASE, "source_cpp")  
  dir_CPP_adj =	file.path(dirSHELL, "work_cases", nameWORKCASE, "source_cpp_adjusted")  
  dir_DLL =	file.path(dirSHELL, "work_cases", nameWORKCASE, "model_code")     
  
  ## prepare the dataframe to connect the output to
  dfSTATES_RESULTS <- as.data.frame(dfSTATES[, which(colnames(dfSTATES) %in% c('iReportState','sInitialStateName'))])
  
  ## load initial states calculation
  dyn.load(file.path(dir_DLL, "model.dll"))
  ini <- function(parm, y, nr_of_states){.C("InitializeModel", param = parm, initState = y, state = double(nr_of_states))}
  
  ## loop over all runs in the dfRUN_SETTINGS
  for(i in colnames(dfRUN_SETTINGS)){
    
    # i <- colnames(dfRUN_SETTINGS)[1]
    state_set <- dfRUN_SETTINGS["iStateSet", i]
    param_set <- dfRUN_SETTINGS["iParamSet", i]
    
    sel_states <- dfSTATES[, state_set + 3]
    names(sel_states)	=	dfSTATES$sInitialStateName
    
    sel_params <- dfPARAMS[, param_set + 4]
    names(sel_params) <- row.names(dfPARAMS)
    # sel_params[which(is.na(sel_params)==T)] <- 0
    
    ## calculate initial states
    calc_inits <- ini(sel_params, sel_states, nrow(dfSTATES))
    calc_inits_sel <- calc_inits$state ## get initial values of state variables
    names(calc_inits_sel) <- row.names(dfSTATES) ## combine name and value
    
    ## plak alles aan elkaar vast: 
    dfSTATES_RESULTS <- cbind.data.frame(dfSTATES_RESULTS, new_col = calc_inits_sel)
    colnames(dfSTATES_RESULTS)[which(colnames(dfSTATES_RESULTS) == "new_col")] <- paste0("runSettings_", i)
    
  }
  
  dyn.unload(file.path(dir_DLL, "model.dll")) ## decouple model.dll
  
  return(dfSTATES_RESULTS)
}

## ****************************************************************************************************************************************************
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## Run Single Model (one node - regular PCLake+ use)
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
## ****************************************************************************************************************************************************

## Run a single instance of PCLake
PCmodelSingleRun <- function(lDATM = NULL,
                             dfRUN_SETTINGS = lDATM$run_settings,
                             nRUN_SET,
                             dfSTATES = lDATM$states, 
                             dfPARAMS = lDATM$params, 
                             dfAUXIL = lDATM$auxils, 
                             lFORCINGS = lDATM$forcings,
                             integrator_method,
                             dirSHELL,
                             nameWORKCASE,
                             tAVERAGE = FALSE){
  
  ## debug
  # lDATM          <- lDATM_SETTINGS     ## list with all PCmodel settings
  # dfRUN_SETTINGS  <- lDATM$run_settings ## default df with PCmodel control settings
  # nRUN_SET       <- 0                  ## the run set which will be used to run the model
  # dfSTATES       <- InitStates         ## default df with PCmodel states;
  # dfPARAMS       <- lDATM$params       ## default df with PCmodel parameters;
  # dfAUXIL        <- lDATM$auxils       ## default df with PCmodel auxiliaries
  # lFORCINGS      <- lDATM$forcings     ## default df with PCmodel forcings
  # integrator_method <- "vode"          ## selected integrator method
  # dirSHELL <- dirShell
  # nameWORKCASE   <- nameWorkCase
  # tAVERAGE       <- FALSE             ## use averaging on the output dataframe; full dataframe will be reduced to only one line. Uses the start and end day of the control settings.
  
  ## check if the input data is present
  if(is.null(lDATM)==TRUE){
    if(is.null(dfSTATES)==TRUE){stop("Please add a data frame with states to the dfSTATES argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(dfPARAMS)==TRUE){stop("Please add a data frame with parameters to the dfPARAMS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(dfAUXIL)==TRUE){stop("Please add a data frame with auxilaries to the dfAUXILS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(lFORCINGS)==TRUE){stop("Please add a list of matrices with forcings (one for each forcing parameter) to the dfFORCINGS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
    if(is.null(dfRUN_SETTINGS)==TRUE){stop("Please add a data frame with run settings to the dfRUN_SETTINGS argument or supply your DATM list file (from ReadDATM function) in lDATM")}
  }
  
  ## define the model dll location
  dirMODEL <- file.path(dirSHELL, "work_cases", nameWORKCASE, "model_code")
  
  
  ##== Get run data ====
  run_set     <- which(dfRUN_SETTINGS["iRuniD",] == as.character(nRUN_SET))
  state_set   <- dfRUN_SETTINGS["iStateSet", colnames(dfRUN_SETTINGS)[run_set]]
  param_set   <- dfRUN_SETTINGS["iParamSet", colnames(dfRUN_SETTINGS)[run_set]]
  forcing_set <- which(names(lFORCINGS) == colnames(dfPARAMS)[param_set+4])

  sel_params <- dfPARAMS[, param_set + 4]
  names(sel_params) <- rownames(dfPARAMS)
  
  sel_inistates	<- dfSTATES[, state_set + 3]
  names(sel_inistates) <- rownames(dfSTATES)  
  
  sel_forcings <- lFORCINGS[[forcing_set]]
  
  
  
  ##== Run settings (control panel) ==== 
  
  ## define time over which results will be reported
  fREP_START_YEAR <- dfRUN_SETTINGS["dRepStart", colnames(dfRUN_SETTINGS)[run_set]] 
  
  ## Define time over which results will be averaged for e.g. bifurcation analysis 
  ## Generally refers to a summer growing season period of e.g. day 150-210 (standard setting PCLake) or day 91-259 (the summer half of the year, 1 April to 30 Sept)
  fAVG_START_YEAR <- dfRUN_SETTINGS["dAvgStart", colnames(dfRUN_SETTINGS)[run_set]]
  fAVG_START_DAY	<- dfRUN_SETTINGS["dAvgStartWithinYear", colnames(dfRUN_SETTINGS)[run_set]]
  fAVG_END_DAY    <- dfRUN_SETTINGS["dAvgEndWithinYear", colnames(dfRUN_SETTINGS)[run_set]]
  
  ## timestep at which derivatives are calculated (if integrator uses fixed time step)
  internal_time_step <- dfRUN_SETTINGS["dIntStep", colnames(dfRUN_SETTINGS)[run_set]]    
  runtime_years      <- dfRUN_SETTINGS["dReady", colnames(dfRUN_SETTINGS)[run_set]] # model run time (in years)
  output_time_step   <- dfRUN_SETTINGS["dRepStep", colnames(dfRUN_SETTINGS)[run_set]] # time step at which output is generated (in days)
  times              <- seq(0, 365*runtime_years, by = output_time_step) # output time step, in dagen
  
  ##== Output settings ====
  state_names <- rownames(dfSTATES[which(dfSTATES$iReportState == 1),, drop = F])
  aux_names 	<- rownames(dfAUXIL[which(dfAUXIL$iReport == 1),, drop = F])
  

  ## == Run the model ====
  int        <- integrator_method
  if(!int %in% c("rk2","rk23","rk23bs","rk34f","rk45f","rk45ck",
                 "rk45e","rk45dp6","rk45dp7","rk78dp","rk78f",
                 "lsoda","lsode","lsodes","lsodar","vode","daspk",
                 "ode23","ode45", "radau","bdf","bdf_d","adams", 
                 "impAdams","impAdams_d","iteration", "euler","rk4")){
    stop("Integrator name does not exist. Please check spelling.")
  }
  
  error      <- class(tryCatch(output <- as.data.frame(RunModel(sel_inistates, 
                                                                times, 
                                                                sel_params, 
                                                                sel_forcings, 
                                                                length(aux_names), 
                                                                aux_names, 
                                                                int, 
                                                                state_names, 
                                                                internal_time_step, 
                                                                dirMODEL)), error = function(e) e))[1] == "simpleError"
  if(any(is.na(output)) | error | nrow(output)<max(times)) {  # run the model again when integrator "vode" returns negative or NA outputs, rerun with integrator "daspk"
    int        <- "ode45"
    error      <- class(tryCatch(output <- as.data.frame(RunModel(sel_inistates, 
                                                                  times,
                                                                  sel_params,
                                                                  sel_forcings,
                                                                  length(aux_names), 
                                                                  aux_names,
                                                                  int,
                                                                  state_names,
                                                                  internal_time_step,
                                                                  dirMODEL)), error = function(e) e))[1] == "simpleError"
    if(any(is.na(output)) | error| nrow(output)<max(times)) { # run the model again when integrator "daspk" returns negative or NA outputs, rerun with integrator "euler"
      int        <- "euler"
      error      <- class(tryCatch(output <- as.data.frame(RunModel(sel_inistates,
                                                                    times,
                                                                    sel_params,
                                                                    sel_forcings,
                                                                    length(aux_names),
                                                                    aux_names,
                                                                    int,
                                                                    state_names,
                                                                    0.003,
                                                                    dirMODEL)),error = function(e) e))[1] == "simpleError"
      if(any(is.na(output)) | error| nrow(output)<max(times)) { # run the model again when integrator "euler" returns negative or NA outputs, rerun with integrator "euler" with timestep 0.001
        error      <- class(tryCatch(output <- as.data.frame(RunModel(sel_inistates,
                                                                      times,
                                                                      sel_params,
                                                                      sel_forcings,
                                                                      length(aux_names),
                                                                      aux_names,
                                                                      int,
                                                                      state_names,
                                                                      0.001,
                                                                      dirMODEL)), error = function(e) e))[1] == "simpleError"
      }
    }
  }
  
  output <- as.data.frame(subset(output, subset = (time %in% c((fREP_START_YEAR*365):max(times)))))							
  
  if(tAVERAGE == FALSE){					
    return(output)
  }else{
    return(output_avg <- as.data.frame(t(colMeans(subset(output, 
                                                         subset = (time %in% c((fAVG_START_YEAR*365+fAVG_START_DAY):(fAVG_START_YEAR*365+fAVG_END_DAY))))))))
  }
  
}



