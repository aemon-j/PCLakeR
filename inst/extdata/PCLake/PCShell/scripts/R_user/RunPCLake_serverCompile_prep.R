
options(scipen = 999)
nearZero <- 1E-28

dirHome <- file.path("C:", "Users", "LilithK", "Documents", "PCModel", "PCModel - work version", "Licence_agreement", "I_accept")	# location of the PCModel1350 folder
dirShell <- file.path(dirHome, "PCModel1350", "PCModel", "3.00", "Models", "PCLake+", "6.13.16", "PCShell")
dirCpp_root <- file.path(dirHome, "PCModel1350", "PCModel", "3.00", "Frameworks", "Osiris", "3.01", "PCLake_plus")
nameWorkCase <- "PCLakePlus_serverVersion" 
fileDATM <- file.path(dirHome, "PCModel1350", "PCModel", "3.00", "Models", "PCLake+", "6.13.16", "PL613162PLUS_official.xls")
# folderTxt  <- file.path(dirHome, "PCModel1350", "PCModel", "3.00", "Models", "PCLake+", "6.13.16", "Txt")

## load all the functions
source(file.path(dirShell, "scripts", "R_system", "functions.R"))  
source(file.path(dirShell, "scripts", "R_system", "functions_PCLake_LERWQ.R")) 

## load output sets
source(file.path(dirShell, "scripts", "R_user", "output_options.R")) 

library(plyr)
library(dplyr)

## NB: you cannot use different combinations of forcings. e.g. 
##      - in model run 1: mQInEpi & mQInHyp
##      - in model run 2: mPLoad
##     If you want to do this you'll have to do for both: mQInEpi, mQInHyp AND mPLoad. 
##     This is due to the fact that the include file for the parameters (...rp.cpp file) has to be 
##     adjusted before compilation - the file will be built into the code.  
##     This adjustment entails changing parameters that are given as forcings should become "dummy". 

## Order of actions
##   1. Making folder structure for running the model
##   2. Load file 
##   < Make adjustments to the model > 
##   3. Make cpp files
##   4. Compile model
##   5. Initialize model
##   6. Run model


## 1. Making folder structure 
PCModelWorkCaseSetup(dirSHELL = dirShell,
                     dirCPP_ROOT = dirCpp_root,
                     nameWORKCASE = nameWorkCase)



## 2. Load file
lDATM_SETTINGS <- PCModelReadDATMFile_PCLakePlus(fileXLS = fileDATM,
                                                 locDATM = "excel",
                                                 locFORCING = "excel",
                                                 readAllForcings = F)


## 3. Make and adjust cpp files + update lDATM_SETTINGS according to the network information (add mSurfArea param)
PCModelAdjustCPPfiles(dirSHELL = dirShell,
                      nameWORKCASE = nameWorkCase,
                      lDATM = lDATM_SETTINGS,
                      nRUN_SET = 0)


# ## 4. Compile model
# system.time({PCModelCompileModelWorkCase(dirSHELL = dirShell,
#                                          nameWORKCASE = nameWorkCase)})

#  ## 5. Initialize model 
#  ##    - make all initial states according to the run settings
#  InitStates <- PCModelInitializeModel(lDATM = lDATM_SETTINGS,
#                                       dirSHELL = dirShell,
#                                       nameWORKCASE = nameWorkCase)
#  
#  ## 6. run one model
#  ##    - Error catching on run_state & restart (if run_state = 0 & you use restart should you be able to do so?)
#  PCModel_run01 <- PCmodelSingleRun(lDATM = lDATM_SETTINGS,
#                                    nRUN_SET = 0,
#                                    dfSTATES = InitStates,
#                                    integrator_method = "rk45ck",
#                                    dirSHELL = dirShell,
#                                    nameWORKCASE = nameWorkCase)
# 
# plot(PCModel_run01$time, PCModel_run01$oChlaEpi)
