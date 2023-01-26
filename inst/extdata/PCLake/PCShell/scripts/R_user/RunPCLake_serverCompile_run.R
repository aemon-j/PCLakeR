options(scipen = 999)
nearZero <- 1E-28

# dirShell <- file.path("home", "nioo", "lilithk", "data", "R", "PCShell")

# dirShell <- "C:/Users/LilithK/Documents/PCModel/PCModel - work version/Licence_agreement/I_accept/PCModel1350/PCModel/3.00/Models/PCLake+/6.13.16/PCShell"
dirShell <- file.path("/home", "nioo", "lilithk", "compilePCLakeforLERWQ", "R", "PCShell")
nameWorkCase <- "MetaPCLake_serverVersion"

## load all the functions
source(file.path(dirShell, "scripts", "R_system", "functions.R"))  ## load base functions by Luuk van Gerven (2012-2016)
source(file.path(dirShell, "scripts", "R_system", "functions_PCLake_LERWQ.R")) 

library(plyr)
library(dplyr)

## NB: you cannot use different combinations of forcings. e.g. 
##      - in model run 1: mQInEpi & mQInHyp
##      - in model run 2: mPLoad
##     If you want to do this you'll have to do for both: mQInEpi, mQInHyp AND mPLoad. 
##     This is due to the fact that the include file for the parameters (...rp.cpp file) has to be 
##     adjusted before compilation - the file will be built into the code.  
##     This adjustment entails changing parameters that are given as forcings should become "dummy". 
##     BTW: this has never been possible! 
##     How is this possible in the Excel version?

## Order of actions
##   1. Making folder structure for running the model
##   2. Load file 
##   < Make adjustments to the model > 
##   3. Make cpp files
##   4. Compile model
##   5. Initialize model
##   6. Run model


# ## 4. Compile model
 system.time({PCModelCompileModelWorkCase(dirSHELL = dirShell,
                                          nameWORKCASE = nameWorkCase)})

## 6. run one model
##    - initialization happens inside the function
##    - Error catching on run_state & restart (if run_state = 0 & you use restart should you be able to do so?)
# system.time({results <- PCModelNetworkRun(lDATM = lDATM_SETTINGS,
#                                           dfNETWORK = pointer_df,
#                                           lFRACFORC = lFracForc,
#                                           dirSHELL = dirShell,
#                                           dfSUBST = tran_subst,
#                                           integrator_method = "rk45dp7",
#                                           nameWORKCASE = nameWorkCase)})
 

