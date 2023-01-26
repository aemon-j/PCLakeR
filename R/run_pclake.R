#'@title Run the PCLake model
#'
#'@description
#'This runs the PCLake model on the specific simulation stored in \code{sim_folder}.
#'The specified \code{sim_folder} must contain a valid .par file.
#'
#'@param sim_folder the directory where simulation files are contained
#'@param par_file the parameter file that needs to be run
#'@param verbose Save output as character vector. Defaults to FALSE
#'@keywords methods
#'@author
#'Jorrit Mesman, Tadhg Moore
#'@examples
#'sim_folder <- system.file('extdata', package = 'SimstratR')
#'run_simstrat(sim_folder, par_file = 'langtjern.par')
#'@export
#'@importFrom utils packageName
run_pclake <- function (sim_folder = ".", par_file="langtjern.par", verbose = TRUE)
{
  ### Windows
  if (.Platform$pkgType == "win.binary") {
    return(run_simstratWin(sim_folder, par_file, verbose))
  }
  
  ### UNIX
  if (.Platform$pkgType == "source") {
    return(run_simstratNIX(sim_folder, par_file, verbose))
  }
  

  ### macOS ###
  if (grepl('mac.binary',.Platform$pkgType)) {
    maj_v_number <- as.numeric(strsplit(
      Sys.info()["release"][[1]],'.', fixed = TRUE)[[1]][1])

    if (maj_v_number < 13.0) {
      stop('pre-mavericks mac OSX is not supported. Consider upgrading')
    }

    return(run_simstratOSx(sim_folder, par_file, verbose))

  }
}

run_simstratWin <- function(sim_folder,par_file="simstrat.par",verbose=TRUE){
  
  if(.Platform$r_arch == 'x64'){
    simstrat_path <- system.file('extbin/win/simstrat.exe', package = packageName())
  }else{
    stop('No Simstrat executable available for your machine yet...')
  }
  
  origin <- getwd()
  setwd(sim_folder)

  tryCatch({
    if (verbose){
      out <- system2(simstrat_path, wait = TRUE, stdout = TRUE,
                     stderr = "", args=par_file)
    } else {
      out <- system2(simstrat_path, stdout = NULL,
                     stderr =NULL,args=par_file)
    }
    setwd(origin)
    return(out)
  }, error = function(err) {
    print(paste("Simstrat_ERROR:  ",err))
    setwd(origin)
  })
}

# run_simstratOSx <- function(sim_folder, par_file = 'langtjern.par', verbose=TRUE){
#
#   # simstrat_path <- system.file('extbinmac/simstrat.exe', package = 'SimstratR')
#   # # File to the folder with the mac-executable for simstrat
#
#   # ship gotm and libs to sim_folder
#   #Sys.setenv(DYLD_FALLBACK_LIBRARY_PATH=lib_path) #Libraries?
#
#
#   origin <- getwd()
#   setwd(sim_folder)
#   
#   tryCatch({
#     if (verbose){
#       out <- system2(simstrat_path, wait = TRUE, stdout = "",
#                      stderr = "", args = par_file)
#
#     } else {
#       out <- system2(simstrat_path, wait = TRUE, stdout = NULL,
#                      stderr = NULL, args=par_file)
#     }
#
#     setwd(origin)
# 	return(out)
#   }, error = function(err) {
#     print(paste("GOTM_ERROR:  ",err))
#
#     setwd(origin)
#   })
# }

run_simstratNIX <- function(sim_folder, par_file = 'langtjern.par', verbose=TRUE){
  simstrat_path <- system.file('exec/model.so', package= packageName())

  Sys.setenv(LD_LIBRARY_PATH=paste(system.file('extbin/nix', 
                                               package=packageName()), 
                                   Sys.getenv('LD_LIBRARY_PATH'), 
                                   sep = ":"))
  origin <- getwd()
  setwd(sim_folder)
  
  tryCatch({
    if (verbose){
      out <- system2(simstrat_path, wait = TRUE, stdout = TRUE,
                     stderr = "", args=par_file)
    } else {
      out <- system2(simstrat_path, stdout = NULL,
                     stderr =NULL,args=par_file)
    }
    setwd(origin)
    return(out)
  }, error = function(err) {
    print(paste("Simstrat_ERROR:  ",err))
    setwd(origin)
  })
}

run_simstratOSx <- function(sim_folder, par_file = 'langtjern.par', verbose=TRUE){
  simstrat_path <- system.file('exec/simstrat', package= packageName())
  
  
  origin <- getwd()
  setwd(sim_folder)
  
  tryCatch({
    if (verbose){
      out <- system2(simstrat_path, wait = TRUE, stdout = TRUE,
                     stderr = "", args=par_file)
    } else {
      out <- system2(simstrat_path, stdout = NULL,
                     stderr =NULL, args=par_file)
    }
    setwd(origin)
    return(out)
  }, error = function(err) {
    print(paste("Simstrat_ERROR:  ",err))
    setwd(origin)
  })
}
# 
# ### From GLEON/gotm3r
# gotm.systemcall <- function(sim_folder, gotm_path, verbose, system.args) {
#   origin <- getwd()
#   setwd(sim_folder)
#   
#   tryCatch({
#     if (verbose){
#       out <- system2(gotm_path, wait = TRUE, stdout = "",
#                      stderr = "", args = system.args)
#     } else {
#       out <- system2(gotm_path, wait = TRUE, stdout = NULL,
#                      stderr = NULL, args = system.args)
#     }
#     setwd(origin)
#     return(out)
#   }, error = function(err) {
#     print(paste("gotm_ERROR:  ",err))
#     setwd(origin)
#   })
# }
# 
# ### macOS ###
# run_gotmOSx <- function(sim_folder, verbose, system.args){
#   gotm_path <- system.file('exec/macgotm', package = 'GOTMr')
#   gotm.systemcall(sim_folder = sim_folder, gotm_path = gotm_path, verbose = verbose, system.args = system.args)
# }
