.onLoad <- function(libname, pkgname) {
  #### Initialize JVM and register Java classes and native code contained in the package. ####
  message(paste("Initializing JVM and appending jars to classpath:", .jpackage(pkgname, lib.loc = libname)))

  # Check if keypathwayminer standalone and core were added to clas path
  core <- FALSE
  standalone <- FALSE
  for(i in 1:length(.jclassPath())){
    if(grepl(pattern = "keypathwayminer-core-5.0.jar", x = .jclassPath()[i]) == TRUE){
      core <- TRUE
    }else if(grepl(pattern = "keypathwayminer-standalone-5.0.jar", x = .jclassPath()[i]) == TRUE){
      standalone <- TRUE
    }
  }

  message(paste("Standalone jar added to class path: ",standalone))
  message(paste("Core jar added to class path: ",core))

  if(standalone & core){
    message("Rkpm ready for local execution")
  }else {
    warning(paste("Local execution not possible at the moment.",
                  "Visit https://exbio.wzw.tum.de/keypathwayminer/ for more information."
                  ,sep = "\n"))
  }

  # @todo Check if Url for api calls is available

}
