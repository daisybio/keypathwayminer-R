.onLoad <- function(libname, pkgname) {
  #### Initialize JVM and register Java classes and native code contained in the package. ####
  message(paste("Initializing JVM and appending jars to classpath:", .jpackage(pkgname, lib.loc = libname)))

  # Check if keypathwayminer standalone and core were added to clas path
  standalone <- FALSE
  for(i in 1:length(.jclassPath())){
  if(grepl(pattern = "keypathwayminer-standalone-5.0.jar", x = .jclassPath()[i]) == TRUE){
      standalone <- TRUE
    }
  }

  message(paste("Standalone jar added to class path: ",standalone))

  if(standalone & test_jvm()){
    # If jar is on the classpath and jvm is set to 1.8
    message("Rkpm ready for local execution")
  }else {
    warning(paste("Local execution not possible at the moment.",
                  "Visit https://exbio.wzw.tum.de/keypathwayminer/ for more information."
                  ,sep = "\n"))
  }

  # @todo Check if Url for api calls is available

}
