# Initializes JVM.
# Checks whether jar is in classpath.
# Checks if the remote server is up.
.onLoad <- function(libname, pkgname) {
  #### Initialize JVM and register Java classes and native code contained in the package. ####
  .jpackage(pkgname, lib.loc = libname)

  # Check if keypathwayminer standalone was added to class path
  standalone <- FALSE
  for (i in 1:length(.jclassPath())) {
    if (grepl(pattern = "keypathwayminer-standalone-5.0.jar", x = .jclassPath()[i]) == TRUE) {
      standalone <- TRUE
    }
  }

  message(paste("Standalone jar added to class path: ", standalone))

  # Check if remote server is up
  remote <- RCurl::url.exists("https://exbio.wzw.tum.de/keypathwayminer/")

  if (standalone & test_jvm() & remote) {
    # If jar is on the classpath and jvm is greater than 1.8
    message("KeyPathwayMineR remote and local ready for execution.")
    message("Use following command to get started: get_started()")
  } else if(!standalone){
    warning(paste("Local execution not possible at the moment. Jar not in classpath.",
      "\nVisit https://github.com/baumbachlab/keypathwayminer-R for more information.",
      sep = ""
    ))
  }else if(!remote){
    warning(paste("Server is not responding.",
                  "\nRemote execution not possible at the moment.",
                  sep = ""
    ))
  }
}
