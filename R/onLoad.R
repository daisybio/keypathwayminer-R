.onLoad <- function(libname, pkgname) {
  #### Initialize JVM and register Java classes and native code contained in the package. ####
  .jpackage(pkgname, lib.loc = libname)

  # Check if keypathwayminer standalone and core were added to clas path
  standalone <- FALSE
  for (i in 1:length(.jclassPath())) {
    if (grepl(pattern = "keypathwayminer-standalone-5.0.jar", x = .jclassPath()[i]) == TRUE) {
      standalone <- TRUE
    }
  }

  message(paste("Standalone jar added to class path: ", standalone))

  if (standalone & test_jvm()) {
    # If jar is on the classpath and jvm is greater than 1.8
    message("KeyPathwayMineR ready for execution. Use following commands to get started:")
    message("\t1. vignette(\"KeyPathwayMineR\")")
    message("\t2. vignette(\"input_files_format\")")
    message("\t3. ?kpm_options()")
  } else {
    warning(paste("Local execution not possible at the moment.",
      "\nVisit https://github.com/baumbachlab/keypathwayminer-R for more information.",
      sep = ""
    ))
  }
}
