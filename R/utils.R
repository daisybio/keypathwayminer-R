#' Checks the version of the JVM.
#'
#' @return TRUE if Java version 1.8 or higher is detected, FALSE otherwise
#' @export
#'
#' @examples
#' test_jvm()
test_jvm <- function() {
  # deterrmine java version
  java_version <- .jcall("java/lang/System", "S", "getProperty", "java.runtime.version")

  message(paste("Java version detected:", java_version))
  if (startsWith(java_version, "1.8")) {
    message("\tA Java virtual machine is available and has a supported version.")
    return(TRUE)
  } else if (startsWith(java_version, "9") ||
    startsWith(java_version, "10") ||
    startsWith(java_version, "11") ||
    startsWith(java_version, "12") ||
    startsWith(java_version, "13") ||
    startsWith(java_version, "14") ||
    startsWith(java_version, "15") ||
    startsWith(java_version, "16") ||
    startsWith(java_version, "17")) {
    warning("KeyPathwayMineR was tested and build with Java 1.8.\nIf you encounter problems with the newer versions of Java, we recommend switching to Java 1.8.")
    message("\tA Java virtual machine is available and has a supported version.")
    return(TRUE)
  } else {
    warning("Java version other than 1.8 detected.\nMake sure you are using Java 1.8 or later. Versions older than Java 1.8 are not supported.")
    return(FALSE)
  }
}
