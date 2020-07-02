#' Fcuntion for testing if rJava works
#'
#' @return TRUE if the correct Java version is detected, FALSE otherwise
#' @export
#'
#' @examples test_jvm()
test_jvm <- function(){
    java_version <- .jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
    if(!grepl(pattern = "1.8", x = java_version)){
        warning("Java version other than 1.8 detected. It is not sure if RJAMI will work since JAMI was developed for Java 8. It does definitely not work for older Java versions.")
        return(FALSE)
    }
    else{
        message("The Java virtual machine is available and has the correct version.")
        return(TRUE)
    }
}