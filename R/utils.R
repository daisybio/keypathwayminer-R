#' Fcuntion for testing if rJava works
#'
#' @return TRUE if the correct Java version is detected, FALSE otherwise
#' @export
#'
#' @examples test_jvm()
test_jvm <- function(){
    java_version <- .jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
    if(!grepl(pattern = "1.8", x = java_version)){
        warning("Utils: Java version other than 1.8 detected. It is not sure if RJAMI will work since JAMI was developed for Java 8. It does definitely not work for older Java versions.")
        return(FALSE)
    }
    else{
        message("Utils: The Java virtual machine is available and has the correct version.")
        return(TRUE)
    }
}

#' Resets kpm_options to default values
#'
#' Default options:
#'  execution = "Remote",
#'  async = TRUE,
#'  quest_id = NULL,
#'  url = "https://exbio.wzw.tum.de/keypathwayminer/",
#'  algorithm = "Greedy",
#'  graph_id = 1,
#'  strategy = "GLONE",
#'  remove_bens = FALSE,
#'  use_range_k = FALSE, k_min = 1, k_max = 3, k_step = 1,
#'  use_range_l = FALSE, l_min =0, l_max = 0, l_step = 1,
#'  l_same_percentage = FALSE, same_percentage = 0,
#'  computed_pathways = 20,
#'  perturbation_start = 10,
#'  perturbation_step = 10,
#'  perturbation_max = 20,
#'  graphs_per_step = 1,
#'  with_perturbation = FALSE,
#'  perturbation_technique = "nodeswap",
#'  unmapped_nodes = "Add to negative list",
#'  link_type = "OR",
#'  resource_folder = "/extdata",
#'  properties_file = "kpm.properties"
#'
#' @export
#'
#' @return Reseted option_manager
reset_options <- function(){
 settings::reset(kpm_options)
 message("Utils: Options reseted successfully.")
}
