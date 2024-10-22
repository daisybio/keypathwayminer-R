#' Checks the version of the JVM.
#'
#' @return TRUE if Java version 1.8 or higher is detected, FALSE otherwise
#' @export
#'
#' @examples
#' test_jvm()
test_jvm <- function() {
  # deterrmine java version
  java_version <- rJava::.jcall("java/lang/System", "S", "getProperty", "java.runtime.version")

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

#' Functions which prints information on how to get started.
#'
#' @export
get_started <- function() {
  message("Use following commands to get started:")
  message("\t1. vignette(\"KeyPathwayMineR\") to access the vignette of the package")
  message("\t2. ?kpm_options() to get an overview of all the options that can be adjusted for execution")
}



#' Loads results from folder with text files
#'
#' User can load result from "Result" folder which is automatically generated during each KPM run
#'
#' @param path path to the folder with subfolders for each configuration
#'
#' @return KPM-R Result object
#' @export
#'
#' @examples
get_results_from_folder <- function(path){
  file_names <- list.files(path)

  result_list <- list()
  for(i in seq_along(file_names)){
    path_tables <- file.path(path, file_names[i] , "tables")
    result <- save_local_results(path_tables)
    result_list <- c(result_list, result@configurations)
  }

  new("Result", configurations = result_list,
      parameters = list(
        computed_pathways = kpm_options()$computed_pathways,
        strategy = kpm_options()$strategy
      )
  )
}


#' Retrieves bioGRID network in suitable form for KPM
#'
#' @param organism available organsims are 'arabidopsis', 'c.elegans', 'fruitFly', 'human', 'mouse', 'yeast', 's.pombe'
#' @param IDType has to be either 'EntrezId' or 'Official' (=HGNC)
#'
#' @return returns a dataframe which can be used as input in kpm()
#' @export
retrieve_biogrid<- function(organism, IDType){

  organism <- "human"
  IDType <- "EntrezId"

  checkmate::assertChoice(organism, c('arabidopsis', 'c.elegans', 'fruitFly', 'human', 'mouse', 'yeast', 's.pombe'))
  checkmate::assertChoice(IDType, c('EntrezId', 'Official'))

  network <- simpIntLists::findInteractionList(organism, IDType)
  result <-  lapply(network, function(x){data.frame(ID1 = x[[1]],
                                                    ID2 = x[[2]])} )
  res <- do.call(rbind, result)
  graph_from_data_frame(res)
}
