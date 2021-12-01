#' Run gprofiler2's gost function on pathway of interesst
#'
#' @param result result object of kpm run
#' @param configuration configuration of interest
#' @param pathway pathway of interesst
#' @param ... additional parameters which can be passed to gost function
#'
#' @return data.frame with all results from gost run
#' @export
#' @import gprofiler2
#' @examples gost_pathway(result = local_example_1, configuration= "K-5-L1-2", pathway= "Pathway-1", organism = "hsapiens", numeric_ns = "ENTREZGENE_ACC")
profile_pathway <- function(result, configuration, pathway = NULL, ...){

  pathways_of_configuration <- get_pathways(result, configuration)

  if(is.null(pathway)){
    # extract all genes from a configuration
    genesOfInteresst <- unlist(lapply(pathways_of_configuration, function(x){x@nodes$node}))
  } else {
    # Extract all genes from a pathway of interesst
    pathway_of_interesst <- pathways_of_configuration[pathway]
    genesOfInteresst <- pathway_of_interesst[[1]]@nodes$node
  }

  gprofiler2::gost(query = genesOfInteresst, ...)
}
