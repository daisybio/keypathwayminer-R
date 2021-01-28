#### Classes ####
#' Result
#'
#' Class for storing the results of a KPM local
#' or remote execution.
#'
#' @slot parameters List with the parameters of the current run.
#' @slot configurations List with all parameter-exceptions-configurations (K and L) and corresponding pathways.
setClass("Result",
  slots = c(parameters = "list", configurations = "list"),
  prototype = list(parameters = list(), configurations = list())
)
#' ResultRemote
#'
#' Contains one additional slot which saves
#' the json as returned by the server.
#'
#' @slot json_result json_result returned from the restfulAPI.
setClass("ResultRemote",
  contains = "Result",
  slots = c(json_result = "list"),
  prototype = list(json_result = list())
)

#' A pathway of a specific configuration.
#'
#' @slot edges Edges of the pathway.
#' @slot nodes Nodes of the pathway.
#' @slot num_edges Number of edges.
#' @slot num_nodes Number of nodes.
#' @slot avg_exp Average expression.
#' @slot info_content Information content.
setClass("Pathway",
  slots = c(
    edges = "data.frame",
    nodes = "data.frame",
    num_edges = "integer",
    num_nodes = "integer",
    avg_exp = "numeric",
    info_content = "numeric"
  ),
  prototype = list(
    edges = data.frame(),
    nodes = data.frame(),
    num_edges = NA_integer_,
    num_nodes = NA_integer_,
    avg_exp = NA_real_,
    info_content = NA_real_
  )
)

#' A configuration of the parameters K and L
#'
#' Contains the configuration and all the
#' correspondig graphs.
#'
#' @slot configuration Name of the configuration (e.g. K-5-L1-15).
#' @slot k Number of node exceptions.
#' @slot l_values Number of case exceptions. Each dataset corresponds to a case_exception parameter.
setClass("Configuration",
  slots = c(configuration = "character", k = "numeric", l_values = "list", union_network = "Pathway", pathways = "list"),
  prototype = list(configuration = NA_character_, k = NA_real_, l_values = list(), union_network = new("Pathway"), pathways = list())
)

#### Pathway - Functions ####
#' Get an iGraph object for a specific pathway
#'
#' @param pathway_object pathway object of a configuration.
#'
#' @return i_graph object of the pathway.
setGeneric(name = "export_to_iGraph", def = function(pathway_object) standardGeneric("export_to_iGraph"))

#' @export
#' @describeIn export_to_iGraph Exports pathway object.
setMethod(f = "export_to_iGraph", signature = "Pathway", definition = function(pathway_object) {
  return(igraph::graph_from_data_frame(d = pathway_object@edges, directed = FALSE))
})

#' Export a graph
#'
#' Export a graph in sif format, tab seperated or custom format.
#'
#' @param  file path where the file is saved
#' @param  pathway_object pathway object of a configuration.
#' @param  sep seperator used to seperate the graph file.
#' @param  format format of the file. If specified sep slot is not used.
setGeneric(name = "export_edges", def = function(pathway_object, file, sep = "\t", format = "") standardGeneric("export_edges"))

#' @export
#' @importFrom tibble add_column
#' @describeIn export_edges Use write.table to export a graph.
setMethod(f = "export_edges", signature = "Pathway", definition = function(pathway_object, file, sep = "\t", format = "") {
  if (format == "") {
    write.table(pathway_object@edges, file = file, quote = FALSE, sep = sep, row.names = FALSE, col.names = FALSE)
  } else if (tolower(format) == "sif") {
    write.table(tibble::add_column(pathway_object@edges, d = rep("pp", nrow(pathway_object@edges)), .after = "source"), file = file, quote = FALSE, sep = sep, row.names = FALSE, col.names = FALSE)
  } else {
    message("The graph will not be exported because the format is not supported")
  }
})



#' Export node ids
#'
#'
#' @param  file path where the file is saved
#' @param  pathway_object pathway object of a configuration.
setGeneric(name = "export_nodes", def = function(pathway_object, file) standardGeneric("export_nodes"))

#' @export
#' @describeIn export_nodes Use write.table to export a graph.
setMethod(f = "export_nodes", signature = "Pathway", definition = function(pathway_object, file) {
  write.table(pathway_object@nodes, file = file, quote = FALSE, row.names = FALSE, col.names = FALSE)
})


##### Result - Functions #####
#' Get all configuration names of a result
#'
#' @param result_object Result object obtained from a KeyPathwayMineR execution.
#'
#' @return All configurations of a result.
setGeneric(name = "get_configurations", def = function(result_object) standardGeneric("get_configurations"))

#' @describeIn get_configurations return all configuration names of a results object.
#' @export
setMethod(f = "get_configurations", signature = "Result", definition = function(result_object) {
  return(names(result_object@configurations))
})

#' Get configuration object
#'
#' @param result_object Result object obtained from a KeyPathwayMineR execution.
#' @param configuration_name Name of the configuration object.
#'
#' @return Configuration object.
#'
#' @examples
setGeneric(name = "get_configuration", def = function(result_object, configuration_name) standardGeneric("get_configuration"))

#' @describeIn get_configuration Given a result object and configuration name a configuration object is returned.
#' @export
setMethod(f = "get_configuration", signature = "Result", definition = function(result_object, configuration_name) {
  return(result_object@configurations[[configuration_name]])
})

#' Get all pathways for a specific configuration
#'
#' @param result_object Result object obtained from a KeyPathwayMineR execution.
#' @param configuration_name Specific configuration for which to get the pathways.
#'
#' @return All pathways of a specific configuration.
setGeneric(name = "get_pathways", def = function(result_object, configuration_name) standardGeneric("get_pathways"))

#' @describeIn get_pathways Returns a list of pathways for a specific configuration
#' @export
setMethod(f = "get_pathways", signature = "Result", definition = function(result_object, configuration_name) {
  return(result_object@configurations[[configuration_name]]@pathways)
})


#' Get specific pathway of a configuration
#'
#' @param configuration Specific configuration for which to get the pathways.
#' @param pathway_name Name of the pathway to be extracted.
#' @param union Boolean. Whether the pathway is a union network.
#'
#' @return Pathway from given configuration with specific pathway name.
setGeneric(name = "get_pathway", def = function(configuration, pathway_name = "", union = FALSE) standardGeneric("get_pathway"))

#' @describeIn get_pathway Return pathway with specific pathway name.
#' @export
setMethod(f = "get_pathway", signature = "Configuration", definition = function(configuration, pathway_name, union) {
  if (union) {
    configuration@union_network
  } else {
    return(configuration@pathways[[pathway_name]])
  }
})

#' Set pathway
#'
#' @param result_object Result object obtained from a KeyPathwayMineR execution.
#' @param configuration_name Name of configuration for which to set the pathway.
#' @param pathway_name Name of the pathway.
#' @param union Boolean. Whether the pathway is a union network.
setGeneric(name = "set_pathway", def = function(result_object, configuration_name, pathway_name = "", pathway, union = FALSE) standardGeneric("set_pathway"))

#' @describeIn set_pathway Set pathway in the result object.
setMethod("set_pathway", signature = "Result", definition = function(result_object, configuration_name, pathway_name, pathway, union) {
  if (union) {
    result_object@configurations[[configuration_name]]@union_network <- pathway
  } else {
    result_object@configurations[[configuration_name]]@pathways[[pathway_name]] <- pathway
  }
  return(result_object)
})


#' Set average differential expressed cases per gene
#' @param pathway Pathway for which to set the value.
#' @param new_avg_exp New average expression value.
setGeneric("set_avg_exp", def = function(pathway, new_avg_exp) standardGeneric("set_avg_exp"))

#' @describeIn set_avg_exp Sets avg_exp for a specific pathway.
setMethod("set_avg_exp", signature = "Pathway", definition = function(pathway, new_avg_exp) {
  pathway@avg_exp <- new_avg_exp
  return(pathway)
})

#' Set number of edges given pathway object
#' @param pathway Pathway for which to set the number of edges.
#' @param num_edges Number of edges.
setGeneric("set_edges", def = function(pathway, num_edges) standardGeneric("set_edges"))

#' @describeIn set_edges Sets num_edges for a specific pathway.
setMethod("set_edges", signature = "Pathway", definition = function(pathway, num_edges) {
  pathway@num_edges <- num_edges
  return(pathway)
})

#' Set number of nodes given pathway object
#' @param pathway Pathway for which to set the number of nodes
#' @param num_nodes Number of nodes
setGeneric("set_nodes", def = function(pathway, num_nodes) standardGeneric("set_nodes"))

#' @describeIn set_nodes Sets num_nodes for a specific pathway.
setMethod("set_nodes", signature = "Pathway", definition = function(pathway, num_nodes) {
  pathway@num_nodes <- num_nodes
  return(pathway)
})

#' Removes configuration from result object
#' @param result_object Result object obtained from a KeyPathwayMineR execution.
#' @param configuration_name Configuration name that should be removed.
setGeneric("remove_configuration", def = function(result_object, configuration_name) standardGeneric("remove_configuration"))

#' @describeIn Remove given configuration.
setMethod("remove_configuration", signature = "Result", definition = function(result_object, configuration_name) {
  result_object@configurations[[configuration_name]] <- NULL
  return(result_object)
})
