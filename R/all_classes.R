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
#' @param result_object Result of the current run.
#'
#' @return All configurations of a result.
setGeneric(name = "get_configurations", def = function(result_object) standardGeneric("get_configurations"))

#' @describeIn get_configurations Retrun all configuration names of a results object.
#' @export
setMethod(f = "get_configurations", signature = "Result", definition = function(result_object) {
  return(names(result_object@configurations))
})


#' Get all pathways for a specific configuration
#'
#' @param result_object Result of the current run.
#' @param configuration Specific configuration for which to get the pathways.
#'
#' @return All pathways of a specific configuration.
setGeneric(name = "get_pathways", def = function(result_object, configuration) standardGeneric("get_pathways"))

#' @describeIn get_pathways Returns a list of pathways for a specific coniguration
#' @export
setMethod(f = "get_pathways", signature = "Result", definition = function(result_object, configuration) {
  return(result_object@configurations[[configuration]])
})

#' Return result url
#'
#' Only for remote execution
#'
#' @param result_object Result of the current run.
#'
#' @return Url to KPM web server.
setGeneric(name = "get_result_url", def = function(result_object) standardGeneric("get_result_url"))

#' @describeIn get_result_url Returns result URL
#' @export
setMethod(f = "get_result_url", signature = "ResultRemote", definition = function(result_object) {
  return(result_object@json_result[["resultUrl"]])
})


#' Check if asynchronous remote run is finished
#'
#' Only for remote execution
#'
#' @param result_object Result of the current run.
#'
#' @return TRUE if results are ready. FASE if still processing .
setGeneric(name = "is_finished", def = function(result_object) standardGeneric("is_finished"))

#' @describeIn is_finished returns status of remote run
#' @export
setMethod(f = "is_finished", signature = "ResultRemote", definition = function(result_object) {
  completed <- get_status(quest_id = result_object@json_result[["questID"]])$completed
  return(completed)
})


#' Fetches results once job is completed
#'
#' Only for remote execution
#'
#' @param result_object Result of the current run.
#'
#' @return Remote result object.
setGeneric(name = "get_results", def = function(result_object) standardGeneric("get_results"))

#' @describeIn get_results returns RemoteResult object
#' @export
setMethod(f = "get_results", signature = "ResultRemote", definition = function(result_object) {
  remote_results <- fetch_results(quest_id = result_object@json_result[["questID"]])
  return(save_remote_results(remote_results))
})
