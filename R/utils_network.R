#' Creates a union network for the pathways of a specific configuration
#'
#' @param configuration  Configuration object for which the union network should be computed.
#'
#' @return The union network as a pathway object
#' @importFrom igraph graph_from_data_frame
create_union_network <- function(configuration) {
  pathways <- configuration@pathways

  if (length(pathways) != 0) {
    # Creates a union network of all pathways of a specific configuration
    for (i in 1:length(pathways)) {
      if (i == 1) {
        union_network <- igraph::graph_from_data_frame(vertices = pathways[[i]]@nodes, d = pathways[[i]]@edges, directed = FALSE)
      } else {
        union_network <- igraph::graph.union(union_network, graph_from_data_frame(vertices = pathways[[i]]@nodes, d = pathways[[i]]@edges, directed = FALSE), byname = "auto")
      }
    }
    # Change col names
    edges <- igraph::as_data_frame(x = union_network, what = "edges")
    names(edges) <- c("source", "target")
    nodes <- igraph::as_data_frame(x = union_network, what = "vertices")
    names(nodes) <- c("node")
    nodes <- determine_union_groups(nodes, configuration)
    return(new("Pathway",
      edges = edges,
      nodes = nodes,
      num_edges = as.integer(igraph::ecount(union_network)),
      num_nodes = as.integer(igraph::vcount(union_network))
    ))
  } else {
    return(new("Pathway"))
  }
}

#' Determines from which pathways a node originates
#'
#' @param configuration  Configuration object for which the groups should be determined,
#'
#' @return Data frame with the node ids and the pathways they originate from
determine_union_groups <- function(union_nodes, configuration) {
  union_nodes[["group"]] <- list(c())
  pathways <- configuration@pathways

  for (id in union_nodes$node) {
    for (pathway in names(pathways)) {
      if (id %in% pathways[[pathway]]@nodes$node) {
        union_nodes$group[union_nodes$node == id][[1]] <- c(union_nodes$group[union_nodes$node == id][[1]], pathway)
      }
    }
  }
  union_nodes$group <- lapply(X = union_nodes$group, FUN = function(x) paste(x, collapse = ", "))
  return(union_nodes)
}

#' Converts igraph object to sif file.
#'
#' @param biological_network Biologial network as igraph object.
#' @param path Path where the file should be saved.
#'
#' @export
#'
#' @import igraph
igraph_to_sif <- function(biological_netwrok, path) {
  edges <- as_data_frame(biological_netwrok, what = c("edges"))
  edges$interaction_type <- "pp"
  edges <- edges[, c("from", "interaction_type", "to")]
  write.table(edges, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t", file = path)
}

#' Computes pathway statistics for all pathways in a result object.
#'
#' @param indicator_matrix Indicator matrix for the computation of the pathway statistics.
#' @param result Result object obtained from a KeyPathwayMiner execution.
#'
#' @export
pathway_statistics <- function(indicator_matrix, result) {
  configurations <- get_configurations(result)
  message("Computing pathway statistics.")
  # Filter out configurations with 0 pathways ####
  message("\tFiltering out configurations with no pathways.")
  removed <- 0
  for (configuration in configurations) {
    if (length(get_pathways(result_object = result, configuration_name = configuration)) == 0) {
      # If no pathways exist remove configuration from solution set
      result <- remove_configuration(result_object = result, configuration_name = configuration)
      removed = removed + 1
    }
  }
  message(paste0("\tFiltering completed. Removed ",removed," configurations."))
  # Get updated configurations
  configurations <- get_configurations(result)
  # Computing pathway statistics####
  message(paste0("Computing pathway statistics for ",length(configurations)," configurations and ",length(configurations)*result@parameters$computed_pathways," pathways."))

  # For all configurations in the result object
  for (configuration in configurations) {
    configuration_object <- get_configuration(result_object = result, configuration_name = configuration)
    # Pathways
    for (pathway_name in names(configuration_object@pathways)) {
      pathway <- get_pathway(configuration = configuration_object, pathway_name = pathway_name)
      nodes <- pathway@nodes$node
      pathway_matrix <- indicator_matrix[indicator_matrix[,1] %in% nodes, ]
      pathway <- set_avg_exp(pathway = pathway, new_avg_exp = round(sum(rowSums(pathway_matrix[-1])) / length(nodes), 2))
      pathway <- set_edges(pathway = pathway, num_edges = nrow(pathway@edges))
      pathway <- set_nodes(pathway = pathway, num_nodes = length(nodes))
      result <- set_pathway(result_object = result, configuration_name = configuration, pathway_name = pathway_name, pathway = pathway)
    }
    # Union network
    union_network <- configuration_object@union_network
    nodes <- union_network@nodes$node
    pathway_matrix <- indicator_matrix[indicator_matrix[,1] %in% nodes, ]
    union_network <- set_avg_exp(pathway = configuration_object@union_network, new_avg_exp = round(sum(rowSums(pathway_matrix[-1])) / length(nodes), 2))
    union_network <- set_edges(pathway = union_network, num_edges = nrow(union_network@edges))
    union_network <- set_nodes(pathway = union_network, num_nodes = length(nodes))
    result <- set_pathway(result_object = result, configuration_name = configuration, pathway = union_network, union = TRUE)
  }
  return(result)
}
