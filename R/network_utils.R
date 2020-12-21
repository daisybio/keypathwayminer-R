#' Creates a union network for the pathways of a specific configuration
#'
#' @param configuration  Configuration object for which the union network should be computed.
#'
#' @return The union network as a pathway object
create_union_network <- function(configuration) {
  pathways <- configuration@pathways
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
