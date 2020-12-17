# TODO write documentation for all classes and methods
# TODO write export functions to save results as txt and sif files
#### Classes ####
# Class for storing the results of a KPM execution
setClass("Result",
         slots = c(parameters = "list", configurations = "list"),
         prototype = list(parameters = list(), configurations = list()))

# A pathway for a specific configuration.
# Contains edges, nodes, number of edges, number of nodes, average expression
setClass("Pathway",
         slots = c(edges = "data.frame",
                   nodes = "data.frame",
                   num_edges = "integer",
                   num_nodes = "integer",
                   avg_exp = "numeric",
                   info_content = "numeric"),
         prototype = list(edges = data.frame(),
                          nodes = data.frame(),
                          num_edges = NA_integer_,
                          num_nodes = NA_integer_,
                          avg_exp = NA_real_,
                          info_content = NA_real_))

# Exceptions parameters of a run configuration and all the corresponding paths
setClass("Configuration",
         slots = c(configuration = "character", k = "numeric", l_values = "list", union_network = "Pathway" , pathways = "list"),
         prototype = list(configuration = NA_character_, k = NA_real_, l_values = list(), union_network = new("Pathway"), pathways = list()))


