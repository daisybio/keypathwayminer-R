#' Main method used for running KPM
#'
#' Runs KeyPathwayMiner localy via the standalone
#' or remotely via the RESTful API of the KeyPathwayMiner
#' website. Input parameters are indicator matrices and a graph_file.
#' You can see or change the run parameters through the
#' kpm_options() function.
#'
#' @param indicator_matrices List of paths to the indicator matrices.
#' @param graph_file Path of the graph file. NULL if you want to use
#' a graph from the website (only for remote runs).
#' Use getNetworks('https://exbio.wzw.tum.de/keypathwayminer/')
#' to see all networks.
kpm <- function(indicator_matrices, graph_file = NULL) {
    #### TODO: Check and process input files####
    indicator_matrices <- NULL
    graph_file <- NULL
    graph_file
    #### TODO: Create options object####
    options <- kpm_options()
    #### TODO: Run####
    if (kpm_options$execution == "Remote") {
        message("Run type: Remote")
        results <- call_kpm(indicator_matrices = indicator_matrices,
                            graph_file = graph_file,
                            options = options)
    } else if (kpm_options$execution == "Local") {
        message("Run type: Local")
    }
    #### TODO: Display results####
}
