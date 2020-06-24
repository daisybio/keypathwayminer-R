#' Main method used for running KPM
#'
#' Runs KeyPathwayMiner localy via the standalone
#' or remotely via the RESTful API of the KeyPathwayMiner
#' website. Input parameters are indicator matrices and a graph_file.
#' You can view or change the run parameters through the
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

    #### TODO: Run####
    if (kpm_options$execution == "Remote") {
        message("Run type: Remote")
        results <- call_kpm_remote(indicator_matrices = indicator_matrices, graph_file = graph_file)
    } else if (kpm_options$execution == "Local") {
        message("Run type: Local")
    }
    #### TODO: Display results####
}

#' Function which checks and processes files
#'
#' Goes through all matrices and the graph file and checks
#' if the input data is correct and exists. It then prepares
#' the data for local or remote use. For local execution return
#' filepaths for remote reutrn data.frames. For the graph_files
#' always return filepaths.
#'
#' @param indicator_matrices Filepath, data.frame or as a list of both.
#' @param graph_file Filepath to the graph file in sif format.
#'
#' @return Returns the data prepared for the respective run (local or remote).
#' Indicator matrices: List of paths for local run and list of data.frames for remote run.
#' Graph file: Null if no graph_file provided else path to graph_file.
check_files <- function(indicator_matrices, graph_file){
    #### Check indicator_matrices ####
    matrices <- list()
    # Before we start we check whether indicator_matrices consists only of one element
    if(is.data.frame(indicator_matrices) | is.character(indicator_matrices)){
        # If this is the case add the element to a list
        indicator_matrices <- list(indicator_matrices)
    }
    if(class(indicator_matrices) == "list"){
    # If multiple indicator matrices are provided
    for(matrix in indicator_matrices){
        if(is.character(matrix)){
            # If matrix is given as path
            if(!file.exists(matrix)){
                stop(paste("File for matrix filepath does not exist. Given filepath: ", matrix))
            }else{
                if(kpm_options()$execution == "Local"){
                    matrices <- append(matrices,matrix)
                }else if(kpm_options()$execution == "Remote"){
                    # Create dataframe of filepath
                    matrices <- append(matrices,as.data.frame.matrix(read.delim(matrix, header = FALSE)))
                }
            }
        }else if(is.data.frame(matrix)){
            # If matrix is given as data.frame
            if(kpm_options()$execution == "Local"){
                # Create temporary file
                message("Writing indicator matrix to temporary file")
                matrix_file <- tempfile(fileext = ".txt")
                write.table(matrix,
                            file = matrix_file,
                            quote = FALSE,
                            sep = "\t",
                            row.names = FALSE,
                            col.names = FALSE)
                # Add filepath to list
                matrices <- append(matrices, matrix_file)
            }else if(kpm_options()$execution == "Remote"){
                matrices <- append(matrices, matrix)
            }
        }else{
            # If function is neither a data.frame nor a file path
            stop(paste("Please enter a valid input for the parameter indicator_matrices",
                       "Valid input: a filepath, a data.frame or a list which can contain both.",
                       "For more information visit: https://exbio.wzw.tum.de/keypathwayminer/",sep = "\n"))
            }
    }
    } else {
        stop(paste("Please enter a valid input for the parameter indicator_matrices",
                   "Valid input: a filepath, a data.frame or a list which can contain both.",
                   "For more information visit: https://exbio.wzw.tum.de/keypathwayminer/",sep = "\n"))
}
    #### Check graph_file ####
    if(!is.null(graph_file)){
        if(is.character(graph_file)&(!file.exists(graph_file) | tools::file_ext(graph_file) != "sif")){
            stop(paste("The filepath of the graph_file does not exist.",
                       "\nMake sure the graph_file is in sif format and has a .sif extension.",
                       "\nGiven filepath: ", graph_file))
        }
    }

    return(matrices, graph_file)
}
