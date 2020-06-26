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
    message(paste("Run type: ",kpm_options()$execution))
    #### Prepare files ####
    files <- check_files(indicator_matrices, graph_file)
    matrices_list <- files[[1]]
    graph <- files[[2]]

    #### Run KPM & Get results ####
    if (kpm_options()$execution == "Remote") {
        results <- call_kpm_remote(indicator_matrices, graph_file)
    } else if (kpm_options()$execution == "Local") {
        results <- call_kpm_local(indicator_matrices, graph_file)
    }
    #### TODO: Visualize results ####
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
    message("Checking files")
    if(!is.null(indicator_matrices)){
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
                        matrices <- append(matrices, matrix)
                    }else if(kpm_options()$execution == "Remote"){
                        # Create dataframe of filepath
                        matrices <- append(matrices,
                                           list(as.data.frame.matrix(read.delim(matrix, header = FALSE))))
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
                    matrices <- append(matrices, list(matrix))
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
    }else {
        # No indicator matrices provided
        stop(paste("No indicator_matrices provided.",
                   "Valid input: a filepath, a data.frame or a list which can contain both.",
                   "For more information visit: https://exbio.wzw.tum.de/keypathwayminer/",sep = "\n"))
    }
    message("Indicator matrices: √")
    #### Check graph_file ####
    if(!is.null(graph_file)){
        if(is.character(graph_file)&(!file.exists(graph_file) | tools::file_ext(graph_file) != "sif")){
            stop(paste("The filepath of the graph_file does not exist.",
                       "\nMake sure the graph_file is in sif format and has a .sif extension.",
                       "\nGiven filepath: ", graph_file))
        }
    message("Graph file: √")
    } else if(is.null(graph_file) & kpm_options()$execution == "Local"){
        # In case a graph_file was not provide on a local run
        stop(paste("For local runs you must provide a graph_file.",
                   "Make sure the graph_file is in sif format and has a .sif extension.",
                   "For more information visit: https://exbio.wzw.tum.de/keypathwayminer/",sep = "\n"))
    } else if (is.null(graph_file) & kpm_options()$execution == "Remote"){
        message("No graph file provided. Network will be selected from the web page using the graph_id.")
    }
    message("Checks completed.")
    return(list(matrices, graph_file))
}


#Initialize JVM and register Java classes and native code contained in the package.
.onLoad <- function(libname, pkgname) {
    message(paste("Initializing JVM and appending jars to classpath:", .jpackage(pkgname, lib.loc = libname)))

    # Check if keypathwayminer standalone and core were added to clas path
    core <- FALSE
    standalone <- FALSE
    for(i in 1:length(.jclassPath())){
        if(grepl(pattern = "keypathwayminer-core-5.0.jar", x = .jclassPath()[i]) == TRUE){
            core <- TRUE
        }else if(grepl(pattern = "keypathwayminer-standalone-5.0.jar", x = .jclassPath()[i]) == TRUE){
            standalone <- TRUE
        }
    }

    message(paste("Standalone jar added to class path: ",standalone))
    message(paste("Core jar added to class path: ",standalone))

    if(standalone & core){
        message("Rkpm ready for local execution")
    }else {
        message(paste("Local execution not possible at the moment.",
                      "Visit https://exbio.wzw.tum.de/keypathwayminer/ for more information."
                      ,sep = "\n"))
    }
}

