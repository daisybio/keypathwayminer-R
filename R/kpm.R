kpm(graph_file = graph_file, indicator_matrices  = huntington_disease_up)

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
    message(paste(">Run type: ",kpm_options()$execution))

    #### Prepare files ####
    files <- check_files(indicator_matrices, graph_file)
    indicator_matrices <- files[[1]]
    graph_file <- files[[2]]

    #### Check parameters ####
    check_parameters(indicator_matrices)

    to_java_arguments(indicator_matrices, graph_file)

    # #### Run KPM & Get results ####
    # if (kpm_options()$execution == "Remote") {
    #     results <- call_kpm_remote(indicator_matrices, graph_file)
    # } else if (kpm_options()$execution == "Local") {
    #     results <- call_kpm_local(indicator_matrices, graph_file)
    # }

    #### @todo: Visualize results ####
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
    message(">Checking files")
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
    message("\tIndicator matrices: √")
    #### Check graph_file ####
    if(!is.null(graph_file)){
        if(is.character(graph_file)&(!file.exists(graph_file) | tools::file_ext(graph_file) != "sif")){
            stop(paste("The filepath of the graph_file does not exist.",
                       "\nMake sure the graph_file is in sif format and has a .sif extension.",
                       "\nGiven filepath: ", graph_file))
        }
    message("\tGraph file: √")
    } else if(is.null(graph_file) & kpm_options()$execution == "Local"){
        # In case a graph_file was not provide on a local run
        stop(paste("For local runs you must provide a graph_file.",
                   "Make sure the graph_file is in sif format and has a .sif extension.",
                   "For more information visit: https://exbio.wzw.tum.de/keypathwayminer/",sep = "\n"))
    } else if (is.null(graph_file) & kpm_options()$execution == "Remote"){
        message("No graph file provided. Network will be selected from the web page using the graph_id.")
    }
    message(">File checks completed")
    return(list(matrices, graph_file))
}

#' Function which check parameters
#'
#' Checks case and gene exception parameters
#' as well as perturbation paramets. Checks sanity
#' of parameters and if they are provided in the correct
#' form.
#'
#'@param indicator_matrices List of paths to the indicator matrices.
#'
check_parameters <- function(indicator_matrices){
    message(">Checking parameters")
#### Check case exceptions parameter ####
    if(kpm_options()$use_range_l){
        # Batch run for l parameter
        if(!(length(indicator_matrices) == length(kpm_options()$l_min) &
             length(indicator_matrices) == length(kpm_options()$l_step) &
             length(indicator_matrices) == length(kpm_options()$l_max))){
            stop("Number of matrices is not equal to the number of given l parameters.")
        }else if(!(class(kpm_options()$l_min) == "numeric" &
                   class(kpm_options()$l_step) == "numeric" &
                   class(kpm_options()$l_max) == "numeric")){
            stop("The parameters l_min, l_step and l_max must be numeric values or numeric vectors.")
        }
        # Sanity check l parameters
        l_min <- kpm_options()$l_min
        l_step <- kpm_options()$l_step
        l_max <- kpm_options()$l_max

        case_1 <- l_max <= 0
        case_2 <- l_min > l_max
        case_3 <- l_step == 0
        case_4 <- (l_max - l_step < l_min)

        if(TRUE %in% case_1) stop("Configuration is incorrect: Invalid l_max")
        if(TRUE %in% case_2) stop("Configuration is incorrect: Invalid l_min. It is larger than l_max.")
        if(TRUE %in% case_3) stop("Configuration is incorrect: Invalid l_step. Must be larger than 0.")
        if(TRUE %in% case_4) stop("Configuration is incorrect: Incrementation must be in range")
    } else if(!kpm_options()$use_range_l) {
        # Normal run for l parameter
        if(!(length(indicator_matrices) == length(kpm_options()$l_min))){
            stop("Number of matrices is not equal to the number of given l parameters.")
        } else if(!(class(kpm_options()$l_min) == "numeric")){
            stop("The parameter l_min must be a numeric value or a numeric vector.")
        }
    }

    message("\tCase exception parameters: √")
#### Check gene exceptions parameter ####
if(kpm_options()$algorithm == "INES"){
    if(kpm_options()$use_range_k){
        # For batch runs
        if(!(class(kpm_options()$k_min) == "numeric" &
           class(kpm_options()$k_step) == "numeric" &
           class(kpm_options()$k_max) == "numeric")){
            # In case one of the k_range parameters is not nummeric
            stop("Please provide numeric (integer) values for k_min, k_step and k_max.")
        }else if(!(length(kpm_options()$k_min) == 1 &
                   length(kpm_options()$k_step) == 1 &
                   length(kpm_options()$k_max) == 1)) {
            # In case one of the k_range parameters has not length 1
            stop("The parameters k_min, k_step and k_max must be all of length 1.")
        }
        # Sanity check k-parameters
        if (kpm_options()$k_max<= 0) stop(paste("Configuration is incorrect: ", "Invalid k_max", sep = ""))

        if (kpm_options()$k_min > kpm_options()$k_max){
            stop(paste("Configuration is incorrect: ", "Invalid k_min. It is larger than k_max", sep = ""))
        }

        if (kpm_options()$k_step == 0) {
            stop(paste("Configuration is incorrect: ", "Invalid k_Step. Must be larger than 0", sep = ""))
        }

        if (kpm_options()$k_max - kpm_options()$k_step < pm_options()$k_min) {
            stop(paste("Configuration is incorrect: ", "Invalid k_Step. Incrementation must be in range", sep = ""))
        }
    } else if(!kpm_options()$use_range_k) {
        # For normal runs
        if(!(class(kpm_options()$k_min) == "numeric")){
            stop("Please provide a numeric (integer) value for k_min")
        } else if(!(length(kpm_options()$k_min) == 1)){
            stop("The parameter k_min must be of length 1.")
        }
    }
}
    message("\tGene exception parameters: √")
#### Check perturbation parameters ####
if(kpm_options()$with_perturbation){
    case_1 <- kpm_options()$perturbation_start <= 0
    case_2 <- kpm_options()$perturbation_start > kpm_options()$perturbation_max
    case_3 <- kpm_options()$perturbation_step == 0
    case_4 <- kpm_options()$perturbation_max - kpm_options()$perturbation_step < kpm_options()$perturbation_start

    if(case_1) stop("Configuration is incorrect: Invalid l_max")
    if(case_2) stop("Configuration is incorrect: Invalid l_min. It is larger than l_max.")
    if(case_3) stop("Configuration is incorrect: Invalid l_step. Must be larger than 0.")
    if(case_4) stop("Configuration is incorrect: Incrementation must be in range")
}
    message("\tPerturbation parameters: √")

    message(">Parameters checks complete")
}
