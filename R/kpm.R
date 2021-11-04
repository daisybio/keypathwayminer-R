#' Main method used for running KPM
#'
#' Runs KeyPathwayMiner localy via the standalone
#' or remotely via the RESTful API of the KeyPathwayMiner
#' website. Input parameters are indicator matrices and a graph_file.
#' You can view or change the run parameters through the
#' kpm_options() function.
#'
#' @param indicator_matrices List of paths to the indicator matrices or data.frames.
#' @param graph Path of the graph file or an igraph object.
#' NULL if you want to use a graph from the web service (only for remote runs).
#' Use get_networks()
#' to see all networks.
#' @export
kpm <- function(indicator_matrices, graph = NULL) {
  message(paste(">Run type: ", kpm_options()$execution))

  # Combine multiple matrices
  if(length(indicator_matrices) > 1 & class(indicator_matrices)!="data.frame" ){
    indicator_matrices <- combine_mutliple_matrices(indicator_matrices)
  }

  # Prepare files ####
  matrix <- indicator_matrices
  files <- check_files(indicator_matrices, graph)
  indicator_matrices <- files[[1]]
  graph_file <- files[[2]]

  # Check parameters ####
  check_parameters(indicator_matrices)


# Run KPM & Get results ---------------------------------------------------
if (!kpm_options()$use_range_k & !kpm_options()$use_range_l) {
      # If not a ranged run
      if (kpm_options()$execution == "Remote") {
          results <- call_kpm_remote(indicator_matrices, graph_file)
        }else if (kpm_options()$execution == "Local") {
          results <- call_kpm_local(indicator_matrices, graph_file)
      }
    }
else {
      # Ranged run
      result_list <- list()

      if (!kpm_options()$use_range_l) {
        kpm_options("l_step" = 0)
        kpm_options("l_max" = kpm_options()$l_min)
      }

      if (!kpm_options()$use_range_k) {
        kpm_options("k_step" = 0)
        kpm_options("k_max" = kpm_options()$k_min)
      }
      # Temporary variables to save the starting k_min
      temp_l_min <- kpm_options()$l_min
      temp_k_min <- kpm_options()$k_min
      # Temporary variables to save use_range values
      temp_use_range_l <- kpm_options()$use_range_l
      temp_use_range_k <- kpm_options()$use_range_k
      kpm_options("use_range_l" = FALSE)
      kpm_options("use_range_k" = FALSE)

      for (l in seq(from = temp_l_min, by = kpm_options()$l_step, to = kpm_options()$l_max)) {
        for (k in seq(from = temp_k_min, by = kpm_options()$k_step, to = kpm_options()$k_max)) {
          kpm_options("l_min" = as.numeric(l))
          kpm_options("k_min" = as.numeric(k))

          if (kpm_options()$execution == "Remote") {
            result_list <- c(result_list, call_kpm_remote(indicator_matrices, graph_file)@configurations)
          }else if (kpm_options()$execution == "Local") {
            result_list <- c(result_list, call_kpm_local(indicator_matrices, graph_file)@configurations)
          }
        }
      }
      # Reset values
      kpm_options("l_min" = temp_l_min)
      kpm_options("k_min" = temp_k_min)
      kpm_options("use_range_l" = temp_use_range_l)
      kpm_options("use_range_k" = temp_use_range_k)

      # Save pathways from several runs in a Result object
      results <- new("Result",
                     configurations = result_list,
                     parameters = kpm_options()
      )
    }


# Compute pathway statistics ----------------------------------------------

  if(kpm_options()$execution == "Local"|(kpm_options()$execution == "Remote" & !kpm_options()$async)){
  results <- pathway_statistics(indicator_matrix = matrix, result = results)
  }else if (kpm_options()$execution == "Remote" & kpm_options()$async){
    # In case of an async run pathway statistics must be computed later
    message("Results are being computed asynchronous.Use is_finished(result) to check wheter the ")
  }

  return(results)
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
#' @param graph Filepath to the graph file in sif format or an igraph object.
#'
#' @return Returns the data prepared for the respective run (local or remote).
#' Indicator matrices: List of paths for local run and list of data.frames for remote run.
#' Graph file: Null if no graph_file provided else path to graph_file.
check_files <- function(indicator_matrices, graph) {
  # Check indicator_matrices ####
  message(">Checking input files and data strucures")
  if (!is.null(indicator_matrices)) {
    matrices <- list()
    # Before we start we check whether indicator_matrices consists only of one element
    if (is.data.frame(indicator_matrices) | is.character(indicator_matrices)) {
      # If this is the case add the element to a list
      indicator_matrices <- list(indicator_matrices)
    }
    if (class(indicator_matrices) == "list") {
      # If multiple indicator matrices are provided
      for (matrix in indicator_matrices) {
        if (is.character(matrix)) {
          # If matrix is given as path
          if (!file.exists(matrix)) {
            stop(paste("File for matrix filepath does not exist. Given filepath: ", matrix))
          } else {
            if (kpm_options()$execution == "Local") {
              matrices <- append(matrices, matrix)
            } else if (kpm_options()$execution == "Remote") {
              # Create dataframe of filepath
              matrices <- append(
                matrices,
                list(as.data.frame.matrix(read.delim(matrix, header = kpm_options()$matrix_header)))
              )
            }
          }
        } else if (is.data.frame(matrix)) {
          # If matrix is given as data.frame
          if (kpm_options()$execution == "Local") {
            # Create temporary file
            message("Writing indicator matrix to temporary file.")
            matrix_file <- tempfile(fileext = ".txt")
            export_matrix(matrix = matrix, path = matrix_file)
            # Add filepath to list
            matrices <- append(matrices, matrix_file)
          } else if (kpm_options()$execution == "Remote") {
            matrices <- append(matrices, list(matrix))
          }
        } else {
          # If function is neither a data.frame nor a file path
          stop(paste("Please enter a valid input for the parameter indicator_matrices",
                     "Valid input: a filepath, a data.frame or a list which can contain both.",
                     "For more information visit: https://exbio.wzw.tum.de/keypathwayminer/",
                     sep = "\n"
          ))
        }
      }
    } else {
      stop(paste("Please enter a valid input for the parameter indicator_matrices",
                 "Valid input: a filepath, a data.frame or a list which can contain both.",
                 "For more information visit: https://exbio.wzw.tum.de/keypathwayminer/",
                 sep = "\n"
      ))
    }
  } else {
    # No indicator matrices provided
    stop(paste("No indicator_matrices provided.",
               "Valid input: a filepath, a data.frame or a list which can contain both.",
               "For more information visit: https://exbio.wzw.tum.de/keypathwayminer/",
               sep = "\n"
    ))
  }
  message("\tIndicator matrices: checked")
  # Check graph_file ####
  if (!is.null(graph)) {
    if (class(graph) == "igraph") {
      message("Writing igraph object to temporary file.")
      # Create temporary file
      temp_path <- tempfile(fileext = ".sif")
      igraph_to_sif(biological_netwrok = graph, path = temp_path)
      graph <- temp_path
    } else if (is.character(graph) & (!file.exists(graph) | tools::file_ext(graph) != "sif")) {
      stop(paste(
        "The filepath of the graph_file does not exist.",
        "\nMake sure the graph_file is in sif format and has a .sif extension.",
        "\nGiven filepath: ", graph
      ))
    }
    message("\tGraph file: checked")
  } else if (is.null(graph) & kpm_options()$execution == "Local") {
    # In case a graph_file was not provide on a local run
    stop(paste("For local runs you must provide a graph_file.",
               "Make sure the graph_file is in sif format and has a .sif extension.",
               "For more information visit: https://exbio.wzw.tum.de/keypathwayminer/",
               sep = "\n"
    ))
  } else if (is.null(graph) & kpm_options()$execution == "Remote") {
    message("No graph file provided. Network will be selected from the web service using the graph_id parameter.")
  }
  message(">File checks completed")
  return(list(matrices, graph))
}

#' Function which check parameters
#'
#' Checks case and gene exception parameters
#' as well as perturbation paramets. Checks sanity
#' of parameters and if they are provided in the correct
#' form.
#'
#' @param indicator_matrices List of paths to the indicator matrices.
#'
check_parameters <- function(indicator_matrices) {
  message(">Checking parameters")
  # Check case exceptions parameter ####
  if (kpm_options()$use_range_l) {
    if (kpm_options()$execution == "Local") {
      # Batch run for l parameter
      if (!(length(indicator_matrices) == length(kpm_options()$l_min) &
            length(indicator_matrices) == length(kpm_options()$l_step) &
            length(indicator_matrices) == length(kpm_options()$l_max))) {
        stop("Number of matrices is not equal to the number of given l parameters.")
      } else if (!(class(kpm_options()$l_min) == "numeric" &
                   class(kpm_options()$l_step) == "numeric" &
                   class(kpm_options()$l_max) == "numeric")) {
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

      if (TRUE %in% case_1) stop("Configuration is incorrect: Invalid l_max")
      if (TRUE %in% case_2) stop("Configuration is incorrect: Invalid l_min. It is larger than l_max.")
      if (TRUE %in% case_3) stop("Configuration is incorrect: Invalid l_step. Must be larger than 0.")
      if (TRUE %in% case_4) stop("Configuration is incorrect: Incrementation must be in range")
    } else if (kpm_options()$execution == "Remote") {
      if (!kpm_options()$l_same_percentage & length(indicator_matrices) > 1) {
        stop("Batch run only supported with one matrix in remote mode. If you want to perform a
                      batch run with multiple matrices consider changing to local execution.")
      } else if (!kpm_options()$l_same_percentage &
                 length(indicator_matrices) == 1 &
                 (!class(kpm_options()$l_min) == "numeric" | length(kpm_options()$l_min) != 1) &
                 (!class(kpm_options()$l_step) == "numeric" | length(kpm_options()$l_step) != 1) &
                 (!class(kpm_options()$l_max) == "numeric" | length(kpm_options()$l_max) != 1)) {
        stop("l_min, l_step and l_max must be numeric and of not a vector.")
      } else if (kpm_options()$l_same_percentage) {
        stop("The l_same_percentage variable is set to TRUE on a batch run. If you want to perform a
                      batch run with multiple matrices consider changing to local execution.")
      }
    }
  } else if (!kpm_options()$use_range_l) {
    # Normal run ony l_min is considered
    if (kpm_options()$execution == "Local") {
      if (!(length(indicator_matrices) == length(kpm_options()$l_min))) {
        stop("Number of matrices is not equal to the number of given l parameters.")
      } else if (!(class(kpm_options()$l_min) == "numeric")) {
        stop("The parameter l_min must be a numeric value or a numeric vector.")
      }
    } else if (kpm_options()$execution == "Remote") {
      if (!kpm_options()$l_same_percentage & length(indicator_matrices) > 1) {
        stop("In remote execution you need to set l_same_percentage = TRUE and set a same_percentage value.
                      If you want to define different L parameters for every matrix consider changing to local execution.")
      } else if (!kpm_options()$l_same_percentage &
                 length(indicator_matrices) == 1 &
                 (!class(kpm_options()$l_min) == "numeric" | length(kpm_options()$l_min) != 1)) {
        stop("l_min must be numeric and not a vector in the remote execution.")
      }
    }
  }
  message("\tCase exception parameters: checked")
  # Check gene exceptions parameter ####
  if (kpm_options()$algorithm == "INES") {
    if (kpm_options()$use_range_k) {
      # For batch runs
      if (!(class(kpm_options()$k_min) == "numeric" &
            class(kpm_options()$k_step) == "numeric" &
            class(kpm_options()$k_max) == "numeric")) {
        # In case one of the k_range parameters is not nummeric
        stop("Please provide numeric (integer) values for k_min, k_step and k_max.")
      } else if (!(length(kpm_options()$k_min) == 1 &
                   length(kpm_options()$k_step) == 1 &
                   length(kpm_options()$k_max) == 1)) {
        # In case one of the k_range parameters has not length 1
        stop("The parameters k_min, k_step and k_max must be all of length 1.")
      }
      # Sanity check k-parameters
      if (kpm_options()$k_max <= 0) stop(paste("Configuration is incorrect: ", "Invalid k_max", sep = ""))

      if (kpm_options()$k_min > kpm_options()$k_max) {
        stop(paste("Configuration is incorrect: ", "Invalid k_min. It is larger than k_max", sep = ""))
      }

      if (kpm_options()$k_step == 0) {
        stop(paste("Configuration is incorrect: ", "Invalid k_Step. Must be larger than 0", sep = ""))
      }

      if (kpm_options()$k_max - kpm_options()$k_step < pm_options()$k_min) {
        stop(paste("Configuration is incorrect: ", "Invalid k_Step. Incrementation must be in range", sep = ""))
      }
    } else if (!kpm_options()$use_range_k) {
      # For normal runs
      if (!(class(kpm_options()$k_min) == "numeric")) {
        stop("Please provide a numeric (integer) value for k_min")
      } else if (!(length(kpm_options()$k_min) == 1)) {
        stop("The parameter k_min must be of length 1.")
      }
    }
  }
  message("\tGene exception parameters: checked")
  # Check perturbation parameters ####
  if (kpm_options()$with_perturbation) {
    case_1 <- kpm_options()$perturbation_start <= 0
    case_2 <- kpm_options()$perturbation_start > kpm_options()$perturbation_max
    case_3 <- kpm_options()$perturbation_step == 0
    case_4 <- kpm_options()$perturbation_max - kpm_options()$perturbation_step < kpm_options()$perturbation_start

    if (case_1) stop("Configuration is incorrect: Invalid l_max.")
    if (case_2) stop("Configuration is incorrect: Invalid l_min. It is larger than l_max.")
    if (case_3) stop("Configuration is incorrect: Invalid l_step. Must be larger than 0.")
    if (case_4) stop("Configuration is incorrect: Incrementation must be in range.")
  }
  message("\tPerturbation parameters: checked")
  # Check positive and negative nodes ####
  if (!is.null(kpm_options()$positive_nodes)) {
    if (!is.character(kpm_options()$positive_nodes) | !is.atomic(kpm_options()$positive_nodes)) {
      stop("Element of option positive_nodes must be of type character and an atomic vector.")
    }
  }
  if (!is.null(kpm_options()$negative_nodes)) {
    if (!is.character(kpm_options()$negative_nodes) | !is.atomic(kpm_options()$negative_nodes)) {
      stop("Element of option negative_nodes must be of type character and atomic vector.")
    }
  }
  message("\tPositive and negative nodes parameters: checked")
  message(">Parameters checks complete")
}

#' Logically combines multiple datasets
#'
#' @param indicator_matrices Indicator matrices to combine
#'
#' @return Indicator matrix
combine_mutliple_matrices <- function(indicator_matrices){
  initial_matrix <- indicator_matrices[[1]]
  for (i in 2:length(indicator_matrices)) {
    if(kpm_options()$link_type == "OR"){
      initial_matrix <- initial_matrix[,-1] | indicator_matrices[[i]][,-1]
    }else if (kpm_options()$link_type == "AND"){
      initial_matrix <- initial_matrix[,-1] & indicator_matrices[[i]][,-1]
    }
  }

  # #Converts TRUE to 1 and FALSE to 0
  initial_matrix <- 1 * initial_matrix
  # # Prepends entite names
  initial_matrix <- cbind(as.character(indicator_matrices[[i]][,1]), as.data.frame(initial_matrix))

  return(initial_matrix)
}
