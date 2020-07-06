#' Method for running KeyPathwayMineR localy
#'
#' @param indicator_matrices List of paths to the indicator matrices that will be used.
#' @param graph_file String. Path to graph_file.
#'
#' @return Saves the results in folder results.
call_kpm_local <- function(indicator_matrices, graph_file){
  # Directory where input files and properties files are stored
  datasets_file <- kpm_options()$resource_folder
  properties_file <- kpm_options()$properties_file
  # Convert option parameters to java command line arguments
  args <- to_java_arguments(indicator_matrices = indicator_matrices, graph_file = graph_file)

  #### Initialize java objects ####
  main <- .jnew(class = "de/mpg/mpiinf/ag1/kpm/main/Main")

  .jcall(obj  = main,
         returnSig = "V",
         method = "runR",
         args,
         .jnew(class = "java/lang/String", datasets_file),
         .jnew(class = "java/lang/String", properties_file))
}

#' Method which transform kpm_options() to java arguments.
#'
#' Takes kpm_options and adds arguments to a list
#' @return String [] args array with the arguments
to_java_arguments <- function(indicator_matrices, graph_file) {
  # List with arguments for local execution
  arguments <- c()
  # Get algorithm
  arguments <- c(arguments, paste("-algo=", toupper(kpm_options()$algorithm), sep = ""))
  # Get remove border exceptions node flag
  if(kpm_options()$remove_bens) arguments <-  c(arguments, "-removeBens", sep = "")
  # Get case exceptions L
  arguments <- get_case_exceptions(indicator_matrices = indicator_matrices, arguments = arguments)
  # Get perturbation parameters
 if(kpm_options()$with_perturbation){
    arguments <- c(arguments, paste("-perturbation=",
                                         kpm_options()$perturbation_start, ",",
                                         kpm_options()$perturbation_step, ",",
                                         kpm_options()$perturbation_max, ",",
                                         kpm_options()$graphs_per_step,
                                         sep=""))

    arguments <- c(arguments, paste("-perturbation_technique=", kpm_options()$perturbation_technique, sep = ""))
  }
  # Get graph file path
  arguments <- c(arguments, paste("-graphFile=", graph_file, sep = ""))
  # Get indicator matrices
  for(i in 1:length(indicator_matrices)){
    arguments <- c(arguments, paste("-matrix", i, "=", indicator_matrices[i], sep = ""))
  }
  # Computed pathways
  arguments <- c(arguments, paste("-maxSolutions=", kpm_options()$computed_pathways, sep = ""))
  # Combine operator if we have more than two matrices
  if(length(indicator_matrices) > 1) arguments <- c(arguments, paste("-combineOp=", kpm_options()$link_type, sep = ""))
  # Add strategy glone to arguments
  if(kpm_options()$strategy == "GLONE") arguments <- c(arguments,"-strategy=GLONE")
  # Add strategy INES to arguments
  if(kpm_options()$strategy == "INES"){
    arguments <- c(arguments,"-strategy=INES")
    if(kpm_options()$use_range_k){
      arguments <- c(arguments, "-batch")
      arguments <- c(arguments, paste("-K_batch=",
                                           kpm_options()$k_min, ",",
                                           kpm_options()$k_step, ",",
                                           kpm_options()$k_max, sep=""))
    } else if(!kpm_options()$use_range_k) {
      arguments <- c(arguments, paste("-K=", kpm_options()$k_min, sep=""))
    }

  }
  return(.jarray(arguments))
}

#' Crates java arguments for case exceptions
get_case_exceptions <- function(indicator_matrices, arguments) {
  # Check whether it is a batch run or not
  if(kpm_options()$use_range_l){
    arguments <- c(arguments, "-batch")
    # Batch run for l parameter
      for(i in 1:length(indicator_matrices)){
        arguments <- c(arguments, paste("-L",i,"_batch=", kpm_options()$l_min[i], ",",
                                            kpm_options()$l_step[i], ",",
                                            kpm_options()$l_max[i],sep=""))
      }
  }else if(!kpm_options()$use_range_l) {
    # Normal run for l parameter
      for(i in 1:length(indicator_matrices)){
        arguments <- c(arguments, paste("-L",i,"=", kpm_options()$l_min[i], sep=""))
      }
  }

  return(arguments)
}

