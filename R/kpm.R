kpm <- function(indicator_matrices, graph_file){
  ####TODO: Check and process input files####
  indicator_matrices = NULL
  graph_file = NULL
  graph_file
  ####TODO: Create options object####
  options <- kpm_options()
  ####TODO: Run####
  if(kpm_options$execution == "Remote"){
    message("Run type: Remote")
    results <- call_kpm(indicator_matrices = indicator_matrices,graph_file = graph_file)
  } else if (kpm_options$execution == "Local"){
    message("Run type: Local")
  }
  ####TODO: Display results####

}



