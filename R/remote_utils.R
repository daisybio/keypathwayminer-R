#### Main - Methods ####
#' Method used to create a job submission
#'
#' @param indicator_matrices List. Indicator matrices that will be used.
#' @param attached_to_id String. The quest id this job has been attached to.
#' @param async Boolean. Submit a new job asynchronously?
#' @param ... Options. Run parameters.
#' @return Results object with runId, resultGraphs, comment, succes,resultUrl
#'  and questId.
call_kpm_remote <- function(matrices, graph_file = NULL){

  #Url of KeyPathwayMiner website
  url <- kpm_options()$url

  #Generate random UUID for the session if none was provided
  if(is.null(kpm_options()$quest_id)){
    kpm_options(quest_id = paste(sample(c(LETTERS[1:6],0:9), 32, replace = TRUE), collapse = ""))
  }

  #Create settings object and pass kpm_options parameters
  kpmSetup <- setup_kpm(indicator_matrices = matrices,
                        graph_file = graph_file,
                        algorithm = kpm_options()$algorithm,
                        strategy =  kpm_options()$strategy,
                        graph_id = kpm_options()$graph_id,
                        remove_bens = kpm_options()$remove_bens,
                        range = kpm_options()$use_range,
                        k_min = kpm_options()$k_min, k_max = kpm_options()$k_max, k_step = kpm_options()$k_step,
                        l_min = kpm_options()$l_min, l_max = kpm_options()$l_max, l_step = kpm_options()$l_step,
                        l_same_percentage = kpm_options()$l_same_percentage, same_percentage = kpm_options()$same_percentage,
                        attached_to_id = kpm_options()$quest_id,
                        computed_pathways = kpm_options()$computed_pathways,
                        with_perturbation = kpm_options()$with_perturbation,
                        unmapped_nodes = kpm_options()$unmapped_nodes,
                        link_type = kpm_options()$link_type)

  # Prepare result object
  result <- NULL

  # Print out settings for debugging purposes
  print(sprintf("url: %s", url))
  print(sprintf("settings: %s", kpmSetup[[1]]))

  # Submit
  result <- submit_kpm(kpmSetup)

  return(result)
}

#' Set up a JSON object in preparation of the job submission
#'
#' @return A list with settings, datasets and graph in JSON format.
setup_kpm <- function(indicator_matrices,
                      algorithm = "Greedy",
                      strategy = "GLONE",
                      graph_id = 1,
                      graph_file,
                      remove_bens = FALSE,
                      range,
                      k_min = 0, k_max = 0,  k_step = 1,
                      l_min = 0, l_max = 0, l_step = 1,
                      l_same_percentage = FALSE, same_percentage = 0,
                      attached_to_id,
                      computed_pathways = 20,
                      with_perturbation = FALSE,
                      unmapped_nodes = "Add to negative list",
                      link_type = "OR"){

  # Base64 encode datasetfiles files
  dataset_list <- dataset_list(indicator_matrices, attached_to_id)

  # Create a run id
  run_id <- paste(sample(c(LETTERS[1:6], 0:9), 6, replace = TRUE), collapse="")

  # Setup the json settings:
  settings <- rjson::toJSON(
    list(
      parameters = c(
        name = paste("R demo client run", run_id),
        algorithm = algorithm,
        strategy = strategy,
        removeBENs = tolower(as.character(remove_bens)),
        unmapped_nodes = unmapped_nodes,
        computed_pathways = computed_pathways,
        graphID = graph_id,
        l_samePercentage = tolower(as.character(l_same_percentage)),
        samePercentage_val = same_percentage,
        k_values = list(c(val = k_min, val_step = k_step, val_max = k_max,
                          use_range = tolower(as.character(range)), isPercentage="false")),
        l_values = list(c(val = l_min, val_step = l_step, val_max = l_max,
                          use_range = tolower(as.character(range)), isPercentage = "false",
                          datasetName = paste("dataset", 1, sep = ""))
        )
      ),
      withPerturbation = tolower(as.character(with_perturbation)),
      perturbation = list(c( # perturbation can be left out, if withPeturbation parameter is set to false.
        technique = "Node-swap",
        startPercent = 5,
        stepPercent = 1,
        maxPercent = 15,
        graphsPerStep = 1
      )),
      linkType = link_type,
      attachedToID = attached_to_id,
      positiveNodes = "",
      negativeNodes = ""
    ))

  # Add custom network if provided
  graph <- graph_kpm(graph_file, attached_to_id)

  return(list(settings, dataset_list, graph))
}

#' Method for submitting a job to KeyPathwayMinerWeb
#' Submits can be submitted asynchronously (returns immediately)
#' or blocked until completed(returns when job is complete).
#'
#' @param kpmSetup kpmSetup
#'
#' @return Results in json format.
submit_kpm <- function(kpmSetup){
  withTryCatch(function(){
    #Choose whether it is an asynchronous job or not
    if(kpm_options()$async) {
      url <- paste(kpm_options()$url, "requests/submitAsync", sep="")
    } else {
      url <- paste(kpm_options()$url, "requests/submit", sep="")
    }

    #If a default graph is used we should not send the graph attribute
    if(is.null(kpmSetup[[3]])){
      result <- RCurl::postForm(url, kpmSettings = kpmSetup[[1]],
                                datasets = kpmSetup[[2]])
    } else {
      result <- RCurl::postForm(url, kpmSettings = kpmSetup[[1]],
                                datasets = kpmSetup[[2]], graph = kpmSetup[[3]])
    }

    #Get results
    jsonResult <- rjson::fromJSON(result)

    #Print status comment to console
    print(jsonResult["comment"])

    #Return results
    return(jsonResult)
  })
}

#### Helper - Methods ####

#'  Encodes a list of datasets
#'
#'  Helper method to encode a list of datasets (indicator matrices)
#'  for the job submission.
#'
#' @param indicator_matrices List of indicator matrices to be encoded
#' @param attached_to_id Quest id for the current run.
#'
#' @return Encoded list of datasets with their respective questId.
#' @importFrom foreach %do%
dataset_list <- function(indicator_matrices, attached_to_id){
  counter <- 0
  datasetList <- foreach::foreach(indicator.matrix = indicator_matrices) %do% {
    txt.con <- textConnection("tmp.file", "w")
    write.table(indicator.matrix, txt.con, sep = "\t", quote = F, col.names = F, row.names = F)
    enc.file <- RCurl::base64(paste(tmp.file, collapse = "\n"))
    close(txt.con)

    counter <- counter + 1
    c(name = paste("dataset", counter, sep=""), attachedToID = attached_to_id, contentBase64 = enc.file)
  }
  return(rjson::toJSON(datasetList))
}

#' Helper method to encode custom network file
#'
#' @param graph_file Path to graph file which should be encoded.
#' @param attached_to_id Respective questId.
#'
#' @return Encoded network file or NULL if no graph file submitted.
graph_kpm <- function(graph_file, attached_to_id){
  if(!is.null(graph_file)){
    graph <- base64EncFile(graph_file)
    graph <- rjson::toJSON(c(name = basename(graph_file), attachedToID = attached_to_id, contentBase64 = graph))
  }else{
    graph <- NULL
  }
  return(graph)
}



#' Helper method for error handling
#'
#' @param surroundedFunc Function in which the TryCatch error handling should be applied.
withTryCatch <- function(surroundedFunc){
  tryCatch({
    surroundedFunc()
  }, error = function(e) {
    if("COULDNT_CONNECT" %in% class(e)){
      stop("Couldn't connect to url.")
    }else{
      stop(paste("Unexpected error:", e$message))
    }
    return(NULL)
  })
}

#' Method to check up on a submitted job.
#'
#' Useful to monitor the job progress and current status.
#'
#' @param quest_id String. The quest id this job has been attached to.
#'
#' @return Status of the job for given quest_id.
get_status <- function(quest_id){
  withTryCatch(function(){
    url <- paste(kpm_options()$url, "requests/runStatus", sep="")
    print(sprintf("url: %s", url))
    result <- RCurl::postForm(url, questID = quest_id)
    jsonResult <- rjson::fromJSON(result)

    if(tolower(jsonResult["success"]) == "cancelled"){
      print("Run has been cancelled.")
      return
    }

    print(jsonResult["completed"])
    print(jsonResult["progress"])

    return(jsonResult)
  })
}

#' Once the run is complete, we can obtain the results
#'
#' @param quest_id Jobs respective quest_id.
#'
#' @return If run was successful return result in json
#' format otherwise null.
get_results <- function(quest_id){
  withTryCatch(function(){
   url <- paste(kpm_options()$url, "requests/results", sep = "")
    print(sprintf("url: %s", url))

    result <- RCurl::postForm(url, questID = quest_id)
    jsonResult <- rjson::fromJSON(result)

    if(tolower(jsonResult["success"]) == "true"){
      return(jsonResult)
    }else{
      return(NULL)
    }
  })
}

#' Get a data frame of available networks
#'
#' @param url Url to keypathwayminer.
#'
#' @return Data frame with different graphs and their Id's.
#'
#' @examples
#' get_networks("https://exbio.wzw.tum.de/keypathwayminer/")
#' @export
#' @importFrom foreach %do%
get_networks <- function(url = "https://exbio.wzw.tum.de/keypathwayminer/"){
  kpm_url <- paste(url, "rest/availableNetworks/", sep="")
  result <- RCurl::getURL(kpm_url)
  jsonResult <- rjson::fromJSON(result)
  networks <- foreach::foreach(network = jsonResult, .combine = append)%do%{network[[1]]}
  names(networks) <- foreach::foreach(network = jsonResult, .combine = append) %do% {network[[2]]}
  return(networks)
}

#' Get progress url
#'
#' Get url to see progress in the browser and see the results
#'
#' @param quest_id
#'
#' @return Returns progress url for the respective run.
#' @export
quest_progress_url <- function(quest_id){
  kpm_progress_url <- paste(kpm_options()$url, "requests/quests?attachedToId=",
                            quest_id, "&hideTitle=false", sep="")

  return(kpm_progress_url)
}

#' Method for base64 encoding
#'
#'  Helper method for base64 encoding.
#'  Needed to transfer network and dataset files-
#'
#' @param fileName String. Name of the file to be encoded.
#'
#' @return Base 64 eencoded file.
base64EncFile <- function(file_name){
  return(RCurl::base64(readChar(file_name, file.info(file_name)$size)))
}

