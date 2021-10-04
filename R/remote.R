#### Main - Methods ####
#' Method for running KeyPathwayMineR remotely
#'
#' Method used to create a job submission.
#'
#' @param indicator_matrices List. Indicator matrices that will be used.
#' @param graph_file String. Path to graph_file.
#'
#' @return Results object containing all execution_configurations and the input parameters of the current run.
call_kpm_remote <- function(indicator_matrices, graph_file = NULL) {
  # Url of KeyPathwayMiner website
  url <- kpm_options()$url

  # Generate random UUID for the session if none was provided
  if (is.null(kpm_options()$session_id)) {
    kpm_options(session_id = paste(sample(c(LETTERS[1:6], 0:9), 32, replace = TRUE), collapse = ""))
  }

  # Create settings object and pass kpm_options parameters
  kpmSetup <- setup_kpm(indicator_matrices = indicator_matrices, graph_file = graph_file)

  # Print out settings for debugging purposes
  print(sprintf("url: %s", url))
  print(sprintf("settings: %s", kpmSetup[[1]]))

  # Prepare result object
  remote_result <- NULL

  # Submit
  remote_result <- submit_kpm(kpmSetup)

  return(save_remote_results(remote_result))
}

#' Set up a JSON object in preparation of the job submission
#'
#' @return A list with settings, datasets and graph in JSON format.
setup_kpm <- function(indicator_matrices, graph_file) {
  # Base64 encode datasetfiles files
  dataset_list <- dataset_list(indicator_matrices)

  # Create a run id
  run_id <- paste(sample(c(LETTERS[1:6], 0:9), 6, replace = TRUE), collapse = "")

  # Setup the json settings:
  settings <- rjson::toJSON(
    list(
      parameters = c(
        name = paste("KeyPathwayMineR remote run", run_id),
        algorithm = kpm_options()$algorithm,
        strategy = kpm_options()$strategy,
        removeBENs = tolower(as.character(kpm_options()$remove_bens)),
        unmapped_nodes = kpm_options()$unmapped_nodes,
        computed_pathways = kpm_options()$computed_pathways,
        graphID = kpm_options()$graph_id,
        l_samePercentage = tolower(as.character(kpm_options()$l_same_percentage)),
        samePercentage_val = kpm_options()$same_percentage,
        k_values = list(c(
          val = kpm_options()$k_min, val_step = kpm_options()$k_step, val_max = kpm_options()$k_max,
          use_range = tolower(as.character(kpm_options()$use_range_k)), isPercentage = "false"
        )),

        l_values = list(c(
          val = kpm_options()$l_min, val_step = kpm_options()$l_step, val_max = kpm_options()$l_max,
          use_range = tolower(as.character(kpm_options()$use_range_l)), isPercentage = "false",
          datasetName = paste("dataset", 1, sep = "")
        ))
      ),
      withPerturbation = tolower(as.character(kpm_options()$with_perturbation)),
      perturbation = list(c( # perturbation can be left out, if withPeturbation parameter is set to false.
        technique = kpm_options()$perturbation_technique,
        startPercent = kpm_options()$perturbation_start,
        stepPercent = kpm_options()$perturbation_step,
        maxPercent = kpm_options()$perturbation_max,
        graphsPerStep = kpm_options()$graphs_per_step
      )),
      linkType = kpm_options()$link_type,
      attachedToID = kpm_options()$session_id,
      positiveNodes = kpm_options()$positive_nodes,
      negativeNodes = kpm_options()$negative_nodes
    )
  )

  # Add custom network if provided
  graph <- graph_kpm(graph_file)

  return(list(settings, dataset_list, graph))
}

#' Method for submitting a job to KeyPathwayMinerWeb
#' Submits can be submitted asynchronously (returns immediately)
#' or blocked until completed(returns when job is complete).
#'
#' @param kpmSetup kpmSetup
#'
#' @return Results in json format.
submit_kpm <- function(kpmSetup) {
  withTryCatch(function() {
    # Choose whether it is an asynchronous job or not
    if (kpm_options()$async) {
      url <- paste(kpm_options()$url, "requests/submitAsync", sep = "")
    } else {
      url <- paste(kpm_options()$url, "requests/submit", sep = "")
    }

    # If a default graph is used we should not send the graph attribute
    if (is.null(kpmSetup[[3]])) {
      result <- RCurl::postForm(url,
        kpmSettings = kpmSetup[[1]],
        datasets = kpmSetup[[2]]
      )
    } else {
      result <- RCurl::postForm(url,
        kpmSettings = kpmSetup[[1]],
        datasets = kpmSetup[[2]],
        graph = kpmSetup[[3]]
      )
    }
    # Get results
    jsonResult <- rjson::fromJSON(result)

    # Print status comment to console
    print(jsonResult["comment"])

    # Return results
    return(jsonResult)
  })
}

#'  Save results of the remote run
#'
#'  Method to save the results provided
#'  from the remote run.
#'
#' @param remote_results The results returned from the remote run
#'
#' @return Result object.
save_remote_results <- function(remote_results) {
  configurations <- list()
  graphs <- remote_results$resultGraphs
  if (is.null(graphs)) {
    # If async == TRUE
    return(new("ResultRemote", parameters = kpm_options(), configurations = configurations, json_result = remote_results))
  }
  last_configuration <- ""
  for (i in 1:length(graphs)) {
    # Determine configuration e.g. K=5 and L=20
    configuration <- paste("K-", graphs[[i]]$k, "-L-", graphs[[i]]$l, sep = "")

    # Get interactions
    extracted_pathway_interactions <- graphs[[i]]$edges
    interactions_source <- character()
    interactions_target <- character()

    if (length(extracted_pathway_interactions) != 0) {
      # In case pathway is not empty
      for (j in 1:length(extracted_pathway_interactions)) {
        interactions_source <- c(interactions_source, extracted_pathway_interactions[[j]]$source)
        interactions_target <- c(interactions_target, extracted_pathway_interactions[[j]]$target)
      }
    }

    # Get nodes
    extracted_pathway_nodes <- graphs[[i]]$nodes
    nodes <- character()

    if (length(extracted_pathway_nodes) != 0) {
      for (k in 1:length(extracted_pathway_nodes)) {
        nodes <- c(nodes, extracted_pathway_nodes[[k]]$id)
      }
    }

    # Save results
    if (!configuration %in% names(configurations)) {
      # Create new entry for new configuration if configuration does not exist already
      configurations[[configuration]] <- new("Configuration", configuration = configuration, k = graphs[[i]]$k, l_values = list(L1 = graphs[[i]]$l), pathways = list())
    }

    if (!graphs[[i]]$isUnionSet) {
      pathway <- paste("Pathway-", length(configurations[[configuration]]@pathways) + 1, sep = "")
      configurations[[configuration]]@pathways <- append(
        configurations[[configuration]]@pathways,
        setNames(list(new("Pathway",
          edges = data.frame(source = interactions_source, target = interactions_target),
          nodes = data.frame(nodes = nodes),
          num_edges = length(interactions_source),
          num_nodes = length(nodes)
        )), pathway)
      )
    }
    last_configuration <- configuration
  }

  # We compute the union set with igraph since it provides with more information than we can obtain from the server result
  configurations[[last_configuration]]@union_network <- create_union_network(configuration = configurations[[last_configuration]])

  return(new("ResultRemote", parameters = kpm_options(), configurations = configurations, json_result = remote_results))
}

#### Helper - Methods ####
#'  Encodes a list of datasets
#'
#'  Helper method to encode a list of datasets (indicator matrices)
#'  for the job submission.
#'
#' @param indicator_matrices List of indicator matrices to be encoded
#'
#' @return Encoded list of datasets with their respective questId.
#' @importFrom foreach %do%
dataset_list <- function(indicator_matrices) {
  counter <- 0
  datasetList <- foreach::foreach(indicator.matrix = indicator_matrices) %do% {
    txt.con <- textConnection("tmp.file", "w")
    write.table(indicator.matrix, txt.con, sep = "\t", quote = F, col.names = F, row.names = F)
    enc.file <- RCurl::base64(paste(tmp.file, collapse = "\n"))
    close(txt.con)

    counter <- counter + 1
    c(name = paste("dataset", counter, sep = ""), attachedToID = kpm_options()$session_id, contentBase64 = enc.file)
  }
  return(rjson::toJSON(datasetList))
}

#' Helper method to encode custom network file
#'
#' @param graph_file Path to graph file which should be encoded.
#'
#' @return Encoded network file or NULL if no graph file submitted.
graph_kpm <- function(graph_file) {
  if (!is.null(graph_file)) {
    graph <- base64EncFile(graph_file)
    graph <- rjson::toJSON(c(name = basename(graph_file), graphName = basename(graph_file), attachedToID = kpm_options()$session_id, contentBase64 = graph))
  } else {
    graph <- NULL
  }
  return(graph)
}

#' Helper method for error handling
#'
#' @param surroundedFunc Function in which the TryCatch error handling should be applied.
withTryCatch <- function(surroundedFunc) {
  tryCatch(
    {
      surroundedFunc()
    },
    error = function(e) {
      if ("COULDNT_CONNECT" %in% class(e)) {
        stop("Couldn't connect to url.")
      } else {
        stop(paste("Unexpected error:", e$message))
      }
      return(NULL)
    }
  )
}

#' Method to check up on a submitted job.
#'
#' Useful to monitor the job progress and current status.
#'
#' @param quest_id String. The quest id this job has been attached to.
#'
#' @return Status of the job for given quest_id
get_status <- function(quest_id, url = "https://exbio.wzw.tum.de/keypathwayminer/") {
  withTryCatch(function() {
    url <- paste(url, "requests/runStatus", sep = "")
    print(sprintf("url: %s", url))
    result <- RCurl::postForm(url, questID = quest_id)
    jsonResult <- rjson::fromJSON(result)

    if (tolower(jsonResult["success"]) == "cancelled") {
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
fetch_results <- function(quest_id, url = "https://exbio.wzw.tum.de/keypathwayminer/") {
  withTryCatch(function() {
    url <- paste(url, "requests/results", sep = "")
    print(sprintf("url: %s", url))

    result <- RCurl::postForm(url, questID = quest_id)
    jsonResult <- rjson::fromJSON(result)

    if (tolower(jsonResult["success"]) == "true") {
      return(jsonResult)
    } else {
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
get_networks <- function(url = "https://exbio.wzw.tum.de/keypathwayminer/") {
  kpm_url <- paste(url, "rest/availableNetworks/", sep = "")
  result <- RCurl::getURL(kpm_url)
  jsonResult <- rjson::fromJSON(result)
  networks <- foreach::foreach(network = jsonResult, .combine = append) %do% {
    network[[1]]
  }
  names(networks) <- foreach::foreach(network = jsonResult, .combine = append) %do% {
    network[[2]]
  }
  return(networks)
}

#' Get progress url
#'
#' Get url to see progress in the browser and see the results
#'
#' @param session_id
#'
#' @return Returns progress url for the respective run.
#' @export
quest_progress_url <- function(session_id, url = "https://exbio.wzw.tum.de/keypathwayminer/") {
  kpm_progress_url <- paste(url, "requests/quests?attachedToId=",
    session_id, "&hideTitle=false",
    sep = ""
  )

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
base64EncFile <- function(file_name) {
  return(RCurl::base64(readChar(file_name, file.info(file_name)$size)))
}

#' Check if asynchronous remote run is finished
#'
#' Only for remote execution
#'
#' @param result_object Result of the current run.
#'
#' @return TRUE if results are ready. FALSE if still processing.
#' @export
is_finished <- function(result_object) {
  if (class(result_object) != "ResultRemote") {
    stop("Result object must be of type ResultRemote.")
  }
  completed <- get_status(quest_id = result_object@json_result[["questID"]])$completed
  return(completed)
}

#' Returns result url
#'
#' Only for remote execution
#'
#'  @param result_object Result of the current run.
#'
#'  @return Url to the results on the KPM web server.
#'  @export
get_result_url <- function(result_object) {
  if (class(result_object) != "ResultRemote") {
    stop("Result object must be of type ResultRemote.")
  }
  return(result_object@json_result[["resultUrl"]])
}

#' Fetches result for remote run.
#'
#' Only for remote execution.
#' The job must be completed to get the results.
#' Completion can be checked with is_finished(result_object).
#'
#' @param result_object Result of the current run.
#'
#' @return Remote result object.
#' @export
get_results <- function(result_object) {
  if (class(result_object) != "ResultRemote") {
    stop("Result object must be of type ResultRemote.")
  }
  remote_results <- fetch_results(quest_id = result_object@json_result[["questID"]])
  return(save_remote_results(remote_results))
}
