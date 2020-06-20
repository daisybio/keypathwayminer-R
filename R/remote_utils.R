#### Main - Methods ####
#' Method used to create a job submission
#'
#' @param indicator_matrices List. Indicator matrices that will be used.
#' @param attached_to_id String. The quest id this job has been attached to.
#' @param async Boolean. Submit a new job asynchronously?
#' @param ... Options. Run parameters.
#' @return Results object with runId, resultGraphs, comment, succes,resultUrl
#'  and questId.
#' @examples
#'call_kpm_remote
# (list(huntington_disease_up), attached_to_id,
#'         async=FALSE, Lmin=8, Kmin=1, strategy="INES",
#'         removeBENs=TRUE, graphID=I2D.id, graph.file=NULL,
#'         range=FALSE, linkType="OR")
call_kpm_remote <- function(matrices, graph_file = NULL){

  #Url of KeyPathwayMiner website
  url <- kpm_options()$url

  #Generate random UUID for the session if none was provided
  if(is.null(kpm_options()$quest_id)){
    kpm_options(quest_id = paste(sample(c(LETTERS[1:6],0:9),32,replace=TRUE),collapse=""))
  }

  #Create settings object and pass kpm_options parameters
  kpmSetup <- setup_kpm(indicator_matrices = matrices,
                        graph.file = graph_file,
                        algorithm = kpm_options()$algorithm,
                        strategy =  kpm_options()$strategy,
                        graphID = kpm_options()$graph_id,
                        removeBENs = kpm_options()$remove_bens,
                        range = kpm_options())

  #prepare result object
  result <- NULL

  #print out settings for debugging purposes
  print(sprintf("url: %s", url))
  print(sprintf("settings: %s", kpmSetup[[1]]))

  #submit
  result <- submit_kpm(url, kpmSetup, async)

  return(result)
}


#' Function to set up a JSON object in preparation of the job submission
#'
#' @param indicator_matrices
#' @param algorithm
#' @param strategy
#' @param graph_id
#' @param graph_file
#' @param remove_bens
#' @param range
#' @param k_min
#' @param k_max
#' @param k_step
#' @param l_min
#' @param l_max
#' @param l_step
#' @param l_same_percentage
#' @param same_percentage
#' @param attached_to_id
#' @param computed_pathways
#' @param with_perturbation
#' @param unmapped_nodes
#' @param link_type
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
  run_id <- paste(sample(c(LETTERS[1:6],0:9),6,replace=TRUE),collapse="")

  # Setup the json settings:
  settings <- toJSON(
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


# method for submitting a job to KeyPathwayMinerWeb asynchronously (returns immediately) or blocking (returns when job is complete)
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
      result <- RCurl::postForm(kpm_options()$url, kpmSettings = kpmSetup[[1]],
                                datasets = kpmSetup[[2]])
    }else{
      result <- RCurl::postForm(kpm_options()$url, kpmSettings = kpmSetup[[1]],
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
# Helper method to encode a list of datasets (indicator matrices) for the job submission
dataset_list <- function(indicator_matrices, attached_to_id){
  counter <- 0
  datasetList <- foreach(indicator.matrix = indicator_matrices) %do% {
    txt.con <- textConnection("tmp.file", "w")

    write.table(indicator.matrix, txt.con, sep="\t",quote=F, col.names = F, row.names = F)
    enc.file <- base64(paste(tmp.file, collapse="\n"))
    close(txt.con)
    counter <- counter + 1
    c(name=paste("dataset", counter, sep=""), attachedToID=attached_to_id, contentBase64=enc.file)
  }
  return(toJSON(datasetList))
}

# Helper method to encode custom network file
graph_kpm <- function(graph.file, attached_to_id){
  if(!is.null(graph.file)){
    graph <- base64EncFile(graph.file)
    graph <- toJSON(c(name=basename(graph.file), attachedToID=attached_to_id, contentBase64=graph))
  }else{
    graph <- NULL
  }
  return(graph)
}


# helper method for error handling
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



# Method to check up on a submitted job. Useful to monitor its progress and current status.
getStatus <- function(url, questId){
  withTryCatch(function(){
    url <- paste(url, "requests/runStatus", sep="")
    print(sprintf("url: %s", url))
    result <- postForm(url, questID=questId)
    jsonResult <- fromJSON(result)

    if(tolower(jsonResult["success"]) == "cancelled"){
      print("Run has been cancelled.")
      return
    }

    print(jsonResult["completed"])
    print(jsonResult["progress"])

    return(jsonResult)
  })
}

# Once the run is complete, we can obtain the results
getResults <- function(url, questId){
  withTryCatch(function(){
    url <- paste(url, "requests/results", sep="")
    print(sprintf("url: %s", url))

    result <- postForm(url, questID=questId)
    jsonResult <- fromJSON(result)

    if(tolower(jsonResult["success"]) == "true"){
      return(jsonResult)
    }
    else{
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
#' getNetworks("https://exbio.wzw.tum.de/keypathwayminer/")
# Get a data frame of available networks
getNetworks <- function(url = "https://exbio.wzw.tum.de/keypathwayminer/"){
  kpm.url <- paste(url, "rest/availableNetworks/", sep="")
  result <- getURL(kpm.url)
  jsonResult <- fromJSON(result)
  networks <- foreach(network = jsonResult, .combine=append) %do% {network[[1]]}
  names(networks) <- foreach(network = jsonResult, .combine=append) %do% {network[[2]]}
  return(networks)
}

# Get url to see progress in the browser and see the results
quest.progress.url <- function(url, attached_to_id){
  kpm.url <- paste(url, "requests/quests?attachedToId=", sep="")
  paste(kpm.url, attached_to_id, "&hideTitle=false", sep="")
}
# Helper method for base64 encoding. Needed to transfer network and dataset files #
base64EncFile <- function(fileName){
  return(base64(readChar(fileName, file.info(fileName)$size)))
}
