# # # ### Example calls to demonstrate the usage of the KeyPathwayMinerWeb RESTful API ### Authors: Markus List and Martin Dissing-Hansen
# # # ### ###
# # #
# # # # Load test data and R functions to access KeyPathwayMiner Web source('restful_utils')
# # #
# # # KeyPathWayMiner URL
# # url <- "https://exbio.wzw.tum.de/keypathwayminer/"
# #
# #
#
# huntington_disease_up <- as.data.frame.matrix(read.delim("/Users/konstantinos/Workspace/keypathwayminer/Rkpm/inst/extdata/api_testing/matrix-hd-up.dat",
#     header = FALSE))
# huntington_disease_down <- as.data.frame.matrix(read.delim("/Users/konstantinos/Workspace/keypathwayminer/Rkpm/inst/extdata/api_testing/matrix-hd-down.dat",
#     header = FALSE))
#
# huntington_list <- list(huntington_disease_up, huntington_disease_down)
#
# # # Generate a unique identifier for this session
# # attached_to_id <- paste(sample(c(LETTERS[1:6], 0:9), 32, replace = TRUE), collapse = "")
# #
# # # List available networks
# # availableNetworks <- get_networks(url)
# # print(availableNetworks)
# #
# # # Use the I2D network
# # I2D.id <- get_networks(url)[["I2D Homo_sapiens entrez"]]
# #
# # # kpm_options(async = FALSE, l_min = 8, k_min = 1,
# # #             strategy = 'INES', remove_bens = TRUE,
# # #             graph_id = I2D.id, use_range = FALSE,
# # #             link_type = 'OR')
# # #
# # # kpm(list(huntington_disease_up))
# #
# # # # Start with a simple run on one dataset with fixed parameters for K and L using INES in a blocking call.
# # # # result.fixed.parameters.one.dataset <- call.KPM(list(huntington_disease_up), attached_to_id, async=FALSE, Lmin=8, Kmin=1,
# # # # strategy='INES', removeBENs=TRUE, graphID=I2D.id, graph.file=NULL, range=FALSE, linkType='OR')
# # #
# # # # results print(paste('browse to', result.fixed.parameters.one.dataset$resultUrl, 'to see the results'))
# # #
# # # # Start a ranged run on one dataset with fixed parameters for K and L using INES in a blocking call.
# # # # result.ranged.parameters.one.dataset <- call.KPM(list(huntington_disease_up), attached_to_id, async=FALSE, Lmin=4, Lmax=8,
# # # # Lstep=4, Kmin=0, Kmax=1, Kstep=1, strategy='INES', removeBENs=TRUE, graphID=I2D.id, graph.file=NULL, range=TRUE, linkType='OR')
# # #
# # # # results print(paste('browse to', result.ranged.parameters.one.dataset$resultUrl, 'to see the results'))
# # #
# # #
# # # # Start a run with INES and K = 1 where we use a percentage of L = 10% for the two datasets.  Note: The web service does not allow
# # # # individual fixed parameters to be set for each dataset at the moment. result.fixed.percentage.two.datasets <-
# # # # call.KPM(huntington_list, attached_to_id, async=TRUE, l_same_percentage = TRUE, strategy='INES', removeBENs=TRUE,
# # # # same_percentage=10, Kmin=1, graphID=I2D.id, graph.file=NULL, range=FALSE, linkType='OR')
# # #
# # # # Now we want to monitor progress in the client or in the browser:
# # #
# # # # extract job id (called quest id) quest.id <- result.fixed.percentage.two.datasets$questID
# # #
# # # # open the result page where you can monitor the progress of both tasks print(quest.progress.url(url, attached_to_id))
# # #
# # # # check status of the second job getStatus(url, quest.id)
# # #
# # # # if complete, download results of the second job if(status$completed){ result.fixed.percentage.two.datasets <- getResults(url,
# # # # quest.id) }
