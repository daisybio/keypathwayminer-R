#### Setup ####
# Load and attach devtools package
library(devtools)

# Install KeyPathwayMineR from github and build vignettes
install_github("baumbachlab/keypathwayminer-R", build_vignettes = TRUE)

# Load and attach KeyPathwayMineR
library(KeyPathwayMineR)

#### Prepare data ####
huntington_disease_down <- as.data.frame.matrix(read.delim(
  system.file(package = "KeyPathwayMineR",
              "extdata/datasets",
              "huntington-gene-expression-DOWN.txt"),header = FALSE))

huntington_disease_up <- as.data.frame.matrix(read.delim(
  system.file(package = "KeyPathwayMineR",
              "extdata/datasets",
              "huntington-gene-expression-UP.txt"),header = TRUE))

huntington_disease_up<- to_indicator_matrix(numerical_matrix  = huntington_disease_up,
                                            operator = "<",threshold = 0.05)

sample_network = system.file("extdata", "sampleNetwork.sif", package="KeyPathwayMineR")

# Get overview of available networks
 get_networks()

#### Remote execution ####
kpm_options(execution = "Remote",
            async = TRUE,
            strategy = "INES",
            remove_bens = TRUE,
            algorithm = "Greedy",
            l_same_percentage = TRUE, same_percentage = 10,
            k_min = 2,
            graph_id = 13,
            link_type = "OR")

# Add the two datasets to a list
indicator_matrices <- list(colon_expression_up, huntington_disease_up)

# Run kpm
run <- kpm(indicator_matrices = indicator_matrices)

# Extract job ID (called quest id)
quest_id <- run$questID

# Open the result page where you can  monitor the progress of both tasks
print(quest_progress_url(session_id = kpm_options()$session_id))
##
#Get status of the Job and if complete download the resuls
if(get_status(quest_id = quest_id)$completed){
  results_example_3 <- get_results(quest_id = quest_id)
}

#### Local execution ####
# Reset settings
reset_options()

# Combine multiple datasets:
kpm_options(execution = "Local",
            strategy = "GLONE",
            algorithm = "Greedy",
            l_min = c(5, 2),
            link_type = "OR",
            resource_folder = system.file(package = "KeyPathwayMineR", "extdata/"),
            properties_file = system.file(package = "KeyPathwayMineR",  "kpm.properties"))
# Run kpm
kpm(graph_file = sample_network, indicator_matrices  = indicator_matrices)

