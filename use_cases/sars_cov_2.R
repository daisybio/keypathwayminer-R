# Load libraries ----------------------------------------------------------
library("GEOquery")
library("DESeq2")
library("KeyPathwayMineR")
library("edgeR")
library("tibble")
library("ggplot2")
library("simpIntLists")
library("ggpubr")
# Get data ----------------------------------------------------------------
# Nasopharyngeal swabs from 430 individuals with SARS-COV-2 and 54 negative controls
getGEOSuppFiles("GSE152075")
nasopharyngeal_swabs_raw_counts <- as.data.frame.matrix(
  read.delim("GSE152075/GSE152075_raw_counts_GEO.txt.gz", sep = " ")
)

control <- colnames(nasopharyngeal_swabs_raw_counts)[startsWith(x = colnames(nasopharyngeal_swabs_raw_counts), prefix = "NEG_")]
sars_cov_2 <- colnames(nasopharyngeal_swabs_raw_counts)[startsWith(x = colnames(nasopharyngeal_swabs_raw_counts), prefix = "POS_")]
nasopharyngeal_swabs_raw_counts <- nasopharyngeal_swabs_raw_counts[, c(control, sars_cov_2)]

# DE analysis: TMM normalization -----------------------------------------------------------------
# TMM normalization of raw counts using edgeR and creation of z-score matrix
# Define contrast groups
contrast <- c(rep("Negative Control", 54), rep("SARS-COV2", 430))
# Save count matrix as dge_list
dge_list <- DGEList(counts = nasopharyngeal_swabs_raw_counts, group = contrast)

# Compute normalization factors with TMM
tmm_normalization_factors <- calcNormFactors(dge_list, method = "TMM")

# Normalize counts
norm_counts <- cpm(tmm_normalization_factors)
# Filter out genes that have very low counts across the samples
keep <- filterByExpr(y = norm_counts, min.count = 2, group = contrast)
norm_counts <- norm_counts[keep, ]

# DE Analysis: Z-score computation ----------------------------------------
z_score_matrix <- compute_z_scores(norm_counts, controls = c(1:54), cases = c(55:484))
# Determine optimal cutoff
z_score_1.5 <- z_score_matrix
z_score_2 <- z_score_matrix
z_score_3 <- z_score_matrix
z_score_4 <- z_score_matrix
z_score_5 <- z_score_matrix
# Cutoff |temp|> 1.5
z_score_1.5[z_score_1.5 >= 1.5 | z_score_1.5 <= -1.5] <- 1
z_score_1.5[z_score_1.5 != 1] <- 0
z_score_1.5 <- z_score_1.5 %>% rownames_to_column(var = "hgnc_symbol")
# Cutoff |temp|> 2
z_score_2[z_score_2 >= 2 | z_score_2 <= -2] <- 1
z_score_2[z_score_2 != 1] <- 0
z_score_2 <- z_score_2 %>% rownames_to_column(var = "hgnc_symbol")

# Cutoff |temp|> 3
z_score_3[z_score_3 >= 3 | z_score_3 <= -3] <- 1
z_score_3[z_score_3 != 1] <- 0
z_score_3 <- z_score_3 %>% rownames_to_column(var = "hgnc_symbol")

# Cutoff |temp|> 4
z_score_4[z_score_4 >= 4 | z_score_4 <= -4] <- 1
z_score_4[z_score_4 != 1] <- 0
z_score_4 <- z_score_4 %>% rownames_to_column(var = "hgnc_symbol")

# Cutoff |temp|> 5
z_score_5[z_score_5 >= 5 | z_score_5 <= -5] <- 1
z_score_5[z_score_5 != 1] <- 0
z_score_5 <- z_score_5 %>% rownames_to_column(var = "hgnc_symbol")



# Prepare biological interaction network in iGraph format -----------------
human_biogrid_network <- findInteractionList(organism = "human", idType = "Official")
from <- c()
to <- c()
for (entry in human_biogrid_network) {
  from <- c(from, rep(entry$name, length(entry$interactors)))
  to <- c(to, entry$interactors)
}
edges <- data.frame(from = from, to = to)
human_biogrid_network <- graph_from_data_frame(d = edges, directed = FALSE)
# Remove duplicate edges and loops
human_biogrid_network <- simplify(human_biogrid_network)

# KPM execution -----------------------------------------------------------
devtools::install_github("baumbachlab/keypathwayminer-R", build_vignettes = TRUE, force = TRUE)
library("KeyPathwayMineR")

human_biogrid_network <- readRDS("CoV/human_biogrid_network.rds")
z_score_1.5 <- readRDS("CoV/z_score_1_5.rds")
z_score_2 <- readRDS("CoV/z_score_2.rds")
z_score_3 <- readRDS("CoV/z_score_3.rds")

options(java.parameters = "-Xmx64000m")
kpm_options(
  execution = "Local",
  strategy = "INES",
  algorithm = "Greedy",
  l_min = 20, l_step = 20, l_max = 220,
  k_min = 2, k_step = 2, k_max = 20,
  use_range_l = TRUE,
  use_range_k = TRUE
)

# Result z 1.5 -------------------------------------------------------------
results_z_1.5 <- kpm(graph = human_biogrid_network, indicator_matrices = z_score_1.5)
pathway_comparison_plots(results_z_1.5)
visualize_result(result = results_z_1.5)
# Result z 2 -------------------------------------------------------------
results_z_2 <- kpm(graph = human_biogrid_network, indicator_matrices = z_score_2)
pathway_comparison_plots(results_z_2)
visualize_result(result = results_z_2)
# Result z 3 -------------------------------------------------------------
results_z_3 <- kpm(graph = human_biogrid_network, indicator_matrices = z_score_3)
pathway_comparison_plots(results_z_3)
visualize_result(result = results_z_3)