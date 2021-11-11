library(KeyPathwayMineR)

# prepare single cell data from limb muscle
limb_muscle_droplet <- readRDS("droplet.normalized.Limb_Muscle.rds")

colData(limb_muscle_droplet)$age_class <- as.character(colData(limb_muscle_droplet)$mouse.id)
ind_young <- grep("^[13]-" ,colData(limb_muscle_droplet)$age_class)
colData(limb_muscle_droplet)$age_class[ind_young] <- "young"

#filter for mesenchymal stem cell
limb_muscle_msc <- limb_muscle_droplet[,colData(limb_muscle_droplet)$cell_ontology_class == "mesenchymal stem cell"]

# find differentially expressed genes
sc_to_indicator_matrix(sc_obj =  limb_muscle_msc,
                        covarariate = "age_class",
                        referenceGroup = "young",
                        saveSummary=F,
                        FCThreshold = 2,
                        designFormula = " ~ condition + cngeneson + sex",
                        pvalueThreshold = 0.05,
                        nameIndicatorMatrix = "limbMuscleMSC_youngVsOld_ngenes_sex_log2.tsv")



#filter for skeletal muscle satellite cell
limb_muscle_smsc <- adata_temp[,colData(adata_temp)$cell_ontology_class == "skeletal muscle satellite cell"]

# find differentially expressed genes
sc_to_indicator_matrix(sc_obj =  limb_muscle_smsc,
                       covarariate = "age_class",
                       referenceGroup = "young",
                       saveSummary=F,
                       FCThreshold = 2,
                       designFormula = " ~ condition + cngeneson + sex",
                       pvalueThreshold = 0.05,
                       nameIndicatorMatrix = "LimbMuscleSMSC_youngVsOld_ngenes_sex_log2.tsv")



options(java.parameters = "-Xmx64000m")

path_indicator_matrix <- "LimbMuscleMSC_youngVsOld_ngenes_sex_log2.tsv"
ind_matrix <- as.data.frame(data.table::fread(path_indicator_matrix))
##biogrid network of mus musculus which only contains genes which were expressed in the limb muscle
sample_network <- "filtered_biogrid_mm.sif"

kpm_options(
  execution = "Local",
  strategy = "INES",
  algorithm = "Greedy",
  use_range_l = TRUE,
  l_min = 0,
  l_max = 6,
  l_step = 1,
  use_range_k = TRUE,
  k_min = 5,
  k_max = 10,
  k_step = 1
)


result_msc <- kpm(graph = sample_network, indicator_matrices = ind_matrix)
saveRDS(result_msc, "MSC_INESL0T06_K0To10_woBN.rds")




path_indicator_matrix <- "LimbMuscleSMSC_youngVsOld_ngenes_sex_log2.tsv"
ind_matrix <- as.data.frame(data.table::fread(path_indicator_matrix))

kpm_options(
  execution = "Local",
  strategy = "INES",
  algorithm = "Greedy",
  use_range_l = TRUE,
  l_min = 1,
  l_max = 6,
  l_step = 1,
  use_range_k = TRUE,
  k_min = 5,
  k_max = 10,
  k_step = 1
)

result_smsc <- kpm(graph = sample_network, indicator_matrices = ind_matrix)
saveRDS(result_smsc, "SMSC_INESL1T06_K0To10_woBN.rds")
