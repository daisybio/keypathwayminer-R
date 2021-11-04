#' Calculates differentially expressed genes from single cell object
#'
#' @description Calculates differentially expressed genes between multiple groups with reference to a control group
#' using a hurdle model tailored to scRNA-seq data
#'
#' @references Andrew McDavid, Greg Finak and Masanao Yajima (2017). MAST: Model-based
#' Analysis of Single Cell Transcriptomics. R package version 1.2.1.
#' https://github.com/RGLab/MAST/
#'
#' @param sc_obj single cell object of type seurat, singleCellAssay or singlCellExperiment
#' @param covarariate colname of metadata with groups for differential expression
#' @param referenceGroup reference/control group
#' @param designFormula string which will be used as formula in MAST's zlm function
#' @param pvalueThreshold p value cut off after FDR correction for differentially expressed genes
#' @param saveSummary if TRUE saves the result of the differential analysis as a .rds file
#' @param summaryName name of the rds file if saveSummary=TRUE
#' @param FCThreshold Threshold of log 2 fold change
#' @param nameIndicatorMatrix indicator matrix which can be used for kpm() execution
#'
#' @details To use this method, MAST has to be installed (see https://github.com/RGLab/MAST/ )
#'
#' @return dataframe of of putative differentially expressed genes
#' @export
sc_to_indicator_matrix <- function(sc_obj,
                                   covarariate,
                                   referenceGroup,
                                   saveSummary = TRUE,
                                   summaryName  = NULL,
                                   designFormula = NULL,
                                   FCThreshold = 2,
                                   pvalueThreshold = 0.05,
                                   nameIndicatorMatrix = NULL){

  fcHurdletest <- do_diff_testing_via_MAST(sc_obj = sc_obj,
                                           covarariate = covarariate,
                                           referenceGroup = referenceGroup,
                                           summaryName = summaryName,
                                           saveSummary = saveSummary,
                                           designFormula = designFormula)

  filtered_results <- filter_fcHurdleTestResults(fcHurdletest = fcHurdletest,
                                                 FCThreshold = FCThreshold,
                                                 pvalueThreshold = pvalueThreshold)

  create_indicator_from_fcurdleTest(fcHurdletest_filtered = filtered_results,
                                    sc_obj = sc_obj,
                                    nameIndicatorMatrix = nameIndicatorMatrix)

}


#' Calculates differentially expressed genes from single cell object
#'
#' @param sc_obj single cell object of type seurat, singleCellAssay or singlCellExperiment
#' @param covarariate colname of metadata with groups for differential expression
#' @param referenceGroup reference/control group
#' @param saveSummary if TRUE saves the result of the differential analysis as a .rds file
#' @param summaryName name of the rds file if saveSummary=TRUE
#' @param designFormula string which will be used as formula in MAST's zlm function
#'
#' @return data.frame of differential expression analysis results
#' @export
#' @import data.table
do_diff_testing_via_MAST <-
  function(sc_obj,
           covarariate,
           referenceGroup,
           saveSummary = TRUE,
           summaryName = NULL,
           designFormula = NULL
  ) {

    if (!("MAST" %in% installed.packages()))
      stop("Package MAST has to be installed. see https://github.com/RGLab/MAST")

    if(saveSummary & is.null(summaryName)){
      stop("Name for the result has to be given")
    }

    if (is(sc_obj, "SingleCellAssay")) {
      sca <- sc_obj
    } else {
      sca <- create_sca(sc_obj)
    }

    ###new
    sca <- sca[Matrix::rowSums(SummarizedExperiment::assay(sca))!= 0, ]
    #sca <-  sca[rowSums(assay(sca)) != 0, ]
    ###

    SummarizedExperiment::colData(sca)$cngeneson <-
      scale(Matrix::colSums(SummarizedExperiment::assay(sca) > 0))
    condition <-
      factor(SummarizedExperiment::colData(sca)[, covarariate])
    condition <- stats::relevel(x = condition, ref = referenceGroup)
    SummarizedExperiment::colData(sca)$condition <- condition

    if (!is.null(designFormula)) {
      zlmCond <-
        MAST::zlm(formula = stats::as.formula(designFormula) , sca = sca)
    } else {
      frmla <- stats::as.formula(" ~ condition + cngeneson")

      zlmCond <- MAST::zlm(formula = frmla , sca = sca)
    }

    nonCtrlConditions <- setdiff(unique(condition), referenceGroup)
    lrt <- paste0("condition", nonCtrlConditions)

    summaryCond <- MAST::summary(zlmCond, doLRT = lrt)

    summaryDt <- summaryCond$datatable
    summaryDtlogfc <- summaryDt[summaryDt$component == "logFC" & summaryDt$contrast %in% lrt, ]
    summaryDtHurdle <- summaryDt[summaryDt$component == "H" & summaryDt$contrast %in% lrt, ]
    fcHurdletest <-
      merge(summaryDtHurdle[, .(primerid, contrast, `Pr(>Chisq)`)],
            summaryDtlogfc[, .(primerid, contrast, coef)],
            by = c('primerid', 'contrast'))


    colnames(fcHurdletest) <-
      c("gene", "condition", "pvalue", "log2FC")
    fcHurdletest$padj <- stats::p.adjust(fcHurdletest$pvalue, method = "fdr")

    fcHurdletest$condition <-
      factor(fcHurdletest$condition, levels = unique(fcHurdletest$condition))



    if(saveSummary){
      saveRDS(fcHurdletest, paste0(format(Sys.time(), "%Y%m%d_%H%M%S_"), summaryName ,  "_fcHurdleTestSummary.rds"))
    }

    return(fcHurdletest)
  }



#' Filters the differential expression analysis results based on fold change and adjusted pvalue
#'
#' @param fcHurdletest result from do_diff_testing_via_MAST() function
#' @param FCThreshold threshold for the log 2 fold change
#' @param pvalueThreshold threshold for the adjusted p value
#'
#' @return data.frame of filtered differential expression analysis result
#' @export
filter_fcHurdleTestResults <- function(fcHurdletest,
                                       FCThreshold = 2,
                                       pvalueThreshold = 0.05
) {
  fcHurdletest[fcHurdletest$padj < pvalueThreshold & abs(log2FC) > FCThreshold, ]
}


#' Creates indicator matrix from differentail
#'
#' @param fcHurdletest_filtered filtered differential expression analysis result
#' @param sc_obj single cell object used for differential expression analysis
#' @param nameIndicatorMatrix name of indicator matrix to be given
#'
#' @return indicator matrix in form of a data.frame
#' @export
create_indicator_from_fcurdleTest <- function(fcHurdletest_filtered,
                                              sc_obj,
                                              nameIndicatorMatrix = NULL){
  suppressMessages(
    if (is(sc_obj, "SingleCellAssay")) {
      sca <- sc_obj
    } else {
      sca <- create_sca(sc_obj)
    })

  result.list <-
    split(fcHurdletest_filtered, by = "condition", drop = F)

  diff.genes.list <-
    lapply(result.list, function(x) {
      rownames(sca) %in% x$gene
    })
  result <- as.data.frame(do.call(cbind, diff.genes.list))
  result <- result*1
  result <- cbind(rownames(sca), result)

  if(!is.null(nameIndicatorMatrix)){
    data.table::fwrite(result, nameIndicatorMatrix, quote = F, sep="\t", row.names = F, col.names = F)
  }

  return(result)

}


create_sca <- function(obj){
  UseMethod("create_sca", obj)
}


#' Converts Seurat object to SingleCellAssay object
#'
#' Raw counts from the Seurat object are log transformed
#' and used for the creation of a SingleCellAssay object
#'
#' @param seurat_object
#'
#' @return SingleCellAssay object
create_sca.Seurat <- function(seurat_object) {
  log_counts <- log2(seurat_object@assays$RNA@counts + 1)

  fData <- seurat_object[["RNA"]][[]]
  rownames(fData) <- rownames(log_counts)
  cData <- data.frame(seurat_object@meta.data)
  rownames(cData) <- colnames(log_counts)

  MAST::FromMatrix(as.matrix(log_counts), cData, fData, check_sanity = TRUE)
}

#' Converts SingleCellExperiment object to SingleCellAssay object
#'
#' @param sc_object SingleCellExperiment object
#'
#' @return SingleCellAssay object
create_sca.SingleCellExperiment <- function(sc_object) {

  name_first_assay <- names(SummarizedExperiment::assays(sc_object))[1]
  SummarizedExperiment::assay(sc_object, name_first_assay) <- log2(SummarizedExperiment::assay(sc_object, name_first_assay) + 1)
  MAST::SceToSingleCellAssay(sc_object)
}
