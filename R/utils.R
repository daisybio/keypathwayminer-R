#' Checks the version of the JVM.
#'
#' @return TRUE if Java version 1.8 or higher is detected, FALSE otherwise
#' @export
#'
#' @examples
#' test_jvm()
test_jvm <- function() {
  # deterrmine java version
  java_version <- .jcall("java/lang/System", "S", "getProperty", "java.runtime.version")

  message(paste("Java version detected:", java_version))
  if (startsWith(java_version, "1.8")) {
    message("\tA Java virtual machine is available and has a supported version.")
    return(TRUE)
  } else if (startsWith(java_version, "9") ||
    startsWith(java_version, "10") ||
    startsWith(java_version, "11") ||
    startsWith(java_version, "12") ||
    startsWith(java_version, "13") ||
    startsWith(java_version, "14") ||
    startsWith(java_version, "15") ||
    startsWith(java_version, "16") ||
    startsWith(java_version, "17")) {
    warning("KeyPathwayMineR was tested and build with Java 1.8.\nIf you encounter problems with the newer versions of Java, we recommend switching to Java 1.8.")
    message("\tA Java virtual machine is available and has a supported version.")
    return(TRUE)
  } else {
    warning("Java version other than 1.8 detected.\nMake sure you are using Java 1.8 or later. Versions older than Java 1.8 are not supported.")
    return(FALSE)
  }
}

#' Resets kpm_options to default values
#'
#' Default options:
#'  execution = "Remote",
#'  async = TRUE,
#'  quest_id = NULL,
#'  url = "https://exbio.wzw.tum.de/keypathwayminer/",
#'  algorithm = "Greedy",
#'  graph_id = 1,
#'  strategy = "GLONE",
#'  remove_bens = FALSE,
#'  use_range_k = FALSE, k_min = 1, k_max = 3, k_step = 1,
#'  use_range_l = FALSE, l_min =0, l_max = 0, l_step = 1,
#'  l_same_percentage = FALSE, same_percentage = 0,
#'  computed_pathways = 20,
#'  perturbation_start = 10,
#'  perturbation_step = 10,
#'  perturbation_max = 20,
#'  graphs_per_step = 1,
#'  with_perturbation = FALSE,
#'  perturbation_technique = "nodeswap",
#'  unmapped_nodes = "Add to negative list",
#'  link_type = "OR",
#'  resource_folder = "/extdata",
#'  properties_file = "kpm.properties"
#'
#' @export
#'
#' @return Reseted option_manager
reset_options <- function() {
  settings::reset(kpm_options)
  message("Utils: Options reseted successfully.")
}

#' Convertes numerical matrix to an indicator matrix
#'
#' Users can select numerical matrix and upload a matrix of numerical values, e.g. p-values or fold changes.
#' These can then be converted to an indicator matrix on the fly with a user-defined threshold.
#'
#' @param numerical_matrix Numerical matrix provided as data.frame.
#' The first row consists of gene_ids and the matrix must not have a header.
#' @param operator Operator for converting the matrix. Either "<" or ">".
#' @param threshold Threshold to use for converting the matrix.
#'
#' @return Indicator matrix.
#' @export
to_indicator_matrix <- function(numerical_matrix, operator = "<", threshold = 0.05) {
  if (!is.null(numerical_matrix) & is.data.frame(numerical_matrix)) {
    # Split gene_id and expression values
    ids <- numerical_matrix[, 1]
    numerical_matrix <- numerical_matrix[, 2:length(numerical_matrix[])]
    if (operator == ">") {
      # if > threshold -> active
      numerical_matrix <- ifelse(numerical_matrix > threshold, 1, 0)
      # Prepend gene_ids
      return(data.frame(ID = ids, numerical_matrix))
    } else if (operator == "<") {
      # if < threshold -> active
      numerical_matrix <- ifelse(numerical_matrix < threshold, 1, 0)
      # Prepend gene_ids
      return(data.frame(ID = ids, numerical_matrix))
    } else {
      stop("Please provide a valid operator (< or >).")
    }
  } else {
    stop("Please provide a valid matrix.")
  }
}

#' Computes z-score values of case samples
#'
#' Computes z-scores of the genes in the case samples
#' in contrast to the control samples.
#'
#' @param norm_counts Normalized counts as data.frame in TMM, TPM or FPKM.
#' @param controls Vector with the column numbers of the control samples.
#' @param cases Vector with the column numbers of the case samples.
#'
#' @return Data.frame with the z.score values of the case samples.
#' @export
#'
#' @examples
#' compute_z_scores(norm_counts, c(1, 2), c(3, 4))
compute_z_scores <- function(norm_counts, controls, cases) {
  # Compute means and standard_deviations from the control groups for every gene
  gene_means <- rowMeans(norm_counts[, controls], na.rm = TRUE)
  gene_standard_deviations <- apply(X = norm_counts[, controls], MARGIN = 1, FUN = sd, na.rm = TRUE)
  # Compute z-score of case samples
  z_score_matrix <- apply(X = norm_counts[, cases], MARGIN = 2, FUN = function(x) {
    (x - gene_means) / gene_standard_deviations
  })
  return(z_score_matrix)
}
