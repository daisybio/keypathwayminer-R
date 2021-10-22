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
compute_z_scores <- function(norm_counts, controls, cases) {
  # Compute means and standard_deviations from the control groups for every gene
  gene_means <- rowMeans(norm_counts[, controls], na.rm = TRUE)
  gene_standard_deviations <- apply(X = norm_counts[, controls], MARGIN = 1, FUN = sd, na.rm = TRUE)
  # Compute z-score of case samples
  z_score_matrix <- apply(X = norm_counts[, cases], MARGIN = 2, FUN = function(x) {
    (x - gene_means) / gene_standard_deviations
  })
  # Remove NaN values
  z_score_matrix <- na.omit(z_score_matrix)
  return(as.data.frame(z_score_matrix))
}

#' Saves indicator matrix as a file.
#'
#' @param matrix Indicator or numerical matrix.
#' @param path Path where the file should be saved.
#'
#' @export
export_matrix <- function(matrix, path) {
  write.table(matrix,
              file = path,
              quote = FALSE,
              sep = "\t",
              row.names = FALSE,
              col.names = FALSE
  )
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
