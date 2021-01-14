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
