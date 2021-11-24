#' Tumor vs Normal
#' Adjust tumor expression by Normal sample expression
#' @importFrom  purrr map_dfc
#' @param data A matrix representing the genomic data such as gene expression data, miRNA expression data.\cr
#' For the matrix, the rows represent the genomic features, and the columns represent the samples.
#' @param tumor A vector contain the names of tumor samples, it must match with `data` column names.
#' @param normal A vector contain the names of normal samples, it must match with `data` column names.
#' And with matched order with `tumor`.
#' @param log whether have log transform the `data`
#' @export
#'
tumor_vs_normal <- function(data, tumor = NULL, normal = NULL, log = FALSE) {
  # check data
  stopifnot(!is.null(tumor), !is.null(normal))
  # get expr
  T_expr <- data[, tumor]
  N_expr <- data[, normal]
  # T vs N
  if (log) {
    temp <- purrr::map_dfc(1:ncol(T_expr), function(x) {
      res <- T_expr[,x] - N_expr[,x]
    })
  } else {
    temp <- purrr::map_dfc(1:ncol(T_expr), function(x) {
      res <- T_expr[,x] / N_expr[,x]
    })
  }
  colnames(temp) <- tumor
  temp <- as.matrix(temp)
  rownames(temp) <- rownames(data)
  return(temp)
}
