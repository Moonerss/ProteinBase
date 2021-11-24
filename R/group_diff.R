#' run differential expression analysis by group
#'
#' differential expression analysis by group
#'
#' @importFrom dplyr pull mutate rename
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom rlang .data
#' @importFrom stats p.adjust
#' @param data the data matrix to calculate the SD.
#' For the matrix, the rows represent the genomic features, and the columns represents the samples.
#' @param group A data frame contain two columns. The first column is sample name matched with colnames of data,
#' The second column is the cluster label of samples.
#' @param method The methods to calculate the differential features
#' @param log2FC The log2 fold change cutoff, default 1
#' @param pval The p value cutoff, default 0.05
#' @param fdr The FDR cutoff, default 0.05
#' @param part Select the unregulated features
#' @export
group_diff <- function(data, group, method = c('wilcox', 'Ttest'),
                       log2FC = 1, pval = 0.05, fdr = 0.05,
                       part = c('all', 'up', 'down')) {
  ## check samples
  colnames(group) <- c('ID', 'Type')
  if (ncol(data) != length(unique(group[,1]))) {
    stop('The sample in `group` not all matched with `data`')
  }

  ## check data whether log2 transformed
  log2s <- check_log2(data)
  if (!log2s) {
    message('Log2 transformed the data ...')
    data <- log2(data)
  }

  ## get group data
  all_group_data <- purrr::map(unique(group[,2]), function(x) {
    special_group <- group %>% dplyr::filter(.data$Type == x) %>% pull(.data$ID) %>% unique()
    other_group <- group %>% dplyr::filter(.data$Type != x) %>% pull(.data$ID) %>% unique()
    return(list(special_group = special_group, other_group = other_group))
  })
  names(all_group_data) <- unique(group[,2])

  ## diff analysis
  method <- match.arg(method)
  message('Use ', method, ' for diff analysis...')
  diff_res <- purrr::map(all_group_data, function(x) {
    if (method == 'wilcox') {
      res <- apply(data, 1, function(y) {Wilcox_test(y[x$special_group], y[x$other_group], log = T)}) %>%
        t() %>% as.data.frame()
    } else if (method == 'Ttest') {
      res <- apply(data, 1, function(y) {T_test(y[x$special_group], y[x$other_group], log = T)}) %>%
        t() %>% as.data.frame()
    }
  })

  ## log2FC
  ## fold_res <- purrr::map(all_group_data, function(x) {
  ##   fold <- apply(data, 1, function(y) {mean(y[x$special_group], na.rm = TRUE) - mean(y[x$other_group], na.rm = TRUE)})
  ## })

  ## all result
  all_res <- purrr::map(diff_res, function(x) {
    x %>%
      rownames_to_column(var = 'Protein accession') %>%
      rename(pvalue = .data$P_value) %>%
      mutate(FDR = as.numeric(p.adjust(.data$pvalue, method = 'BH'))) %>%
      as.data.frame()
  })

  ## select proteins
  p1 <- 'Select genes with condition:\n'
  p2 <- paste0(paste0('p < ', pval), paste0(', logFC > ', log2FC), paste0(', FDR <', fdr))
  p3 <- paste0(paste0('p < ', pval), paste0(', logFC < ', -log2FC), paste0(', FDR <', fdr))
  part <- match.arg(part)
  if (part == 'up') {
    message(p1, p2)
    res <- all_res %>% purrr::map(dplyr::filter, .data$pvalue < pval & .data$logFC > log2FC & .data$FDR < fdr) %>%
      purrr::map(mutate, `Regulated Type` = 'Up')
  } else if (part == 'down') {
    message(p1, p3)
    res <- all_res %>% purrr::map(dplyr::filter, .data$pvalue < pval & .data$logFC < -log2FC & .data$FDR < fdr) %>%
      purrr::map(mutate, `Regulated Type` = 'Down')
  } else if (part == 'all') {
    message(p1, '1. ', p2, '\n', '2. ', p3)
    res <- all_res %>% purrr::map(dplyr::filter, (.data$pvalue < pval & .data$logFC > log2FC & .data$FDR < fdr)|(.data$pvalue < pval & .data$logFC < -log2FC & .data$FDR < fdr)) %>%
      purrr::map(function(x) {x %>% mutate(`Regulated Type` = ifelse(.data$logFC > log2FC, "Up", ifelse(.data$logFC < -log2FC, "Down", "")))})
  } else {
    stop('The `part` argument must be `up`, `down` or `all`')
  }

  ## return
  return(res)
}




# check whether done log2 transformation
# logical value, FALSE not log2, TRUE, log2ed
#' @name check_log2
#' @title Check whether log2 transformed
#' @description \code{check_log2} can check data whether have a log2 transformation
#' @param mat a numeric matrix or a numeric vector for log2 transformation
#' @export
#' @examples
#' check_log2(1:10)
#' check_log2(matrix(log2(1:10)))
check_log2 <- function(mat) {
  qx <- as.numeric(quantile(mat, c(0., 0.25, 0.5, 0.75, 0.99, 1.0), na.rm=T))
  loged <- (qx[5] > 100) || (qx[6]-qx[1] > 50 && qx[2] > 0) || (qx[2] > 0 && qx[2] < 1 && qx[4] > 1 && qx[4] < 2)
  if (loged) {
    message('The data don\'t log2 transformed!')
    return(FALSE)
  } else {
    message('The data have log2 transformed!')
    return(TRUE)
  }
}
