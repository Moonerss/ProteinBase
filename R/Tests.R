#' Do t-test
#'
#' @importFrom stats na.omit prcomp t.test var.test
#' @param v1 a (non-empty) numeric vector of data values.
#' @param v2 an optional (non-empty) numeric vector of data values.
#' @param paired a logical indicating whether you want a paired t-test.
#' @param log whether data have log2 transformation.
#' @note Only for three and more repeat experiment.
#' @return
#' Return a log2 fold change value and t-test p value
#'
#' @export
#'

T_test <- function(v1, v2, paired = FALSE, log = FALSE) {
  v1 <- as.numeric(v1)
  v2 <- as.numeric(v2)
  ## Ratio
  if (sum(!is.na(v1)) == 0 | sum(!is.na(v2)) == 0) {
    R_1 <- NA
  } else {
    if (log) {
      R_1 <- mean(v1, na.rm = T) - mean(v2, na.rm = T)
    } else {
      R_1 <- log2(mean(v1, na.rm = T) / mean(v2, na.rm = T))
    }
  }

  if (sum(!is.na(v1)) < 2 | sum(!is.na(v2)) < 2) {
    P_1 <- NA
  } else {
    if (log == FALSE) {
      message('Do log2 transforming ...')
      v1 <- log2(v1)
      v2 <- log2(v2)
    }
    ## 判断方差齐性
    P_var <- var.test(v1, v2, alternative = "two.sided")$p.value
    if (is.na(P_var)) {
      P_1 <- NA
      #v1 <- c(-9.965784,NA,-9.965784); v2 <- c(-9.965784,-9.965784,NA)  #这种情况P_var=NAN
    } else if (P_var < 0.05) {
      P_1 <- tryCatch(P_1 <- t.test(v1 , v2, paired = paired, var.equal = F)$p.value, error = function(e) {P_1 <- NA})
    } else {
      ##p值大于0.05，说明方差齐（没有显著差异）
      P_1 <- tryCatch(P_1 <- t.test(v1 , v2, paired = paired, var.equal = T)$p.value, error = function(e) {P_1 <- NA})
    }
  }
  temp <- c(R_1, P_1)

  if (is.na(temp[1])) {
    names(temp) <- c('logFC', 'P_value')
    return(temp)
  } else{
    # temp[1] <- round(temp[1], digits = 3)
    names(temp) <- c('logFC', 'P_value')
    return(temp)
  }
}


#' Do Wilcox-test
#'
#' @importFrom stats na.omit prcomp wilcox.test
#' @param v1 a (non-empty) numeric vector of data values.
#' @param v2 an optional (non-empty) numeric vector of data values.
#' @param paired a logical indicating whether you want a paired Wilcox-test.
#' @param log whether data have log2 transformation.
#' @note Only for three and more repeat experiment.
#' @return
#' Return a log2 fold change value and Wilcox-test p value
#'
#' @export
#'
Wilcox_test <- function(v1, v2, paired = FALSE, log = FALSE) {
  v1 <- as.numeric(v1)
  v2 <- as.numeric(v2)
  ## Ratio
  if (sum(!is.na(v1)) == 0 | sum(!is.na(v2)) == 0) {
    R_1 <- NA
  } else {
    if (log) {
      R_1 <- mean(v1, na.rm = T) - mean(v2, na.rm = T)
    } else {
      R_1 <- log2(mean(v1, na.rm = T) / mean(v2, na.rm = T))
    }
  }

  if (sum(!is.na(v1)) < 2 | sum(!is.na(v2)) < 2) {
    P_1 <- NA
  } else {
    if (log == FALSE) {
      message('Do log2 transforming ...')
      v1 <- log2(v1)
      v2 <- log2(v2)
    }
    P_1 <- tryCatch(P_1 <- wilcox.test(v1 , v2, paired = paired)$p.value, error = function(e) {P_1 <- NA})
  }
  temp <- c(R_1, P_1)

  if (is.na(temp[1])) {
    names(temp) <- c('logFC', 'P_value')
    return(temp)
  } else{
    # temp[1] <- round(temp[1], digits = 3)
    names(temp) <- c('logFC', 'P_value')
    return(temp)
  }
}



#' @title log2 transformation of expression matrix
#' @description Do the expression matrix needs log2 transformation
#' @usage log2expr(exprMat, gene_col = NULL)
#' @param exprMat gene expression matrix with row as genes and sample in the column
#' @param gene_col column name or number of gene id, default is NULL, choose the first column
#' @return a gene expression matrix after log2 transformation
#' @details The function will first check whether the expression matrix has undergone
#'   log2 transformation; if the expression matrix have done log2 transformation, return
#'   the raw expression matrix, else do the log2 transformation
#' @importFrom cli cli_alert_info
#' @importFrom stats quantile
#' @name log2expr
#' @export
#'
log2expr <- function(exprMat = NULL, gene_col = NULL){

  ## get gene expression
  if (is.data.frame(exprMat)) {
    if (is.null(gene_col)) {
      message("`gene_col = NULL`, choose first column as gene id.")
      gene_col <- 1
      colnames(exprMat)[1] <- "genes_id"
    } else if (is.character(gene_col)) {
      colnames(exprMat)[which(colnames(exprMat) == gene_col)] <- "genes_id"
    } else if (is.numeric(gene_col)) {
      colnames(exprMat)[gene_col] <- "genes_id"
    }
    exprMat <- exprMat %>% column_to_rownames(var = "genes_id") %>% as.matrix()
  }

  ## check whether done log2 transformation
  qx <- as.numeric(quantile(exprMat, c(0., 0.25, 0.5, 0.75, 0.99, 1.0), na.rm=T))
  loged <- (qx[5] > 100) ||
    (qx[6]-qx[1] > 50 && qx[2] > 0) ||
    (qx[2] > 0 && qx[2] < 1 && qx[4] > 1 && qx[4] < 2)

  if (loged) {
    exprMat[exprMat < 0] <- 0
    exprMat <- log2(exprMat)
    cli::cli_alert_info("log2 transformation finished!")
  } else {
    cli::cli_alert_info("log2 transformation is not necessary.")
  }

  return(exprMat)
}
