
#' @name na_ratio
#' @title NA ratio in data
#' @description This function get the NA ratio in the whole data
#' @param x a matrix with any type data
#' @return a value of NA ratio
#' @export
#'
na_ratio <- function(x){
  sum(is.na(x))/dim(x)[1]/dim(x)[2]
}

#' @name na_ratio_row
#' @title NA ratio of each row in data
#' @description This function get the NA ratio in the row of data
#' @param x a matrix with any type data
#' @return a vector value of NA ratio in each row
#' @export
#'
na_ratio_row <- function(x) {
  apply(x, 1, function(y) sum(is.na(y))/length(y))
}

#' @name na_ratio_col
#' @title NA ratio of each column in data
#' @description This function get the NA ratio in the column of data
#' @param x a matrix with any type data
#' @return a vector value of NA ratio in each column
#' @export
#'
na_ratio_col <- function(x) {
  apply(x, 2, function(y) sum(is.na(y))/length(y))
}
