#' Read all file under dictionary in a batch
#'
#' @param path a character vector of full path names; the default corresponds to the working directory.
#' Tilde expansion (see \links[base::path.expand]) is performed. Missing values will be ignored.
#' Elements with a marked encoding will be converted to the native encoding (and if that fails, considered non-existent).
#' @param pattern an optional regular expression. Only file names which match the regular expression will be returned.
#' @param merge whether merge the data, default TRUE. or return a list.
#' @param verbose output other useful message, defult TRUE.
#' @param ... other argument of \code{\links[data.table::fread]}
#'
#' @importFrom data.table fread
#'
#' @return
#'
#' @export
#'
read_all_file <- function(path, pattern = NULL, merge = TRUE, recursive = FALSE, verbose = TRUE, ...) {
  if (!requireNamespace("data.table")) {
    stop("Your need install the `data.table` package")
  }
  if (verbose) message("=> Starting")
  filenames <- dir(path, pattern = pattern, recursive = recursive)
  filepath <- sapply(filenames, function(x) {paste(path,x,sep='/')})
  if (length(filepath) > 1) {
    if (merge) {
      if (verbose) message("==> Reading ", filenames[1])
      merge_data <- data.table::fread(file = filepath[1], header = T, sep = "\t", stringsAsFactors = FALSE, ...)
      for (i in 2:length(filepath)) {
        if (verbose) message("==> Reading ", filenames[i])
        new_data <- data.table::fread(file = filepath[i], header = T, sep = "\t", stringsAsFactors = FALSE, ...);
        merge_data <- rbind(merge_data, new_data)
      }
      res <- merge_data
    } else {
      if (verbose) message("==> Reading files ...")
      res <- data <- lapply(filepath, data.table::fread, ...)
    }
  } else {
    if (verbose) message("==> Reading file ...")
    res <- data.table::fread(file = filepath, ...)
    res <- as.data.frame(res)
  }

  if (verbose) message("=> Done!")
  return(res)
}




####################################################################
#          read all sheets in an excel file
#  Author: Zhao erjie
#  Date: 2021-10-09
#  file     - path of the .xls or .xlsx file
#  sheets   - sheet name or number to read, if NULL read all sheets
#  merge    - whether merge the data
#  verbose  - whether print useful message
#  ...      - other arguments of `read_excel` function
####################################################################

#' Read all sheets in an excel file
#' @param file path of the .xls or .xlsx file
#' @param sheets sheet name or number to read, if NULL read all sheets
#' @param merge whether merge the data
#' @param verbos whether print useful message
#' @param ... other arguments of `read_excel` function
#'
#' @import readxl
#' @importFrom dplyr bind_rows
#'
read_all_sheets <- function(file, sheets = NULL, merge = FALSE, verbose = TRUE, ...) {

  # check package
  if(!requireNamespace('readxl')) {
    stop("Your need install the `readxl` package")
  }
  if (verbose) message("=> Starting")
  # get sheets
  if (is.null(sheets)) {
    all_sheets <- readxl::excel_sheets(file)
  } else {
    all_sheets <- readxl::excel_sheets(file)
    if (is.numeric(sheets)) {
      all_sheets <- all_sheets[sheets]
    } else if (is.character(sheets)) {
      all_sheets <- intersect(all_sheets, sheets)
    } else {
      stop('`sheets` argument must be a numeric or character vector')
    }
  }

  # read sheets
  if (verbose) message("==> Reading sheets: \n", paste0('==> ', paste(all_sheets, collapse = ' ')))
  all_list <- lapply(all_sheets, function(x) {readxl::read_excel(file, sheet = x, ...)})
  names(all_list) <- all_sheets
  if (merge) {
    res <- dplyr::bind_rows(all_list, .id = 'sheet')
    # res <- purrr::map2(all_list, names(all_list), function(x, y) {
    #   sheet <- rep(y, nrow(x))
    #   x <- cbind.data.frame(sheet, x)
    # })
    # res <- do.call(rbind.data.frame, res)
  } else {
    res <- all_list
  }
  # return result
  if (verbose) message("=> Done")
  return(res)
}


#' decompress files
#'
#' @name decompress
#'
#' @description 目前支持zip, tar.gz, tar.bz2, tar, gz
#' @param file 需要进行解压的压缩文件名字
#' @param outdir 解压后的文件路径名
#' @return
decompress <- function(file, outdir=NULL) {
  if(is.null(outdir)){
    outdir <- dirname(file)
  }

  fileType <- basename(file)
  if (grepl("zip$", fileType)) {
    unzip(file, exdir=outdir)
    t1 <- unzip(file, exdir=outdir, list=TRUE)
    return(as.character(t1$Name))
  } else if(grepl("tar\\.gz$", fileType)) {
    untar(file, exdir=outdir)
    return(untar(file, exdir=outdir, list=TRUE))
  } else if(grepl("tar$", fileType)) {
    untar(file, exdir=outdir)
    return(untar(file, exdir=outdir, list=TRUE))
  } else if(grepl("tar\\.bz2$", fileType)) {
    untar(file, exdir=outdir)
    return(untar(file, exdir=outdir, list=TRUE))
  } else if(grepl("\\.gz$", fileType)) {
    library(R.utils)
    gunzip(file, overwrite=TRUE, remove=FALSE)
    if (outdir != dirname(file)) {
      #解压后的文件地址
      t1 <- gsub("[.]gz$", "", file)
      #拷贝到规定的目录
      file.copy(t1, file.path(outdir, basename(t1)))
      file.remove(t1)
      return(basename(t1))
    } else {
      return("OK")
    }
  }
}

