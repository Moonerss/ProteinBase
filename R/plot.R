#' PCA analysis of data
#'
#' @details
#' Note that \code{scale = TRUE} cannot be used if there are zero or constant (for \code{center = TRUE}) variables.
#' @importFrom rlang .data
#' @import ggplot2
#' @importFrom stats prcomp
#' @param data A matrix representing the genomic data such as gene expression data, miRNA expression data.\cr
#' For the matrix, the rows represent the genomic features, and the columns represent the samples.
#' @param group A data frame contain two columns. The first column is sample name matched with colnames of data,
#' The second column is the cluster label of samples.
#' @param center a logical value indicating whether the variables should be shifted to be zero centered. Alternately, a vector of length equal the number of columns of x can be supplied. The value is passed to \code{scale}.
#' @param scale a logical value indicating whether the variables should be scaled to have unit variance when use \code{\link{prcomp}}
#' @param pic_title The title of plot.
#' @param ellipse Whether add the confidence ellipse.
#'
#' @return
#' Return a `ggplot` object contained a PCA plot.
#'
#' @export
#'
plot_pca <- function(data, group, center = TRUE, scale = FALSE,
                     pic_title = "All-PCA", ellipse = FALSE) {
  # check samples
  colnames(group) <- c('ID', 'Type')
  if (ncol(data) != length(unique(group$ID))) {
    stop('The sample in `group` not all matched with `data`')
  }

  ##
  ID <- unique(as.vector(group$ID))
  subdata <- subset(data, select = ID)
  #subdata=log2(subdata)  #定量矩阵有0值，不能直接log转换
  subdata <- t(na.omit(subdata))
  pcobj <- prcomp(subdata, scale = scale, center = center)
  # summary(data.pca)
  ## extract data for plot
  ## Reference: StatQuest
  pca <- as.data.frame(pcobj$x)
  pca$ID <- rownames(pca)
  pca <- merge(pca, group, by = "ID")
  # labels
  u.axis.labs <- paste('PC', 1:2, sep='')
  u.axis.labs <- paste(u.axis.labs,
                       sprintf('(%0.1f%%)',
                               100 * pcobj$sdev[1:2]^2/sum(pcobj$sdev^2)))

  p <- ggplot(pca, aes(x = .data$PC1, y = .data$PC2, fill = .data$Type)) +
    geom_point(shape = "circle filled", size = 2.5, colour = 'black') +
    # geom_text(aes(label = ID), hjust = 0, vjust = 0) +
    scale_fill_brewer(palette = "Set1", direction = 1) +
    scale_x_continuous(labels = function(x) sprintf("%g", x)) +
    scale_y_continuous(labels = function(x) sprintf("%g", x)) +
    labs(x = u.axis.labs[1], y = u.axis.labs[2], title = pic_title) +
    guides(fill = guide_legend(title = "Sample type")) +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))

  if (ellipse) {
    # add confidence ellipses use stat_conf_ellipse from ggpubr
    p <- p +
      stat_ellipse(aes(colour = .data$Type), level = 0.95, geom = "polygon",
                   alpha = 0.3, lwd = 1, show.legend = F) +
      scale_color_brewer(palette = "Set1", direction = 1)
    # stat_conf_ellipse(alpha = 0.3, geom = 'polygon')
  }
  return(p)
}


#' Draw Venn plot
#'
#' @importFrom VennDiagram venn.diagram
#' @importFrom grid grid.draw
#' @importFrom rlang .data
#' @importFrom ggplot2 ggsave
#' @param x A list of vectors (e.g., integers, chars), with each component corresponding to a separate circle in the Venn diagram
#' @param filename Filename for image output, or if NULL returns the grid object itself
#' @param width Integer giving the width of the output figure in units
#' @param height Integer giving the height of the output figure in units
#' @param ... other argument of \code{magrittr::\link[magrittr:pipe]{\%>\%}}
#'
#' @export
#'
plot_venn <- function(x, filename = NULL, width = 7, height = 7, ...) {

  ## set base attributes
  dots <- list(...)
  if(is.null(dots$cat.cex)) dots$cat.cex = 1
  if(is.null(dots$cat.col)) dots$cat.col = "black"
  if(is.null(dots$cat.fontface)) dots$cat.fontface = "plain"
  if(is.null(dots$cat.fontfamily)) dots$cat.fontfamily = "serif"

  ## get plot
  # op <- par(mar=c(0,0,0,0))
  # on.exit(par(op))
  # grid.newpage()
  dots$x <- x
  dots <- c(dots, filename = list(NULL))
  venngrid <- do.call(VennDiagram::venn.diagram, dots)
  unlink(dir(getwd(), pattern="^VennDiagram[0-9_\\-]+.log$"))
  ## show picture in windows
  if (Sys.info()['sysname'] == "Windows") {
    grid.draw(venngrid)
  }
  if (!is.null(filename)) {
    ggplot2::ggsave(filename = filename, plot = venngrid, width = width, height = height)
  } else {
    return(venngrid)
  }

}

#' Plot data density curve
#' @param dat_matrix The data matrix with column in sample and row in feature
#' @param type Plot the density curve by sample or in all data
#'
#' @importFrom utils stack
#' @importFrom rlang .data
#' @import ggplot2
#' @return Return a `ggplot` object
#'
#' @export
#'
plot_density <- function(dat_matrix, type = c('all', 'group')) {

  dat_df <- stack(as.data.frame(dat_matrix))
  type <- match.arg(type)
  if (type == 'all') {
    p <- ggplot(dat_df, aes(x= .data$values)) + geom_density()
  } else if (type == 'group') {
    p <- ggplot(dat_df, aes(x = .data$values)) +
      geom_density(aes(group = .data$ind, colour = .data$ind)) +
      theme(legend.position = 'none')
  }
  return(p)
}

#' Plot data density curve of each sample by facet
#' @param dat_matrix The data matrix with column in sample and row in feature
#' @importFrom utils stack
#' @importFrom rlang .data
#' @import ggplot2
#'
#' @return Return a `ggplot` object
#'
#' @export
#'
plot_density_by_sample <- function(dat_matrix) {
  dat_df <- stack(as.data.frame(dat_matrix))
  pic <- ggplot(dat_df) +
    facet_wrap(vars(.data$ind)) +
    aes(x = .data$values, colour = .data$ind) +
    geom_density(adjust = 1L)  +
    scale_color_hue(direction = 1) +
    theme_bw() +
    theme(legend.position = 'none')
  return(pic)
}


#' Plot the data distribution of each sample
#'
#' @param data_matrix The data matrix with column in sample and row in feature
#' @param group A data frame contain two columns. The first column is sample name matched with colnames of data,
#' The second column is the cluster label of samples.
#' @param trans plot after tansfrom the value
#' @param color Character giving the color of the plot
#' @importFrom rlang .data
#' @import ggplot2
#' @importFrom dplyr arrange left_join
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect everything
#'
#' @return Return a `ggplot` object
#'
#' @export
#'
qc_boxplot <- function(data_matrix, group = NULL, trans = c('log10', 'log2'), color = "#EF562D") {

  trans = match.arg(trans)
  if (is.null(group)) {
    dat_df <- stack(as.data.frame(data_matrix))
    pic <- ggplot(dat_df) +
      aes(x = .data$ind, y = .data$values) +
      geom_boxplot(shape = "circle", color = color) +
      scale_y_continuous(trans = trans, labels = function(x) sprintf("%g", x)) +
      labs(x = 'Sample', y = paste0(trans, ' value')) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } else {

    ## order after groups
    colnames(group) <- c('ID', 'Type')
    group <- group %>% arrange(.data$Type)
    dat <- data_matrix[, group$ID]
    plot_dat <- dat %>%
      as.data.frame() %>%
      pivot_longer(cols = everything(), names_to = 'ID', values_to = 'value') %>%
      left_join(group, by = 'ID') %>%
      mutate(ID = factor(.data$ID, levels = group$ID))

    ## plot
    if (length(color) < length(unique(group$Type))) {
      warning('The `color` argument don\'t match with group number, we use random color')
      color <- get_color_palette(n = length(unique(group$Type)), theme = 'protigy')
    }
    pic <- ggplot(plot_dat) +
      aes(x = .data$ID, y = .data$value, colour = .data$Type) +
      geom_boxplot(shape = "circle") +
      scale_color_manual(values = color) +
      scale_y_continuous(trans = trans, labels = function(x) sprintf("%g", x)) +
      labs(x = 'Sample', y = paste0(trans, ' value')) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            legend.title = element_blank())
  }

  return(pic)
}


