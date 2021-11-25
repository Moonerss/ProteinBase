#' get color themes
#' @param n the number of color
#' @param alpha
#' @param theme use color from which theme
#'
#' @importFrom grDevices terrain.colors cm.colors topo.colors
#' @importFrom RColorBrewer brewer.pal
#'
#' @return return color code
get_color_palette <- function(n, alpha = 1, theme = c('simple', 'maftools_mutation1',
                                                      'maftools_mutation2', 'maftools_snv',
                                                      'cellchat', 'protigy')) {
  ## get colors from different articles or packages
  theme <- match.arg(theme)
  ## from maftools package
  maftools_mutation_col1 <- c("#F44336", "#E91E63", "#9C27B0", "#673AB7", "#3F51B5", "#2196F3",
                              "#03A9F4", "#00BCD4", "#009688", "#4CAF50", "#8BC34A", "#CDDC39",
                              "#FFEB3B", "#FFC107", "#FF9800", "#FF5722", "#795548", "#9E9E9E",
                              "#607D8B")
  maftools_mutation_col2 <- c(RColorBrewer::brewer.pal(11, name = "Paired"),
                              RColorBrewer::brewer.pal(11,name = "Spectral")[1:3],
                              'black', 'violet', 'royalblue', '#7b7060', '#535c68')
  maftools_snv_col <- c("#F44336", "#3F51B5", "#2196F3", "#4CAF50", "#FFC107", "#FF9800")

  ## from cellchat
  cellchat_col <- c('#E41A1C','#377EB8','#4DAF4A','#984EA3','#F29403','#F781BF',
                    '#BC9DCC','#A65628','#54B0E4','#222F75','#1B9E77','#B2DF8A',
                    '#E3BE00','#FB9A99','#E7298A','#910241','#00CDD1','#A6CEE3',
                    '#CE1261','#5E4FA2','#8CA77B','#00441B','#DEDC00','#B3DE69',
                    '#8DD3C7','#999999')

  ## from Protigy shiny
  protigy_col <- c(RColorBrewer::brewer.pal(8, "Set1"),
                   RColorBrewer::brewer.pal(8, "Dark2"),
                   RColorBrewer::brewer.pal(8, "Set2"),
                   terrain.colors(20), cm.colors(20), topo.colors(20))

  ## from article
  # 1. a simple article
  simple_col <- c("#f3a683", "#f7d794", "#778beb", "#e77f67", "#cf6a87", "#f19066",
                  "#f5cd79", "#546de5", "#e15f41", "#c44569", "#786fa6", "#f8a5c2",
                  "#63cdda", "#ea8685", "#596275", "#574b90", "#f78fb3", "#3dc1d3",
                  "#e66767", "#303952")
  col <- switch (theme,
                 simple = simple_col,
                 maftools_mutation1 = maftools_mutation_col1,
                 maftools_mutation2 = maftools_mutation_col2,
                 maftools_snv = maftools_snv_col,
                 cellchat = cellchat_col,
                 protigy  = protigy_col
  )
  ## set alpha
  col <- grDevices::adjustcolor(col = col, alpha.f = alpha)

  ## return
  if (n <= length(col)) {
    colors <- col[1:n]
  } else {
    warning('The ', theme, ' theme only contain ', length(col), ' colors, we keep all.')
    colors <- col
  }
  return(colors)
}
