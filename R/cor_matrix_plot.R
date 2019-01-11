#' cor.matrix.plot
#'
#' Plots a correlation matrix plot. The to
#'
#' @param data Data frame to plot. Use dplyer::select to filter variables to use.
#' @param conf.level Level of significance (default .95)
#'
#' @return Returns nothing. Is called for sideeffects (i.e. the plot)
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' mtcars %>% select(disp, qsec) %>% cor_matrix_plot()
#' @import corrplot
#' @import magrittr
#' @import stats
#' @import dplyr
cor_matrix_plot <- function(data, conf.level = .95) {

  if(dim(data)[2]<2){
    stop("Data must contain at least 2 columns.")
  }

  data %>% dplyr::mutate_if(is.ordered, as.numeric) -> data

  rwthcolors <- rwth.colorpalette()
  p <- corrplot::cor.mtest(data, conf.level = .95)
  col <- grDevices::colorRampPalette(c(rwthcolors$red, "#FFFFFF", rwthcolors$blue))
  cor(data, use = "pairwise.complete.obs") %>% corrplot::corrplot( method = "color", col = col(200),
                                                         type = "upper", order = "hclust", number.cex = .7,
                                                         addCoef.col = "black", # Add coefficient of correlation
                                                         tl.col = "black", tl.srt = 90, # Text label color and rotation
                                                         # Combine with significance
                                                         p.mat = p$p, sig.level = c(.001, .01, .05), insig = "n",
                                                         # hide correlation coefficient on the principal diagonal
                                                         diag = TRUE, tl.pos = "lt")
  cor(data, use = "pairwise.complete.obs") %>% corrplot::corrplot( method = "color", col = col(200),
                                                         type = "lower", order = "hclust", number.cex = .7,
                                                         #addCoef.col = "black", # Add coefficient of correlation
                                                         #tl.col = "black", tl.srt = 90, # Text label color and rotation
                                                         # Combine with significance
                                                         p.mat = p$p, sig.level = c(.001, .01, .05), insig = "label_sig",
                                                         pch.cex = 0.8,
                                                         # hide correlation coefficient on the principal diagonal
                                                         diag = TRUE, add = TRUE, tl.pos = "n")

}
