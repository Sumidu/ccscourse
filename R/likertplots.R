#' Method to generate a Likert plot using the likert pkg
#'
#' @param items A data-frame or tibble with ordered factors only.
#' @param item_labels A character vector to replace the variable names in the plot
#' @param response_label The label for the legend
#' @param xlab Label for x-Axis
#' @param ylab Label for y-Axis
#' @param title A plot title
#' @param subtitle A plot subtitle
#' @param caption A plot caption
#' @param legend Where the legend is placed ("top", "bottom", "right", "left")
#' @param ... additional options passed to plot.likert from \link{likert.options}
#' @seealso \link{likert.options} \link{plot.likert}
#'
#' @returns a ggplot object
#' @export
#' @importFrom likert likert
#' @importFrom graphics plot
#' @importFrom ggplot2 guides labs guide_legend
#' @import grDevices
#' @examples
#' data <- data.frame(a = factor(c("a","b","a")), b=factor(c("a", "b", "b")))
#' plikert(data)
plikert <- function(items, item_labels = NULL,
                    response_label = "Response",
                    xlab = NULL,
                    ylab = "Percent",
                    title = NULL,
                    subtitle = NULL,
                    caption = NULL,
                    legend = "right",
                    ...)  {

  requireNamespace("likert", quietly = T)
  # generate palette from red to blue
  rwthcolors <- rwth.colorpalette()
  res <- likert::likert(as.data.frame(items))
  if(!is.null(item_labels)){
    res$results$Item <- item_labels
    names(res$items) <- item_labels
  }
  plot(res, low.color = rwth.colorpalette()$red, high.color = rwth.colorpalette()$green, ...) +
    ggplot2::guides(fill = ggplot2::guide_legend(response_label)) +
    ggplot2::labs(title = title, subtitle = subtitle,
                  y = xlab, x = ylab, caption = caption)
}
