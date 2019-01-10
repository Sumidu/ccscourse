
#' as.numeric.factor
#'
#' Returns a numerical vector that was a factor vector. Is used to create means from likert data.
#'
#' @param x Vector that contains a factor.
#'
#' @return Vector that contais numerics.
#' @export
#'
#' @examples
#' x <- factor(c("a", "b", "a"))
#' as_numeric_factor(x)
as_numeric_factor <- function(x) {
  if (is.numeric(x)){
    return(x)
  } else
    seq_along(levels(x))[x]

}


#' draw.palette
#'
#' Utility function that draws a palette.
#'
#' @param col Palette vector
#' @param border Border used for plotting
#' @param ... Additional params forwarded to plot.
#'
#' @return Nothing. Draws a plaette.
#' @export
#'
#' @examples
#' draw.palette(unlist( rwth.colorpalette() ))
draw.palette <- function(col, border = "light gray", ...) {
  n <- length(col)
  graphics::plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
       axes = FALSE, xlab = "", ylab = "", ...)
  graphics::rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

#' converts rgb values to hex notation
#'
#' @param r red value
#' @param g green value
#' @param b blue value
rgb2hex <- function(r,g,b) {
  grDevices::rgb(r, g, b, maxColorValue = 255)
}






