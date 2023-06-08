#' Line plot
#' @description Create a line plot.
#'
#' @import ggplot2
#' @importFrom ggpubr ggline theme_pubr
#' @inheritParams ggpubr::ggline
#' @param title.hjust (defaut: title.hjust = 0.5); title hjust value
#'
#' @return A ggplot object
#' @export
#'
#' @seealso \code{\link{ggline}}
#' @examples
#' # Examples 1
#' library(ggplot2)
#' data <- data.frame('x' = c(1:20), 'y' = rnorm(20), 'Type' = rep(c('A','B'), 10))
#' palette <- c("#f89588","#63b2ee")
#' Bior_LinePlot(data, x = "x", y = "y", color = "Type", title = "Test Bior_LinePlot",
#'               palette = palette, plot_type = "l", size = 1, ggtheme = theme_minimal())
Bior_LinePlot <- function(data, x, y, group = 1,
                          numeric.x.axis = FALSE,
                          combine = FALSE, merge = FALSE,
                          color = "black", palette = NULL,
                          linetype = "solid",
                          plot_type = c("b", "l", "p"),
                          size = 0.5, shape = 19, stroke = NULL,
                          point.size = size, point.color = color,
                          title = NULL, xlab = NULL, ylab = NULL,
                          facet.by = NULL, panel.labs = NULL, short.panel.labs = TRUE,
                          select = NULL, remove = NULL, order = NULL,
                          add = "none",
                          add.params = list(),
                          error.plot = "errorbar",
                          label = NULL, font.label = list(size = 11, color = "black"),
                          label.select = NULL, repel = FALSE, label.rectangle = FALSE,
                          show.line.label = FALSE,
                          position = "identity",
                          ggtheme = theme_pubr(),
                          # add new Params
                          title.hjust = 0.5
                          )
{

  # Default options
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .opts <- list(
    group = group, numeric.x.axis = numeric.x.axis,
    combine = combine, merge = merge,
    color = color, palette = palette,
    linetype = linetype, plot_type = plot_type,
    size = size,  shape = shape, stroke = stroke,
    point.size = point.size, point.color = point.color,
    title = title, xlab = xlab, ylab = ylab,
    facet.by = facet.by, panel.labs = panel.labs, short.panel.labs = short.panel.labs,
    select = select , remove = remove, order = order,
    add = add, add.params = add.params, error.plot = error.plot,
    label = label, font.label = font.label, label.select = label.select,
    repel = repel, label.rectangle = label.rectangle,
    show.line.label = show.line.label, position = position, ggtheme = ggtheme)
  if(!missing(data)) .opts$data <- data
  if(!missing(x)) .opts$x <- x
  if(!missing(y)) .opts$y <- y

  p <- do.call(ggpubr::ggline, .opts) +
    theme(plot.title = element_text(hjust = title.hjust))

  return(p)
}






