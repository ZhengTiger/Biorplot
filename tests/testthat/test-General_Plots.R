test_that("Bior_LinePlot() Line plot", {
  library(ggplot2)
  data <- data.frame('x' = c(1:20), 'y' = rnorm(20), 'Type' = rep(c('A','B'), 10))
  palette <- c("#f89588","#63b2ee")
  Bior_LinePlot(data, x = "x", y = "y", color = "Type", title = "Test Bior_LinePlot",
                palette = palette, plot_type = "l", size = 1, ggtheme = theme_minimal())
})









