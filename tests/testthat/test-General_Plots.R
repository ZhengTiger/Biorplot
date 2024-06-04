test_that("Bior_LinePlot() test", {
  # test1
  data <- data.frame('x' = c(1:20), 'y' = rnorm(20), 'Type' = rep(c('A','B'), 10))
  palette <- c("#f89588","#63b2ee")
  Bior_LinePlot(data, x = "x", y = "y", color = "Type", title = "Test Bior_LinePlot",
                palette = palette, plot_type = "l", size = 2, text.size = 30,
                ggtheme = theme_minimal()) +
    font("title", size = 35)

  # test2
  data <- data.frame('x' = c(1:10), 'y' = c(1,1.5,1.8,2.3,3.3,5.3,7.5,8,9,10))

  Bior_LinePlot(data, x = "x", y = "y",
                color = "firebrick3", plot_type = "l", size = 2,
                cor.test = TRUE, cor.label.x=1, cor.label.y=9, R.digits = 2, P.digits = 2,
                cor.label.size = 10,
                text.size = 30, ggtheme = theme_classic()) +
    geom_point(color="black", fill="firebrick3", shape=21, size=4, stroke=1) +
    font("title", size = 30)
})



test_that("Bior_PiePlot() test", {
  # test1
  value <- c(0.1,0.2,0.4,0.1,0.3)
  type <- c("A (10%)", "B (20%)", "C (40%)", "D (10%)", "E (30%)")
  col <- c("#AEC7E8B2", "#FFBB78B2", "#98DF8AB2", "#FF9896B2", "#C5B0D5B2")
  p <- Bior_PiePlot(value=value, type=type, col=col, title="Test Bior_pie")
  p

  # test2
  value <- c(0.1,0.2,0.4,0.1,0.3)
  type <- c("A", "B", "C", "D", "E")
  label <- c("10%","20%","40%","10%","30%")
  col <- c("#1F77B4B2", "#FF7F0EB2", "#2CA02CB2", "#D62728B2", "#9467BDB2")
  p <- Bior_PiePlot(value=value, type=type, label=label, col=col, title="Test Bior_pie", label.x=1.2, label.color="white", label.size=5)
  p
})


test_that("Bior_BarPlot() test", {
  # test1
  df <- data.frame(
    x = c("A", "B", "C"),
    y = c(3, 2, 1))
  Bior_BarPlot(df, "x", "y", fill = "steelblue", color = "steelblue")

  # test2
  df <- data.frame(
    x = rep(c('sample1','sample2','sample3','sample4'), each=2),
    y = c(20,16,29,16,14,11,22,21),
    type = rep(c('gene1','gene2'), 4),
    label = c(20,16,29,16,14,11,"","")
  )
  col <- c("#AEC7E8FF","#FFBB78FF")
  Bior_BarPlot(df, "x", "y", fill = "type", color = "type", label = df$label,
               palette = col, lab.pos = "in") +
    theme(legend.position = "right", legend.key.size=unit(1, "cm"))

  # test3
  Bior_BarPlot(df, "x", "y", fill = "type", color = "type", palette = col,
               label = TRUE, position = position_dodge(0.9))
})


test_that("Bior_DotPlot() test", {

})



test_that("Bior_Sankeyplot() test", {
})
