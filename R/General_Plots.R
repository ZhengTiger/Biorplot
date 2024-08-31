
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Line plot
#' @description Create a line plot.
#'
#' @import ggplot2
#' @importFrom ggpubr ggline theme_pubr
#'
#' @inheritParams ggpubr::ggline
#' @inheritParams stats::cor.test
#' @param title.hjust (defaut: title.hjust = 0.5); title hjust value
#' @param text.size (defaut: text.size = 20); text size value
#' @param cor.test (defaut: cor.test = FALSE); whether to use cor.test to calculate correlations
#' @param R.digits (defaut: R.digits = 2); digits for R
#' @param P.digits (defaut: P.digits = 2); digits for P
#' @param cor.label.x (defaut: cor.label.x = 1); cor.label x position
#' @param cor.label.y (defaut: cor.label.y = 1); cor.label y position
#' @param cor.label.size (defaut: cor.label.size=10); cor.label size
#'
#' @return A ggplot object
#' @export
#'
#' @seealso \code{\link{ggline}}
#' @examples
#' # Examples 1
#' data <- data.frame('x' = c(1:20), 'y' = rnorm(20), 'Type' = rep(c('A','B'), 10))
#' palette <- c("#f89588","#63b2ee")
#' Bior_LinePlot(data, x = "x", y = "y", color = "Type", title = "Test Bior_LinePlot",
#'               palette = palette, plot_type = "l", size = 2, text.size = 30,
#'               ggtheme = theme_minimal()) +
#'   font("title", size = 35)
#'
#'
#' # Examples 2
#' data <- data.frame('x' = c(1:10), 'y' = c(1,1.5,1.8,2.3,3.3,5.3,7.5,8,9,10))
#' Bior_LinePlot(data, x = "x", y = "y",
#'               color = "firebrick3", plot_type = "l", size = 2,
#'               cor.test = TRUE, cor.label.x=1, cor.label.y=9, R.digits = 2, P.digits = 2,
#'               cor.label.size = 10,
#'               text.size = 30, ggtheme = theme_classic()) +
#'   geom_point(color="black", fill="firebrick3", shape=21, size=4, stroke=1) +
#'   font("title", size = 30)
#'
#'
Bior_LinePlot <- function(
    # ggpubr::ggline Arguments
    data, x, y, group = 1, numeric.x.axis = FALSE, combine = FALSE, merge = FALSE,
    color = "black", palette = NULL, linetype = "solid", plot_type = c("b", "l", "p"),
    size = 0.5, shape = 19, stroke = NULL, point.size = size, point.color = color,
    title = NULL, xlab = NULL, ylab = NULL, facet.by = NULL, panel.labs = NULL,
    short.panel.labs = TRUE, select = NULL, remove = NULL, order = NULL, add = "none",
    add.params = list(), error.plot = "errorbar", label = NULL, font.label = list(size = 11, color = "black"),
    label.select = NULL, repel = FALSE, label.rectangle = FALSE, show.line.label = FALSE,
    position = "identity", ggtheme = theme_pubr(),
    # add new Arguments
    title.hjust = 0.5, text.size = 20,
    # stats::cor.test Arguments
    alternative = "two.sided", method = "pearson", exact = NULL, conf.level = 0.95,
    continuity = FALSE,
    # add new Arguments
    cor.test = FALSE, R.digits = 2, P.digits = 2, cor.label.x = 1, cor.label.y = 1,
    cor.label.size=10,
    ...)
{

  ggline.Arguments <- list(
    data = data, x = x, y = y, group = group, numeric.x.axis = numeric.x.axis,
    combine = combine, merge = merge, color = color, palette = palette, linetype = linetype,
    plot_type = plot_type, size = size,  shape = shape, stroke = stroke,
    point.size = point.size, point.color = point.color, title = title, xlab = xlab,
    ylab = ylab, facet.by = facet.by, panel.labs = panel.labs, short.panel.labs = short.panel.labs,
    select = select , remove = remove, order = order, add = add, add.params = add.params,
    error.plot = error.plot, label = label, font.label = font.label, label.select = label.select,
    repel = repel, label.rectangle = label.rectangle, show.line.label = show.line.label,
    position = position, ggtheme = ggtheme, ...)

  cor.test.Arguments <- list(
    alternative = alternative, method = method, exact = exact, conf.level = conf.level,
    continuity = continuity, ...
  )

  p <- do.call(ggpubr::ggline, ggline.Arguments) +
    theme(plot.title = element_text(hjust = title.hjust),
          text = element_text(size = text.size))

  if (cor.test == TRUE) {
    cor <- do.call(stats::cor.test, c(data[x], data[y], cor.test.Arguments))
    R <- round(cor$estimate, R.digits)
    P_value <- format(cor$p.value, digits = P.digits)
    p <- p +
      geom_text(x = cor.label.x, y = cor.label.y, label = paste('R =',R,'\nP =',P_value,sep=' '),
                hjust=0, color="black", size=cor.label.size)
  }

  return(p)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Pie Plot
#' @description Create a Pie plot.
#'
#' @import ggplot2
#' @importFrom ggsci pal_d3
#'
#' @param value A vector of value
#' @param type A vector of type
#' @param label (defaut: label=NULL); A vector of label
#' @param col (defaut: col=pal_d3("category20,",alpha=0.7)(20)); colour for type
#' @param title (defaut: title=""); title for plot
#' @param text.size (defaut: text.size=15); text size
#' @param plot.title.size (defaut: plot.title.size=20); plot.title size
#' @param label.x (defaut: label.x=1.2); geom_text x for label
#' @param label.color (defaut: label.color="white"); geom_text color for label
#' @param label.size (defaut: label.size=5); geom_text size for label
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' # Examples 1
#' value <- c(0.1,0.2,0.4,0.1,0.3)
#' type <- c("A (10%)", "B (20%)", "C (40%)", "D (10%)", "E (30%)")
#' col <- c("#AEC7E8B2", "#FFBB78B2", "#98DF8AB2", "#FF9896B2", "#C5B0D5B2")
#' Bior_PiePlot(value=value, type=type, col=col, title="Test Bior_pie")
#'
#' # Examples 2
#' value <- c(0.1,0.2,0.4,0.1,0.3)
#' type <- c("A", "B", "C", "D", "E")
#' label <- c("10%","20%","40%","10%","30%")
#' col <- c("#AEC7E8B2", "#FFBB78B2", "#98DF8AB2", "#FF9896B2", "#C5B0D5B2")
#' Bior_PiePlot(value=value, type=type, label=label, col=col, title="Test Bior_pie",
#'                   label.x=1.2, label.color="white", label.size=5)
#'
Bior_PiePlot <- function(
    value, type, label=NULL, col=pal_d3("category20",alpha=0.7)(20), title="",
    text.size=15, plot.title.size=20,
    label.x=1.2, label.color="white", label.size=5)
{
  df <- data.frame(value=value, type=type)
  df$label <- label
  df$type <- factor(df$type, levels=type)

  p <-
    ggplot(df, aes(x='', y=value, fill=type)) +
    geom_bar(stat="identity", width=1, color="white",
             position = position_stack(reverse =T)) +
    coord_polar("y", start=0) +
    theme_void() +
    theme(text = element_text(size = text.size),
          plot.title = element_text(size=plot.title.size, hjust = 0.5),
          legend.title = element_blank()) +
    scale_fill_manual(values = col) +
    labs(title = title)

  if (!is.null(label)){

    p <- p +
      geom_text(aes(x = label.x, label = label), color = label.color, size=label.size,
                position = position_stack(reverse =T, vjust=0.5))
  }
  return(p)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Bar plot
#' @description Create a stacked barplot.
#'
#' @importFrom ggpubr ggbarplot
#' @import ggplot2
#'
#' @inheritParams ggpubr::ggbarplot
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' # Examples 1
#' df <- data.frame(
#' x = c("A", "B", "C"),
#' y = c(3, 2, 1))
#' Bior_BarPlot(df, "x", "y", fill = "steelblue", color = "steelblue")
#'
#' # Examples 2
#' df <- data.frame(
#'   x = rep(c('sample1','sample2','sample3','sample4'), each=2),
#'   y = c(20,16,29,16,14,11,22,21),
#'   type = rep(c('gene1','gene2'), 4),
#'   label = c(20,16,29,16,14,11,"","")
#'   )
#' col <- c("#AEC7E8FF","#FFBB78FF")
#' Bior_BarPlot(df, "x", "y", fill = "type", color = "type", label = df$label,
#'              palette = col, lab.pos = "in") +
#'   theme(legend.position = "right", legend.key.size=unit(1, "cm"))
#'
#' # Examples 3
#' Bior_BarPlot(df, "x", "y", fill = "type", color = "type", palette = col,
#'              label = TRUE, position = position_dodge(0.9))
#'
Bior_BarPlot <- function(data, x, y, combine = FALSE, merge = FALSE,
                              color = "black", fill = "white", palette = NULL,
                              size = NULL, width = NULL,
                              title = NULL, xlab = NULL, ylab = NULL,
                              facet.by = NULL, panel.labs = NULL, short.panel.labs = TRUE,
                              select = NULL, remove = NULL, order = NULL,
                              add = "none", add.params = list(), error.plot = "errorbar",
                              label = FALSE, lab.col = "black", lab.size = 4,
                              lab.pos = c("out", "in"), lab.vjust = NULL, lab.hjust = NULL,
                              lab.nb.digits = NULL,
                              sort.val = c("none", "desc", "asc"), sort.by.groups = TRUE,
                              top = Inf,
                              position = position_stack(),
                              ggtheme = theme_pubr(),
                              ...)
{
  # Default options
  .opts <- list(
    data = data, x = x, y = y, combine = combine, merge = merge,
    color = color, fill = fill, palette = palette,
    size = size, width = width,
    title = title, xlab = xlab, ylab = ylab,
    facet.by = facet.by, panel.labs = panel.labs, short.panel.labs = short.panel.labs,
    select = select , remove = remove, order = order,
    add = add, add.params = add.params, error.plot = error.plot,
    label = label, lab.col = lab.col, lab.size = lab.size,
    lab.pos = lab.pos, lab.vjust = lab.vjust, lab.hjust = lab.hjust,
    lab.nb.digits = lab.nb.digits,
    sort.val = sort.val, sort.by.groups = sort.by.groups, top = top,
    position = position, ggtheme = ggtheme, ...)

  p <- do.call(ggpubr::ggbarplot, .opts)

  return(p)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Dot Plot
#' @description Create a dot plot.
#'
#' @importFrom ggpubr ggdotchart theme_pubr
#' @import ggplot2
#'
#' @inheritParams ggpubr::ggdotchart
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' # Examples 1
#' df <- data.frame(
#'   Sample = rep(paste('sample', 1:5, sep=''), 4),
#'   Gene = rep(paste('gene', 1:4, sep=''), 5),
#'   size = round(rnorm(20, mean = 10, sd = 5))
#'   )
#' colour <- c("#1F77B4FF","#FF7F0EFF","#2CA02CFF","#D62728FF","#9467BDFF")
#' Bior_DotPlot(data = df, x = "Sample", y = "Gene", size = "size", color = "Gene",
#'              x.text.col = FALSE, ggtheme = theme_bw()) +
#'   theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
#'
#' # Examples 2
#' df <- data.frame(
#'   Sample = rep(paste('sample', 1:4, sep=''), each=4),
#'   Gene = rep(paste('gene', 1:4, sep=''), 4),
#'   Pct = c(80,10,10,10,10,80,10,10,10,10,80,10,10,10,10,80),
#'   Expression = c(3,0.5,0.1,0.3,0.3,3,0.2,0.6,0.1,0.7,3,0.1,0.5,0.2,0.1,3)
#'   )
#'
#'  Bior_DotPlot(data = df, x = "Sample", y = "Gene", size="Pct", color = "Expression",
#'               x.text.col = FALSE, ggtheme = theme_bw()) +
#'   theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
#'   scale_color_gradientn(colours = c("lightblue3", "lightblue", "white", "red", "red4"))
#'
Bior_DotPlot <- function(data, x, y, group = NULL,
                         combine = FALSE,
                         color = "black", palette = NULL,
                         shape = 19, size = NULL, dot.size = size,
                         sorting = c("ascending", "descending", "none"),
                         x.text.col = TRUE,
                         rotate = FALSE,
                         title = NULL, xlab = NULL, ylab = NULL,
                         facet.by = NULL, panel.labs = NULL, short.panel.labs = TRUE,
                         select = NULL, remove = NULL, order = NULL,
                         label = NULL, font.label = list(size = 11, color = "black"),
                         label.select = NULL, repel = FALSE, label.rectangle = FALSE,
                         position = "identity",
                         ggtheme = theme_pubr(),
                         ...)
{
  # Default options
  .opts <- list(data = data, x = x, y = y, group = group,
                combine = combine,
                color = color, palette = palette,
                shape = shape, size = size, dot.size = size,
                sorting = sorting,
                x.text.col = x.text.col,
                rotate = rotate,
                title = title, xlab = xlab, ylab = ylab,
                facet.by = facet.by, panel.labs = panel.labs, short.panel.labs = short.panel.labs,
                select = select, remove = remove, order = order,
                label = label, font.label = font.label,
                label.select = label.select, repel = repel, label.rectangle = label.rectangle,
                position = position,
                ggtheme = ggtheme,
                ...)

  p <- do.call(ggpubr::ggdotchart, .opts)

  return(p)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Sankey Plot
#' @description Create a sankey plot.
#'
#' @importFrom networkD3 sankeyNetwork
#'
#' @inheritParams networkD3::sankeyNetwork
#'
#' @param Group.order (defaut: Group.order=NULL); text size Set Group order
#' @param Group.colour (defaut: Group.colour=NULL); Set Group colour
#' @param Nodes.order (defaut: Nodes.order=NULL); Set nodes order
#' @param Nodes.colour (defaut: Nodes.colour=NULL); Set Nodes colour
#'
#' @export
#'
#' @examples
#' # Examples 1
#' links <- data.frame(
#'   Source=c("C","A", "B", "E", "D"),
#'   Target=c("b","c", "a", "e", "d"),
#'   Value=c(1, 2, 1, 4, 5)
#'  )
#' nodes <- data.frame(
#'   name = c("A", "B", "C", "D", "E", "a", "b", "c", "d", "e")
#'  )
#' links$IDsource <- match(links$Source, nodes$name) -1
#' links$IDtarget <- match(links$Target, nodes$name) -1
#' Nodes.colour <- c("#1F77B4B2","#FF7F0EB2","#2CA02CB2","#D62728B2","#9467BDB2",
#'                   "#8C564BB2","#E377C2B2","#7F7F7FB2","#BCBD22B2","#17BECFB2")
#'
#' p <- Bior_SankeyPlot(links, nodes, Nodes.colour=Nodes.colour, Nodes.order = nodes$name,
#'                      fontSize=20,iterations=0)
#' p
#' # save plot
#' # saveNetwork(p,"sankey.html")
#' # webshot("sankey.html" , "sankey.pdf")
#'
#'
#' # Examples 2
#' links <- data.frame(
#'   Source = c(rep(c("A_1","B_1","C_1","D_1"),each=4), rep(c("A_2","B_2","C_2","D_2"),each=4)),
#'   Target = c(rep(c("A_2","B_2","C_2","D_2"),4), rep(c("A_3","B_3","C_3","D_3"),4)),
#'   Value = c(0.4,0.4,0.1,0.1, 0.1,0.8,0.05,0.05, 0.05,0.05,0.8,0.1, 0.05,0.1,0.05,0.8,
#'             0.4,0.4,0.1,0.1, 0.1,0.8,0.05,0.05, 0.05,0.05,0.8,0.1, 0.05,0.1,0.05,0.8)
#' )
#' links$Group <- ""
#' links$Group[which(links$Value > 0.5)] <- "Type1"
#' links$Group[which(links$Value > 0.1 & links$Value <= 0.5)] <- "Type2"
#' links$Group[which(links$Value <= 0.1)] <- "Type3"
#' nodes <- data.frame(
#'   name = c("A_1","B_1","C_1","D_1","A_2","B_2","C_2","D_2","A_3","B_3","C_3","D_3")
#' )
#' links$IDsource <- match(links$Source, nodes$name) - 1
#' links$IDtarget <- match(links$Target, nodes$name) - 1
#' Group.order <- c("Type1", "Type2", "Type3")
#' Group.colour <- c("#6860ff","#e489dc","#d0d5da")
#' Nodes.order <- nodes$name
#' Nodes.colour <- rep(c('#ffda11', '#f68d45', '#26d5ff', '#f05a9e'),3)
#'
#' Bior_SankeyPlot(
#'   Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget",
#'   Value = "Value", NodeID = "name", colourScale = colourScale, LinkGroup="Group",
#'   fontSize = 20, iterations=0,
#'   Group.order = Group.order, Group.colour = Group.colour,
#'   Nodes.order = Nodes.order, Nodes.colour = Nodes.colour)
#'
Bior_SankeyPlot <- function(Links, Nodes, Source = "IDsource", Target = "IDtarget",
                            Value = "Value", NodeID = "name", NodeGroup = NodeID,
                            LinkGroup = NULL, units = "",
                            colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"), fontSize = 7,
                            fontFamily = NULL, nodeWidth = 15, nodePadding = 10, margin = NULL,
                            height = NULL, width = NULL, iterations = 32, sinksRight = TRUE,
                            Group.order=NULL, Group.colour=NULL,
                            Nodes.order=NULL, Nodes.colour=NULL)
{

  if (is.null(Group.order)){
    Group.order <- sort(unique(Links$Group))
  }
  if (is.null(Nodes.order)){
    Nodes.order <- Nodes$name
  }

  if ((!is.null(Group.order)) & (is.null(Nodes.order))){
    domain <- c(Group.order)
    range <- c(Group.colour)
  }else if ((is.null(Group.order)) & (!is.null(Nodes.order))){
    domain <- c(Nodes.order)
    range <- c(Nodes.colour)
  }else if ((!is.null(Group.order)) & (!is.null(Nodes.order))){
    domain <- c(Group.order, Nodes.order)
    range <- c(Group.colour, Nodes.colour)
  }else{
    domain <- NULL
    range <- NULL
  }

  colourScale <- paste('d3.scaleOrdinal() .domain(["', domain[1], sep = '')
  for (i in 2:length(domain)){
    colourScale <- paste(colourScale, '", "', domain[i], sep = '')
  }
  colourScale <- paste(colourScale, '"]) .range(["', sep = '')
  colourScale <- paste(colourScale, range[1], sep = '')
  for (i in 2:length(range)){
    colourScale <- paste(colourScale,'", "', range[i], sep = '')
  }
  colourScale <- paste(colourScale,'"])', sep = '')

  if (is.null(domain) & is.null(range)){
    colourScale <- "d3.scaleOrdinal(d3.schemeCategory20);"
  }

  p <-
    sankeyNetwork(
      Links = Links, Nodes = Nodes, Source = Source, Target = Target,
      Value = Value, NodeID = NodeID, NodeGroup = NodeID,
      LinkGroup = LinkGroup, units = units,
      colourScale = colourScale, fontSize = fontSize,
      fontFamily = fontFamily, nodeWidth = nodeWidth, nodePadding = nodePadding,
      margin = margin,
      height = height, width = width, iterations = iterations, sinksRight = sinksRight)

  return(p)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%









