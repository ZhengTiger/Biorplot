
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
#' @param x A vector of value
#' @param labels A vector of labels for x
#' @param col colour
#' @param title title
#' @param fontsize fontsize
#' @param legend.key.size legend size
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' # Examples 1
#' x <- c(3,7,9,1,2)
#' labels <- c("A", "B", "C", "D", "E")
#' col <- c("#AEC7E8B2", "#FFBB78B2", "#98DF8AB2", "#FF9896B2", "#C5B0D5B2")
#' p <- Bior_PiePlot(x, labels, col=col, title="Test Bior_PiePlot")
#' p
Bior_PiePlot <- function(x, labels, col=pal_d3("category20,",alpha=0.7)(20), title="",
                     fontsize=20, legend.key.size=1){
  df <- data.frame(x=x, labels=labels)
  df$labels <- factor(df$labels, levels=labels)
  ggplot(df, aes(x='', y=x, fill=labels)) +
    geom_bar(stat='identity')+
    coord_polar(theta='y')+
    theme_void() +
    labs(title=title) +
    theme(text=element_text(size=fontsize), plot.title=element_text(hjust=0.5),
          legend.title=element_blank(), legend.key.size=unit(legend.key.size, "cm")) +
    scale_fill_manual(values=col)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Stack Barplot
#' @description Create a stacked barplot.
#'
#' @importFrom scales hue_pal
#' @import ggplot2
#'
#' @param data A dataframe, the columns are x-axis sample, the rows are fill type
#' @param x.order x axis sample order
#' @param type.order fill type order
#' @param col fill colours
#' @param label.size label size
#' @param labs.x x axis title
#' @param labs.y y axis title
#' @param title figure title
#' @param legend.key.size legend size
#' @param text.size text size
#' @param bar.width bar width
#' @param theme choose ggthemes, eg:theme_bw(), theme_classic()
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' # Examples 1
#' data <- data.frame(matrix(rnorm(20, mean = 20, sd = 5),c(4,5)))
#' data <- round(data)
#' rownames(data) <- paste('gene',1:4, sep='')
#' colnames(data) <- paste('sample',1:5,sep='')
#' x.order <- c('sample1','sample5','sample4','sample3','sample2')
#' type.order <- c('gene1','gene2','gene3','gene4')
#' col <- c("#AEC7E8FF","#FFBB78FF","#98DF8AFF","#FF9896FF")
#' p <- Bior_StackBarplot(data, x.order=x.order, type.order=type.order, col = col,
#'                        labs.x='Samples', labs.y='Numbers', title='Test Bior_StackBarplot',
#'                        theme=theme_minimal())
#' p
Bior_StackBarplot <- function(data, x.order=NULL, type.order=NULL, col=scales::hue_pal(),
                              label.size=5, labs.x='', labs.y='', title='',
                              legend.key.size=1, text.size=15, bar.width=0.7,
                              theme=theme_bw()){
  # adjust data format
  x <- c()
  type <- c()
  value <- c()
  for (i in 1:nrow(data)){
    for (j in 1:ncol(data)){
      x <- c(x, colnames(data)[j])
      type <- c(type, rownames(data)[i])
      value <- c(value, data[i,j])
    }
  }
  df <- data.frame(x=x, type=type, value=value)
  if (!is.null(x.order)){
    df$x <- factor(df$x, levels = x.order)
  }
  if (!is.null(type.order)){
    df$type <- factor(df$type, levels = type.order)
  }
  # plot
  p <- ggplot(data=df, aes(x=x, y=value, fill=type)) + geom_bar(stat="identity", width=bar.width) +
    scale_fill_manual(values=col) +
    labs(x=labs.x, y=labs.y, title=title) +
    geom_text(aes(label=value), color="black", size=label.size, position=position_stack(0.5)) +
    theme +
    theme(panel.grid = element_blank(),
          text=element_text(size=text.size), plot.title=element_text(hjust=0.5),
          legend.title=element_blank(),
          legend.key.size=unit(legend.key.size, "cm"))
  return(p)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Dot Plot
#' @description Create a dot plot.
#'
#' @importFrom scales hue_pal
#' @import ggplot2
#'
#' @param x A vector, x-axis value
#' @param y A vector, y-axis value
#' @param size A vector, point size value
#' @param group.by color group by x or y
#' @param colour colour
#' @param max_size max point size
#' @param text.size text size
#' @param breaks scale_size_area(breaks)
#' @param theme choose ggthemes, default:theme_bw()
#' @param labs.x labs x
#' @param labs.y labs y
#' @param title title
#' @param legendtitle.color guides(color=guide_legend(legendtitle.color))
#' @param legendtitle.size guides(size=guide_legend(legendtitle.size))
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' # Examples 1
#' x <- rep(paste('sample', 1:5, sep=''), 4)
#' y <- rep(paste('gene', 1:4, sep=''), 5)
#' size <- round(rnorm(20, mean = 10, sd = 5))
#' colour <- c("#1F77B4FF","#FF7F0EFF","#2CA02CFF","#D62728FF","#9467BDFF")
#' p <- Bior_DotPlot(x = x, y = y, size = size, group.by = x, colour = colour, max_size=10)
#' p
Bior_DotPlot <- function(x, y, size, group.by, colour=NULL, max_size=5, text.size=15,
                         breaks=waiver(), theme=theme_bw(), labs.x='',
                         labs.y='', title='', legendtitle.color='Types',
                         legendtitle.size='Value'){
  if (is.null(colour)){
    colour <- hue_pal()(length(unique(group.by)))
  }else{
    colour <- colour
  }
  p <- ggplot() +
    geom_point(aes(x = x, y = y, size = size, colour = group.by)) +
    scale_size_area(max_size = max_size, breaks = breaks) +
    scale_color_manual(values = colour) +
    theme +
    theme(plot.title = element_text(hjust = 0.5), text=element_text(size=text.size)) +
    labs(x=labs.x, y=labs.y, title=title) +
    guides(color=guide_legend(legendtitle.color), size=guide_legend(legendtitle.size))
  return(p)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Sankey Plot
#' @description Create a sankey plot.
#'
#' @importFrom networkD3 sankeyNetwork
#'
#' @param links A dataframe, colnames must have ‘source’ ‘target’ ‘value’
#' @param Nodes.colour Set nodes colour
#' @param Nodes.order Set nodes order
#' @param fontSize Set fontsize
#' @param nodeWidth Set nodewidth
#' @param nodePadding Set the gap size between nodes
#' @param margin R margin
#' @param height numeric height for the network graph’s frame area in pixels
#' @param width numeric width for the network graph’s frame area in pixels
#' @param sinksRight boolean. If TRUE, the last nodes are moved to the right border of the plot
#'
#' @export
#'
#' @examples
#' # Examples 1
#' # links data, colnames must have 'source' 'target' 'value'
#' links <- data.frame(
#'   source=c("C","A", "B", "E", "D"),
#'   target=c("b","c", "a", "e", "d"),
#'   value=c(1, 2, 0, 4, 5)
#' )
#'
#' # Set Nodes order and colour
#' Nodes.order <- c("A", "B", "C", "D", "E", "a", "b", "c", "d", "e")
#' Nodes.colour <- c("#1F77B4B2","#FF7F0EB2","#2CA02CB2","#D62728B2","#9467BDB2",
#'                   "#8C564BB2","#E377C2B2","#7F7F7FB2","#BCBD22B2","#17BECFB2")
#' p <- Bior_Sankeyplot(links, Nodes.order=Nodes.order, Nodes.colour=Nodes.colour, fontSize=20)
#' p
Bior_Sankeyplot <- function(links, Nodes.colour=NULL, Nodes.order=NULL, fontSize=12,
                            nodeWidth=30, nodePadding=10, margin=NULL, height=600,
                            width=600, sinksRight=TRUE){
  # adjust data format
  links <- links[which(links$value != 0),]
  nodes <- unique(c(links$source, links$target))
  if (!is.null(Nodes.order)){
    nodes <- Nodes.order[which(Nodes.order %in% nodes)]
  }
  nodes <- data.frame(name=nodes)
  nodes$index <- 0:(nrow(nodes) - 1)
  links <- merge(links, nodes, by.x="source", by.y="name")
  links <- merge(links, nodes, by.x="target", by.y="name")
  names(links) <- c("target","source","Value","IDsource","IDtarget")
  # set colour
  if (is.null(Nodes.colour)){
    colourScale <- "d3.scaleOrdinal(d3.schemeCategory20);"
  }else{
    pastecolor <- paste('d3.scaleOrdinal() .domain(["', Nodes.order[1], sep = '')
    for (i in 2:length(Nodes.order)){
      pastecolor <- paste(pastecolor, '", "', Nodes.order[i], sep = '')
    }
    pastecolor <- paste(pastecolor, '"]) .range(["', sep = '')
    pastecolor <- paste(pastecolor, Nodes.colour[1], sep = '')
    for (i in 2:length(Nodes.order)){
      pastecolor <- paste(pastecolor,'", "', Nodes.colour[i], sep = '')
    }
    pastecolor <- paste(pastecolor,'"])', sep = '')
    colourScale <- pastecolor
  }
  # plot
  links$Group <- links$source
  links$Group <- as.factor(links$Group)
  sank <- sankeyNetwork(Links=links, Nodes=nodes, Source="IDsource",
                        Target="IDtarget", Value="Value", NodeID="name", fontSize=fontSize,
                        nodeWidth=nodeWidth, nodePadding=nodePadding, margin=margin,
                        height=height, width=width, sinksRight=sinksRight,
                        colourScale=colourScale, LinkGroup="Group",iterations=0)
  return(sank)
}











