library(scales)
library(ggsci)
library(ggthemes)
library(ggplot2)
library(cowplot)
library(ggrepel)

# some colors
category20 <- pal_d3("category20")(20)
two_Y_B <- c('#f8cb7f','#90a5e1')


# 2 Basic Plot =================================================================
# 2.1 Bior_Sankey --------------------------------------------------------------
Bior_Sankey <- function(links, Nodes.colour=NULL, Nodes.order=NULL, fontSize=12,
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


# 2.2 Bior_pie -----------------------------------------------------------------
Bior_pie <- function(x, labels, col=pal_d3("category20,",alpha=0.7)(20), title="", 
                     fontsize=20, legend.key.size=1){
  df <- data.frame(values=x, labels=labels)
  df$labels <- factor(df$labels,levels=labels)
  ggplot(df, aes(x='', y=values, fill=labels)) + 
    geom_bar(stat='identity')+
    coord_polar(theta='y')+
    theme_void() +
    labs(title=title) + 
    theme(text=element_text(size=fontsize), plot.title=element_text(hjust=0.5), 
          legend.title=element_blank(), legend.key.size=unit(legend.key.size, "cm")) +
    scale_fill_manual(values=col)
}


# 2.3 Bior_StackBarplot --------------------------------------------------------
Bior_StackBarplot <- function(data, x.order=NULL, type.order=NULL, col=category20, 
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


# 2.4 Bior_Line ----------------------------------------------------------------
Bior_Line <- function(data, line.size=1.5, labs.x='', labs.y='', title='',
                      text.size=20, title.size=30, legend.position='right',
                      col=category20, vline=NULL, theme=theme_minimal()){
  p <- ggplot() +
    geom_line(aes(x=data[,1], y=data[,2], color=data[,3]), size=line.size) +
    theme + 
    labs(x=labs.x,y=labs.y,title=title) +
    theme(text=element_text(size=text.size), 
          plot.title=element_text(size=title.size,hjust=0.5),
          legend.title=element_blank(), legend.position=legend.position) + 
    scale_color_manual(values = col) + 
    geom_vline(aes(xintercept= vline),colour="#9192ab",linetype="dashed",size = 1.5)
  return(p)
}


# 2.5 Bior_Dotplot -------------------------------------------------------------
Bior_Dotplot <- function(x, y, size, group.by, colour=NULL, max_size=5, text.size=15,
                         breaks=waiver(), theme=theme_bw(), labs.x='',
                         labs.y='', title='', legendtitle.color='Types',
                         legendtitle.size='Value'){
  if (is.null(colour)){
    colour <- hue_pal()(length(unique(df$col)))
  }else{
    colour <- colour
  }
  p <- ggplot(df) + 
    geom_point(aes(x = x, y = y, size = size, colour = group.by)) +
    scale_size_area(max_size = max_size, breaks = breaks) +
    scale_color_manual(values = colour) +
    theme +
    theme(plot.title = element_text(hjust = 0.5), text=element_text(size=text.size)) +
    labs(x=labs.x, y=labs.y, title=title) +
    guides(color=guide_legend(legendtitle.color), size=guide_legend(legendtitle.size))
  return(p)
}


# 3 scRNAseq Plot ==============================================================
# 3.1 Bior_DimPlot -------------------------------------------------------------
Bior_DimPlot <- function(seuratobject, reduction="umap", pt.size=1, label = TRUE,
                         label.size=5, cols = NULL, group.by=NULL, legend.key.size=1,
	         text.size=15,cells.highlight = NULL,cols.highlight='#f8cb7f',
	         sizes.highlight=1){
  p <- DimPlot(seuratobject, reduction=reduction, pt.size=pt.size, label=label, 
               label.size=label.size, cols=cols, group.by=group.by,cells.highlight = cells.highlight,
	cols.highlight=cols.highlight,sizes.highlight= sizes.highlight) + 
    theme_bw() + 
    theme(panel.grid=element_blank(), panel.border=element_rect(size=1.5), 
          text=element_text(size=text.size, face='bold'), legend.key.size=unit(legend.key.size, "cm"))
  return(p)
}


# 3.2 Bior_FeatureVlnplot ------------------------------------------------------
Bior_FeatureVlnplot <- function(seuratobject, genes, title.size=15, axis.text.size=10,
                                pt.size=1, nrow=1, scale=1, cols=NULL,reduction='umap'){
  two_plot <- function(gene){
    featureplot <- FeaturePlot(seuratobject, features=c(gene), pt.size=pt.size,reduction=reduction) +
      NoLegend() +
      theme(axis.line=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(),
            axis.title=element_blank(), plot.title=element_text(size=title.size))
    
    vlnplot <- VlnPlot(seuratobject, features=c(gene), pt.size=0, cols=cols) +
      NoLegend() +
      theme(axis.text=element_text(size=axis.text.size), axis.title=element_blank(),
            plot.title=element_blank()) +
      stat_summary(fun="median", geom="point",size=1)
    
    two_plot <- plot_grid(featureplot, vlnplot, nrow=2)
    return(two_plot)
  }
  plist <- lapply(genes,two_plot)
  p <- plot_grid(plotlist = plist, nrow=nrow, scale = scale)
  return(p)
}


# 3.3 Bior_DoHeatmap -----------------------------------------------------------
Bior_DoHeatmap <- function(seuratobject, features, group.by="ident", group.bar=TRUE,
                           group.colors=NULL, group.size=5, group.label=TRUE,
                           gene.size=5, gene.label=TRUE, margin=margin(0,0,0,0)){
  if (gene.label){
    axis.text.y <- element_text(size=gene.size)
  }else{
    axis.text.y <- element_blank()
  }
  
  p <- DoHeatmap(seuratobject, features=features, group.by=group.by, group.bar=group.bar,
                 group.colors=group.colors, size=group.size, label=group.label) +
    theme(axis.text.y = axis.text.y, 
          plot.margin = margin) +
    scale_fill_gradientn(colors = c("navy","white","firebrick3")) +
    NoLegend()
  return(p)
}


# 3.4 Bior_Featurebox ----------------------------------------------------------
Bior_Featurebox <- function(seuratobject, feature, text.size=18, hline.min=NULL, 
                            hline.size=NA, cols=NULL){
  p <- VlnPlot(seuratobject, feature=feature, adjust=0, pt.size=0, cols=cols) + 
    geom_boxplot() +
    geom_hline(yintercept=hline.min, colour="dimgray", linetype="dashed",size=hline.size)+
    theme_bw() + 
    labs(x='') +
    theme(panel.border=element_rect(size=1.5) ,text = element_text(size = text.size, face='bold'),
          plot.title = element_text(hjust = 0.5)) +
    coord_flip() +
    NoLegend()
  return(p)
}


# 3.5 Bior_StackVlnplot --------------------------------------------------------
Bior_StackVlnplot <- function(seuratobject, gene, fontsize = 10, cols = NULL){
  plist <- list()
  for (i in 1:length(gene)){
    if (i==length(gene)){
      axis.text.x <- element_text(angle=45, hjust=1, size=fontsize, colour='black')
    }else{
      axis.text.x <- element_blank()
    }
    plist[[i]] <- VlnPlot(seuratobject, features = gene[i], pt.size = 0, cols = cols) +
      theme_tufte() +
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_text(angle = 0, vjust = 0.5, size = fontsize),
            axis.text.x = axis.text.x, axis.text.y = element_blank(),
            axis.ticks = element_blank(), plot.title = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "lines")) + 
      labs(y = gene[i]) +
      stat_summary(fun = "median", geom = "point", size = 1) +
      NoLegend()
    if (i == 1){
      p <- plist[[1]]
    }else{
      p <- p + plist[[i]]
    }
  }
  return(p)
}





# 4 NGS Plot ===================================================================
# 4.1 Bior_Volcano -------------------------------------------------------------
Bior_Volcano <- function(gene, logfc, pvalue, label.gene='',label.size=5, logfc.threshold.up=1, 
                         logfc.threshold.Down=-1, pvalue.threshold=0.01, point.size=2, 
                         point.shape= 20, fontsize=20, title='', limits.x=c(-10,10), 
                         limits.y=c(0,15), legend.position='right', 
                         color=c('#2f5688','#BBBBBB','#CC0000')){
  # adjust data format
  deg.data <- data.frame(gene=gene, logfc=logfc, pvalue=pvalue)
  deg.data$logpv <- -log10(deg.data$pvalue)
  deg.data$Group <- "not significant"
  deg.data$Group[which((deg.data$pvalue < pvalue.threshold) & 
                       (deg.data$logfc > logfc.threshold.up))] <- "Up"
  deg.data$Group[which((deg.data$pvalue < pvalue.threshold) & 
                       (deg.data$logfc < logfc.threshold.Down))] <- "Down"
  deg.data$label <- ""
  deg.data$label[match(label.gene,deg.data$gene)] <- label.gene
  # plot
  ggplot(deg.data, aes(logfc, logpv, color = Group)) + 
    geom_point(size=point.size, shape=point.shape) +
    geom_text_repel(aes(label=label), max.overlaps= 100, force =5 ,size=label.size) +
    geom_hline(yintercept = -log10(pvalue.threshold) ,linetype='dashed') +
    geom_vline(xintercept = c(logfc.threshold.Down,logfc.threshold.up) ,linetype='dashed') +
    theme_bw() +
    labs(title=title) +
    theme(text=element_text(size=fontsize), plot.title=element_text(hjust = 0.5),
          legend.title=element_blank(), legend.position=legend.position, 
          panel.grid=element_blank()) +
    scale_color_manual(values = color) + 
    scale_x_continuous(limits=limits.x) +
    scale_y_continuous(limits=limits.y)
}





# 5 Data analysis ==============================================================
# 5.1 Bior_Dim2to1 -------------------------------------------------------------
Bior_Dim2to1 <- function(data){
  row_vectory <- c()
  col_vectory <- c()
  value <- c()
  for (i in 1:dim(data)[1]){
    for (j in 1:dim(data)[2]){
      row_vectory <- c(row_vectory,rownames(data)[i])
      col_vectory <- c(col_vectory,colnames(data)[j])
      value <- c(value,data[i,j])
    }
  }
  df <- data.frame('row'=row_vectory,'col'=col_vectory,'value'=value)
  return(df)
}


# 5.2 Bior_Combn ---------------------------------------------------------------
Bior_Combn <- function(inputdata){
  motifs <- c()
  for (i in 1:length(inputdata)){
    combi <- combn(inputdata,i)
    for (j in 1:dim(combi)[2]){
      motifs <- c(motifs,paste(combi[,j],collapse='+'))
    }
  }
  return(motifs)
}
