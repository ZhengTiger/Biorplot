library(scales)
library(ggsci)
library(ggthemes)
library(ggplot2)
library(cowplot)
library(ggrepel)


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
  # 画图
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





# 3 scRNAseq Plot ==============================================================
# 3.1 Bior_DimPlot -------------------------------------------------------------
Bior_DimPlot <- function(seuratobject, reduction="umap", pt.size=1, label = TRUE,
                         label.size=5, cols = NULL){
  p <- DimPlot(seuratobject, reduction=reduction, pt.size=pt.size, label=label, 
               label.size=label.size, cols=cols) + 
    theme_bw() + 
    theme(panel.grid=element_blank(), panel.border=element_rect(size=1.5), 
          text=element_text(size=15, face='bold'))
  return(p)
}


# 3.2 Bior_FeatureVlnplot ------------------------------------------------------
Bior_FeatureVlnplot <- function(seuratobject, genes, title.size=15, axis.text.size=10,
                                pt.size=1, nrow=1, scale=1, cols=NULL){
  two_plot <- function(gene){
    featureplot <- FeaturePlot(seuratobject, features=c(gene), pt.size=pt.size) +
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





# 4 NGS Plot ===================================================================
# 4.1 Bior_Volcano -------------------------------------------------------------
Bior_Volcano <- function(gene, logfc, pvalue, label.gene='',label.size=5, logfc.threshold.up=1, 
                         logfc.threshold.Down=-1, pvalue.threshold=0.01, point.size=2, 
                         point.shape= 20, fontsize=20, title='', limits.x=c(-10,10), 
                         limits.y=c(0,15), legend.position='right'){
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
    scale_color_manual(values = c('#2f5688','#BBBBBB','#CC0000')) + 
    scale_x_continuous(limits=limits.x) +
    scale_y_continuous(limits=limits.y)
}






