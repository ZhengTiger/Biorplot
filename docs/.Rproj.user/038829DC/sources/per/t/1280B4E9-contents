# 
library(scales)
library(ggsci)
library(Seurat)
library(ggthemes)
library(ggplot2)
library(cowplot)


# DimPlot
Bior_DimPlot <- function(seuratobject, reduction="umap", pt.size=1, label = TRUE,
                         label.size=5, cols = NULL){
  p <- DimPlot(seuratobject, reduction=reduction, pt.size=pt.size, label=label, 
               label.size=label.size, cols=cols) + 
    theme_bw() + 
    theme(panel.grid=element_blank(), panel.border=element_rect(size=1.5), 
          text=element_text(size=15, face='bold'))
  return(p)
}


# Featureplot and Vlnplot
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







