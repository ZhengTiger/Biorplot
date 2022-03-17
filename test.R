# code for testdata
#library(Seurat)
#library(ggthemes)
#library(ggplot2)
source('Biorplot.R')


seuratobject <- readRDS("testdata/pbmc3k_final.rds")
cols <- pal_d3("category20")(20)

# Bior_DimPlot
p <- Bior_DimPlot(seuratobject, cols = cols)
p


# Bior_FeatureVlnplot
genes <- c("MS4A1", "GNLY", "CD3E", "CD14", "FCER1A", "FCGR3A")
p <- Bior_FeatureVlnplot(seuratobject, genes, scale=0.8, pt.size=0.5, nrow=2, cols=cols)
p



  
  
p2
pp1 <- plot_grid(p1,p2,nrow=2)
pp1
"GNLY"
p1 <- FeaturePlot(seuratobject, features = c("GNLY"))
p2 <- VlnPlot(seuratobject, features = c("GNLY"),pt.size=0) + labs(x="",y="") + NoLegend() +
  theme(axis.line = element_blank(),axis.text = element_blank(), axis.ticks = element_blank(),plot.title = element_text(size=40))
pp2 <- plot_grid(p1,p2,nrow=2)
pp2
ppp <- plot_grid(pp1,pp2,nrow=1)
ppp

1