# code for testdata

source('Biorplot.R')

# 2 Basic Plot =================================================================
# 2.1 Bior_Sankey --------------------------------------------------------------
# first you should install and library networkD3 packages
install.packages("networkD3") 
library(networkD3)
# links data, colnames must have 'source' 'target' 'value'
links <- data.frame(
  source=c("C","A", "B", "E", "D"), 
  target=c("b","c", "a", "e", "d"), 
  value=c(1, 2, 0, 4, 5)
)
# Set Nodes order and colour
Nodes.order <- c("A", "B", "C", "D", "E", "a", "b", "c", "d", "e")
Nodes.colour <- pal_d3("category20", alpha = 0.7)(20)
p <- Bior_Sankey(links, Nodes.order=Nodes.order, Nodes.colour=Nodes.colour, fontSize=20)
p
# Use saveNetwork() to save the plot as html
saveNetwork(p,"sankey.html")


# 2.2 Bior_pie -----------------------------------------------------------------
x <- c(3,7,9,1,2)
labels <- c("A", "B", "C", "D", "E")
col <- c("#AEC7E8B2", "#FFBB78B2", "#98DF8AB2", "#FF9896B2", "#C5B0D5B2")
p <- Bior_pie(x, labels, col=col, title="Test Bior_pie")
p




# 3 scRNAseq Plot ==============================================================
# first you should install and library Seurat packages
install.packages("Seurat") 
library(Seurat)
# load test data
seuratobject <- readRDS("testdata/pbmc3k_final.rds")
# colors
cols <- pal_d3("category20")(20)

# Bior_DimPlot -----------------------------------------------------------------
p <- Bior_DimPlot(seuratobject, cols = cols)
p


# Bior_FeatureVlnplot ----------------------------------------------------------
genes <- c("MS4A1", "GNLY", "CD3E", "CD14", "FCER1A", "FCGR3A")
p <- Bior_FeatureVlnplot(seuratobject, genes, scale=0.8, pt.size=0.5, nrow=2, cols=cols)
p





# 4 NGS Plot ===================================================================
# 4.1 Bior_Volcano -------------------------------------------------------------
data <- read.table('testdata/Bior_Volcano.txt', header=T)
gene <- data$Gene
logfc <- data$log2FoldChange
pvalue <- data$pvalue
label.gene <- c('DOK6','TBX5','EMILIN2')
p <- Bior_Volcano(gene, logfc, pvalue, label.gene, label.size=5, title="Test Bior_Volcano", 
                  legend.position="none", limits.x=c(-3,3),limits.y=c(0,10))
p


  
  
