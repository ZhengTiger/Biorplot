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


# 2.3 Bior_StackBarplot --------------------------------------------------------
data <- data.frame(matrix(rnorm(20, mean = 20, sd = 5),c(4,5)))
data <- round(data)
rownames(data) <- paste('gene',1:4, sep='')
colnames(data) <- paste('sample',1:5,sep='')
x.order <- c('sample1','sample5','sample4','sample3','sample2')
type.order <- c('gene1','gene2','gene3','gene4')
col <- pal_d3("category20")(20)[11:20]
p <- Bior_StackBarplot(data, x.order=x.order, type.order=type.order, col = col,
                  labs.x='Samples', labs.y='Numbers', title='Test Bior_StackBarplot',
                  theme=theme_minimal())
p


# 2.4 Bior_Line ----------------------------------------------------------------
data <- data.frame('x' = c(1:20), 'y' = rnorm(20), 'Type' = rep(c('A','B'), 10))
col <- c("#f89588","#63b2ee")
p <- Bior_Line(data, title = 'Test Bior_Line', col = col)
p





# 3 scRNAseq Plot ==============================================================
# first you should install and library Seurat packages
install.packages("Seurat")
library(Seurat)
# load test data
seuratobject <- readRDS("pbmc3k_final.rds")


# 3.1 Bior_DimPlot -------------------------------------------------------------
cols <- pal_d3("category20")(20)
p <- Bior_DimPlot(seuratobject, reduction='umap', cols = cols)
p
# highlight cell
highlight_cell <- names(seuratobject@active.ident)[which(seuratobject@active.ident=='Naive CD4 T')]
p <- Bior_DimPlot(seuratobject, reduction='umap', text.size=20, cells.highlight=highlight_cell,
             cols.highlight='#f8cb7f', sizes.highlight=1, pt.size=1, cols='#90a5e1', label = F)
p

# 3.2 Bior_FeatureVlnplot ------------------------------------------------------
genes <- c("MS4A1", "GNLY", "CD3E", "CD14", "FCER1A", "FCGR3A")
cols <- pal_d3("category20")(20)
p <- Bior_FeatureVlnplot(seuratobject, genes, scale=0.8, pt.size=0.5, nrow=2, cols=cols)
p


# 3.3 Bior_DoHeatmap -----------------------------------------------------------
library(dplyr)
pbmc.markers <- FindAllMarkers(seuratobject, only.pos = TRUE, min.pct = 0.25, logfc.threshold = 0.25)
top10 <- pbmc.markers %>% group_by(cluster) %>% top_n(n = 10, wt = avg_log2FC)
cols <- pal_d3("category20")(20)
p <- Bior_DoHeatmap(seuratobject, top10$gene, group.colors=cols, group.size=3, gene.label=F, margin=margin(20,20,0,0))
p


# 3.4 Bior_Featurebox ----------------------------------------------------------
cols <- pal_d3("category20")(20)
P <- Bior_Featurebox(seuratobject, feature = 'nFeature_RNA', cols = cols)
P




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


  
  
# 5 Data analysis ==============================================================
# 5.1 Bior_Dim2to1 -------------------------------------------------------------
data <- matrix(c(1:9), nrow = 3, ncol = 3)
colnames(data) <- c('A','B','C')
rownames(data) <- c('a','b','c')
data
df <- Bior_Dim2to1(data)
df


# Bior_Combn -------------------------------------------------------------------
inputdata <- c('A','B','C')
motifs <- Bior_Combn(inputdata)
motifs


