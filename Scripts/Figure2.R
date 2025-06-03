# Figure 2. Characteristics of fibroblast subtypes.

setwd("D:/SC_Fibroblast/0.Manuscript_Fibroblast/Github/")

library(ggplot2)
library(openxlsx)
library(reshape2)
library(ggthemes)
library(scales)
library(ggsci)

data<-read.csv("Figure2A.csv")
dim(data)
head(data)
pdf("Figure2A.pdf")
ggplot(data,aes(x=UMAP_1,y=UMAP_2,color=AgeGroup))+
geom_point(size=0.1)+
theme_bw()+
scale_color_manual(values=c(
'Fetal'='#38608F','Age[20,60)'='#F8B931','Age[60,90)'='#F16221'))
dev.off()


data<-read.csv("Figure2B.csv",check.names=F)
dim(data)
head(data)
pdf("Figure2B.pdf")
ggplot(melt(data), aes(x=variable, y=value, fill=CellType)) +
geom_bar(stat = "identity",width=0.5, col='black')  + 
theme_bw()+ 
scale_fill_manual(values=c(
'ActFB'='#4E79A7','apCAF'='#A0CBE8','CycFB'='#F28E2B','CytFB'='#FFBE7D',
'DerFB'='#59A14F','DevFB'='#8CD17D','EpiFB'='#B6992D','FCC'='#F1CE63',
'FCPC'='#499894','iCAF'='#86BCB6','LipFB'='#E15759','MetFB'='#FF9D9A',
'myoCAF'='#79706E','PapFB'='#BAB0AC','ProFB'='#D37295','UniFB'='#FABFD2'
))+
scale_y_continuous(expand=c(0,0))+
#guides(fill = guide_legend(ncol = 2))+
theme(legend.position="none",
axis.text=element_text(size=15))+
xlab("AgeGroup")+
scale_x_discrete(limits=c("Fetal","Age[20,60)","Age[60,90)"),
labels=c("Fetal","Age[20,60)","Age[60,90)"))
dev.off()


data<-read.csv("Figure2C.csv")
dim(data)
head(data)
pdf("Figure2C.pdf")
ggplot(data,aes(x=Statistic,y=Taxa,size=logp,fill=Label))+
geom_bar(stat="identity",position="dodge")+
geom_point()+
theme_bw()+
scale_fill_npg()+
scale_y_discrete(limits=label,labels=label)+
ylab("")+xlab("Statistic")+
geom_vline(xintercept=c(-30),linetype=2,color="black")+
geom_vline(xintercept=c(30),linetype=2,color="black")+
theme(legend.position="right",
plot.title = element_text(hjust = 0.5),
#axis.text.y=element_text(hjust=1,size=5),
text=element_text(hjust = 0.5))
dev.off()


data<-read.csv("Figure2D.csv",check.names=F)
dim(data)
head(data)
pdf("Figure2D.pdf")
ggplot(data,aes(x=Differentiation,y=Stemness,color=AgeGroup))+
geom_point()+theme_classic()+
theme(legend.position=c(0.2,0.8))+
scale_colour_manual(values=c('Fetal'='#38608F','Age[20,60)'='#F8B931','Age[60,90)'='#F16221'))+
guides(colour = guide_legend(title = ""))+
geom_hline(yintercept=c(0),linetype=2,color="black")+
geom_vline(xintercept=c(0),linetype=2,color="black")
dev.off()


data<-read.csv("Figure2E.csv",check.names=F)
dim(data)
head(data)
pdf("Figure2E.pdf")
ggplot(data,aes(x=Overexpressed,y=Underexpressed,color=AgeGroup))+
geom_point()+theme_classic()+
theme(legend.position=c(0.8,0.8))+
scale_colour_manual(values=c('Fetal'='#38608F','Age[20,60)'='#F8B931','Age[60,90)'='#F16221'))+
guides(colour = guide_legend(title = ""))+
geom_hline(yintercept=c(0),linetype=2,color="black")+
geom_vline(xintercept=c(0),linetype=2,color="black")
dev.off()



library(slingshot)
library(SingleCellExperiment)
.libPaths("/xtdisk/jiapl_group/zhuxl/R/x86_64-pc-linux-gnu-library/4.1")
setwd("/p300s/jiapl_group/zhuxl/2.ReFB/05.Slingshot")
scRNA<-readRDS(""scRNA_object_cluster1.1.rds"")
sce <- as.SingleCellExperiment(scRNA)
sim <- slingshot(sce,clusterLabels='seurat_clusters',reducedDim='UMAP')
summary(sim$slingPseudotime_1)
pdf("Figure2F.pdf")
plot(reducedDims(sim)$UMAP,col='grey')
lines(SlingshotDataSet(sim))
dev.off()
