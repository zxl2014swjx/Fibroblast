# The mechanism and function of fibroblast in distinct tissues.

setwd("D:/SC_Fibroblast/0.Manuscript_Fibroblast/Github/")
library(ggplot2)
library(openxlsx)
library(reshape2)
library(ggthemes)
library(scales)
library(ggsci)
library(pheatmap)

data<-read.table("Figure3A.xls",header=T,sep="\t")
dim(data)
head(data)
pdf("Figure3A.pdf")
ggplot(data,aes(x=CellType,y=log_score,col=CellType))+
geom_boxplot(outlier.colour="grey50",outlier.size=0.01)+
facet_wrap(.~Module)+
xlab("")+ylab("Cell State log_score")+
theme_classic()+
geom_hline(yintercept=c(0),linetype=1,color="black")+
guides(colour = guide_legend(ncol = 1))+
theme(legend.position="right",
text=element_text(hjust = 0.5),
plot.title=element_text(hjust = 0.5),
axis.ticks.x=element_blank(),
axis.text.x=element_blank())+
scale_colour_tableau(palette = "Tableau 20")
dev.off()


data<-read.csv("Figure3B.csv")
dim(data)
head(data)
pdf("Figure3B.pdf")
pheatmap(data,cluster_rows=FALSE,cluster_cols=FALSE,
show_rownames = T,angle_col=c("90"))
dev.off()


data<-read.xlsx("Figure3C.xlsx",sheet=1,rowNames=T)
rowcn<-read.xlsx("Figure3C.xlsx",sheet=2,rowNames=T)
ann_colors = list(
CellType=c('ActFB'='#4E79A7','apCAF'='#A0CBE8','CycFB'='#F28E2B','CytFB'='#FFBE7D',
'DerFB'='#59A14F','DevFB'='#8CD17D','EpiFB'='#B6992D','FCC'='#F1CE63',
'FCPC'='#499894','iCAF'='#86BCB6','LipFB'='#E15759','MetFB'='#FF9D9A',
'myoCAF'='#79706E','PapFB'='#BAB0AC','ProFB'='#D37295','UniFB'='#FABFD2'))
pdf("Figure3C.pdf")
pheatmap(data,annotation_colors=ann_colors,
	cluster_cols = F,cluster_rows = F,
         annotation_row = rowcn)
dev.off()


data<-read.csv("Figure3D.csv",row.names=1)
cli<-read.csv("Figure3D_anno.csv",row.names=1)
pdf("Figure3D.pdf")
pheatmap(data,annotation_col=cli,
show_colnames=F,size=3,annotation_colors=cols1,
cluster_cols=FALSE,annotation_legend=FALSE,
gaps_col = c(12846,16203,36085,45137,50782,53483,54540,
60591,65187,86865,98819,110986,133598,134574,135163),
col=colorRampPalette(c("#66C2A5","#3288BD","white",
"#9E0142","#FEE08B","#F46D43"))(1000))
dev.off()
