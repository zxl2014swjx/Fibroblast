setwd("D:/SC_Fibroblast/0.Manuscript_Fibroblast/Github/")
library(ggplot2)
library(openxlsx)
library(reshape2)
library(ggthemes)
library(scales)
library(ggsci)

data<-read.csv("Figure1A.csv")
dim(data)
head(data)
pdf("Figure1A.pdf")
ggplot(need,aes(x=UMAP1,y=UMAP2,color=major_celltype))+
geom_point(size=0.1)+
theme_bw()+
scale_color_manual(values=c(
'Myeloid'='#9E7F7E','Endothelial'='#5CA763','Neuron'='#D183B7',
'Smooth Muscle'='#D3D3A2','Fibroblast'='#9C88BE',
'Epithelial'='#CA575C','Plasma'='#BABCBD',
'T'='#F48140','B'='#4580A9'))
dev.off()


data<-read.csv("Figure1B.csv")
dim(data)
head(data)
pdf("Figure1B.pdf")
ggplot(data,aes(x=UMAP_1,y=UMAP_2,color=CellType))+
geom_point(size=0.1)+
theme_bw()+
scale_color_manual(values=c(
'ActFB'='#4E79A7','apCAF'='#A0CBE8','CycFB'='#F28E2B','CytFB'='#FFBE7D',
'DerFB'='#59A14F','DevFB'='#8CD17D','EpiFB'='#B6992D','FCC'='#F1CE63',
'FCPC'='#499894','iCAF'='#86BCB6','LipFB'='#E15759','MetFB'='#FF9D9A',
'myoCAF'='#79706E','PapFB'='#BAB0AC','ProFB'='#D37295','UniFB'='#FABFD2'
))
dev.off()


data<-read.csv("Figure1C.csv")
cols = c('ActFB'='#4E79A7','apCAF'='#A0CBE8','CycFB'='#F28E2B','CytFB'='#FFBE7D',
'DerFB'='#59A14F','DevFB'='#8CD17D','EpiFB'='#B6992D','FCC'='#F1CE63',
'FCPC'='#499894','iCAF'='#86BCB6','LipFB'='#E15759','MetFB'='#FF9D9A',
'myoCAF'='#79706E','PapFB'='#BAB0AC','ProFB'='#D37295','UniFB'='#FABFD2')
pdf("Figure1C.pdf")
ggplot(data, aes(x=Organ, y=CellNum, fill=CellType)) +
geom_bar(stat="identity",position="fill")+
theme_bw()+
coord_flip()+
scale_y_continuous(expand=c(0,0))+
scale_fill_manual(values=cols)+
#guides(fill=guide_legend(ncol=2))+
xlab("Clinical")+ylab("Percentage")


H<-read.csv("Figure1D.csv")
library(d3Network)  
H <- H[,-3]
colnames(H)<-c("N1","N2","Value")
d3links<-H
d3nodes <- data.frame(name = unique(c(H$N1, H$N2)), stringsAsFactors = FALSE)  
d3nodes$seq <- 0:(nrow(d3nodes) - 1)   
d3links <- merge(d3links, d3nodes, by.x="N1", by.y="name")  
names(d3links)[4] <- "source"  
d3links <- merge(d3links, d3nodes, by.x="N2", by.y="name")  
names(d3links)[5] <- "target"  
names(d3links)[3] <- "value"  
d3links<-subset(d3links,select=c("N1","N2","value","source","target"))
d3links<-d3links[order(d3links$target),]
d3links <- subset(d3links, select=c("source", "target", "value"))  
d3nodes <- subset(d3nodes, select=c("name"))  
library(d3Network)
d3Sankey(Links = d3links, Nodes = d3nodes, 
Source = "source",  Target = "target", 
Value = "value", NodeID = "name",  
fontsize = 20, nodeWidth = 20, 
height = 1000,  width = 600,
standAlone=FALSE,
file = "Figure1D.html") 

 