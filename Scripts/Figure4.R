setwd("D:/SC_Fibroblast/0.Manuscript_Fibroblast/Github/")
library(ggplot2)
library(openxlsx)
library(reshape2)
library(ggthemes)
library(scales)
library(ggsci)
library(pheatmap)
library(RColorBrewer)

data<-read.csv("Figure4A.csv")
dim(data)
head(data)
pdf("Figure4A.pdf")
ggplot(data,aes(x=UMAP_1,y=UMAP_2,color=Disease))+
geom_point(size=0.1)+
theme_bw()+
scale_color_manual(values=c(
'Abortion'='#4E79A7',
'Cancer'='#E15759',
'Cerebrovascular Disease'='#F28E2B',
'Circulation Disease'="#FFBE7D",
'Normal'='#59A14F',
'Digestive Disease'='#8CD17D',
'Endocrine Disease'='#B6992D',
'Head injury'='#F1CE63',
'Heart Failure'='#499894',
'Nervous Disease'='#86BCB6',
'Respiratory Disease'='#A0CBE8',
'Urinary Disease'='#FF9D9A'))
dev.off()


data<-read.csv("Figure4B.csv")
dim(data)
head(data)
label<-sort(unique(as.vector(data$Disease)),decreasing=T)
pdf("Figure4B.pdf")
ggplot(data,aes(x=Disease,y=CellNum,fill=CellType))+
geom_bar(stat="identity",position="fill")+
theme_bw()+
scale_x_discrete(limits=label,labels=label)+
scale_fill_manual(values=c('ActFB'='#4E79A7','apCAF'='#A0CBE8','CycFB'='#F28E2B','CytFB'='#FFBE7D',
'DerFB'='#59A14F','DevFB'='#8CD17D','EpiFB'='#B6992D','FCC'='#F1CE63',
'FCPC'='#499894','iCAF'='#86BCB6','LipFB'='#E15759','MetFB'='#FF9D9A',
'myoCAF'='#79706E','PapFB'='#BAB0AC','ProFB'='#D37295','UniFB'='#FABFD2'))+
theme(legend.position="right",
axis.text.x=element_text(hjust=1,angle=90))
dev.off()


data<-read.xlsx("Figure4C.xlsx",sheet=1)
row<-read.xlsx("Figure4C.xlsx",sheet=2,colNames=F)
col<-read.xlsx("Figure4C.xlsx",sheet=3,colNames=F)
myPalette<-colorRampPalette(rev(brewer.pal(11,"Spectral")))
pdf("Figure4C.pdf")
ggplot(data, 
aes(y = (factor((Var1))), x = factor(Var2))) + 
geom_tile(aes(fill = Log10_Pvalue)) + 
scale_fill_continuous(low = "white", high = "white") + 
guides(fill = FALSE)+theme_bw() + xlab("") + ylab("")+
geom_point(aes(size = Intersection_count, colour = Log10_Pvalue)) + 
scale_colour_gradientn(colours=myPalette(100),
limits = range(data$Log10_Pvalue)) + 
geom_text(aes(label = Label),size=3) + 
scale_y_discrete(position = "right",limits=rev(row[,1]))+
scale_x_discrete(limits=col[,1])+
theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5),
axis.ticks.x = element_blank(), 
axis.ticks.y = element_blank())
dev.off()


zscore<-read.xlsx("Figure4D.xlsx",sheet=1,rowNames=T,check.names=T)
pmat<-read.xlsx("Figure4D.xlsx",sheet=2,rowNames=T,check.names=T)
col<-read.xlsx("Figure4D.xlsx",sheet=3,rowNames=T)
ann_colors = list(Label= c("Anthropometric" = "#F42F89", 
"Metabolic" = "#F68F7C","Immune" = "#073B6C"),
Class=c("AutoImmune" = "#80B6DC", "Blood Cells"="#ACEA9F",
"Body"="#F5AEEA","CVMD"="#F150E3","Glycemic"="#E0FC8F",
"Lipids"="#9DD2CD","Others"="#737C78"))
pdf("Figure4D.pdf")
pheatmap(as.matrix(zscore1),display_numbers = psig1,
cluster_cols = F,cluster_rows = T,
annotation_col=col,
cutree_rows=3,
gaps_col=c(3,19,37),
annotation_colors = ann_colors,
col=myPalette(100),
angle_col=c("90"))
dev.off()



