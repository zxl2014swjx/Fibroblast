########################START########################
##Figure 6A
setwd("/usr/path/Figure6")
data<-read.table("scRNA_CellType_average_TAG.xls",header=T,sep="\t")
colnames(data)[1:2]<-c("Var1","Var2")

col<-read.table("Trait_order",header=T,sep="\t",row.names=1)
a<-unique(data[,2])
setdiff(a,col[,1])
setdiff(col[,1],a)
library(RColorBrewer)
myPalette<-colorRampPalette((brewer.pal(9,"Blues")))
col<-read.table("Trait_order",skip=1,header=F,sep="\t")
row<-read.table("CellType_order",header=F,sep="\t")
data1<-data[grep("FEV",data[,2]),]
data1<-data[grep("ENT",data[,2]),]

library(ggplot2)
p1<-ggplot(data, 
aes(y = (factor((Var1))), x = factor(Var2))) + 
geom_tile(aes(fill = Intersection_count)) + 
scale_fill_continuous(low = "white", high = "white") + 
guides(fill = FALSE)+
geom_hline(yintercept=c(8.5,11.5),linetype=2,color="black")+
geom_vline(xintercept=c(21.5,27.5,40.5,61.5),linetype=2,color="black")+
geom_point(aes(size = Log10_Pvalue, colour = Intersection_count)) + 
scale_size(range = c(1, 5)) + 
theme_bw() + xlab("") + ylab("")+
scale_colour_gradientn(
#colours = c("white", "green", "yellow", "orange", "red"), 
colours=myPalette(100),
limits = range(data$Intersection_count)) + 
geom_text(aes(label = Label, size =0.5)) + 
scale_y_discrete(position = "right",limits=rev(row[,1]))+
scale_x_discrete(limits=col[,1])+
theme(axis.text.x = element_text(angle = 90,size=8,
hjust = 0.5, vjust = 0.5),
axis.ticks.y = element_blank())
ggsave("Figure6A.pdf",p1,height=5,width=14)

########################START########################
##Figure 6B
setwd("/usr/path/Figure6")

zscore<-read.table("74_trait_celltype_hetero_mcz",header=T,sep="\t",row.names=1)
pvalue<-read.table("74_trait_celltype_hetero_mcp",header=T,sep="\t",row.names=1)
zscore<-t(zscore)
colnames(zscore)<-gsub("BMD-HT","BMD.HT",colnames(zscore),fix=TRUE)
colnames(zscore)<-gsub("RR-ENT","RR.ENT",colnames(zscore),fix=TRUE)
colnames(zscore)<-gsub("FEVi/FvC","FEVi.FvC",colnames(zscore),fix=TRUE)
pvalue<-t(pvalue)
colnames(pvalue)<-gsub("BMD-HT","BMD.HT",colnames(pvalue),fix=TRUE)
colnames(pvalue)<-gsub("RR-ENT","RR.ENT",colnames(pvalue),fix=TRUE)
colnames(pvalue)<-gsub("FEVi/FvC","FEVi.FvC",colnames(pvalue),fix=TRUE)

pmat<-pvalue
psig<-matrix("",nrow=nrow(pmat),ncol=ncol(pmat))
rownames(psig)<-rownames(pmat)
colnames(psig)<-colnames(pmat)
for(i in 1:dim(pmat)[1]){
for(j in 1:dim(pmat)[2]){
if(pmat[i,j]< 0.001 ){psig[i,j]<-"***"}
if(pmat[i,j]> 0.001 & pmat[i,j]< 0.01 ){psig[i,j]<-"**"}
if(pmat[i,j]> 0.01 & pmat[i,j]< 0.05 ){psig[i,j]<-"*"}
if(pmat[i,j]>= 0.05 ){psig[i,j]<-""}
}}
psig[1:4,1:4]
t(psig)[1:4,1:4]
table(psig)
col<-read.table("Trait_order",header=T,sep="\t",row.names=1)
intersect(colnames(zscore),rownames(col))
setdiff(colnames(zscore),rownames(col))
setdiff(rownames(col),colnames(zscore))
st<-table(col[,1])
sum(st[1:1]);
sum(st[1:2]);sum(st[1:3]);sum(st[1:4]);sum(st[1:5]);sum(st[1:6])
zscore1<-zscore[,rownames(col)]
psig1<-psig[,rownames(col)]
col_fun = colorRamp2(c(min(zscore1), 0, max(zscore1)),
 c("#3A3A98", "white", "#832424"))
ann_colors = list(Category=c("blood/immune"="#FF9289","brain"="#00D1FF",
"heart"="#00DCAF","metabolic"="#C0A6FF","other"="#E0B400"))

library(pheatmap)
pdf("Figure6B.pdf",height=5,width=14)
pheatmap(
as.matrix(zscore1),display_numbers = psig1,
cluster_cols = F,cluster_rows = T,
annotation_col=col,
annotation_colors = ann_colors,
cutree_rows=3,
gaps_col=c(21,27,40,61),
col=col_fun,
angle_col=c("90"))
dev.off()

########################END########################
