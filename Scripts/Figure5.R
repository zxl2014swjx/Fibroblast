setwd("D:/SC_Fibroblast/0.Manuscript_Fibroblast/Github/")
library(ggplot2)
library(openxlsx)
library(reshape2)
library(ggthemes)
library(scales)
library(ggsci)
library(pheatmap)
library(RColorBrewer)
library(survival)
library(pROC)
library(plotROC)
library(survminer)
library(survivalROC)
library(riskRegression)
library(grid)
library(futile.logger)
library(cowplot)

pmat<-read.xlsx("Figure5A.xlsx",sheet=1,rowNames=T)
psig<-read.xlsx("Figure5A.xlsx",sheet=2,rowNames=T)
psig[is.na(psig)]<-""
pdf("Figure5A.pdf")
pheatmap(as.matrix(pmat),
cluster_rows=F,cluster_cols=F,
display_numbers = as.matrix(psig),
cellheight=14,cellwidth=14,
col=colorRampPalette(c("#5AAE61","white",
"#E7D4E8","#C2A5CF","#9970AB","#762A83","#40004B"))(100))
dev.off()


data<-read.csv("Figure5B.csv",row.names=1)
pdf("Figure5B.pdf")
pheatmap(as.matrix(data),
cluster_rows=F,cluster_cols=F,
col=colorRampPalette((brewer.pal(9,"Blues")))(100))
dev.off()



data1<-read.csv("Figure5D.csv")
custom_theme<-function(){
  theme_survminer()%+replace%
  theme(plot.title=element_text(hjust=0.5))}
my.surv <- Surv(data1$OS_MONTHS, data1$OS_Event)
fit <- survfit(my.surv ~ data1$group)
data1.survdiff <- survdiff(my.surv ~ data1$group) 
p.val = 1 - pchisq(data1.survdiff$chisq, length(data1.survdiff$n) - 1)
HR = (data1.survdiff$obs[1]/data1.survdiff$exp[1])/(data1.survdiff$obs[2]/data1.survdiff$exp[2])
up95 = exp(log(HR) + qnorm(0.975)*sqrt(1/data1.survdiff$exp[2]+1/data1.survdiff$exp[1])) 
low95 = exp(log(HR) - qnorm(0.975)*sqrt(1/data1.survdiff$exp[2]+1/data1.survdiff$exp[1]))
pdf("Figure5D.pdf")
ggsurvplot(fit, data =data1 ,
           ggtheme=custom_theme(),
           conf.int = F,
           conf.int.style = "step",
           censor = T,
           size=1.2,censor.size=8,
           title="Overall Survival",
           palette = c("#D6372E","#FADD4B","#70B460","#E690C1","#985EA8","#A3A3A3"), 
           risk.table=TRUE, 
           risk.table.title="",
           risk.table.y.text=T,
           risk.table.height=0.4,
           font.legend = 13,       
           legend.title="",
           legend="none",
           font.x = 0,font.y = 13,font.tickslab=13,
           pval = paste(
           paste("P = ",format(p.val,scientific=TRUE,digit=3), sep = ""),
           paste("HR = ",round(HR,2),sep = ""), 
           paste("95%CI: ", paste(round(low95,2), 
           round(up95,2), sep = " - "), sep = ""),
           sep = "\n"),
           pval.coord=c(200, 0.75))
dev.off()


data2<-read.csv("Figure5E.csv")
var<-as.data.frame(table(data2$variable))
li<-list()
for(i in 1:dim(var)[1]){
var1<-as.vector(var[i,1])
data3<-data2[data2$variable==var1,]
label<-names(sort(tapply(data3$value,data3$Ecotype,median)))
li[[i]]<-ggplot(data3,aes(x = Ecotype, y = value,fill = Ecotype)) +
geom_violin(position = position_dodge(0.9),
alpha = 0.8,width = 1.2,trim = F,color = "grey50") +
geom_boxplot(width = 0.3,
show.legend = F,position = position_dodge(0.9),
color = 'grey20',alpha = 0.8,
outlier.color = 'grey50') +
scale_x_discrete(limits=label,labels=label)+
#geom_point(color="black",size=1,position="identity")+
#geom_jitter(position="jitter",colour="grey20")+
xlab("")+ylab(var1)+
theme_bw()+
scale_fill_manual(values = c("E1"="#D6372E","E2"="#FADD4B",
"E3"="#70B460","E4"="#E690C1",
"E5"="#985EA8","E6"="#A3A3A3"))+
theme(legend.position="none",
text=element_text(size=12,hjust = 0.5))}
pdf("Figure5E.pdf")
plot_grid(li[[1]],li[[2]],li[[3]],li[[4]])
dev.off()


pmat<-read.csv("Figure5F.csv",row.names=1)
pdf("Figure5F.pdf")
pheatmap(as.matrix(pmat),
show_rownames=T,show_colnames=T,
#cellheight=10,cellwidth=10,
angle_col=('45'),
col=colorRampPalette(c("#5AAE61",
"#E7D4E8","#C2A5CF","#9970AB","#762A83",
"#40004B","white"))(100))
dev.off()




