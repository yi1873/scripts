args <- commandArgs(TRUE);
exp_data=args[1]
outdir=args[2]
#########################
library("Vennerable")
library('reshape2')
library("ggplot2")
library("pheatmap")
setwd(outdir)

union<-read.table(exp_data,head=T)

rownames(union)<-union[,1]
union<-union[,-1]
union<-log10(union + 1)
if(length(union[,1])<=50){
        showname=TRUE;
}else{
	showname=FALSE;
}
if(length(union[1,])<=10){
        cell_widths=36
        cell_width=34
}else{
        cell_widths=floor(360/length(union[1,]))
        cell_width=floor(300/length(union[1,]))
}
num=length(union[,1])
if(dim(union)[2]==2){
      scale_row_col="column"
}else{
       scale_row_col="row"
}

if(length(union[,1])>=50){
        pdf("heatCluster.test.detail.pdf",height=0.015*num+8)
        pheatmap(union, color=colorRampPalette(rev(c("red","white","blue")))(100),  scale="row",legend=T,show_rownames=TRUE, fontsize_row=9, cellwidth=cell_width, cluster_rows =FALSE, main="Cluster analysis of differentially expressed genes")
        dev.off()
}

pdf("heatCluster.pdf",height=8, width=6)
pheatmap(union, color=colorRampPalette(rev(c("red","white","blue")))(100), scale="row",legend=T,show_rownames=TRUE,cellwidth=cell_widths,cluster_cols=TRUE,fontsize_row=6,cluster_rows=FALSE,  main="Cluster analysis of differentially expressed genes")
dev.off()
png("heatCluster.png", type="cairo-png")
pheatmap(union, color=colorRampPalette(rev(c("red","white","blue")))(100),  scale="row",legend=T,show_rownames=showname,cellwidth=cell_widths,cluster_cols=FALSE,  main="Cluster analysis of differentially expressed genes")
dev.off()
