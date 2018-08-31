library(reshape2)
library(ggplot2)
df <- read.table("test.out",header = T,sep="\t",fill=TRUE);

colnames(df)<-c("GOid","GOdes","type","spe","number","padj")
df$padj=ifelse(df$padj<0.05,'*','')
df$sort <- order(df$type, df$GOid)
ymax = max(df$number)*1.1

trim_string<-function(str,num){
    strTrim<-str
        if(nchar(str)>num){
            strTrim<-paste(substr(str,1,num),"...",sep="");
        }
    strTrim
}

df$GO<-sapply(as.character(df$GOdes),function(x) trim_string(x,30))

p<-ggplot(df, aes(x = reorder(df$GOid, df$sort, sum),fill=spe,y=number))+ geom_bar(stat='identity',position="dodge") + theme(axis.text.x=element_text(angle=60,hjust=1,size=8.5)) + geom_text(aes(label=padj,x= reorder(df$GOid, df$sort, sum),hjust=0.25,vjust=-0.25),color=1) + 
xlab("") + ylab("Number of genes")+ggtitle("Gene Function Classification (GO)")+coord_cartesian(ylim=c(0,ymax*1.1)) + scale_y_continuous(breaks=pretty(1:ymax))+facet_grid(.~type,shrink=F,scales='free',space='free') 
ggsave("GO_classification_bar.v3.pdf",width=6,height=4)

