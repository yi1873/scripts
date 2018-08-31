########## set the current working directory ##########
setwd("/path")

########## load the required package ##########
	library(ade4)
	library(psych)
	library(grid)
	library(RColorBrewer)

########## read and deal the data ##########
	rawdata <- read.table("zaoshu.flt.change.txt", header=T, row.names=1, sep="\t")
	groups <- read.table("group.txt", header=F)
	data <- t(rawdata)
	b <- matrix(0, nrow=nrow(data), ncol=ncol(data))
	for (i in 1:ncol(data)){
		b[,i] <- data[,i]/sum(data[,i])
	}
	rownames(b) <- rownames(data)
	colnames(b) <- colnames(data)
	data <- t(b)

########## PCA analysis based on ade4 ##########
	pca <- dudi.pca(data[,1:ncol(data)], scannf=F, nf=6)
	pc1 <- round(100*pca$eig/sum(pca$eig),2)[1]
	pc2 <- round(100*pca$eig/sum(pca$eig),2)[2]
	PC_c1 <- pca$c1
	write.table(PC_c1,file="PC.c1.txt",sep="\t")
########## calculate error bar ##########
tmp <- c()
for (n in 1:50){
	sample_data <- sample(rawdata,replace=TRUE)
	sample_data<-t(sample_data)
	m <- matrix(0, nrow=nrow(sample_data), ncol=ncol(sample_data))
	for (i in 1:ncol(sample_data)){
		if(all(sample_data[,i]==0)){
			m[,i] <- 0;
		}else{
			m[,i] <- sample_data[,i]/sum(sample_data[,i])
		}
	}
	rownames(m) <- rownames(sample_data)
	colnames(m) <- colnames(sample_data)
	sample_data <- t(m)
	pcaR <- dudi.pca(sample_data[,1:ncol(sample_data)], scannf=F, nf=2)
	PCr1 <- pcaR$li[,1]
	PCr2 <- pcaR$li[,2]
	a<- rbind(PCr1,PCr2)
	tmp<- rbind(a,tmp)
}
combine <- data.frame(tmp,rownames(tmp))
colnames(combine) <- c(levels(groups$V1),"PCA")
stat <- describeBy(combine[1:length(combine)-1],combine$PCA)

for(i in 1:length(groups$V1)){
	order <- grep(paste0("^",rownames(stat$PCr1)[i],"$"), rownames(pca$li),perl=T)
	stat$PCr1[i,3] <- pca$li[order,1]
	stat$PCr2[i,3] <- pca$li[order,2]
}

Group <- c()
for(i in 1:length(groups$V1)){
	order <- grep(paste0("^", rownames(stat$PCr1)[i], "$"), groups$V1,perl=T)
	Group[i] <- groups$V2[order]
}


###########plot PCA Figure ##########
colors <- c("#93D150","#00B1F1","#7030A1","#FFB300")
error.crosses(stat$PCr1,stat$PCr2,xlab=paste("PC1 ( ",pc1,"%"," )",sep=""),ylab=paste("PC2 ( ",pc2,"%"," )",sep=""),main="",pch=15,cex=.8,col=colors[Group],arrow.len=.04,offset=.3)
abline(h=0,lty=4,col="grey")
abline(v=0,lty=4,col="grey")
legend("bottomright",pch=15,col=colors,legend=unique(groups$V3),bty="n")

dev.off()
