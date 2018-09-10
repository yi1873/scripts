setwd("~/Desktop/work/05.projects/23.KBP000116/report")

#library(metaflow)
library(phyloseq)
library(ggplot2)
library(gridExtra)
library(ggsci)
#library(xlsx)

dir.create('20180906.results/')

meta <- read.table('report.data/metadata.txt',header = T)

rownames(meta) <- meta$short_id

# metaphlan2
readAllBioms2PS=function(path2bioms,meta){
  require(phyloseq)
  biom.files=dir(path2bioms)
  biom.files=biom.files[grep('biom',biom.files)]
  biom.data=import_biom(paste(path2bioms,biom.files[1],sep="/"))
  otu.tab = data.frame(otu_table(biom.data))
  tax.tab = data.frame(tax_table(biom.data))
  colnames(otu.tab) = sub('\\.biom','',biom.files[1])
  for(i in 2:length(biom.files)){
    tmp=import_biom(paste(path2bioms,biom.files[i],sep="/"))
    tmp.otu = data.frame(otu_table(tmp))
    tmp.tax = data.frame(tax_table(tmp))
    colnames(tmp.otu) = sub('\\.biom','',biom.files[i])
    otu.tab = merge(otu.tab,tmp.otu,by=0,all=T)
    rownames(otu.tab)=otu.tab[,1]
    otu.tab = otu.tab[,-1]
    tax.tab = rbind(tax.tab,tmp.tax[-which(rownames(tmp.tax) %in% rownames(tax.tab)),])
  }
  tax.tab = tax.tab[!duplicated(row.names(tax.tab)),]
  tax.tab = tax.tab[order(row.names(tax.tab)),]
  otu.tab = otu.tab[order(row.names(otu.tab)),]
  otu.tab = as.matrix(otu.tab)
  colnames(otu.tab) = sub('_S.*','',colnames(otu.tab))
  otu.tab[is.na(otu.tab)] = 0
  colnames(tax.tab) = c('Kindom','Phylum', 'Class','Order', 'Family','Genus', 'Species', 'Strain')
  ps = phyloseq(tax_table(as.matrix(tax.tab)),otu_table(otu.tab,taxa_are_rows = TRUE),sample_data(meta))
  return(ps)
}

mp.ps <- readAllBioms2PS('report.data/data/taxaAbund/',meta)

#ps.F <- subset_samples(mp.ps, group == 'T1')
#ps.P <- subset_samples(mp.ps, group == 'T2')
#ps.B <- subset_samples(mp.ps, subgroup == '1')
#ps.A <- subset_samples(mp.ps, subgroup == '2')



relAbundPlot2 <- function(ps, taxonomy = 'Phylum', top, colors = NULL){
	ps <- tax_glom(ps, taxonomy)
	otu.tab <- otu_table(ps)
	taxas <- tax_table(ps)[, taxonomy]
	top.ft <- head(taxas[order(apply(otu.tab, 1, sum),decreasing = T)],top)
    df <- psmelt(ps)
    df$group <- factor(df$group, levels = c('T1', 'T2'))
    df$Taxa <- as.character(df[[taxonomy]])
    df$Taxa <- ifelse(df$Taxa %in% top.ft, df$Taxa, 'other')
    df$Taxa <- factor(df$Taxa, levels = c(top.ft, 'other'))
    n <- length(unique(df$Taxa))
    if(is.null(colors)){
		colors = pal_d3()(10)[1:n]
	}
    p <- ggplot(df, aes(group, Abundance, fill = Taxa)) + geom_bar(stat = "identity", position = "stack", width = 0.95)
    p <- p + facet_grid(name_group~., scales = 'free_x', space = 'free_x')  + scale_fill_manual(values = colors) + theme(axis.text.x = element_blank()) + theme_bw()
    p + coord_flip()
}

ranks <- c('Phylum', 'Class', 'Order', 'Family', 'Genus')

for(i in 1:5){

	p <- relAbundPlot2(mp.ps, taxonomy = ranks[i], top = 8)
	pdf(paste0('20180906.results/relAbund_', ranks[i], '.pdf'), width = 15)
	#relAbundPlot2(mp.ps, taxonomy = ranks[i], top = 8)
	grid.arrange(p,ncol = 1)
	dev.off()

}


#--------------------------------- statistics ---------------------------------

#ps <- ps.P

#temp <- table(sample_data(ps)$name_group)
#ps <- subset_samples(ps, name_group %in% names(temp)[temp != 0])

tax.tab  <- as.data.frame(tax_table(mp.ps))
ranks <- c("Phylum", "Class", "Order", "Family", "Genus", "Species")
inds <- unique(sample_data(ps)$name_group)
testing.res <- list()


for(i in 1:6){

	tmp.ps <- tax_glom(mp.ps, ranks[i])
	tmp.x <- subset_samples(tmp.ps, group == 'T1')
	tmp.y <- subset_samples(tmp.ps, group == 'T2')
	idx <- match(sample_data(tmp.x)[['name_group']],sample_data(tmp.y)[['name_group']])
	otu_table(tmp.y) <- otu_table(tmp.y)[, idx]
	taxas <- tax_table(tmp.y)[, ranks[i]]
	#idx <- which(is.na(taxas))
	tmp.x <- otu_table(tmp.x)
	tmp.y <- otu_table(tmp.y)
	n <- nrow(tmp.y)

	res <- lapply(1:n, 
		function(x){t.res = wilcox.test(as.vector(tmp.x[x, ]), as.vector(tmp.y[x, ]), paired = T); 
			return(data.frame(T1 = mean(as.vector(tmp.x[x, ])), T2 = mean(as.vector(tmp.y[x, ])), W = t.res$statistic, pval = t.res$p.value))
		})
	res <- do.call('rbind', res)
	res$padj <- p.adjust(res$pval, method = 'BH')
	idx <- order(res$padj, decreasing = F)
	res <- res[idx, ]
	res <- as.matrix(res)
	rownames(res) <- taxas[idx]
	testing.res[[i]] <- res
	#names(testing.res) <- ranks
	
	#paired.res <- as.data.frame(do.call('rbind',testing.res))
	#paired.res <- res[order(res$pval), ]
	
	write.csv(res, paste0('20180906.results//taxonomy_wilcox_',ranks[i],'.csv'))

}


#names(testing.res) <- ranks

#paired.res <- as.data.frame(do.call('rbind', testing.res))
#paired.res <- paired.res[order(paired.res$pval), ]

#write.csv(paired.res, '20180906.results//taxonomy_wilcox.csv')


diff.taxa <- na.omit(rownames(paired.res)[paired.res$pval < 0.05])

library(ggpubr)

for(i in 1:6){
	pdf(paste0('20180906.results/wilcox_', ranks[i], '.pdf'))
	t.ps <- tax_glom(mp.ps, ranks[i])
	t.df <- psmelt(t.ps)
	t.df$group <- factor(t.df$group, levels = c('T1', 'T2'))
	t.df$taxonomy <- t.df[[ranks[i]]]
	#t.df <- subset(t.df, taxonomy %in% diff.taxa)
	tmp.list <- list()
	taxas <- unique(t.df$taxonomy)
	for(j in 1:length(taxas)){
		tmp <- subset(t.df, taxonomy == taxas[j])
		tmp <- tmp[with(tmp, order(group, name_group)), ]
		t.p <- ggpaired(tmp, x = 'group', y = 'Abundance', color = 'group', line.color = "gray", line.size = 0.4, palette = "jco") 
		t.p <- t.p + facet_wrap(~taxonomy, scale = 'free') + stat_compare_means(paired = TRUE)
		t.p <- t.p + theme_bw()  + scale_color_lancet()
		tmp.list[[j]] <- t.p
		print(t.p)
	}
	dev.off()
}
