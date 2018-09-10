library(phyloseq)
library(SpiecEasi)
library(igraph)
#library(RCyjs)
#library(RJSONIO)


setwd('~/Desktop/work/05.projects/network/Network')

meta <- read.csv('metadata.txt', row.names = 1, header = T, sep = '\t', stringsAsFactors = F)

count.kraken <- read.csv('72sample.filter0.01.taxon.readscount.txt', sep = '\t', header = T, stringsAsFactors = F, comment = "#")
rownames(count.kraken) <- count.kraken[, 1]
kraken.taxa <- count.kraken[, 2:8]
kraken.tab <- count.kraken[, -c(1:8)]
smp.nms <- colnames(kraken.tab)
smp.nms <- sub('\\.', '-', sub('X', '', smp.nms))
colnames(kraken.tab) <- smp.nms

kraken.ps <- phyloseq(otu_table(kraken.tab, taxa_are_rows = T), tax_table(as.matrix(kraken.taxa)), sample_data(meta))


#idx <- apply(otu_table(kraken.ps), 1, function(x)(sum(x > 0)/ncol(otu_table(kraken.ps))) > 0.1)
#all.ps <- subset_taxa(kraken.ps, idx)
#physeq <- motu.count.ps
physeq <- kraken.ps

asd.ps  <- subset_samples(physeq, group == 'ASD')
idx <- apply(otu_table(asd.ps), 1, function(x)(sum(x > 100)/ncol(otu_table(asd.ps))) > 0.2)
asd.ps <- subset_taxa(asd.ps, idx)

ctrl.ps <- subset_samples(physeq, group == 'HC')
idx <- apply(otu_table(ctrl.ps), 1, function(x)(sum(x > 100)/ncol(otu_table(ctrl.ps))) > 0.2)
ctrl.ps <- subset_taxa(ctrl.ps, idx)

#se.all <- spiec.easi(all.ps, method='mb',lambda.min.ratio=2e-2, nlambda=100, icov.select.params=list(rep.num=20, ncores=6))
se.asd <- spiec.easi(asd.ps, method='mb',lambda.min.ratio=2e-2, nlambda=100, icov.select.params=list(rep.num=30, ncores=6))
se.ctrl <- spiec.easi(ctrl.ps, method='mb',lambda.min.ratio=2e-2,nlambda=100, icov.select.params=list(rep.num=30, ncores=6))

#ig.all = adj2igraph(se.all$refit, vertex.attr=list(name=taxa_names(all.ps)))

ig.asd = adj2igraph(se.asd$refit, vertex.attr=list(name=taxa_names(asd.ps)))
ig.asd.size = degree(ig.asd)*2
p.net1 = plot_network(ig.asd, asd.ps, type='taxa', color="phylum",point_size = ig.asd.size, label="species",layout.method = layout.auto)
p.net1

# igraph to Cytoscape
network <- as_data_frame(ig.asd)
write.csv(network,"ig.asd.csv")
write.csv(tax_table(asd.ps),"ig.asd.taxonomy.csv")


# Ctrl
ig.ctrl = adj2igraph(se.ctrl$refit, vertex.attr=list(name=taxa_names(ctrl.ps)))
ig.ctrl.size = degree(ig.ctrl)*2
p.net2 = plot_network(ig.ctrl, ctrl.ps, type='taxa', color="phylum", point_size = ig.ctrl.size, label="species",layout.method = layout.auto)
p.net2

dd.asd = degree_distribution(ig.asd, cumulative=FALSE)
dd.ctrl = degree_distribution(ig.ctrl, cumulative=FALSE)
#sum(seq_along(dd.all)*dd.all)-1
sum(seq_along(dd.asd)*dd.asd)-1
sum(seq_along(dd.ctrl)*dd.ctrl)-1

plot(seq_along(dd.asd)-1,dd.asd, type='b', xlim=c(0,20), ylab="Frequency", xlab="Degree", col='red')
points(seq_along(dd.ctrl)-1, dd.ctrl, type='b')
legend("topright",c("Control","ASD"), col=c("black", "red"),pch=1, lty=1)
