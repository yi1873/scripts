# Generate a Nonpareil plot with multiple curves

library(Nonpareil)
files = paste('nonpareil',dir('nonpareil/'),sep = '/')
#files = files[-grep('output',files)]
#tmp.df = data.frame(File = files, Name = rownames(df))
#attach(tmp.df)


col <- c("orange","darkcyan","firebrick4","blue","red","black")
pdf('saturation_curve.pdf')
nps.R031 = Nonpareil.set(files[1:6], col = col, plot.opts=list(plot.observed=FALSE, model.lwd=2))
dev.off()
