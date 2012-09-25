#Assignment 3
library(TraMineR)
data(biofam)
biofam$cohort <- cut(biofam$birthyr, c(1900,1930,1940,1950,1960), labels=c("1900-1929", "1930-1939", "1940-1949", "1950-1959"), right=FALSE)
bf.states <- c("Parent", "Left", "Married", "Left/Married",  "Child", "Left/Child", "Left/Married/Child", "Divorced")
bf.shortlab <- c("P","L","M","LM","C","LC", "LMC", "D")
biofam.seq <- seqdef(biofam[,10:25], states=bf.shortlab, labels=bf.states, weights=biofam$wp00tbgs)
seqIplot(biofam.seq, sortv="from.end", group=biofam$cohort)
seqtab(biofam.seq, tlim=1:20)
jpeg(file="biofam-fplot.jpg")
seqfplot(biofam.seq, group=biofam$cohort, tlim=1:20)
dev.off()
bf.trate <- seqtrate(biofam.seq)
round(bf.trate, digits=2)
bf.trate[which(bf.shortlab=="LM"),which(bf.shortlab=="LMC")]
bf.trate["[LM ->]","[-> LMC]"]
seqdplot(biofam.seq, group=biofam$cohort)
bf.seqstatd <- seqstatd(biofam.seq)
bf.t.entrop <- bf.seqstatd$Entropy
names(biofam.seq)[which(bf.t.entrop==max(bf.t.entrop))]
par(mfrow = c(2, 2))
seqmtplot(biofam.seq, withlegend = FALSE)
seqmsplot(biofam.seq, withlegend = FALSE, cex.plot = .7)
seqlegend(biofam.seq, fontsize = 1.2)