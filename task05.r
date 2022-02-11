library(learnPopGen)
?coalescent.plot
?learnPopGen
pdf('r05-plot1.pdf', height=6, width= 6)
coalescent.plot(n=5, ngen=15, colors=NULL)
dev.off()
pdf('r05-plot2.pdf',height=6, width=6)
coalescent.plot(n=10, ngen=20, colors=NULL)
dev.off()
pdf('r05-plot3.pdf', height=6, width=6)
coalescent.plot(n=10, ngen=5, colors=NULL)
dev.off()
#1Each simulation began with 5 alleles but one of them had 10 due to changing it in the plot 
#2 It takes around three generations 
#3 The average number is 1.2, which the variance is 43
#4 Fitness randomly generates the inital population
#5 Yes
install.packages("rehh", dep=T)
install.packages("assertthat", dep=T)
install.packages("RcppArmadillo", dep=T)
install.packages("https://cran.r-project.org/src/contrib/Archive/scrm/scrm_1.7.3-1.tar.gz", repos=NULL, type="source")
install.packages("https://cran.r-project.org/src/contrib/Archive/coala/coala_0.6.0.tar.gz", repos=NULL, type="source")
install.packages("phytools")
library("phytools")
install.packages("ape")
install.packages("maps")
library("ape")
library("maps")
library("rehh")

model<- coal_model(sample_size=5, loci_number= 10, loci_length = 
                     500, ploidy = 2) +
  feat_mutation(10) +
  feat_recombination(12) +
  sumstat_trees() +
  sumstat_nucleotide_div()

stats<- simulate(model, nsim=1)
Diversity <- stats$pi
# No, the numbers are not the same. Pi
Nloci<- length(stats$trees) 
t1<- read.tree(text=stats$trees [[1]][[1]])
plot(t1)
axisPhylo()
#6 Because, we our ancestors are a result of interbreeding. 
Age1 <-max(nodeHeights(t1))
t2<-read.tree(text= stats$trees[[2]][1])
plot(t2)
axisPhylo()
#Age2 <-max(nodeHeights(t2)), the most recent common ancestor has a difference in age of 0.3641234. 
#7 Yes
par(mfrow= c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
compare.chronograms(t1, t2)
for(locus in 1:Nloci) { 
  ntrees<- length(stats$trees[[locus]])
for(n in 1:ntrees) {
  if (locus ==1 && n== 1){
    outPhy <-read.tree (text =stats$trees [[locus]] [n])
  }
 else { 
   outPhy <-ape::: c.phylo(outPhy, read.tree(text=stats$trees [[locus]] [n]))
 }
}
}
par(mfrow =c(1,1))
densityTree(outPhy)
#2 
model2<- coal_model(sample_size=5, loci_number= 10, loci_length = 
500, ploidy = 2) +
  feat_mutation(10) +
  feat_recombination(12) +
  sumstat_trees() +
  sumstat_nucleotide_div()
#3 I changed the feat_recombination, changing the feat recombination would increase by two the changes of the locus through generations.
#4 Yes

model3<- coal_model(10,50) +
  feat_mutation(par_prior("theta", sample.int(100,1))) +
  sumstat_nucleotide_div()
stats <-simulate(model3, nsim=40)
mean_pi <-sapply(stats, function(x) mean(x$pi))
theta<- sapply(stats, function(x) x$pars[["theta"]])
plot(mean_pi)
plot(theta)
graphics::abline(mean_pi, theta)

