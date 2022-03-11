install.packages("phytools")
install.packages("ape")
install.packages("maps")
library(phytools)
text.string <- "(((((((cow,pig), whale), (bat,(lemur,human))), (robin,iguana)), coelacanth), (gold_fish, trout)), shark);"
vert.tree <- read.tree(text=text.string)
plot(vert.tree, edge.width = 2)
nodelabels(frame = "circle", bg= 'white', cex=1)
#Q1- A shark 
vert.tree
#Q2- No, there is no branch lengths
str(vert.tree)
tree <- read.tree(text = "(((A,B), (C,D)), E);")
plotTree(tree, offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame= "circle", bg='white', cex=1)
tree$tip.label
tree$edge
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length, col='black', border='white', main="", xlab="edge lengths for the Anolis tree", ylim=c(0, 50), xlim=c(0,6))
tipEdges <- which(AnolisTree$edge[,2] <= Ntip(AnolisTree))
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths)[which(Lengths==min(Lengths))]
plot(AnolisTree, cex= 0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text = Labs, cex=0.25)
?plot.phylo
#Q3 - 
plot(AnolisTree, show.tip.label= FALSE)
#Q4 - 
plot.phylo()
#Q5

#Q6 - 8 : which()
#1: , 2:, 3: 
ltt(AnolisTree)
abline(0, 1, lwd=2, col='red', lty=2)
#Q9- Is increasing and has some drift, difference will growing, Yes, its growth has some decreasing at the end.  Yes, the lizards are probably selecting for a trait. 
#Q10 - 
fit.bd(rho= 0.2)


