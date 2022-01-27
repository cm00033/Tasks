head () #to investigate the objects made
makeBaby() # you can compare the output from many rounds, to what you expect 
#to make our populations we can use samples 
trueMean1 <- 5
trueSD1 <- 5 
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4 
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)

size () #to specifry the size of the sample 
Sample() #to take the sample from the population 
Size <- 50 
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)
#The populations where slightly different. 
#To compare the samples use boxplot()
boxplot(Sample1, Sample2)
makeFounder() #can be used to manipulate the number of loci with the len argument 

source("http://jonsmitchell.com/code/simFxn04.R") 
#head() and nrow() can be used to examine objects and understand them 
MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")
head(MatGrandma)
head(MatGrandpa)
head(PatGrandma)
head(PatGrandpa)
Alan <- makeBaby(PatGrandma, PatGrandpa)
head(Alan)
Brenda <- makeBaby(MatGrandma, MatGrandpa)
head(Brenda)
Focus <- makeBaby(Brenda, Alan)
nrow(Focus)
head(Focus)
#grep() is like a ctrl+F, it finds any matches 
# The number should be half or 0.5.
ToMom <- length( grep("mom", Focus ) ) / length( Focus )
#This numbers should be at least half of the above percentage of the grandma mom and less than than the half in the grandpa mom.
ToMomMom <- length( grep( "grandma_mom", Focus ) ) / length( Focus )
ToMomDad <- length( grep( "grandpa_mom", Focus ) ) / length( Focus )
#No, Focus is a mixture of both the maternal and paternal, however, Focus is mostly connected to the maternal grandparent.
head(ToMomMom)
Sibling_01 <- makeBaby(Brenda, Alan)
#I expect Focus to share .25 or half of DNA with his sibling. The amount actually shared is 0.5.
ToSib <- length( intersect( Focus, Sibling_01 ) ) / length( Focus )
#Focus will share with his siblings an approximate of .50 of genes, because the shared genes between siblings does not change with the amount. 
ManySiblings <- replicate( 1e3, length( intersect( Focus, makeBaby( Brenda, Alan) ) ) / length( Focus ) )
#Data can be summarized utilizing quantile function and the mean
quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main = "", xlab = "proportion shared genes")
# The range of values in the plot is represented by the amount of children conceived and the chance of crossing the genes, causing a high in the .50 shared genes between siblings. 
HWE <- function(p) {
  aa <- p ^ 2
  ab <- 2 * p * (1 - p)
  bb <- (1 - p) ^ 2
  return(c(aa=aa, ab=ab, bb=bb))
} 
HWE(0.5)
plot(1, 1, type = "n", xlim = c(0, 1), ylim=c(0, 1), xlab= "freq. allele a", ylab = "geno. freq" )
p <- seq(from= 0, to= 1, by= 0.01)
GenoFreq <- t(sapply(p, HWE))

lines(p, GenoFreq[, "aa"] , lwd=2, col="red")
#The plot is showing the frequency of the "aa" allele time the geno frequency. 
#The frequency of the "aa" individuals increases in the population of geno frequency as well as the allele "a" frequency. 
#The "aa" frequency will decrease as both x,y frequency decrease. No time is presented in the graph. 
# However, geographic space is shown in the graph, which represents the set of ordered locations.

lines(p, GenoFreq[ , "ab"] , lwd=2, col="purple")
lines(p, GenoFreq[ , "bb"] , lwd=2, col="blue")
legend("top", legend=c("aa", "ab", "bb"), col=c("red", "purple", "blue") , lty=1, lwd=2, bty="n")
Pop <- simPop(500)
points(Pop[ , "freqa"], Pop[, "Genotypes.aa"]/500, pch=21, bg="red")
#Yes, the allele frequencies stayed the same during the generations, clustering in the increasing line. 
Pop <- simPop(50)
points(Pop[ , "freqa"], Pop[ , "Genotypes.aa"]/50, pch=22, bg="red")
#The points dispersed through the line, due to change in the frequency through generations.

install.packages("learnPopGen")
library(learnPopGen)
#Ne - how many indv there are in each pop
#nrep- how many pop you are simulating at once
#pause- how fast the lines grow
x <-genetic.drift(Ne=200, nrep = 5, pause = 0.01)
x <-genetic.drift(Ne=150, nrep = 5, pause = 0.01)
x <-genetic.drift(Ne=300, nrep = 5, pause = 0.01)
Popsizes <- 5:50
Samples<- rep(Popsizes, 5)
tExt<- sapply(Samples, function(x) nrow(simPop(x, 500)))
#lm- to fit a line in R data 
Line<- lm(tExt ~ Samples)
#to see the fit use summary function
summary(Line)
#to extract the coefficients, use $coef
Line$coef 
#to add it to the plot use abline function 
plot(Samples, tExt)
abline(Line)
Line2<- lm(tExt~Samples + 0)
summary(Line2)
Line2$coef
abline(Line2)
#As the population increases the points disperse more, resulting in differences in the genetic drift. The distance between can represent the generations of the samples. 

