setwd(~Users/coralysmaldonado/Desktop/Evolution/Project/Data)
getwd()
library(readr)
pop.size<- read.csv("Popsize.csv")
mutationrate<- read.csv("Mutation by country and size .csv")
nsmutations<- read.csv("Non-synonymous mutations.csv")
Mutation_ <- read_csv("Mutation .csv")
head(mutationrate)
head(nsmutations)
head(pop.size)
head(Mutation_)
population<-pop.size[33,c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62)]
print(population)
Newmutation<-Mutation_[,12]
print(Newmutation)
mutation<-mutationrate[,15]
print(mutation)

Newmutation<-Mutation_[c(1:57), 12]
sample<-c(38876458, 39294349,39711702, 40112607, 40484458, 40822183, 41120610, 41382996, 41605172, 41791621, 41936390,42020991, 42061773, 42074670,42064666, 42040548, 42000525, 41947038, 41875321, 41786718, 41678056, 41556701,41413021, 41239803, 41025668, 40768903, 40461769, 40113249, 39724495, 39305246, 38857061, 37879934,37359884,36824175, 36276253, 35718094, 35033639, 34328880, 33609034, 32874567, 32687356, 32532199, 32356591, 32162492,31950579, 3171937, 31470391, 31206854, 30936531, 30658541, 30356010, 30046545,29735221, 29416964, 29096331,28776239,28454529)
sample
model<-lm(Newmutation~sample)
model
length(Newmutation)
length(sample)
dim(Newmutation)

par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(sample,Newmutation, col='blue',pch=16,cex=1.5,xlab="Population", ylab="Newmutation")
title(main="Mutation rate  vs Rural Population size")
abline(model,col='black')
PearTest<-cor.test(Newmutation,sample)
PearTest

ANOVA<-aov(Newmutation~sample)
 