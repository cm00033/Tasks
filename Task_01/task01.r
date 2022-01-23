# pat: 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
#install.packages("swirl")

# This is a comment. R won't read it.
library(swirl)
swirl()
c()
?c
sqrt()
my_div
getwd()
ls()
x <- 9 
list.files()
dir()
?list.files
args()
args(list.files)
old.dir <- getwd()
dir.create("testdir")
setwd("testdir")
file.create("mytest.R")
file.exists("mytest.R")
file.info("mytest.R")
file.rename("mytest.R","mytest2.R")
file.copy("mytest2.R", "mytest3.R")
file.path("mytest3.R")
file.path('folder1', 'folder2')
?dir.create
dir.create(file.path('testdir2', 'testdir3'), recursive = TRUE)
setwd(old.dir)
1:20
pi:10
15:1
?`:`
seq(1,20)
seq(0, 10, by=0.5)
my_seq <- seq(5, 10, length=30)
length(my_seq)
1:length(my_seq)
seq(along.with = my_seq)
seq_along(my_seq)
rep(0, times = 40)
rep(c(0, 1, 2), times = 10)
rep(c(0, 1, 2), each = 10)
x <- c(44, NA, 5, NA)
x*3
y <- rnorm(1000)
z <- rep(NA, 1000)
my_data <- sample(c(y,z),100)
my_na <- is.na(my_data)
my_na
sum(my_na)
my_data
0/0 
Inf - Inf 
x[1:10]
x[is.na(x)]
y <- x[!is.na(x)]
y[y > 0]
x[x > 0]
x[!is.na(x) & x > 0]
x[c(3, 5, 7)]
x[0]
x[3000]
x[c(-2, -10)]
x[-c(2, 10)]
vect <- c(foo= 11, bar= 2, norf= NA)
vect
names(vect)
vect2 <- c(11, 2, NA)
names(vect2) <- c("foo", "bar", "norf")
identical(vect, vect2)
vect["bar"]
vect[c("foo", "bar")]
my_vector <- 1:20
my_vector
dim(my_vector)
length(my_vector)
dim(my_vector) <- c(4, 5)
dim(my_vector)
attributes(my_vector)
class(my_vector)
my_matrix <- my_vector
?matrix
my_matrix2 <- matrix(1:20, 4, 5)
identical(my_matrix, my_matrix2)
patients <- c("Bill", "Gina", "Kelly", "Sean")
cbind(patients, my_matrix)
my_data <- data.frame(patients, my_matrix)
class(my_data)
cnames <- c("patient", "age", "weight", "bp", "rating", "test")
colnames(my_data) <- cnames
TRUE == TRUE
(FALSE == TRUE) == FALSE
6 == 7 
6 < 7 
10 <= 10 
5 != 7
!5 == 7
FALSE & FALSE
TRUE & c(TRUE, FALSE, FALSE)
TRUE && c(TRUE, FALSE, FALSE)
TRUE | c(TRUE, FALSE, FALSE)
TRUE || c(TRUE, FALSE, FALSE)
5 > 8 || 6 != 8 && 4 > 3.9 
isTRUE(6 > 4)
identical('twins', 'twins')
xor(5 == 6, !FALSE)
ints <- sample(10)
ints >5
which(c(TRUE, FALSE, TRUE))
which(ints > 7)
all(ints > 0)
Sys.Date()
mean(c(2, 4, 5))
data(cars)
?cars
head(cars)
plot(cars)
?plot
plot(x = cars$speed, y = cars$dist)
plot(x= cars$dist, y = cars$speed)
plot(x = cars$speed, y = cars$dist, xlab = "Speed")
plot(x = cars$speed, y = cars$dist, xlab = "Speed")
plot(x = cars$speed, y = cars$dist,  xlab = "Speed", ylab = "Stopping Distance")
plot(cars, main = "My Plot")
plot (cars, sub = "My Plot Subtitle")
plot(cars, col = 2)
plot(cars, xlim = c(10, 15))
plot(cars, pch= 2)
data(mtcars)
boxplot()
?boxplot
boxplot(formula = mpg ~ cyl, data = mtcars)
hist(mtcars$mpg)

setwd("/Users/coralysmaldonado/Desktop/Evolution/Tasks/Task_02")
Data <- read.csv ('http://jonsmitchell.com/data/beren.csv', stringsAsFactors = F)
write.csv(Data, 'rawdata.csv', quote = F)
Data[1,]
Data[2,]
Data[1 : 3,]
Data[1 : 3, 4]
Data[1 : 5 , 1 : 3]

Feeds <- which(Data[, 9] == 'bottle')
berenMilk <- Data[Feeds,]
head(berenMilk)
Feeds <- which(Data[ , 'event'] == 'bottle')
Feeds <- which(Data$event == 'bottle')

dayID <- apply(Data, 1, function(x) paste(x[1:3], collapse = '-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data$age <- dateID - dateID[which(Data$event == 'birth')]
head(Data)
beren2 <- Data
beren3 <- beren2[order(beren2$age) ,]
head(berenMilk)
head(beren2)
head(beren3)
write.csv(beren3, 'beren_new.csv', quote = F, row.names = FALSE)

#Question 1: The first hypothesis shows no concrete data that can be referred to quantity of his meals resulting in not enough valuable data.
# In the second hypothesis the amount of time and the measurements of how or what he drinks are missing. 
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value [Feeds])
tapply()
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds],length)

cor() #function that tells the correlation between the set of 2 numbers 
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test() #works like cor but conducts the appropriate test for the type of correlation turns things like a p-value
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor<- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)

aov() #is the anova function in R 
t.test() #is the t-Test 
chi.test() #is the chi squared test
wilcox.test() #Wilcoxon's test
ks.test() #Kolmogorov smirnov test 
# ~  #means as a function of 
  
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot(beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab = "who gave the bottle", ylab = "amount of milk consumed (oz)")
par() #function used to know which parameters to use 
?par
par(las = 1, mar = c(5, 5, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type = "b", pch = 16, xlab = "age in days", ylab = "ounces of milk")
abline(h = mean(totalFeed), lty = 2, col= 'red')

pdf("r02-totalMilkByDay.pdf", height = 4, width = 4)
par(las = 1, mar = c(5, 5, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type = "b", pch = 16, xlab = "age in days", ylab= " ounces of milk")
abline(h = mean(totalFeed), lty = 2, col = 'red')
dev.off()

#Q.2 The graph does not have units, meaning the graph cannot be interpreted do to the lack of information in variables. 
source("http://jonsmitchell.com/code/plotFxn02b.R")
pdf("r02b-cumulativeMilkByTime.pdf")
unique(beren3$event)

       