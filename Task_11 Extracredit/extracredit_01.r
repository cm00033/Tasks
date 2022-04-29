#Extracredit
#Part 1 
?rnorm
x<-rnorm(100, mean=5,sd=2)
#I chose the standard deviation as 2 due to the variance being the Sd^2 equaling 4. 
?runif
y<- ((x*5)+2)+ runif(100, 0, 0.1)
plot(x,y)
abline(lm(y~x))
coef(lm(y~x))
#Utilizing the coef function we can establish the "y" intercept and the slope in which y= 2.05123 and slope is 4.999074
# This is because the slope-intercept equation was establish to be y=5x+2. 
#Due to adding random values at the end with the runif function these numbers are not precise. 

z<-c()
for(i in 1:100){
  z[i] <- runif(1,0,1)
  y<- ((x*z[1])+2)+runif(100,0,0.1)
  cmr<- coef(lm(y~z[1:100]))
}
cmr
plot(z,z[1:100])
#This plot reveals that slope and y-intercept are similar to the previous plot with the random numbers, indicating that the actual values of x and y are consider less but the slope is increasing the same way as the last plot. 

#Part 2
n<- 10000
Prize<- sample(c("C","M","R"), size=n, replace=TRUE)
PuertaOpened<- ifelse(Prize=="C", sample(c("M","R"), size=n, replace=TRUE), ifelse(Prize== "M","R","M"))
PuertaUnopened<- ifelse(PuertaOpened=="M", "R", "M")
WinChance<- sum(Prize=="C")/n
NotWinChance<-sum(Prize==PuertaUnopened)/n
?barplot
Winner<- c(NotWinChance,WinChance)
barplot(Winner, names.arg = c("NotShiftedPuerta","ShiftedPuerta"), ylab= "Winner chance", main= "Odds of winning The Monty hall", col="blue")

#Part 3
install.packages("meme")
library(meme)
?meme
install.packages("jpeg")
library(jpeg)
C <-readJPEG("Dog.jpg")
C3 <- meme(C,"When Dr.Michell says:", "Not everything is selection")
plot(C3,size=1.5)
