View(`50_Startups`)
#Rename the data



#Convert State column in numeric#

library(plyr)
`50_Startups`$State <- revalue(`50_Startups`$State,
                       c("New York"="0", "California"="1", "Florida"="2"))


start<-`50_Startups`



attach(start)
library(readxl)
summary(start)

pairs(start[-4])
plot(Profit,R.D.Spend)
plot(start)
cor(start[,-4])####highest correlation between R.D Spend and profit


panel.cor<- function(x,y,digits=2,prefix="",cex.cor)
{
  usr<-par("usr");on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r=(cor(x,y))
  txt<- format(c(r,0.123456789),digits=digits)[1]
  txt<- paste(prefix,txt,sep="")
  if (missing(cex.cor))cex<- 0.4/strwidth(txt)
  text(0.5,0.5,txt,cex=cex)
 }
pairs(start,upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

#as.numeric(start$State)

####Simple Linear model##############
View(start)
attach(start)

SLR <- lm(Profit ~ R.D.Spend + Administration + Marketing.Spend+State)
summary(SLR)
##Multiple R-squared:  0.9508,	Adjusted R-squared:  0.9452####

library(car)
library(corpcor)

plot(SLR)
#Residuals vs regressors
residualPlots(SLR)

#adedvariable plots

vif(SLR)
avPlots(SLR,id.n=2,id.cex=0.7)
influence.measures(SLR)

panel.cor<-function(x,y,digits=2,prefix="",cex.cor)
{
  usr<- par("usr"); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r=(cor(x,y))
  txt<- format(c(r,0.123456789),digits=digits)[1]
  txt<- paste(prefix,txt,sep="")
  if(missing(cex.cor)) cex<-0.4/strwidth(txt)
  text(0.5,0.5,txt,cex=cex)
 }

pairs(start,upper.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")





----------------------------------------------------------------------------------------------
attach(start)
LR1<- lm(Profit~R.D.Spend+log(Administration))
summary(LR1)
#Multiple R-squared:  0.9474,	Adjusted R-squared:  0.9451 
plot(LR1)
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(LR1,id.n=2,id.cex=0.7)



cor2pcor(cor(start))

#install.packages("mvinfluence")
library(mvinfluence)
influence.measures(LR1)

#Index Plot of influence measure
influenceIndexPlot(LR1,id = 3)
influencePlot(LR1,id=3)

#Correlation and Barplot
barplot(height=start$Profit, names=start$Profit)
View(start)

cor(start$Profit,start$R.D.Spend)
cor(start$Profit,start$Administration)
------------------------------------------------------------------------------------------------------------
#logarthimic Transformation deleting 49th and 50th observation
  
LM2 <- lm(Profit~R.D.Spend+log(Administration)+Marketing.Spend,data=start[-c(49,50),])


plot(LM2)
summary(LM2)
#Multiple R-squared:  0.9625,	Adjusted R-squared:  0.9599
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(LM2,id.n=2,id.cex=0.7)
-------------------------------------------------------------------------------------------------------------------
#Exponential Transformation deleting 49th and 50th observation
LM3 <- lm(log(Profit)~R.D.Spend+Administration+Marketing.Spend+State,data=start[-c(49,50),])
plot(LM3)
summary(LM3)
#Multiple R-squared:  0.9252,	Adjusted R-squared:  0.9163
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(LM3,id.n=2,id.cex=0.7)


#Function to predict the model
p <- predict(SLR,interval = 'predict')
p1 <- predict(LR1,interval = 'predict')
p2 <- predict(LM2,interval = 'predict')
p3 <- predict(LM3,interval = 'predict')

rmse <- sqrt(mean(p2-Profit)^2)
rmse

rmse1 <- sqrt(mean(p3-Profit)^2)
rmse1

rmse2<-sqrt(mean(p-Profit)^2)
rmse2

rmse3<-sqrt(mean(p1-Profit)^2)
rmse3





















