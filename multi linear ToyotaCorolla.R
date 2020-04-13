#Prepare a prediction model for predicting Price.

View(ToyotaCorolla)
Corolla <- ToyotaCorolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(Corolla)
summary(Corolla)
attach(Corolla)

Corolla <- cbind(Price,Age_08_04,KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight)
Corolla <- as.data.frame(Corolla)

View(Corolla)
attach(Corolla)

#Scatter Plots
plot(Age_08_04,Price)
plot(KM,Price)
plot(HP,Price)
plot(cc,Price)
plot(Doors,Price)
plot(Gears,Price)
plot(Quarterly_Tax,Price)
plot(Weight,Price)


# Find Correlation between input and output
pairs(Corolla)

# Correlation Coefficient matrix - Strength & Direction of Correlation
cor(Corolla)
library(corpcor)
cor2pcor(cor(Corolla))

#Linear Model
LM1 <- lm(Price ~ Age_08_04 + KM + HP + cc + Doors + Gears + Quarterly_Tax + Weight) 
plot(LM1)
summary(LM1)##### CC  And Doors are influence each other.
#Multiple R-squared:  0.8638,	Adjusted R-squared:  0.863

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(LM1,id.n=2,id.cex=0.7)

#New model

cc.model <- lm(Price ~ cc)
summary(cc.model) #Multiple R-squared:  0.01597,	Adjusted R-squared:  0.01529 


Doors.model<-lm(Price ~ Doors)
summary(Doors.model)Multiple R-squared:  0.03435,	Adjusted R-squared:  0.03367 

LM2 <- lm(Price ~ cc + Doors)
summary(LM2)###Multiple R-squared:  0.04688,	Adjusted R-squared:  0.04555 
influenceIndexPlot(LM2)
influencePlot(LM2)
--------------------------------------------------------------------
#Delete 81 observation
model1 <- lm(Price ~ ., data = Corolla[-c(81),])
summary(model1)#### Multiple R-squared:  0.8694,	Adjusted R-squared:  0.8686 
avPlots(model1)

------------------------------------------------------------------------------------------


finalmodel1<-lm(Price ~ Age_08_04 + KM + HP + cc + Gears + Quarterly_Tax + Weight, data = Corolla[-c(81),])
summary(finalmodel1)####Multiple R-squared:  0.8693,	Adjusted R-squared:  0.8687 
 
plot(finalmodel1)

avPlots(finalmodel1,id.n=2,id.cex=0.7)

#Influence Plot
library(mvinfluence)
library(car)
influencePlot(finalmodel1, id.n=3) 






