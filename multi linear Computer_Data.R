Predict Price of the computer
View(Computer_Data)


computerdata<-Computer_Data
computerdata<-computerdata[,-1]
summary(computerdata)


# character to numeric
library(plyr)
computerdata$cd <- as.numeric(revalue(computerdata$cd,c("yes"=1, "no"=0)))
computerdata$multi <- as.numeric(revalue(computerdata$multi,c("yes"=1, "no"=0)))
computerdata$premium <- as.numeric(revalue(computerdata$premium,c("yes"=1, "no"=0)))
View(computerdata)
class(computerdata)

attach(computerdata)
summary(computerdata)
pairs(computerdata)

#Plot Scatter Plots
plot(speed,price)
plot(hd,price)
plot(ram,price)
plot(screen,price)
plot(cd,price)
plot(multi,price)
plot(premium,price)
plot(ads,price)
plot(trend,price)

#Correlation
cor(computerdata)
#Barplot
barplot(height=computerdata$price, names=computerdata$price)

#Correlation
cor(computerdata$price,computerdata$X)
cor(computerdata$price,computerdata$speed)
cor(computerdata$price,computerdata$hd)
cor(computerdata$price,computerdata$screen)
cor(computerdata$price,computerdata$cd)
cor(computerdata$price,computerdata$multi)
cor(computerdata$price,computerdata$premium)
cor(computerdata$price,computerdata$ads)
cor(computerdata$price,computerdata$trend)

#Build Linear Model
model <- lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend)
summary(model)
#Multiple R-squared:  0.7756,	Adjusted R-squared:  0.7752
plot(model)
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model,id.n=2,id.cex=0.7)


panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
 }
pairs(computerdata, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")


library(corpcor)
cor2pcor(cor(computerdata))

###influence Index Plot
library(mvinfluence)
library(car)
influenceIndexPlot(model,id.n=3)
influencePlot(model,id.n=3)


predict(model,interval = 'predict')





# Logarthimic Transformation after deleting 1441 and 1701 observation
ModelLog <- lm(price~log(speed)+log(hd)+log(ram)+log(screen)+
                               log(cd)+log(multi)+log(premium)+log(ads)+log(trend)
                             ,data=computerdata[-c(1441,1701),])
summary(ModelLog)      ## Adjusted R2 Value - 0.7441



confint(ModelLog,level=0.95)

predict(ModelLog,interval="predict")


################################### Quad Model
ModelQuad <- lm(price~speed+I(speed^2)+hd+I(hd^2)+ram+I(ram^2)+screen+I(screen^2)+
                            +cd+I(cd^2)+multi+I(multi^2)+premium+I(premium^2)
                          +ads+I(ads^2)+trend+I(trend^2),data=computerdata[-c(1441,1701),])
summary(ModelQuad)  #Adjusted R2 value is 0.8054
confint(ModelQuad,level=0.95)

predict(ModelQuad,interval="predict")
###############################Polynomial Regression

ModelPoly <- lm(price~speed+I(speed^2)+I(speed^3)+
                            hd+I(hd^2)+I(hd^3)+
                            ram+I(ram^2)+I(ram^3)+
                            screen+I(screen^2)+I(screen^3)+
                            cd+I(cd^2)+I(cd^3)+
                            multi+I(multi^2)+I(multi^3)+
                            premium+I(premium^2)+I(premium^3)+
                            ads+I(ads^2)+I(ads^3)+
                            trend+I(trend^2)+I(trend^3),data=computerdata[-c(1441,1701),])
summary(ModelPoly) #Adjusted R Square Value is 0.813
## 

confint(ModelPoly,level=0.95)

predict(ModelPoly,interval="predict")



# Final Model



FinalModel<-lm(price~speed+I(speed^2)+I(speed^3)+
                 hd+I(hd^2)+I(hd^3)+
                 ram+I(ram^2)+I(ram^3)+
                 screen+I(screen^2)+I(screen^3)+
                 cd+I(cd^2)+I(cd^3)+
                 multi+I(multi^2)+I(multi^3)+
                 premium+I(premium^2)+I(premium^3)+
                 ads+I(ads^2)+I(ads^3)+
                 trend+I(trend^2)+I(trend^3),data=computerdata[-c(1441,1701),])

summary(FinalModel) #Adjusted R2 Value = 0.813 


#Prediction
predf<- predict(FinalModel)
View(predf)

finalplot <-computerdata[-c(1441,1701),]
View(finalplot)

Final <- cbind(speed,hd,ram,screen,cd,multi,premium,ads,trend,price,predf)
View(Final)
