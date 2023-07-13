
mydata <-read.csv(file.choose(), header=TRUE) #read the real estate csv file 
head(mydata) # check the columns in the dataset
summary(mydata)
#plot sale price vs lot size, add a lowess line to the plot
plot(sale.price~lot.size, data=mydata,xlab="Lot size in acres", ylab="Sale price in dollars")
lines(lowess(mydata$lot.size, mydata$sale.price, delta=10), col="red") 
#plot sale price vs living area, add a lowess line to the plot
plot(sale.price~living.area, data=mydata,xlab="Living area in square feet", ylab="Sale price in dollars")
lines(lowess(mydata$living.area, mydata$sale.price, delta=0.1), col="red") 
#plot sale price vs bedrooms, add a lowess line to the plot
plot(sale.price~bedrooms, data=mydata,xlab="Number of bedrooms in the house", ylab="Sale price in dollars")
lines(lowess(mydata$bedrooms, mydata$sale.price, delta=0.1), col="red") 


#squareroot transform sale price
sale.price.sqrt <- sqrt(mydata$sale.price)
mydata$sale.price.sqrt <-sale.price.sqrt
#plot sqaureroot sale price vs lot size and add a lowess line
plot(sale.price.sqrt~lot.size, data=mydata,xlab="Lot size in acres", ylab = "Square root of sale price")
lines(lowess(mydata$lot.size, mydata$sale.price.sqrt, delta=10), col="red") 
#plot squareroot sale price vs living area and add a lowess line
plot(sale.price.sqrt~living.area,data=mydata,xlab="Living area in square feet", ylab = "square root of sale price")
lines(lowess(mydata$living.area, mydata$sale.price.sqrt, delta=10), col="red") 
#plot square root sale price vs bedrooms and add a lowess line
plot(sale.price.sqrt~bedrooms,data=mydata,xlab="Number of bedrooms in the house",ylab="square root of sale price")
lines(lowess(mydata$bedrooms, mydata$sale.price.sqrt, delta=10), col="red") 


#log transform sale price
sale.price.log <- log(mydata$sale.price)
mydata$sale.price.log <- sale.price.log
#log transform lot size
lot.size.log <- log(mydata$lot.size)
mydata$lot.size.log <- lot.size.log
#log transform living area
living.area.log <- log(mydata$living.area)
mydata$living.area.log <- living.area.log
#plot log trasnformed sale price against log transformed lot size and add a lowess line
plot(sale.price.log ~lot.size.log, data=mydata, xlab="natural log of lot size", ylab="natural log of sale price")
lines(lowess(mydata$lot.size.log, mydata$sale.price.log, delta=10), col="red") 
#plot log transformed sale price against log transformed living area and add a lowess line
plot(sale.price.log ~living.area.log, data=mydata, xlab="natural log of living area", ylab="natural log of sale price")
lines(lowess(mydata$living.area.log, mydata$sale.price.log, delta=10), col="red") 



#create the model with square root of sale price as response variable and living area as explanatory variable
z1 <- lm(sale.price.sqrt~living.area, data=mydata)
#Create the residual plot
resid1 <- resid(z1)
predict1 <- predict(z1)
plot(resid1~predict1,xlab = "sqaure root of sale price", ylab="residuals")
abline(h=0,ity=2)


#conduct the shapiro test for normality test
shapiro.test(resid1)


#create the histogram and normal probability plot for residuals
hist(resid1,xlab="residuals",main="Histogram of residuals")
qqnorm(resid1,xlab="normal scores",ylab="residuals",pch=16)
qqline(resid1)


#F test 
anova(z1)


#Critical value for F test
qf(0.05,1,1198,lower.tail=FALSE)


#Crtical value for t test
qt(0.025,1198,lower.tail = FALSE)


#confidence interval for coefficients
confint(z1, level = 0.95)
summary(z1)


#Calculate the Psuedo R2 with backtransformed original sale price 
mydata1 <- cbind(mydata, predict1)
mydata1$predict1.orig <- (mydata1$predict1)^2
SSE <- sum((mydata1$sale.price - mydata1$predict1.orig)^2)
SSY <- sum((mydata1$sale.price - mean(mydata1$sale.price))^2)
pseudo.R2 <- 1- SSE/SSY
pseudo.R2


#calculate root MSE
nrow(mydata1) 
rootMSE <- sqrt(SSE/1198)
rootMSE


#plot the original scatter plot between sale price and living area and the prediction interval and the fit line.
xnew <- seq(min(mydata1$living.area), max(mydata1$living.area), length.out = 100)
xnew
ynew.predi <- data.frame(predict(z1, newdata = data.frame(living.area = xnew), interval = "prediction", level = 0.95))
ynew.predi
new.values <- cbind(xnew, ynew.predi) 
new.values$ynew.predi.orig <- (ynew.predi)^2
print(new.values)
plot(sale.price ~ living.area, data = mydata1, pch = 16,xlab ="Living area in square feet", ylab="Sale price in dollars")
lines(new.values$ynew.predi.orig$fit ~ xnew, lty = 1)
lines(new.values$ynew.predi.orig$lwr ~ xnew, lty = 2)
lines(new.values$ynew.predi.orig$upr ~ xnew, lty = 2)


#get the coefficients
summary(z1)


head(mydata)
#create model B
z2 <- lm(sale.price.log ~ living.area.log + living.area + bedrooms, data= mydata)
resid2 <- resid(z2)
predict2 <- predict(z2)
plot(resid2 ~ predict2, xlab = "log of sale price", ylab = "residuals")
abline(h=0)


shapiro.test(resid2)


hist(resid2,xlab="residuals",main="Histogram of residuals")
qqnorm(resid2,xlab="normal scores",ylab="residuals",pch=16)
qqline(resid2)


#Calculate SSE, SSR and F statistics
mydata2 <- cbind(mydata, predict2)
SSE2 <- sum((mydata2$sale.price.log - mydata2$predict2)^2)
SSE2
SSReg2 <- sum((mydata2$predict2 - mean(mydata2$sale.price.log))^2)
SSReg2
MSE2 <- SSE2/1196
MSE2
MSSReg2 <- SSReg2/3
MSSReg2
Fstat2 <- MSSReg2/MSE2
Fstat2


#test the living area variables
z2.reduced <- lm(sale.price.log ~bedrooms, data=mydata)
predict2.reduced <- predict(z2.reduced)
mydata2$predict2.reduced <-predict2.reduced
anova(z2.reduced)
SSE2.reduced <- sum((mydata2$sale.price.log - mydata2$predict2.reduced)^2)
SSE2.reduced
partial.F <- ((SSE2.reduced-SSE2)/2)/(SSE2/1196)
partial.F


#Calculate F critical value
qf(0.95,2,1196,lower.tail=TRUE)


#test the bedrooms variable significance
z2.reduced1 <- lm(sale.price.log ~ living.area.log + living.area, data= mydata)
anova(z2.reduced1)
predict2.reduced1 <-predict(z2.reduced1)
mydata2$predict2.reduced1 <-predict2.reduced1
SSE2.reduced1 <- sum((mydata2$sale.price.log - mydata2$predict2.reduced1)^2)
partial.F1 <- ((SSE2.reduced1-SSE2)/1)/(SSE2/1196)
partial.F1


#calculate Fcritical value
qf(0.95,1,1196,lower.tail=TRUE)


#backtransform and calculate pseudo R2
mydata2
mydata2$predict2.orig <- exp(mydata2$predict2)
head(mydata2)
SSE2 <- sum((mydata2$sale.price - mydata2$predict2.orig)^2)
SSY2 <- sum((mydata2$sale.price - mean(mydata2$sale.price))^2)
pseudo.R2.2 <- 1- SSE2/SSY2
pseudo.R2.2


#Calculate MSE
rootMSE2 <- sqrt(SSE2/1196)
rootMSE2


#plot the original scatterplot and 2 model fit lines in original units
xnew2 <- seq(min(mydata2$living.area), max(mydata1$living.area), length.out = 100)
xnew2
xnew2.log <- log(xnew2)
xnew2.log
xnew2.lowbed <- rep_len(min(mydata2$bedrooms), length.out=100)
xnew2.lowbed
ynew.predi2.low <- data.frame(predict(z2, newdata = data.frame(living.area = xnew2,living.area.log = xnew2.log, bedrooms=xnew2.lowbed), interval = "prediction", level = 0.95))
ynew.predi2.low
new.values2.low <- cbind(xnew2,xnew2.lowbed, ynew.predi2.low) 
new.values2.low$ynew.predi2.low.orig <- exp(ynew.predi2.low)
head(new.values2.low)
xnew2.highbed <- rep_len(max(mydata2$bedrooms), length.out=100)
xnew2.highbed
ynew.predi2.high <- data.frame(predict(z2, newdata = data.frame(living.area = xnew2,living.area.log = xnew2.log, bedrooms=xnew2.highbed), interval = "prediction", level = 0.95))
ynew.predi2.high
new.values2.high <- cbind(xnew2,xnew2.highbed, ynew.predi2.high) 
new.values2.high$ynew.predi2.high.orig <- exp(ynew.predi2.high)
head(new.values2.high)
plot(sale.price ~ living.area, data = mydata2, pch = 16,xlab ="Living area in square feet", ylab="Sale price in dollars")
lines(new.values2.low$ynew.predi2.low.orig$fit ~ xnew2, lty = 1)
lines(new.values2.high$ynew.predi2.high.orig$fit ~ xnew2, lty = 1,col="red")
legend("topleft", legend = c("Lowest # of bedrooms", "Highest # of bedrooms"), fill=c(1, 2))


#Get the coefficients
summary(z2)


head(mydata)
#Create Model C and plot the residual plots
z3 <- lm(sale.price.log~lot.size.log + bedrooms + lot.size.log*bedrooms,data=mydata)
resid3 <-resid(z3)
predict3 <-predict(z3)
plot(resid3~predict3,xlab = "log of sale price", ylab="residuals")
abline(h=0)


# run shapiro-wilk test
shapiro.test(resid3)


#plot histogram and normal probability plot
hist(resid3,xlab="residuals",main="Histogram of residuals")
qqnorm(resid3,xlab="normal scores",ylab="residuals",pch=16)
qqline(resid3)


#Calculate SSE, SSY,SSReg, Fstatistics for ANOVA tabel
mydata3 <- cbind(mydata, predict3)
head(mydata3)
SSE3 <- sum((mydata3$sale.price.log - mydata3$predict3)^2)
SSE3
MSE3 <- SSE3/1196
MSE3
SSreg3 <- sum((mydata3$predict3 - mean(mydata3$sale.price.log))^2)
SSreg3
MSSreg3 <- SSreg3/3
MSSreg3
Fstat3 <- MSSreg3/MSE3
Fstat3

anova(z3)


#get the t statistics
summary(z3)


#calculate the t critical value
qt(0.975,1996,lower.tail=TRUE)


#backtransform response variable and calculate pseudo r2
mydata3$predict3.orig <- exp(mydata3$predict3)
head(mydata3)
SSE3.orig <- sum((mydata3$sale.price - mydata3$predict3.orig)^2)
SSY3.orig <- sum((mydata3$sale.price - mean(mydata3$sale.price))^2)
pseudo.R2.3 <- 1- SSE3.orig/SSY3.orig
pseudo.R2.3


#calculate root MSE
anova(z3)
rootMSE3 <- sqrt(SSE3.orig/1196)
rootMSE3


#plot the original scatter plot between sale price and lot size, add 2 model fit lines to the graph in original units
xnew3 <- seq(min(mydata3$lot.size), max(mydata3$lot.size), length.out = 100)
xnew3
summary(z3)
xnew3.log <- log(xnew3)
xnew3.log
xnew3.lowbed <- rep_len(min(mydata3$bedrooms), length.out=100)
xnew3.lowbed
xnew3.interaction <- xnew3.log * xnew3.lowbed
xnew3.interaction
ynew.predi3.low <- data.frame(predict(z3, newdata = data.frame(lot.size.log=xnew3.log,bedrooms=xnew3.lowbed), interval = "prediction", level = 0.95))
ynew.predi3.low
new.values3.low <- cbind(xnew3,xnew3.lowbed, xnew3.log,xnew3.interaction,ynew.predi3.low) 
new.values3.low$ynew.predi3.low.orig <- exp(ynew.predi3.low)
head(new.values3.low)
xnew3.highbed <- rep_len(max(mydata3$bedrooms), length.out=100)
xnew3.highbed
ynew.predi3.high <- data.frame(predict(z3, newdata = data.frame(lot.size.log=xnew3.log,bedrooms=xnew3.highbed), interval = "prediction", level = 0.95))
ynew.predi3.high
new.values3.high <- cbind(xnew3,xnew3.highbed, xnew3.log,xnew3.interaction,ynew.predi3.high) 
new.values3.high$ynew.predi3.high.orig <- exp(ynew.predi3.high)
head(new.values3.high)
plot(sale.price ~ lot.size, data = mydata3, pch = 16,xlab ="Lot size in acres", ylab="Sale price in dollars")
lines(new.values3.low$ynew.predi3.low.orig$fit ~ xnew3, lty = 1)
lines(new.values3.high$ynew.predi3.high.orig$fit ~ xnew3, lty = 1,col="red")
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
legend("topright", inset=c(-0.35,0),legend = c("1 bedroom", "7 bedrooms"), fill=c(1, 2))


#get the coefficients of the Model C
summary(z3)
