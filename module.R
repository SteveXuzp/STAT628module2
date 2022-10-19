rm(list=ls())

mydata = read.csv("BodyFat.csv")

# analyse raw data
# briefly see the data structure
head(mydata)
tail(mydata)

summary(mydata)

## clean up data
# first we look at the response variable, which is BODYFAT
jpeg(filename = "hist.jpg")
hist(mydata$BODYFAT,breaks=30,cex.lab=1.5,cex.main=1.5,
     main="Histogram of Body Fat %",xlab="Body Fat %")
dev.off()
sort(mydata$BODYFAT)

# we first consider the appropriate range of bodyfat
# 0 and 1.9 might be too low
compute.bodyfat = function(density){
  return (495/density - 450)
}

index0 = which(mydata$BODYFAT == 0)
compute.bodyfat(mydata[index0, ]$DENSITY) # the result is less than 0, which is impossible!

index1 = which(mydata$BODYFAT == 1.9)
compute.bodyfat(mydata[index1, ]$DENSITY) # the result is 0.69, which is even less!

mydata = mydata[-c(index0, index1), ] # remove these two rows

# consider the consistency of bodyfat and density!

BODYFAT.get = compute.bodyfat(mydata$DENSITY)

jpeg(filename = "body_dif.jpg")
plot(BODYFAT.get - mydata$BODYFAT, pch=20, ylim=c(-10, 10), cex.lab=1.5, cex.main=1.5, xlab='IDNO', ylab='Difference',main="Difference between original and computed body fat") 
dev.off()
# we find that for most data , the difference between actual and calculated are [-2.5, 2.5]

# Show the data we delete IDNO and BODYFAT
index_bodyfat = mydata$IDNO[which(abs(BODYFAT.get - mydata$BODYFAT) > 2.5)]
bodyfat_delet = mydata$BODYFAT[which(abs(BODYFAT.get - mydata$BODYFAT) > 2.5)]
mydata = mydata[-which(abs(BODYFAT.get - mydata$BODYFAT) > 2.5), ] # remove those points that are not consistent!

# then we look at the predictor variable, which is ADIPOSITY

sort(mydata$ADIPOSITY)


# Then we consider the consistency of ADIPOSITY and weight, heigth
# The constant here is for convert unit.
compute.adiposity = function(weight, height){
  return ((weight * 0.4545) / (height * 0.0254)^2)
}

ADIPOSITY.get = compute.adiposity(weight=mydata$WEIGHT, height=mydata$HEIGHT)
jpeg(filename = "bmi_dif.jpg")
plot(ADIPOSITY.get - mydata$ADIPOSITY, pch=20, cex.lab=1.5, cex.main=1.5, ylim=c(-3,3), xlab='IDNO', ylab='Difference',main="Difference between original and computed BMI")
dev.off()
# we find that for most data , the difference between actual and calculated are [-0.5, 0.5]

index_bmi = mydata$IDNO[which(abs(ADIPOSITY.get - mydata$ADIPOSITY) > 0.5)]
bmi_delet = mydata$ADIPOSITY[which(abs(ADIPOSITY.get - mydata$ADIPOSITY) > 0.5)]
mydata = mydata[-which(abs(ADIPOSITY.get - mydata$ADIPOSITY) > 0.5), ]

# height is (inches)! but not cm
# while other distacne is cm
# change unit of height to cm!

mydata$HEIGHT = mydata$HEIGHT*2.54

sort(mydata$HEIGHT)
sort(mydata$WEIGHT)

# from the articles:
#   
#   https://www.cdc.gov/healthyweight/assessing/index.html#:~:text=If%20your%20BMI%20is%20less,falls%20within%20the%20obese%20range.
# 
# https://www.forbes.com/health/body/body-fat-percentage/#footnote_1
#   
#    https://pubmed.ncbi.nlm.nih.gov/29894868/
#   
#   we conclude BMI and ABDOMEN are very important!!!!!

# so firstly we consider use the variable, adiposity and abdomen
lm1.1 = lm(BODYFAT ~ ADIPOSITY, data=mydata)
lm1.2 = lm(BODYFAT ~ ABDOMEN, data=mydata)

summary(lm1.1)
summary(lm1.2) # we find that lm1.2 is better (R^2 is larger!)

# lm1.2 is better because of larger R^2

# make graph of lm1.2
library(ggplot2)

# lm1.2
jpeg(filename = "fat_abdomen.jpg")
ggplot(data=mydata, aes(x=ABDOMEN, y=BODYFAT)) +
  geom_point(size=3,shape=20) +
  labs(x="Abdomen", y="Bodyfat %", title="Model for body fat and abdomen")
dev.off()

# we find that there are some high leverage points.
# we plot the leverage plot
pii = hatvalues(lm1.2)
n = dim(mydata)[1]
jpeg(filename = "leverage_first.jpg")
plot(1:n, pii, type="p",pch=19,cex=1.2,cex.lab=1.5,cex.main=1.5,
     xlab="Index (Each Observation)",ylab="Pii",main="Leverage Values(lm1.2) (Pii)")
dev.off()

# we delete those high leverage points and fit the model again.
index_lev = mydata$IDNO[which(pii > 0.04)]
mydata[which(pii > 0.04),] # check these points
write.csv(x = mydata,file = "cleaned_data.csv", row.names = FALSE) #output cleaned data

lm1.2 = lm(BODYFAT ~ ABDOMEN, data=mydata)
summary(lm1.2)

# then we use stepwise to consider the next variable to add into our model.
# then we would like to add some other variables!
# if we just use the stepwise selection!
# we only allow at most four variales in our model!!! to avoid complex result.
fit <- lm(BODYFAT ~ 1, data=mydata)
fit <- update(fit, .~. + ABDOMEN)
add1(fit, ~AGE+WEIGHT+HEIGHT+ADIPOSITY+NECK+CHEST+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST+ABDOMEN, test = "F")

# The first variable to add is weight! because of its lowest p-value.
# weight is also to interpret and easy to measure
lm2 = lm(BODYFAT ~ ABDOMEN+WEIGHT, data=mydata)
summary(lm2)

# this is high improvement while only one variable is added into model.
fit <- update(fit, .~. + WEIGHT)
add1(fit, ~AGE+WEIGHT+HEIGHT+ADIPOSITY+NECK+CHEST+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST+ABDOMEN, test = "F")
# then we add the wrist to the model
lm3 = lm(BODYFAT ~ ABDOMEN+WEIGHT+WRIST, data=mydata)
summary(lm3)

fit <- update(fit, .~. + WRIST)
add1(fit, ~AGE+WEIGHT+HEIGHT+ADIPOSITY+NECK+CHEST+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST+ABDOMEN, test = "F")
# then we add the FOREARM
lm4 = lm(BODYFAT ~ ABDOMEN+WEIGHT+WRIST+FOREARM, data=mydata)
summary(lm4)

fit <- update(fit, .~. + FOREARM)
add1(fit, ~AGE+WEIGHT+HEIGHT+ADIPOSITY+NECK+CHEST+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST+ABDOMEN, test = "F")
# we consider drop variable now!
drop1(fit, test='F')
# all the variables are significant!

# Results for four candidate models
lm1 = lm(BODYFAT ~ ABDOMEN, data=mydata)
lm2 = lm(BODYFAT ~ ABDOMEN+WEIGHT, data=mydata)
lm3 = lm(BODYFAT ~ ABDOMEN+WEIGHT+WRIST, data=mydata)
lm4 = lm(BODYFAT ~ ABDOMEN+WEIGHT+WRIST+FOREARM, data=mydata)
summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)

# although lm3 and lm4 have higher R^2
# they are more complex
# From the R^2, we conclude that our final model is lm2

# Check for independence
plot(resid(lm2),type="l",xlab="Index", ylab="Residuals",main="Residual Line Plot")

# diagnostic
par(mfrow = c(2, 2))

# Residual Plot
plot(predict(lm2), resid(lm2), pch=19, cex=1.2, cex.lab=1.5, cex.main=1.5,
     xlab="Predicted Body Fat %", ylab="Residuals",main="Residual Plot")
abline(a=0,b=0,col="black",lwd=3)

# QQ plot
qqnorm(rstandard(lm2), pch=19, cex=1.2, cex.lab=1.5, cex.main=1.5,
       main="Normal Q-Q Plot of the Residuals")
abline(a=0,b=1,col="black",lwd=3)

# leverage plot
pii = hatvalues(lm2)
n = dim(mydata)[1]
plot(1:n, pii, type="p", pch=19, cex=1.2, cex.lab=1.5, cex.main=1.5,
     xlab="Index (Each Observation)", ylab="Pii", main="Leverage Values final model (Pii)")

# cook's distance
cooki = cooks.distance(lm2)
plot(1:n, cooki, type="p", pch=19, cex=1.2, cex.lab=1.5, cex.main=1.5,
     xlab="Index (Each Observation)",ylab="Cook's Distance",main="Influence Values (Cook's Distance)")

# Prediction for average Americans
predict(lm2,newdata=data.frame(ABDOMEN=40.5*2.54, WEIGHT = 199.8),interval="predict")

# Prediction for Usain Bolt 
predict(lm2,newdata=data.frame(ABDOMEN=33*2.54, WEIGHT = 207),interval="predict")

# 95% CI of coefficients
confint(lm2)

# export the graph
jpeg(filename = "residline.jpg")
plot(resid(lm2),type="l",xlab="Index", ylab="Residuals",main="Residual Line Plot")
dev.off()

jpeg(filename = "resid.jpg")
plot(predict(lm2), resid(lm2), pch=19, cex=1.2, cex.lab=1.5, cex.main=1.5,
     xlab="Predicted Body Fat %", ylab="Residuals",main="Residual Plot")
abline(a=0,b=0,col="black",lwd=3)
dev.off()

jpeg(filename = "qqplot.jpg")
qqnorm(rstandard(lm2), pch=19, cex=1.2, cex.lab=1.5, cex.main=1.5,
       main="Normal Q-Q Plot of the Residuals")
abline(a=0,b=1,col="black",lwd=3)
dev.off()

jpeg(filename = "leverage_final.jpg")
pii = hatvalues(lm2)
n = dim(mydata)[1]
plot(1:n, pii, type="p", pch=19, cex=1.2, cex.lab=1.5, cex.main=1.5,
     xlab="Index (Each Observation)", ylab="Pii", main="Leverage Values final model (Pii)")
dev.off()

jpeg(filename = "cook's distance.jpg")
cooki = cooks.distance(lm2)
plot(1:n, cooki, type="p", pch=19, cex=1.2, cex.lab=1.5, cex.main=1.5,
     xlab="Index (Each Observation)",ylab="Cook's Distance",main="Influence Values (Cook's Distance)")
dev.off()

# Prediction situations
n = length(mydata$IDNO)
per5 = sum(abs(resid(lm2)/mydata$BODYFAT) < 0.05)/n
per15 = sum(abs(resid(lm2)/mydata$BODYFAT) < 0.15)/n

