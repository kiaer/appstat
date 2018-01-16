climate = read.table("climate.txt", header=T, sep = "\t")
weekly = read.csv("weekly_pos.csv", header=T)
camp = read.table("case2regions4.txt", header=T, sep = "\t")
library(MASS)
library(car)
library(lattice)
#camp <- data.frame(weekly, climate)
sum(camp$total)
summary(camp)
#plot(camp, panel=panel.smooth)
boxplot(pos/total ~ week, camp)

plot(camp$week, camp$aveTemp)
plot(camp$week, camp$pos/camp$total)

lm1 <- lm((pos / total) ~ (aveTemp + maxTemp + relHum + sunHours + precip )^2, data=camp)
par(mfrow=c(2,2))
plot(lm1)

residualPlots(lm1)
summary(lm1)
boxcox(lm1, lambda = seq(0, 1, length.out = 20), data=camp)

lm2 <- stepP(lm1)$object
summary(lm2)
anova(lm2)
drop1(lm2, test="F")
library(xtable)
xtable(residualPlots(lm1))

stepP <- function(object, level=0.05, verbose=FALSE){
  if (!("lm" %in% class(object))){
    error("First argument should be an lm object")
  }
  d1 <- drop1(object, test="F")[-1,]
  maxP <- max(d1[["Pr(>F)"]])
  lmTmp <- object
  maxVar <- row.names(d1)[d1[["Pr(>F)"]]==maxP]
  history <-NULL # For storing the history of models
  tmpFormula <- paste(as.character(formula(lmTmp))[c(2,1,3)], collapse=" ")
  
  while(maxP > level & nrow(d1)>=1){
    maxVar <- row.names(d1)[d1[["Pr(>F)"]]==maxP]
    history <- rbind(history,data.frame(formula= tmpFormula, maxP=maxP, maxVar = maxVar) )
    lmTmp <- update(lmTmp, paste(".~.-",maxVar))
    d1 <- drop1(lmTmp, test="F")[-1,]
    maxP <- max(d1[["Pr(>F)"]])
    tmpFormula <- paste(as.character(formula(lmTmp))[c(2,1,3)], collapse=" ")
    if (verbose)
      print(tmpFormula) # Print the formula after each reduction
  }
  # Also adding the final model to document the p-value
  maxVar <- row.names(d1)[d1[["Pr(>F)"]]==maxP]
  tmpFormula <- paste(as.character(formula(lmTmp))[c(2,1,3)], collapse=" ")
  history <- rbind(history,data.frame(formula= tmpFormula, maxP=maxP, maxVar = maxVar) )
  return(list(object=lmTmp, history=history))  
}

mod <- stepP(lm2)
lm3 <- mod$object

# par(mfrow=c(2,2))
# plot(lm12)
# residualPlots(lm12)

a1 <- lm((pos / total)^0.7 ~ (maxTemp + relHum  + precip + sunHours + I(aveTemp^2) + I(sunHours^2) + I(precip^2))^2, data=camp, subset= c(-496))
par(mfrow=c(2,2))
plot (a1)
summary(a1)
boxcox(a1)
residualPlots(a1)

a3 <- stepP(a1)$object
drop1(a3, test="F")
anova(a3)
summary(a3)
plot(a3)

b1 <- lm((pos / total)^0.7 ~ (aveTemp + maxTemp + relHum + precip  + sunHours + I(aveTemp^2) + I(sunHours^2) + I(precip^2))^2, data=camp, subset = -496)
par(mfrow=c(2,2))
plot(b1)
b2 <- stepP(b1)$object
summary(b2)
anova(a3)

anova(a3,b2)
sum(camp$total)

lec.fun<-function(data, reference, others=names(data)[names(data)!=reference], ref.values=seq(min(data[[reference]]),max(data[[reference]]),length=30)){
  pdata<-data.frame(reference=ref.values)
  names(pdata)<-reference
  for(i in others){
    lmtmp<-lm(as.formula(paste(i,"~",reference)),data)
    pdata[[i]]<-predict(lmtmp,newdata=pdata[reference])
  }
  return(pdata)
}

pred.frame <- function(reference, data, others=names(data)[ !(names(data)%in%names(reference)) ]){
  if (class(reference) == "list"){  ## Need to run expand.grid( <list>)
    pdata <- expand.grid(reference)
  } else {
    pdata <- reference
  }
  ref.model <- names(reference)
  if(length(names(reference))>1)
    ref.model <- paste(ref.model, sep="+")
  for(i in others){
    lmtmp<-lm(as.formula(paste(i,"~",ref.model)),data)
    pdata[[i]]<-predict(lmtmp,newdata=pdata)
  }
  return(pdata)
}

par(mfrow=c(1,1))
plot((pos / total) ~ maxTemp,data=camp, ylab = "Proportion of infections", xlab = "Average temperature")

campy <- lec.fun(camp,reference="maxTemp",others=c("aveTemp", "relHum", "precip", "sunHours"), ref.values=0:30)
campyb <- lec.fun(camp,reference="sunHours",others=c("maxTemp", "relHum", "precip", "aveTemp"), ref.values=0:90)

#pred.campy<-predict(b2, int="p",newdata=campy)
pred.campya<- predict(a3, int="p",newdata=campy)
#matlines(campy$aveTemp,pred.campy^(10/7),lty=c(1,2,2),col=3,lwd=2)
matlines(campy$maxTemp,pred.campya^(10/7),lty=c(1,2,2),col=2,lwd=2)

plot((pos / total) ~ sunHours,data=camp, ylab = "Proportion of infections", xlab = "Weekly hours of sun")
pred.campyb<- predict(a3, int="p",newdata=campyb)
#matlines(campy$aveTemp,pred.campy^(10/7),lty=c(1,2,2),col=3,lwd=2)
matlines(campyb$sunHours,pred.campyb^(10/7),lty=c(1,2,2),col=2,lwd=2)


p.sunHours <- seq(0,100,by = 1)
p.aveTemp <- seq(0, 22)
par(mfrow=c(1,1))

## Creating prediction data.frame and then predicting
pred.data <- pred.frame(reference = list(sunHours=p.sunHours, aveTemp=p.aveTemp), data = camp, others = c("maxTemp", "relHum", "precip"))
pred <- predict(a3, newdata = pred.data, interval = "predict")

## Wrapping the predictions as a matrix 
z <- matrix(pred[,"fit"], nrow=length(p.sunHours))
z2 <- z
z2[z2 > 1] <- NA

## First an image:
image(p.sunHours, p.aveTemp, z2, xlab = "sunHours", ylab = "Average Temperature")
## Adding a contour:
contour(p.sunHours, p.aveTemp, z2, add=TRUE, labcex = 1.5)
points(aveTemp ~ sunHours, data= camp, cex=0.5) # To show the observations
z2 <- z
z2[z2 > 1] <- NA
## First an image:
image(p.sunHours, p.aveTemp, z2, xlab = "sunHours", ylab = "Precipitation")
## Adding a contour:
contour(p.sunHours, p.aveTemp, z2, add=TRUE, labcex = 1.5)
points(aveTemp ~ sunHours, data= camp, cex=0.5) # 

