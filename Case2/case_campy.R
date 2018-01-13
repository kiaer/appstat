climate = read.table("climate.txt", header=T, sep = "\t")
weekly = read.csv("weekly_pos.csv", header=T)
library(MASS)
camp <- data.frame(weekly, climate)

summary(camp)
plot(camp, panel=panel.smooth)

lm1 <- lm((Positive / Total) ~ (aveTemp + maxTemp + relHum + sunHours + precip + week)^3, data=camp)
par(mfrow=c(2,2))
plot (lm1)
summary(lm1)
boxcox(lm1)

lm2 <- step(lm1)
summary(lm2)
anova(lm2)
drop1(lm2, test="F")

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
anova(lm3)
drop1(lm3, test = "F")
lm4 <- update(lm3, .~. -aveTemp:maxTemp:sunHours)
drop1(lm4, test="F")
lm5 <- step(lm4)
drop1(lm5, test="F")
lm6 <- update(lm5, .~. -maxTemp:sunHours)
drop1(lm6, test="F")
lm7 <- update(lm6, .~. -aveTemp:sunHours)
drop1(lm7, test="F")
lm8 <- update(lm7, .~. -maxTemp)
drop1(lm8, test="F")
anova(lm8)
lm9 <- update(lm8, .~. -relHum:precip)
drop1(lm9, test="F")
lm10 <- update(lm9, .~. -aveTemp:relHum:precip)
drop1(lm10, test="F")
lm11 <- update(lm10, .~. -aveTemp:precip)
drop1(lm11, test="F")
anova(lm11)
lm12 <- update(lm11, .~. -sunHours)
drop1(lm12, test="F")
anova(lm12)

par(mfrow=c(2,2))
plot(lm12)
