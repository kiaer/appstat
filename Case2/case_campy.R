climate = read.table("climate.txt", header=T, sep = "\t")
weekly = read.csv("weekly_pos.csv", header=T)
library(MASS)
camp <- data.frame(weekly, climate)

summary(camp)
plot(camp, panel=panel.smooth)

lm1 <- glm((Positive / Total) ~ aveTemp * maxTemp * relHum * sunHours * precip * week, data=camp)
par(mfrow=c(2,2))
plot (lm1)
summary(lm1)
boxcox(lm1)

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

mod <- stepP(lm1)
lm2 <- mod$object
anova(lm2, test="LRT")
drop1(lm2, test = "LRT")
lm3<-update(lm2,.~.-relHum:precip:week)
mod <- stepP(lm3)
lm4 <- mod$object
lm5 <- step(lm4)
drop1(lm5, test = "LRT")
anova(lm5, test="LRT")
lm6<-update(lm5,.~.-relHum:sunHours)
drop1(lm6,test="LRT")
mod <- stepP(lm6)
anova((lm7 <- mod$object), test="LRT")
lm8 <- update(lm7, .~. -relHum)
drop1(lm8, test="LRT")
anova(lm8, test="LRT")
lm9 <- update(lm8, .~. -sunHours)
drop1(lm9, test="LRT")
anova(lm9, test="LRT")
lm10 <- update(lm9, .~. -precip)
drop1(lm10, test="LRT")
mod <- stepP(lm10)
drop1((lm11 <- mod$object), test="LRT")
anova(lm11, test="LRT")
lm12 <- update(lm11, .~. -relHum:precip:sunHours)
drop1(lm12,test="LRT")
anova(lm12, test="LRT")
lm13 <- update(lm12, .~. -week:relHum)
drop1(lm13,test="LRT")
anova(lm13, test="LRT")
lm14 <- update(lm13, .~. -relHum:precip)
drop1(lm14,test="LRT")
anova(lm14, test="LRT")

par(mfrow=c(2,2))
plot(lm14)

a1 <- glm(((Total - Positive) / Total) ~ .*., data=camp)
plot(a1)
boxcox(a1, lambda = seq(0.3, 1.2, length.out = 20), data=camp)

a2 <- (mod <- stepP(a1))$object
drop1(a2, test="F")
plot(a2)

boxcox(lm1, lambda = seq(0.3, 1, length.out = 20), data=camp)

lm2 <- step(lm1)
anova(lm2, test="F")
drop1(lm2, test="F")
lm3 <- stepP(lm2)
summary(lm3$object)
anova(lm3$object, test="F")
drop1(lm3$object, test="F")



camp$fracpow <- (camp$Positive / camp$Total)^0.7
lm2 <- glm(pospow   ~ aveTemp * maxTemp * relHum * sunHours * precip * week, data=camp)
par(mfrow=c(2,2))
plot(lm2)
anova(lm2, test="F")
summary(lm2)



lm3<- step(lm2)
anova(lm3, test="F")
drop1(lm3, test="F")
lm4<-update(lm3,.~.-aveTemp:maxTemp:relHum)
drop1(lm4, test="F")
lm5<-update(lm4,.~.-aveTemp:maxTemp:sunHours)
drop1(lm5, test="F")
lm6<-update(lm5,.~.-aveTemp:maxTemp)
drop1(lm6, test="F")
summary(lm6)
anova(lm6, test="F")
lm7<-update(lm6,.~.-relHum:sunHours)
drop1(lm7, test="F")
lm8<-update(lm7,.~.-aveTemp:relHum:sunHours)
drop1(lm8, test="F")
lm9<-update(lm8,.~.-aveTemp:relHum)
drop1(lm9, test="F")
lm10<-update(lm9,.~.-maxTemp:relHum:sunHours)
drop1(lm10, test="F")
lm11<-update(lm10,.~.-maxTemp:relHum)
drop1(lm11, test="F")
anova(lm11, test="F")
lm12<-update(lm11,.~.-aveTemp:sunHours)
drop1(lm12, test="F")
lm13<-update(lm12,.~.-maxTemp:sunHours)
drop1(lm13, test="F")
anova(lm13, test="F")
summary(lm13)

par(mfrow=c(2,2))
plot(lm13)
coef(lm13)

lmtest <- glm(pospow ~ aveTemp * relHum, data=camp )
plot (lmtest)
summary(lmtest)
