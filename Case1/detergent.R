det <- read.table("SPR.txt",header=TRUE, colClasses = c("factor", "numeric", "numeric", "factor", "numeric", "factor", "factor"))
det$RunDate <- factor(det$RunDate,
                      labels = c("25/11/2008","27/11/2008", "3/12/2008", "5/12/2008", "8/12/2008"))

plot(det, panel=panel.smooth)
summary(det)
barplot(tapply(det$Response,list(det$EnzymeConc, det$Enzyme),mean),beside=TRUE)
par(mfrow=c(2,1),mar=c(3,3,2,1),mgp=c(2,0.7,0))
boxplot(Response ~ EnzymeConc,det)
boxplot(Response ~ Enzyme, det)

par(mfrow=c(2,2))
lm1 <- lm(Response ~ RunDate+Cycle+Enzyme*EnzymeConc+DetStock+CaStock, det)
summary(lm1)
plot(lm1)
anova(lm1)
lm2 <- step(lm1)
summary(lm2)
anova(lm2)
drop1(lm2, test="F")

lmt <- lm(Response ~ Enzyme * EnzymeConc, det)
library(MASS)
par(mfrow=c(1,1))
boxcox(lm1, data=det)
boxcox(lm1, lambda = seq(-0.5, 1, length.out = 20), data=det)
det$concpow <- det$EnzymeConc^0.4

plot(det$concpow, det$Rpower)

det$logR <- log(det$Response)
lm3<- step(lm(logR ~ RunDate+Cycle+Enzyme*EnzymeConc*DetStock*CaStock, det, subset = -14))
par(mfrow=c(2,2))
plot(lm3, col=as.numeric(as.factor(det$EnzymeConc[-14])))
summary(lm3)
anova(lm3)
AIC(lm3)
drop1(lm3, test="F")
coef(lm3)

det$Rpower <- det$Response^0.4
lm4<- step(lm(Rpower ~ RunDate+Cycle+Enzyme*concpow+DetStock+CaStock, det, subset = -147))
par(mfrow=c(2,2))
plot(lm4, col=as.numeric(as.factor(det$EnzymeConc[-147])))
summary(lm4)
anova(lm4)
AIC(lm4)
AIC(lm3)
drop1(lm4, test="F")
summary(lm5<-update(lm4,.~.-CaStock))
drop1(lm5, test="F")
summary(lm6<-update(lm5,.~.-DetStock:CaStock))
anova(lm6)


anova(lm3, lm5)
AIC(lm3)
AIC(lm5)

plot(det$EnzymeConc, exp(det$logR), col=as.numeric(det$EnzymeA), pch=19)
interaction.plot(det$EnzymeConc, det$Enzyme, det$, ylim = c(0,1500))

# One style
# Calculating 95% confidence intervals:
pred.d<-expand.grid(Enzyme=levels(det$Enzyme), concpow=seq(0,3, 0.2), DetStock="Det0", CaStock="Ca0")
pred<-predict(lm4,pred.d,int="c")^(5/2) # Predictions on original scale
# Plotting
par(mfrow=c(1,2))
matplot(c(0,15),range(pred),type="n",ylim=c(0,2000), ylab="Response", xlab="Concentration", main="Det0")
matlines((0:15), cbind(matrix(pred[,1],nrow=16,byrow=TRUE),matrix(pred[,2],nrow=16,byrow=TRUE),matrix(pred[,3],nrow=16,byrow=TRUE)),col=2:6,lty=rep(c(1,2,2),each=5),lwd=1)
legend("topleft",legend=levels(det$Enzyme),lty=1,col=2:6)

pred.d<-expand.grid(Enzyme=levels(det$Enzyme),concpow=seq(0,3,0.2),DetStock="Det+", CaStock="Ca0")
pred<- predict(lm4,pred.d,int="c")^(5/2) # Predictions on original scale
matplot(c(0,15),range(pred),type="n",ylim=c(0,2000), ylab="Response", xlab="Concentration", main="Det+")
matlines(0:15, cbind(matrix(pred[,1],nrow=16,byrow=TRUE),matrix(pred[,2],nrow=16,byrow=TRUE),matrix(pred[,3],nrow=16,byrow=TRUE)),col=2:6,lty=rep(c(1,2,2),each=5),lwd=1)
legend("topleft",legend=levels(det$Enzyme),lty=1,col=2:6)

