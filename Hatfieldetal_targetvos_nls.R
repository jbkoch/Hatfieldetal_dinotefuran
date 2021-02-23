# Target B. vosnesenskii
# Make plot of the target Bombus dataset

# This R code represents the analysis conducted for Figure 4 of Hatfield et al.
# The data used for this analysis is "targetbombus.csv"

# setwd
setwd("/Users/jonathankoch/Google Drive/TargetBombus")

# list.files
list.files()

# library
library(car)
library(ggplot2)
library(effects)
library(nlme)
library(MASS)
library(nlstools)
library(propagate)

# read dataset
df <- read.csv("targetbombus.csv", header = TRUE)
names(df)
attach(df)
nrow(df)

# scatterplot
scatterplot(total.families~n, data = df)

# residuals vs. fitted
lm1<-lm(total.families~n, data = df)

par(mfrow=c(1,1))
plot(lm1) # clearly non-normal residuals

# examine the diagnostics
par(mfrow=c(1,1))
plot(resid(tar.nls)~fitted(tar.nls)) # alpha = 0.1, beta =1; still wedge shaped

# fit an asymptoic relationship (makes more sense)
tar.nls1 <- nls(total.families~SSasymp(n, a,b,c), df)
plot(n,total.families,pch=16,
     xlim=c(0,3000),ylim=c(0,700),
     xlab = "Sampled individvuals",
     ylab = "Estimated number of colonies")
xv<-seq(0,3000, length.out = 300)
length(xv)
yv<-predict(tar.nls1, newdata = data.frame(n=xv),
            interval = 'confidence')
length(yv)
lines(xv,yv)


# fit an asymptoic relationship (makes more sense)
tar.nls1 <- nls(total.families~SSasymp(n, a,b,c), df)
predict(tar.nls1) #fitted values at observed times
opar <- par(las=1)
plot(total.families~n,data=df,col="black", bg = "gray",
     xlim=c(0,5000),ylim=c(0,700),
     xlab="Individuals",
     ylab="Estimated number of colonies", 
     pch = 21)
tt<-seq(0,3000,length=300)
lines(tt,predict(tar.nls1,list(n=tt)))
par(opar)

# confidence interval excercise
# Arguments3
# input: a numeric vector of values at which to evaluate the model.

# (a) Asym: a numeric parameter representing the horizontal asymptote on the right side (very large values of input).
# (b) R0: a numeric parameter representing the response when input is zero.
# (c) lrc: a numeric parameter representing the natural logarithm of the rate constant.

#Model
xv<-seq(0,5000, length.out = 500)
yv <- predictNLS(tar.nls1, newdata = data.frame(n=xv),
                 nsim=100000, interval = "confidence")
yv<- yv$summary
lines(yv$Sim.Mean ~ xv, lwd=2)
lines(yv$Prop.Mean.1 ~ xv, lwd=2,col = "black")
lines(yv$"Sim.2.5%" ~ xv, lwd=1, lty = 2)
lines(yv$"Sim.97.5%" ~ xv, lwd=1, lty = 2)
# lines(yv$"Prop.2.5%" ~ xv, lwd=1, lty = 2, col = "blue")
# lines(yv$"Prop.97.5%" ~ xv, lwd=1, lty = 2, col = "blue")

confint2(tar.nls1)
summary(tar.nls1)

