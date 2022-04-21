library(devtools)
library(robustbase)
library(FRB)
library(MASS)
#plot(phones$year,phones$calls,xlab="Year",ylab="Calls",main="Belgian International phone calls dataset")
rm(list=ls())
phones$dd=phones$year-c(mean(phones$year))
phones
a <- lmrob(calls ~dd,method="MM", data=phones)
set.seed(123)
startTime1 <- Sys.time()
tmp <- frb(lmrob.object=a, nboot=5000, return.coef=TRUE)
endtime1 <- Sys.time()
print(endtime1-startTime1)
cc=cov(tmp$coef)
plot(phones$year,phones$calls,main="Belgian international phone calls")
abline(lm(phones$calls~phones$year),col="red")
abline(-52.424,1.101)
legend(50,200,legend=c("OLS","MM-estimation"),fill=c("red","black"))
lconf=matrix(c(rep(0,2)),nrow=2)
uconf=matrix(c(rep(0,2)),nrow=2)
lconf=matrix(a$coef,nrow=2)-qnorm(.975)*matrix(sqrt(diag(cc)),nrow=2)
uconf=matrix(a$coef,nrow=2)+qnorm(.975)*matrix(sqrt(diag(cc)),nrow=2)
cbind(lconf,uconf)
##########Classical bootstrap########
# Containers for the coefficients
sample_coef_intercept <- NULL
sample_coef_x1 <- NULL
library(MASS)
data1=phones
data1=as.data.frame(data1)
d=nrow(data1)
data1
sample_coef_intercept=c()
sample_coef_x1=c()
startTime <- Sys.time()
for (i in 1:5000) {
  #Creating a resampled dataset from the sample data
  sample_d = as.data.frame(data1[sample(1:d,d, replace = TRUE), ])
  #Running the regression on these data
  model_bootstrap <- lm(calls ~ dd, data = sample_d)
  
  #Saving the coefficients
  sample_coef_intercept[i] <-model_bootstrap$coefficients[1]
  
  sample_coef_x1[i] <-model_bootstrap$coefficients[2]
}
dd=cbind(sample_coef_intercept,sample_coef_x1)
endtime <- Sys.time()
print(endtime-startTime)
###############
par(mfrow=c(1,2))
c=cbind(tmp$coef[,1],tmp$coef[,2])
c[,1]=c[,1] - a$coef[1]
c[,2]=c[,2]-  a$coef[2]
plot(c[,1],c[,2],xlab="Intercept",ylab="slope",main="Fast Bootstrap",xlim=c(-20,100),ylim=c(-5,15))
dd[,1]=dd[,1]-a$coef[1]
dd[,2]=dd[,2]-a$coef[2]
plot(dd[,1],dd[,2],xlab="Intercept",ylab="slope",main="Classical bootstrap",xlim=c(-20,100),ylim=c(-5,15))
lconf1=mean(sample_coef_intercept)-qnorm(.975)*var(sample_coef_intercept)
lconf2=mean(sample_coef_x1)-qnorm(.975)*var(sample_coef_x1)
uconf1=mean(sample_coef_intercept)+qnorm(.975)*var(sample_coef_intercept)
uconf2=mean(sample_coef_x1)+qnorm(.975)*var(sample_coef_x1)
cbind(c(lconf1,uconf1),c(lconf2,uconf2))
library(ggplot2)
library(lattice)
par(mfrow=c(1,2)) 
# Create data
gfg <- data.frame(x = c(2,197,0.21,287), 
                   Bootstrap = rep(c("Length-Intercept", "Length-Slope"),each =2),
                   subgroup = c("Fast Bootstrap","Classical Bootstrap"))
 
# Create grouped barplot using ggplot2
ggplot(gfg,aes(x = Bootstrap, y =x, fill = subgroup)) +
geom_bar(stat = "identity", position = "dodge",width=0.5)+labs(title="Comparison of length for Belgian international phone calls")

gfgg <- data.frame(Time = c(0.25,44.4), 
                   Bootstrap = rep(c("Computation Time"),each =2),
                   subgroup = c("Fast Bootstrap","Classical Bootstrap"))
 
p <- ggplot(gfgg,aes(x = Bootstrap, y =Time, fill = subgroup)) + geom_bar(stat = "identity", position = "dodge",width=0.5)
p + labs(title="Computation time for Belgian International phone calls dataset ")


