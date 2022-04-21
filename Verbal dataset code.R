rm(list=ls())
library(devtools)
library(robustbase)
library(FRB)
library(MASS)
library("readxl")
data1=as.data.frame(read.csv("C:/Users/LENOVO/Downloads/Coleman_Data_Set_1213_55.csv"))
a <- lmrob(Y~salaryP+fatherWc+sstatus+teacherSc+motherLev,method="MM", data=data1)
set.seed(123)
startTime1 <- Sys.time()
tmp <- frb(lmrob.object=a, nboot=5000, return.coef=TRUE,return.indices = TRUE)
endtime1 <- Sys.time()
print(endtime1-startTime1)
cc=cov(tmp$coef)
a1=rlm(Y~salaryP+fatherWc+sstatus+teacherSc+motherLev,method="MM", data=data1)
a1
lconf=matrix(c(rep(0,6)),nrow=6)
uconf=matrix(c(rep(0,6)),nrow=6)
lconf=matrix(a$coef,nrow=6)-qnorm(.975)*matrix(sqrt(diag(cc)),nrow=6)
uconf=matrix(a$coef,nrow=6)+qnorm(.975)*matrix(sqrt(diag(cc)),nrow=6)
cbind(lconf,uconf)
ll=uconf-lconf
##########Classical bootstrap########
# Containers for the coefficients
library(MASS)
d=nrow(data1)
sample_coef_intercept=c()
sample_coef_x1=c()
sample_coef_x2=c()
sample_coef_x3=c()
sample_coef_x4=c()
sample_coef_x5=c()
startTime <- Sys.time()
for (i in 1:5000) {
  #Creating a resampled dataset from the sample data
  sample_d = as.data.frame(data1[sample(1:d,d, replace = TRUE), ])
  
  #Running the regression on these data
  model_bootstrap <- lm(Y~salaryP+fatherWc+sstatus+teacherSc+motherLev, data = sample_d)
  
  #Saving the coefficients
  sample_coef_intercept[i] <-model_bootstrap$coefficients[1]
  sample_coef_x1[i] <-model_bootstrap$coefficients[2]
  sample_coef_x2[i] <-model_bootstrap$coefficients[3]
  sample_coef_x3[i] <-model_bootstrap$coefficients[4]
  sample_coef_x4[i] <-model_bootstrap$coefficients[5]
  sample_coef_x5[i] <-model_bootstrap$coefficients[6]
}
dd=cbind(sample_coef_intercept,sample_coef_x1,sample_coef_x2,sample_coef_x3,sample_coef_x4,sample_coef_x5)
endtime <- Sys.time()
print(endtime-startTime)
#########confidence#############
mean1=apply(dd,2,mean)
var1=apply(dd,2,var)
lconf1=matrix(c(rep(0,6)),nrow=6)
uconf1=matrix(c(rep(0,6)),nrow=6)
lconf1=matrix(mean1,nrow=6)-qnorm(.975)*matrix(sqrt(var1),nrow=6)
uconf1=matrix(mean1,nrow=6)+qnorm(.975)*matrix(sqrt(var1),nrow=6)
cbind(lconf1,uconf1)
lll=uconf1-lconf1
######Plotting#############
model=lm(Y~salaryP+fatherWc+sstatus+teacherSc+motherLev, data =data1)
res <- resid(model)
plot(1:20,res,ylim=c(-4,6),xlab="Index",ylab="residual",main="OLS regression")
abline(-4,0)
abline(4,0)
rob_y=30.73520476 -1.66453808*data1$salaryP +  0.08434655*data1$fatherWc + 0.66819458*data1$sstatus + 1.16461657*data1$teacherSc -4.16441751*data1$motherLev
rob_res=data1$Y-rob_y
length(rob_res)
plot(1:20,rob_res,xlab="Index",main="Fast bootstrap")
abline(-2,0)
abline(2,0)
##########QQ-Plots##########
par(mfrow=c(2,2))
qqnorm(tmp$coef[,3], frame = FALSE,main="White collar-Fast Bootstrap")
qqline(tmp$coef[,3], col = "steelblue", lwd = 2)
qqnorm(sample_coef_x2, frame = FALSE,main="White collar-Classical Bootstrap")
qqline(sample_coef_x2, col = "steelblue", lwd = 2)
qqnorm(tmp$coef[,4], frame = FALSE,main="socio economic status-Fast Bootstrap")
qqline(tmp$coef[,4], col = "steelblue", lwd = 2)
qqnorm(sample_coef_x3, frame = FALSE,main="socio economic status-Classical Bootstrap")
qqline(sample_coef_x3, col = "steelblue", lwd = 2)
ll
lll[3,1]
library(ggplot2)
library(lattice)
par(mfrow=c(1,2)) 
# Create data
gfg <- data.frame(x = c(ll[1,1],lll[1,1],ll[2,1],lll[2,1],ll[3,1],lll[3,1],ll[4,1],lll[4,1],ll[5,1],lll[5,1],ll[6,1],lll[6,1]), 
                   Bootstrap = rep(c("Intercept","X1","X2","X3","X4","X5"),each =2),
                   subgroup = c("Fast Bootstrap","Classical Bootstrap"))
 
# Create grouped barplot using ggplot2
ggplot(gfg,aes(x = Bootstrap, y =x, fill = subgroup)) +
geom_bar(stat = "identity", position = "dodge",width=0.5)+labs(title="Comparison of length of confidence interval for verbal test score data")

gfgg <- data.frame(Time = c(0.066,18.72), 
                   Bootstrap = rep(c("Computation Time"),each =2),
                   subgroup = c("Fast Bootstrap","Classical Bootstrap"))
 
# Create grouped barplot using ggplot2
p <- ggplot(gfgg,aes(x = Bootstrap, y =Time, fill = subgroup)) + geom_bar(stat = "identity", position = "dodge",width=0.5)
p + labs(title="Computation time(in seconds)")
###########