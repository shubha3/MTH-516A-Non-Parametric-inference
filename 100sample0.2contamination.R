library(devtools)
library(robustbase)
library(FRB)
library(MASS)
rm(list=ls())
beta=matrix(c(5,5),nrow=2)
len=c(rep(0,2))
covr=c(rep(0,2))
f1=function(n,ep,beta){
M1=matrix(rnorm(n),nrow=n)
X=cbind(rep(1,n),M1)
xbeta=X%*%beta
e=c(rep(0,n))
for (i in 1:n)
{
if(runif(1)<=1-ep)
e[i]=rnorm(1,0,1)
else{
e[i]=runif(1,20,25)
}
}
y=xbeta + e
data=cbind(X,y)
colnames(data)=c("X1","X2","Y")
return(data)
}
test=function(lconf,uconf,beta)
{
l=c()
for (i in 1:2)
{
if((lconf[i,1]<=beta[i,1])&&(beta[i,1]<=uconf[i,1]))
{
l[i]=1
}
else 
{
l[i]=0
}
}
return(l)
}
startTime2 <- Sys.time()
for (i in 1:1000)
{
data1=as.data.frame(f1(100,0.2,beta))
a <- lmrob(Y~X2,data=data1,k.max=5000)
foo <- frb(lmrob.object=a, nboot=1000,return.coef=TRUE,return.indices=TRUE)
cc=cov(foo$coef)
lconf=matrix(a$coef,nrow=2)-qnorm(.995)*matrix(sqrt(diag(cc)),nrow=2)
uconf=matrix(a$coef,nrow=2)+qnorm(.995)*matrix(sqrt(diag(cc)),nrow=2)
len=len+(uconf-lconf)
covr=covr+test(lconf,uconf,beta)
}
cbind(covr/1000,len/1000)
endTime2 <- Sys.time()
print(endTime2-startTime2)
########Classical Bootstrap############
len1=c(rep(0,2))
covr1=c(rep(0,2))
startTime3 <- Sys.time()
for (i in 1:1000)
{
data2=as.data.frame(f1(100,0.2,beta))
sample_coef_intercept <- NULL
sample_coef_x1 <- NULL
for (i in 1:1000) {
  #Creating a resampled dataset from the sample data
  sample_d = data2[sample(1:nrow(data2), nrow(data2), replace = TRUE), ]
  #Running the regression on these data
  model_bootstrap <- lm(Y ~X2, data = sample_d)
  #Saving the coefficients
  sample_coef_intercept <-c(sample_coef_intercept, model_bootstrap$coefficients[1])
  sample_coef_x1 <-c(sample_coef_x1, model_bootstrap$coefficients[2])
}
lconf1=matrix(c(rep(0,2)),nrow=2)
uconf1=matrix(c(rep(0,2)),nrow=2)
lconf1[1,1]=mean(sample_coef_intercept)-qnorm(.995)*sqrt(var(sample_coef_intercept))
lconf1[2,1]=mean(sample_coef_x1)-qnorm(.995)*sqrt(var(sample_coef_intercept))
uconf1[1,1]=mean(sample_coef_intercept)+qnorm(.995)*sqrt(var(sample_coef_intercept))
uconf1[2,1]=mean(sample_coef_x1)+qnorm(.995)*sqrt(var(sample_coef_intercept))
len1=len1+(uconf1-lconf1)
covr1=covr1+test(lconf1,uconf1,beta)
}
cbind(covr1/1000,len1/1000)
endTime3 <- Sys.time()
print(endTime3-startTime3)
plot(data1$X2,data1$Y,main="simulated analysis with n=100 and 20% contamination")
abline(a$coef[1],a$coef[2])
abline(lm(Y~X2,data=data1),col="red")
abline(5,5,col="blue")
legend(-2, 35, legend=c("OLS","MM","TRUE model"), fill = c("red","black","blue"))

