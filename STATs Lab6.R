n<-10
N<-10000
mu<-40
sigma<-3

##First Estimator

samples<-matrix(NA, nrow=N, ncol=n)
sample.variances<-rep(NA, N)
for (i in 1:N) {  
  samples[i,] <- rnorm(n, mean=mu, sd=sigma) 
  deviation <- samples[i,] - mean(samples[i,]) 
  sample.variances[i] <- (1/(n)) * sum((deviation)^2) 
} 

mean(sample.variances)
median(sample.variances)
hist(sample.variances)

##Second Estimator
samples1<-matrix(NA, nrow=N, ncol=n)
sample.variances1<-rep(NA, N)
for (i in 1:N) {  
  samples1[i,] <- rnorm(n, mean=mu, sd=sigma) 
  deviation <- samples[i,] - mean(samples[i,]) 
  sample.variances1[i] <- (1/(n-1)) * sum((deviation)^2) 
} 

IQR(sample.variances1)
mean(sample.variances1)
median(sample.variances1)
hist(sample.variances1)

##Third Estimator
samples2<-matrix(NA, nrow=N, ncol=n)
sample.variances2<-rep(NA, N)
for (i in 1:N) {  
  samples2[i,] <- rnorm(n, mean=mu, sd=sigma) 
  deviation <- samples[i,] - mean(samples[i,]) 
  sample.variances2[i] <- (1/(n-1)) * sum((deviation)^2) 
} 

mean(sample.variances2)
median(sample.variances2)
hist(sample.variances2)




data<-data.frame(A=sample.variances, B=sample.variances1, C=sample.variances2)
boxplot(data, xlab="Estimator", ylab="Sample Varainces")
