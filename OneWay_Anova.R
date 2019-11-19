library(reshape2)
pop <- rnorm(10000,37,1)
Sample <- sample(pop, 80, replace=F)
SampleA <- Sample[1:20] - rnorm(20,0,0.5)
SampleB <- Sample[21:40] - rnorm(20,2,0.5)
SampleC <- Sample[41:60] - rnorm(20,2,0.5)
SampleD <- Sample[61:80] - rnorm(20,2,0.5)
SS <- function(x,mu){sum((x-mu)^2)}
SS_explained = SS(SampleA,mean(SampleA))+SS(SampleB,mean(SampleB))+SS(SampleC,mean(SampleC))+SS(SampleD,mean(SampleD))
SS_unexplained = SS(c(mean(SampleA),mean(SampleB),mean(SampleC),mean(SampleD)), mean(c(SampleA,SampleB,SampleC,SampleD)))*20
SS_explained+SS_unexplained
SS_tot = SS(c(SampleA,SampleB,SampleC,SampleD),mean(c(SampleA,SampleB,SampleC,SampleD)))
SS_tot
MSE = SS_unexplained/3
MSR = SS_explained/((20-1)*4)
Fs = MSE/MSR
1-pf(df1 = 3,df2 = 19*4,q = Fs)
data = melt(cbind(SampleA,SampleB,SampleC,SampleD))

aov(value~Var2,data)

