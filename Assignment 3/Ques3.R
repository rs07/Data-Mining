# load entropy library 
library("entropy")
library(sn)


#partition<- function(x1, mid)
#{
	
#}

### 1D example ####

# sample from continuous uniform distribution
x1 = runif(10000)
#plot(x1)
print(x1)
hist(x1, xlim=c(0,1), freq=TRUE, main='Distribution')

##y1 = discretize(x1, numBins=10, r=c(0,1))
##y1

# compute entropy from counts
entropy(x1) # empirical estimate near theoretical maximum
#log(10) # theoretical value for discrete uniform distribution with 10 bins 

size= 10000

for(i in (1:10000)
{
	 