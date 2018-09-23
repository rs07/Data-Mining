#BIN BY MEANS
#mtcars data set

#Work on this attribute -> mpg
dataset <- mtcars$mpg 

#Range of the dataset 
m2 <- max(dataset)
m1 <- min(dataset)

#Consider no_of_bins is 5
no_of_bins <- 5
diff <- (m2-m1)/no_of_bins

R <- cut(dataset, no_of_bins)

bin <- matrix(,nrow=length(dataset), ncol=length(dataset))
l <- list() # for mean
l1 <- list() # for median
l2 <- list() # for boundary

# computing bins and keeping them in l
for(i in 0:4)
{	y <- c()
	for(j in 1:length(dataset))
	{
		if((dataset[j]>= (m1 + i*diff)) && (dataset[j]<= m1 + (i+1)*diff)){
			y <- c(y,dataset[j])
		}
	}
	l[[i+1]] <- y
	l1[[i+1]] <- y
	l2[[i+1]] <- y
}

# bin by means separation
for(i in 1:no_of_bins)
{
	x<-l[[i]]
	for(j in 1:length(x))
	{
		x[j] <- mean(l[[i]])
	}
	l[[i]] <- x
}

# bin by median separation
for(i in 1:no_of_bins)
{
	x<-l1[[i]]
	for(j in 1:length(x))
	{
		x[j] <- median(l1[[i]])
	}
	l1[[i]] <- x
}

#bin by boundary separation
for(i in 0:4)
{
	x<-l2[[i+1]]
	for(j in 1:length(x))
	{
		if(x[j]-(m1 + i*diff) > ((m1 + (i+1)*diff)-x[j]))
		{
			x[j] <- m1 + (i+1)*diff
		}
		else
		{
			x[j] <- m1 + i*diff
		}
	}
	l2[[i+1]] <- x
}




