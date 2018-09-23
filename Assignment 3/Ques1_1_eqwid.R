#Dividing into K-clusters by Equal Width

#mtcars data set

#Work on this attribute -> mpg
dataset1 <- mtcars$mpg
dataset2 <- mtcars$mpg

min1= min(dataset1)
max1= max(dataset1)

#Number of Clusters= K
K= 10

diff <- (max1-min1)/K

l <- list()

for(i in 1:K)
{
	y <- c()
	for(j in 1:length(dataset1))
	{
		if(dataset1[j]>= (min1+(i-1)*diff) && (dataset1[j]<= min1+ i*diff))
		{
			y <- c(y,dataset1[j])
		}
	}
	print(y)
	l[[i]] <- y
}
print(l)

#Dividing into K-Clusters by equal Depth
c=0
len=1
list1<- list()
diff1= length(dataset1)/K
print(length(dataset1))
for(i in 1:length(dataset1))
{
	y<- c(y,dataset1[i])
	c=c+1
	if(c==as.integer(diff1))
	{
		list1[[len]]<- y
		c=0
		len= len+1
		y<-c()
	}
}
print(list1)