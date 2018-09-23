MyData <- read.csv("C:\\Users\\Rishabh_7\\Desktop\\Data Mining\\Assignment 1\\Iris.csv",head=FALSE)
MyData= MyData[,c(1,2,3,4)]

min1= min(MyData[,1])
min2= min(MyData[,2])
min3= min(MyData[,3])
min4= min(MyData[,4])

max1= max(MyData[,1])
max2= max(MyData[,2])
max3= max(MyData[,3])
max4= max(MyData[,4])
print(MyData)
paste0("This is Max of 1-> " , max1)
paste0("This is Max of 2-> " , max2)
paste0("This is Max of 3-> " , max3)
paste0("This is Max of 4-> " , max4)

for( i in c(1,2,3,4)){
	MyData[,i]= (MyData[,i]-min(MyData[,i]))/(max(MyData[,i])-min(MyData[,i]))
}

#print(MyData)
#write.table(MyData,"C:\\Users\\Rishabh_7\\Desktop\\Norm_Iris.csv",row.names=FALSE,col.names=FALSE,append=FALSE,sep=",")

Dice<- matrix(, nrow=150,ncol=150)
Jaccard<- matrix(, nrow=150,ncol=150)
Cosine<- matrix(, nrow=150,ncol=150)
Overlap<- matrix(, nrow=150,ncol=150)
for(i in 1:150)
{
	for(j in 1:150)
	{
		sum1= MyData[i,1]*MyData[j,1]+MyData[i,2]*MyData[j,2]+MyData[i,3]*MyData[j,3]+MyData[i,4]*MyData[j,4]
		sum2= MyData[i,1]*MyData[i,1]+MyData[i,2]*MyData[i,2]+MyData[i,3]*MyData[i,3]+MyData[i,4]*MyData[i,4]
		sum3= MyData[j,1]*MyData[j,1]+MyData[j,2]*MyData[j,2]+MyData[j,3]*MyData[j,3]+MyData[j,4]*MyData[j,4]
		Dice[i,j]= 2*sum1 /( sum2 + sum3)
		Jaccard[i,j]= sum1 / (sum2 + sum3 - sum1)
		Cosine[i,j]= sum1/ (sqrt(sum2*sum3))
		Overlap[i,j]= sum1/ min(sum2,sum3)
	}
}

for(i in 1:150)
{
	Dice[i,i]=0
	Jaccard[i,i]=0
	Cosine[i,i]=0
	Overlap[i,i]=0
}
Dice_Thresold<- matrix(,nrow=150,ncol=1)
Jaccard_Thresold<- matrix(,nrow=150,ncol=1)
Cosine_Thresold<- matrix(,nrow=150,ncol=1)
Overlap_Thresold<- matrix(,nrow=150,ncol=1)

for(i in 1:150)
{
	Dice_Thresold[i,1]=mean(Dice[i,])
	Jaccard_Thresold[i,1]=mean(Jaccard[i,])
	Cosine_Thresold[i,1]=mean(Cosine[i,])
	Overlap_Thresold[i,1]=mean(Overlap[i,])
}

new_Dice<- matrix(, nrow=150,ncol=150)
new_Jaccard<- matrix(, nrow=150,ncol=150)
new_Cosine<- matrix(, nrow=150,ncol=150)
new_Overlap<- matrix(, nrow=150,ncol=150)

list_Dice<-list()
list_Jaccard<-list()
list_Cosine<-list()
list_Overlap<-list()

for(i in 1:150)
{
	a<-c(i)
	b<-c(i)
	x<-c(i)
	y<-c(i)
	for(j in 1:150)
	{
		if(Dice[i,j] > Dice_Thresold[i,1])
		{
			new_Dice[i,j]= 1
			a<-c(a,j)
		}
		else
			new_Dice[i,j]= 0
		if(Jaccard[i,j] > Jaccard_Thresold[i,1])
		{
			new_Jaccard[i,j]=1
			b<-c(b,j)
		}
		else
			new_Jaccard[i,j]=0
		if(Cosine[i,j] > Cosine_Thresold[i,1])
		{
			new_Cosine[i,j]=1
			x<-c(x,j)
		}
		else
			new_Cosine[i,j]=0
		if(Overlap[i,j] > Overlap_Thresold[i,1])
		{
			new_Overlap[i,j]=1
			y<-c(y,j)
		}
		else
			new_Overlap[i,j]=0
		list_Dice[[i]]=a
		list_Jaccard[[i]]=b
		list_Cosine[[i]]=x
		list_Overlap[[i]]=y
	}
}


#---------------------------------------------------------------Dice Similarity Clustering--------------------------------------------------------------------#

N<-3
size= length(list_Dice)
print(size)

r=0
while(1)
{
	
	if(N==size)
		break
	r=r+1
	#---------------------------------------------------------Creating of Similarity Matrix(Using union & Intersection)----------------------------------------#
	chk<- matrix(,nrow=size,ncol=size)
	for(i in 1:size)
	{
		for(j in 1:size)
		{
			if(i!=j)
				chk[i,j]= length(intersect(list_Dice[[i]],list_Dice[[j]]))/length(union(list_Dice[[i]],list_Dice[[j]]))
			else
				chk[i,j]= 0
		}
	}
	maxi=max(chk[,])
	#print(chk)
	#print(maxi)
	#----------------------------------------------------------------Joining and doing Union------------------------------------------------------------------#
	for(i in 1:size)
	{
		if(list_Dice[[i]]!=-1)
		if(max(chk[i,])==maxi)
		{
			for(j in 1:size)
			{
				if(list_Dice[[j]]!=-1)
				if(chk[i,j]==maxi)
				{
					list_Dice[[i]]=union(list_Dice[[i]],list_Dice[[j]])
					list_Dice[[j]]<-c(-1)
					#print(j)
				}
			}
		}
	}
	#print(list_Dice)
	d=1
	for(i in 1:size)
	{
		if(list_Dice[[i]]!=-1)
		{
			list_Dice[[d]]=list_Dice[[i]]
			d=d+1
		}
	}
	d=d-1
	size=d
}
#--------------------------------------------------------------------Printing Dice Cluster-----------------------------------------------------------------------#
for(i in 1:size)
{
	print(list_Dice[[i]])
}
print(r)


#---------------------------------------------------------------Jaccard Similarity Clustering--------------------------------------------------------------------#
N<-3
size= length(list_Jaccard)
print(size)

r=0
while(1)
{
	
	if(N==size)
		break
	r=r+1
	#---------------------------------------------------------Creating of Similarity Matrix(Using union & Intersection)----------------------------------------#
	chk<- matrix(,nrow=size,ncol=size)
	for(i in 1:size)
	{
		for(j in 1:size)
		{
			if(i!=j)
				chk[i,j]= length(intersect(list_Jaccard[[i]],list_Jaccard[[j]]))/length(union(list_Jaccard[[i]],list_Jaccard[[j]]))
			else
				chk[i,j]= 0
		}
	}
	maxi=max(chk[,])
	#print(chk)
	#print(maxi)
	#----------------------------------------------------------------Joining and doing Union------------------------------------------------------------------#
	for(i in 1:size)
	{
		if(list_Jaccard[[i]]!=-1)
		if(max(chk[i,])==maxi)
		{
			for(j in 1:size)
			{
				if(list_Jaccard[[j]]!=-1)
				if(chk[i,j]==maxi)
				{
					list_Jaccard[[i]]=union(list_Jaccard[[i]],list_Jaccard[[j]])
					list_Jaccard[[j]]<-c(-1)
					#print(j)
				}
			}
		}
	}
	#print(list_Jaccard)
	d=1
	for(i in 1:size)
	{
		if(list_Jaccard[[i]]!=-1)
		{
			list_Jaccard[[d]]=list_Jaccard[[i]]
			d=d+1
		}
	}
	d=d-1
	size=d
}
#-------------------------------------------------------------------Printing Jaccard List----------------------------------------------------------------------#
for(i in 1:size)
{
	print(list_Jaccard[[i]])
}
print(r)



#---------------------------------------------------------------Cosine Similarity Clustering--------------------------------------------------------------------#
N<-3
size= length(list_Cosine)
print(size)

r=0
while(1)
{
	
	if(N==size)
		break
	r=r+1
	#---------------------------------------------------------Creating of Similarity Matrix(Using union & Intersection)----------------------------------------#
	chk<- matrix(,nrow=size,ncol=size)
	for(i in 1:size)
	{
		for(j in 1:size)
		{
			if(i!=j)
				chk[i,j]= length(intersect(list_Cosine[[i]],list_Cosine[[j]]))/length(union(list_Cosine[[i]],list_Cosine[[j]]))
			else
				chk[i,j]= 0
		}
	}
	maxi=max(chk[,])
	#print(chk)
	#print(maxi)
	#----------------------------------------------------------------Joining and doing Union------------------------------------------------------------------#
	for(i in 1:size)
	{
		if(list_Cosine[[i]]!=-1)
		if(max(chk[i,])==maxi)
		{
			for(j in 1:size)
			{
				if(list_Cosine[[j]]!=-1)
				if(chk[i,j]==maxi)
				{
					list_Cosine[[i]]=union(list_Cosine[[i]],list_Cosine[[j]])
					list_Cosine[[j]]<-c(-1)
					#print(j)
				}
			}
		} 
	}
	#print(list_Cosine)
	d=1
	for(i in 1:size)
	{
		if(list_Cosine[[i]]!=-1)
		{
			list_Cosine[[d]]=list_Cosine[[i]]
			d=d+1
		}
	}
	d=d-1
	size=d
}
#-------------------------------------------------------------------Printing Cosine List----------------------------------------------------------------------#
for(i in 1:size)
{
	print(list_Cosine[[i]])
}
print(r)

