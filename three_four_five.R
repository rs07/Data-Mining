y <- rnorm(1000,mean=300,sd=40)
y =sort(y,decreasing= FALSE)
interval=0

order=ceiling(log10(max(y)))
msb=array(-1,10)
count=0
for(i in 1:1000)
{
  msdig=y[i]/(10^(order-1))
  if(msb[msdig+1]!=msdig)
  {
    msb[msdig+1]=msdig;
    count=count+1
    
  }
}
if ((count==2) || (count==4) || (count==8))
{
  interval=4
}
if ((count==1) || (count==5) || (count==10))
{
  interval=5
}
else
{
  interval=3
}



#Equal Width

interval_gap=(max(y)-min(y))/interval
binned_data=array(1000)
multiplier=1
k=1
m=1
temp=array(1000)
for(i in 1:1000)
{
  
  #cat(y[i], min(y)+ (multiplier-1) * interval_gap,min(y) + multiplier * interval_gap," ")
  if((y[i]<=min(y) + multiplier * interval_gap) && (y[i] >= min(y)+ (multiplier-1) * interval_gap))
  {
    temp[k]=y[i]
    #print(temp[k])
    k=k+1
  }
  else
  {
    #print("hey")
    for(x in 1:k-1)
    {
      binned_data[m]=mean(temp[1:k-1],na.rm=TRUE)
      #print(binned_data1[m])
      m=m+1
    }
    
    multiplier=multiplier+1
    temp=array(1000)
    k=1
    temp[k]=y[i]
  }
  
}
for(x in m:1000)
{
  binned_data[x]=mean(temp[1:k-1],na.rm=TRUE)
  #print(binned_data1[m])
}

x = array(1000)
for (i in 1:1000)
{
  x[i] = i
}
plot(x, y, col = "blue",
     cex = 0.3,pch = 100)
lines(x, binned_data, col = "red",
      cex = 0.5,pch = 100)

