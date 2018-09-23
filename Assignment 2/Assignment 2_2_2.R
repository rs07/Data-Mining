#Choosing the dataset to train the Multiple Model
house_price= read.csv(file.choose(),header=TRUE)
x1<- house_price$Size
y<- house_price$Price
x2<- house_price$Bedrooms
x3<- house_price$Bathrooms


#plot(x,y,xlab="Size",ylab="Price",col="violet")

model<- lm(y~x1+x2+x3)

Slope1<- coef(model)[2]
Intercept<- coef(model)[1]
Slope2<- coef(model)[3]
Slope3<- coef(model)[4]

predict_val<-function(xy1,xy2,xy3){
	print(Slope1*xy1+Slope2*xy2+Slope3*xy3+Intercept)
}