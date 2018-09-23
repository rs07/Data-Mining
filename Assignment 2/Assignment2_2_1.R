#Choosing the dataset to train the Linear Model
house_price= read.csv(file.choose(),header=TRUE)
x<- house_price$Size
y<- house_price$Price


plot(x,y,xlab="Size",ylab="Price",col="violet")


abline(model<- lm(y~x),col="red",lwd=3)

Slope<- coef(model)[2]
Intercept<- coef(model)[1]

predict_val<-function(x){
	print(Slope*x+Intercept)
}