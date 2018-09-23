library(sn)

#To calculate Mode

#Calculating Mode based on density as all data are different so we have to calculate density
estimate_mode <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]
}

#Normal Distribution
X0<- rsn(n=100000, omega= 20, alpha= 0)
dev.new()
hist(X0, prob= TRUE, main='Normal Distribution')
lines(density(X0), col = "red", lwd = 2)
abline(v= mean(X0), col= "green", lwd = 4)
print(mean(X0))
abline(v= median(X0), col="purple", lwd = 4)
print(median(X0))
abline(v= estimate_mode(X0), col="yellow", lwd = 4)


#Positive Skew Normal Distribution
X1<- rsn(n=100000, omega= 20, alpha= 3.64)
dev.new()
hist(X1, prob= TRUE, main='Positive Skew Normal Distribution')
lines(density(X1), col = "red", lwd = 2)
abline(v= mean(X1), col= "green", lwd = 4)
print(mean(X1))
abline(v= median(X1), col="purple", lwd = 4)
print(median(X1))
abline(v= estimate_mode(X1), col="yellow", lwd = 4)

#Negative Skew Normal Distribution
X2<- rsn(n=100000, omega= 20, alpha= -3.64)
dev.new()
hist(X2, prob= TRUE, main='Negative Skew Normal Distribution')
lines(density(X2), col = "red", lwd = 2)
abline(v= mean(X2), col= "green", lwd = 4)
print(mean(X2))
abline(v= median(X2), col="purple", lwd = 4)
print(median(X2))
abline(v= estimate_mode(X2), col="yellow", lwd = 4)
