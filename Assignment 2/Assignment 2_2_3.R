#mtcars is a inbuilt data set in R which can be used for logistic regression
#mtcars has 3 fields: vs(0 or 1), wt and disp
#wt and disp are independent variables
#We will determine whether a given engine belongs(1) to vs or not(0)

data(mtcars)
model <- glm(formula = vs~wt+disp, data=mtcars, family="binomial")

#summary of the Generalized Regression Model(glm)
summary(model)



# x is weight and y is displacement
prob_val <- function(x,y){
	newdata <- data.frame(wt=x, disp=y)
	predict(model,newdata,type="response")
}
