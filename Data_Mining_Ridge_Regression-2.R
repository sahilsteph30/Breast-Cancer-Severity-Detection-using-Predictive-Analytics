library(ISLR)
#
df <- read.csv("D:/BUDT 758T/Group Project/Breast-Cancer-Detection-using-Machine-Learning-master/cancer dataset.csv")
df$id <-NULL
df$diagnosis = as.factor(df$diagnosis)
df$diagnosis <- ifelse(df$diagnosis=="M",1,0)
x=model.matrix(diagnosis~.,df)[,-1]
y=df$diagnosis
library(glmnet)
# Create a grid of 100 lambda values from 10^-2 to 10^10
grid=10^seq(10,-2,length=100)
# Now create a matrix to hold coefficient values for the different lambdas
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
# The dimension of the coef matrix 
dim(coef(ridge.mod))
## The dimensions are [1]  20 100
# The value of lambda at any position in the grid
ridge.mod$lambda[50]
## In this instance, at 50, [1] 11497.57
# The corresponding coefficients are 
coef(ridge.mod)[,50]
## Predict coefficients at lambda = 50, i.e. a new value not in grid
predict(ridge.mod,s=50,type="coefficients")[1:20,]
##
##+-------------------------------------------------+
#     Validation set approach to selecting lambda
##+-------------------------------------------------+
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
#
# Now generate predictions for the test data using a lambda value of 4
M = rep(0,100)
for (i in 1:100)
{ 
  w = grid[i]
  ridge.pred=predict(ridge.mod,s=w,newx=x[test,])
  M[i] = mean((ridge.pred-y.test)^2)
  }
# plot(c(1,100),c(80000,200000),type="n", xlab="p",ylab="RMSE")
plot(log(grid),M,type='l',col="blue",xlab="Log(lambda)",ylab="MSE")
bestlam = grid[which.min(M)]
min(M)
bestlam
predict(ridge.mod,type="coefficients",s=bestlam)[1:20,]

out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

#
# Let us compare this with linear regression
dftrain <- data.frame(df[train,])
dftest <- data.frame(df[-train,])
fit <- lm(diagnosis ~ ., data=dftrain)
summary(fit)
Predicted <- predict(fit)
mse <- function(xA,xP){(mean((xA-xP)^2))}
mse(dftrain$diagnosis,Predicted)
#
PredictedT <- predict(fit,newdata=dftest)
mse(dftest$diagnosis,PredictedT)
#
# So clearly, we have better performance with shrinkage model


#+--------------------------------------------------------------------+
#+      Using K-fold cross-validation                                 +
#+--------------------------------------------------------------------+
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
##
bestlam=cv.out$lambda.min
bestlam
## [1] 211.7416
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
## [1] 96015.51
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]
