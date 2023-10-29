df <- read.csv("cancer dataset.csv")
df[,1] <- NULL
#Suppose we want M to be the success class
df$diagnosis <- factor(df$diagnosis,levels=c("B","M"))


# Normalize each variable
fun <- function(x){ 
  a <- mean(x) 
  b <- sd(x) 
  (x - a)/(b) 
}

df[,2:31] <- apply(df[,2:31], 2, fun)

set.seed(12345)
inTrain <- sample(nrow(df), 0.6*nrow(df))
#
dftrain <- data.frame(df[inTrain,])
dftemp <- data.frame(df[-inTrain,])
inVal <- sample(nrow(dftemp),0.5*nrow(dftemp))
dfvalidation <- data.frame(dftemp[inVal,])
dftest <- data.frame(dftemp[-inVal,])
dftemp <- NULL
#
# Knn Model
library(class)
# 
train_input <- as.matrix(dftrain[,-1])
train_output <- as.vector(dftrain[,1])
validate_input <- as.matrix(dfvalidation[,-1])
test_input <- as.matrix(dftest[,-1])
#
# We look for the value of K which minimizes validation error rate
kmax <- 15
ER1 <- rep(0,kmax)
ER2 <- rep(0,kmax)
#
# We fit a model for each value of K in the range 1:15
set.seed(457)
for (i in 1:kmax){
  prediction <- knn(train_input, train_input,train_output, k=i)
  prediction2 <- knn(train_input, validate_input,train_output, k=i)
  prediction3 <- knn(train_input, test_input,train_output, k=i)
  #
  # The confusion matrix for training data is:
  CM1 <- table(prediction, dftrain$diagnosis)
  CM1
  # The training error rate is:
  ER1[i] <- (CM1[1,2]+CM1[2,1])/sum(CM1)
  # The confusion matrix for validation data is: 
  CM2 <- table(prediction2, dfvalidation$diagnosis)
  CM2
  # The validation error rate is:
  ER2[i] <- (CM2[1,2]+CM2[2,1])/sum(CM2)
}

#Now plotting this;
plot(c(1,kmax),c(0,0.1),type="n", xlab="k",ylab="Error Rate")
lines(ER1,col="red")
lines(ER2,col="blue")
legend(7, 0.1, c("Training","Validation"),lty=c(1,1), col=c("red","blue"))
z <- which.min(ER2)
cat("Minimum Validation Error k:", z)
points(z,ER2[z],col="red",cex=2,pch=20)

#
# Scoring at optimal k
prediction <- knn(train_input, train_input,train_output, k=z)
prediction2 <- knn(train_input, validate_input,train_output, k=z)
prediction3 <- knn(train_input, test_input,train_output, k=z)
#
CM1 <- table(prediction, dftrain$diagnosis)
CM2 <- table(prediction2, dfvalidation$diagnosis)
CM3 <- table(prediction3, dftest$diagnosis)
CM1
CM2
CM3
(ER1 <- (CM1[1,2]+CM1[2,1])/sum(CM1))
(ER2 <- (CM2[1,2]+CM2[2,1])/sum(CM2))
(ER3 <- (CM3[1,2]+CM3[2,1])/sum(CM3))
#

# Now we compute the lift curve for k=z. 
prediction3 <- knn(train_input, test_input, train_output, k=z, prob=T)
#
predicted.probability <- attr(prediction3, "prob")

#Ifelse is used to avoid only getting probabilities of the winning class
predicted.probability <- ifelse(prediction3 ==1, predicted.probability, 1-predicted.probability)
#
df1 <- data.frame(prediction3, predicted.probability,dftest$diagnosis)

# When prediction is 1, we will use predicted.probability; else use 1-predicted.probability
df1S <- df1[order(-predicted.probability),]
df1S$Gains <- cumsum(as.numeric(df1S$dftest.diagnosis)-1)
plot(df1S$Gains,type="n",main="Lift Chart",xlab="Number of Cases",ylab="Cumulative Success")
lines(df1S$Gains,col="blue")
abline(0,sum(as.numeric(df1S$dftest.diagnosis)-1)/nrow(df1S),lty = 2, col="red")



#Part II (Naive - Baiyes)
df <- read.csv("cancer dataset.csv")
df[,1] <- NULL
#Suppose we want M to be the success class
df$diagnosis <- factor(df$diagnosis,levels=c("B","M"))

library("caret")
set.seed(12345)
inTrain <- createDataPartition(df$diagnosis, p=0.6, list=FALSE)


dftrain <- data.frame(df[inTrain,])
dfvalidation <- data.frame(df[-inTrain,])

# We require the library e1071
library(e1071)
# Can handle both categorical and numeric input, 
# but output must be categorical
model <- naiveBayes(diagnosis~., data=dftrain)
model
prediction <- predict(model, newdata = dfvalidation[,-1])
CM4 <- table(dfvalidation$diagnosis,prediction,dnn=list('actual','predicted'))
CM4
(ER4 <- (CM4[1,2]+CM4[2,1])/sum(CM4))
(A4 <- (CM4[1,1]+CM4[2,2])/sum(CM4))
cat('The error rate is:', ER4, '\n\n')
cat('The accuracy is:', A4, '\n\n')
model$apriori
#
# For class probabilities
predicted.probability <- predict(model, newdata = dfvalidation[,-1], type="raw")
#
# The first column is class 0, the second is class 1
PL <- as.numeric(dfvalidation$diagnosis)-1
prob <- predicted.probability[,2]
df1 <- data.frame(prediction, PL, prob)
#
#
df1S <- df1[order(-prob),]
df1S$Gains <- cumsum(df1S$PL)
plot(df1S$Gains,type="n",main="Lift Chart",xlab="Number of Cases",ylab="Cumulative Success")
lines(df1S$Gains,col="blue")
abline(0,sum(df1S$PL)/nrow(df1S),lty = 2, col="red")
#
#
