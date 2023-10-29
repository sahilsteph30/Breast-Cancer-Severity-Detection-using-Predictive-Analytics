install.packages("readxl")
install.packages("tree")
library(dplyr)
library(readxl)
library(tree)
library(ISLR)
data <- cancer_dataset
data <- data %>% select(-id)
attach(data)
set.seed(123)
data$diagnosis = as.factor(data$diagnosis)
inTrain <- sample(nrow(data), 0.5*nrow(data))

#
train <- data.frame(data[inTrain,])
temp <- data.frame(data[-inTrain,])
inTrain2 <- sample(nrow(temp), 0.5*nrow(temp))

validation <- data.frame(temp[inTrain2,])
test <- data.frame(temp[-inTrain2,])
rm(temp)

#
# Computing the error rate on validation data (full tree)
tree.Data=tree(diagnosis~.,data=train)
data <- na.omit(data)
tree.pred=predict(tree.Data,validation,type="class")
confusion = table(tree.pred,validation$diagnosis)
confusion
# Calculate accuracy rate from confusion matrix
accuracy <- sum(diag(confusion)) / sum(confusion)

# Print accuracy rate
accuracy

# printing the tree 

plot(tree.Data)
text(tree.Data,pretty=0)
tree.Data


