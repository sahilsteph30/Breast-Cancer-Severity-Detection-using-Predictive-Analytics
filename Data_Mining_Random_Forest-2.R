library(readxl)
library(dplyr)
library(ggplot2)
library(tree)
#install.packages("partykit")
install.packages("randomForestSRC")
library(randomForestSRC)

data1 <- cancer_dataset
data1 <- data1 %>% select(-id)

# Encode diagnosis column as factor variable
data1$diagnosis <- as.factor(data1$diagnosis)

data1 <- data1 %>% rename(concave_points_mean = `concave points_mean`, 
                          concave_points_se = `concave points_se`, 
                          concave_points_worst = `concave points_worst`)

# Split data into training and test sets
data1 <- na.omit(data1)
library(caret)
set.seed(123) # Set random seed for reproducibility
train_index <- sample(1:nrow(data1), size = round(0.7 * nrow(data1)), replace = FALSE)
train_data1 <- data1[train_index, ]
test_data1 <- data1[-train_index, ]
df_control <- trainControl(method="cv",
                           number = 15,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
library(randomForest)
# Build random forest model
rf_model <- randomForest(diagnosis ~ ., data = train_data1, ntree = 500, mtry = sqrt(ncol(train_data1)-1))

# Predict on test set
rf_pred <- predict(rf_model, test_data1)

confusion_matrix <- confusionMatrix(rf_pred, test_data1$diagnosis)
print(confusion_matrix)


# Calculate accuracy of predictions
accuracy <- sum(rf_pred == test_data1$diagnosis) / nrow(test_data1)
print(paste("Random forest accuracy:", round(accuracy, 3)))



importance_df <- data.frame(Feature = row.names(rf_model$importance),
                            Importance = rf_model$importance[, "MeanDecreaseGini"])
importance_df <- importance_df %>% arrange(desc(Importance))

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Feature Importance", x = "Feature", y = "Importance") +
  theme_minimal()

plot(rf_pred)
text(rf_model,pretty=0)
rf_model


rf_model.plot(tree, type = 2, extra = 1, under = TRUE, faclen = 0)
