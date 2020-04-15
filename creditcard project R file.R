#installiotn 
install.packages("glmnet")
install.packages("Rtsne")
install.packages("xgboost")

# Load libraries
library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(pROC)
library(glmnet)
library(caret)
library(Rtsne)
library(xgboost)

# Load data
setwd("C:/Users/pc/Desktop/creditcared")
data <- read_csv("creditcard.csv")

#____________________________________________________________________________________

# Data Exploration and Data Manipulation

head(data)
apply(data, 2, function(x) sum(is.na(x)))
summary(data$Amount)
common_theme <- theme(plot.title = element_text(hjust = 0.5, face = "bold"))
p <- ggplot(data, aes(x = Class)) + geom_bar() + ggtitle("Number of class labels") + common_theme
print(p)
summary(data)
p <- ggplot(data, aes(x = Class, y = Amount)) + geom_boxplot() + ggtitle("Distribution of transaction amount by class") + common_theme
print(p)
data %>% group_by(Class) %>% summarise(mean(Amount), median(Amount))

#_____________________________________________________________________________________

#normalization 
normalize <- function(x){
  return((x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE))
}
data$Amount <- normalize(data$Amount)

#___________________________________________________________________________________________


#model bulding  
# Set random seed for reproducibility
set.seed(42)
data$Class <- as.numeric(data$Class)

# Create training and testing set with stratification ( preserving the proportions of false/true values from the "Class" column)

train_index <- createDataPartition(data$Class, times = 1, p = 0.8, list = F)
X_train <- data[train_index]
X_test <- data[!train_index]
y_train <- data$Class[train_index]
y_test <- data$Class[-train_index]

#Logistic regression
log_mod <- glm(Class ~ ., family = "binomial", data = X_train)
summary(log_mod)

#evaluation 
# Use a threshold of 0.5 to transform predictions to binary
conf_mat <- confusionMatrix(y_test, as.numeric(predict(log_mod, X_test, type = "response") > 0.5))
print(conf_mat)
fourfoldplot(conf_mat$table)

#ROC
roc_data <- roc(y_test, predict(model_rf_smote, X_test, type = "prob")$X1)
plot(roc_data, main = paste0("AUC: ", round(pROC::auc(roc_data), 3)))
#________________________________________________________________________________________________



