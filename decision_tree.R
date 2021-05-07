library(tidyverse)
library(dplyr)
library(ggplot2)
library(car)
library(corrplot)
library(pROC)
library(MLmetrics)
library(rpart)
library(rpart.plot) 
library(varImp)
library(gbm)
library(caret)

data <- read.table(file = "/home/mounika/Documents/IUPUI/Courses/Spring_2021/Data_Analytics/Project/Breast-Cancer-Prediction/breast-cancer-wisconsin.data", 
                   header = FALSE, sep=",", 
                   col.names = c("ID","clump_thickness", "uniformity_size", "uniformity_shape", 
                                 "marginal_adhesion", "single_epithelial_cell_size", "bare_nuclei",
                                 "bland_chromatin", "normal_nucleoli", "mitoses", "diagnosis"))



View(data)
head(data)

str(data)

# checking the number of missing values in the data
sum(data$bare_nuclei == "?")

# removing the first column which is the unique identifier - as we don't need it for our model
data <- select(data, -1)

head(data)

# excluding all the 16 missing values in the data
data <- data[data$bare_nuclei != "?",] %>% mutate(bare_nuclei = as.integer(as.character((bare_nuclei))))


# converting the values of 2 and 4 into binary data => 0 and 1
data <- data %>% mutate(diagnosis = ifelse(diagnosis == 2, 'B', 'M'),
                        diagnosis = as.factor(diagnosis))
summary(data)


# plotting the graph to check how many cases are benign and malignant
ggplot(data, aes(x = diagnosis)) +
  geom_bar(fill = "skyblue") +
  ggtitle("Distribution of Diagnosis in the Entire Dataset") +
  theme_minimal() +
  theme(legend.position = "none")

# plotting the correlation between the features
correlation <- cor(data[,-10])
corrplot(correlation, type = "lower", col = c("#fcbba1", "#b2d2e8"), addCoef.col = "black", tl.col = "black")

# splitting the data into training set and testing set
set.seed(3011) 
train_index <- sample(nrow(data), size = round(0.80 * nrow(data)), replace = FALSE)
train <- data[train_index,]
test <- data[-train_index,]

#View(train)
#View(test)

#Decision Tree Algorithm
AUC_train_besttree <- 0
AUC_test_besttree <- 0
AUC_tree <- data.frame(AUC_train_tree = numeric(), AUC_test_tree = numeric())

set.seed(3011)
tree_parameters <- data.frame(minsplit_para = floor(runif(8, 10, 60)), 
                              maxdepth_para = floor(runif(8, 10, 30)))

for(para_comb in 1:nrow(tree_parameters)){
  decision_tree <- rpart(diagnosis ~ .,  data = train,
                         control = rpart.control(minsplit = tree_parameters[para_comb, "minsplit_para"], 
                                                 maxdepth = tree_parameters[para_comb, "maxdepth_para"])) 
  
  pred_train_tree <- as.data.frame(predict(decision_tree, train, type='prob'))
  AUC_train_tree <- roc(train$diagnosis, pred_train_tree$`M`, percent = TRUE, plot = TRUE, print.auc=TRUE)
  
  pred_test_tree <- as.data.frame(predict(decision_tree, test, type='prob'))
  AUC_test_tree <- roc(test$diagnosis, pred_test_tree$`M`, percent = TRUE, plot = TRUE, print.auc=TRUE)
  
  AUC_tree[para_comb, ] <- c(round(AUC_train_tree$auc, 2), round(AUC_test_tree$auc, 2))
  AUC_train_besttree = ifelse(AUC_train_besttree > AUC_train_tree$auc, AUC_train_besttree, AUC_train_tree$auc)
  AUC_test_besttree = ifelse(AUC_test_besttree > AUC_test_tree$auc, AUC_test_besttree, AUC_test_tree$auc)
  
}


best_decision_tree <- rpart(diagnosis ~., data = train,
                            control = rpart.control(minsplit = 11,
                                                    maxdepth = 10))
rpart.plot(x = best_decision_tree, box.palette="RdBu", shadow.col="gray", nn=TRUE, yesno = 2)

best_decision_tree
