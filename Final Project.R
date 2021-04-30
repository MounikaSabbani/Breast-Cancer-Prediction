library(randomForest)
library(caTools)
library(tree)
library(dplyr)

breast.cancer.wisconsin <- read.csv("C:/Users/colin/Desktop/Work/Masters Program/INFO-H 515/Final Project/breast-cancer-wisconsin.data", header=FALSE)

Z<- breast.cancer.wisconsin

head(Z)

Z<-rename(Z, Thickness = V2, Unisize = V3, Unishape = V4, Adhesion = V5,
       Episize = V6, Bare = V7, Chromatin = V8, Nucleoli = V9, 
       Mitoses = V10, Diagnosis = V11)

Z$Diagnosis<-ifelse(Z$Diagnosis==2,"B","M")

Z$Diagnosis<-as.factor(Z$Diagnosis)

set.seed(222)
sample <- sample.split(Z$V1, SplitRatio = .75)
train <- subset(Z, sample == TRUE)
test <- subset(Z, sample == FALSE)

dim(train)
dim(test)

rf <- randomForest(Diagnosis~., data=train[,2:11], ntree=500, mtry=3, importance=TRUE)

pred<- predict(rf, newdata=test[,2:11])

cm<- table(test$Diagnosis,pred)
cm

plot(rf)
varImpPlot(rf)

Random_Forest<- rf

plot(Random_Forest)
varImpPlot(Random_Forest)

#Tree model
treemod<- tree(Diagnosis~ ., data=train[,2:11])
summary(treemod)
plot(treemod)
text(treemod,pretty=0,cex=0.75)

cv.treeMod <- cv.tree(treemod, FUN=prune.tree, K = 10)
cv.treeMod

pruneTree <- prune.tree(treemod, best = 5)
plot(pruneTree)
text(pruneTree)

TMpred<- predict(treemod, newdata = test)

TMpred2<- predict(pruneTree, newdata = test)

pred<- ifelse(TMpred[,1]>0.5,"B","M")
pred2<- ifelse(TMpred2[,1]>0.5,"B","M")

table(pred,test$Diagnosis)
table(pred2,test$Diagnosis)
