#PRASHANT MURALI - ID:29625564 - FIT3152 Assignment 2

#Install and get packages
install.packages("tree")
install.packages("e1071")
install.packages(("ROCR"))
install.packages("randomForest")
install.packages("adabag")
install.packages("pastecs")
install.packages("caret")
install.packages("rpart")
install.packages("corrplot")
install.packages("car")
install.packages("neuralnet")

library(tree)
library(e1071)
library(ROCR)
library(randomForest)
library(adabag)
library(pastecs)
library(caret)
library(rpart)
library(corrplot)


#Note: Ensure R is in correct working directory before running rest of the code

#Get the data
rm(list = ls())
WAUS <- read.csv("WAUS2020.csv")
L <- as.data.frame(c(1:49))
set.seed(29625564) # Your Student ID is the random seed
L <- L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS <- WAUS[(WAUS$Location %in% L),]
WAUS <- WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample 2000 rows


#Set appropriate sig fig.
options(digits=4)



#TASK 1


#Descirbe data and variables before omitting attributes and NA values
summary(WAUS)
str(WAUS)
stat.desc(WAUS)

#Proportion of rainy days to fine days before processing data
rainy_days <- nrow(WAUS[which(WAUS$RainToday == "Yes"),])
fine_days <- nrow(WAUS[which(WAUS$RainToday == "No"),])
rainy_to_fine <- rainy_days/fine_days
cat(rainy_days,":",fine_days)
rainy_to_fine




# Create another dataset with omitted NA value rows - for testing attributes
WAUS_omitted <- na.omit(WAUS) 

# Run a quick chi-squared test on factor variables
# Find variables that are factors
factor.var <- names(which(sapply(WAUS_omitted, class) == "factor"))
factor.var <- setdiff(factor.var, "RainTomorrow")
chisq.test.res <- lapply(factor.var, function(x) { 
  chisq.test(WAUS_omitted[,x], WAUS_omitted[, "RainTomorrow"], simulate.p.value = TRUE)
})
names(chisq.test.res) <- factor.var
chisq.test.res

# Run a quick correlation test on numeric variables
factor.var <- names(which(sapply(WAUS_omitted, class) == "factor"))
numeric.var <- setdiff(colnames(WAUS_omitted), factor.var)
numeric.var <- setdiff(numeric.var, "RainTomorrow")
numeric.var
numeric.var.matrix <- as.matrix(WAUS_omitted[, numeric.var, drop=FALSE])
numeric.vars.cor <- cor(numeric.var.matrix)
corrplot(numeric.vars.cor)

# Based on findings, Day and Year can be omitted, along with rows that have
# missing values.




# TASK 2

# Now manipulate the actual data

#Remove Day and Year from the data
WAUS<-subset(WAUS, select=-c(Day, Year))
WAUS

#Find NA's
is.na(WAUS)
#Remove NA's
WAUS <- na.omit(WAUS) 

#Descirbe data and variables after omitting NA values
summary(WAUS)
str(WAUS)
stat.desc(WAUS)

#Proportion of rainy days to fine days
rainy_days <- nrow(WAUS[which(WAUS$RainToday == "Yes"),])
fine_days <- nrow(WAUS[which(WAUS$RainToday == "No"),])
rainy_to_fine <- rainy_days/fine_days
cat(rainy_days,":",fine_days)
rainy_to_fine


#TASK 3

#Split training and test data
set.seed(29625564) #Student ID as random seed
train.row = sample(1:nrow(WAUS), 0.7*nrow(WAUS))
WAUS.train = WAUS[train.row,]
WAUS.test = WAUS[-train.row,]



#TASK 4
#IMPLEMENTING CLASSIFICATION MODELS

#fit tree model on training data
WAUS.tree=tree(RainTomorrow~., data = WAUS.train)
summary(WAUS.tree)
#Plot decision tree
plot(WAUS.tree)
title(main="Decision Tree")
text(WAUS.tree, pretty = 0)

#fit naive bayes
WAUS.naivebayes=naiveBayes(RainTomorrow~., data = WAUS.train)

#fit bagging
WAUS.bag <- bagging(RainTomorrow~., data = WAUS.train, mfinal=10)

#fit boosting
WAUS.boost <- boosting(RainTomorrow~., data = WAUS.train, mfinal=10)

#fit Random Forest
WAUS.RF <- randomForest(RainTomorrow~., data = WAUS.train, na.action = na.exclude)
plot(WAUS.RF)





#TASK 5

# DECISION TREE
#Confusion matrix
WAUS.predtree = predict(WAUS.tree, WAUS.test, type = "class")
tree.confmatrix=table(Predicted_Class = WAUS.predtree, Actual_Class = WAUS.test$RainTomorrow)
cat("\n#Decision Tree Confusion\n")
print(tree.confmatrix)

#Calculate accuracy using formula manually
tree.accuracy<-(tree.confmatrix[1,1]+tree.confmatrix[2,2])/(sum(tree.confmatrix))
tree.accuracy

#Alternate method, using caret package to get confusion matrix and accuracy
cm.tree <- confusionMatrix(WAUS.predtree, WAUS.test$RainTomorrow)
tree.accuracy2<-cm.tree$overall["Accuracy"]
tree.accuracy2



# NAIVE BAYES
# Confusion matrix
WAUS.predNB = predict(WAUS.naivebayes, WAUS.test)
NB.confmatrix=table(Predicted_Class = WAUS.predNB, WAUS.test$RainTomorrow)
cat("\n#NaiveBayes Confusion\n")
print(NB.confmatrix)

#Calculate accuracy using formula manually
NB.accuracy<-(NB.confmatrix[1,1]+NB.confmatrix[2,2])/(sum(NB.confmatrix))
NB.accuracy

#Alternate method, using caret package to get confusion matrix and accuracy
cm.NB <- confusionMatrix(WAUS.predNB, WAUS.test$RainTomorrow)
NB.accuracy2<-cm.NB$overall["Accuracy"]
NB.accuracy2



# BAGGING
# Confusion matrix
WAUS.predBag <- predict.bagging(WAUS.bag, WAUS.test)
WAUS.predBag$confusion

#Calculate accuracy using formula
Bagging.accuracy<-(WAUS.predBag$confusion[1,1]+WAUS.predBag$confusion[2,2])/(sum(WAUS.predBag$confusion))
Bagging.accuracy



# BOOSTING
# Confusion matrix
WAUS.predBoost <- predict.boosting(WAUS.boost, WAUS.test)
WAUS.predBoost$confusion

#Calculate accuracy using formula
Boosting.accuracy<-(WAUS.predBoost$confusion[1,1]+WAUS.predBoost$confusion[2,2])/(sum(WAUS.predBoost$confusion))
Boosting.accuracy



# RANDOM FOREST
# Confusion matrix
WAUS.predRF <- predict(WAUS.RF, WAUS.test)
RF.confmatrix=table(Predicted_Class = WAUS.predRF, Actual_Class = WAUS.test$RainTomorrow)
cat("\n#Random Forest Confusion\n")
print(RF.confmatrix)

#Calculate accuracy using formula manually
RF.accuracy<-(RF.confmatrix[1,1]+RF.confmatrix[2,2])/(sum(RF.confmatrix))
RF.accuracy

#Alternate method, using caret package to get confusion matrix and accuracy
cm.RF <- confusionMatrix(WAUS.predRF, WAUS.test$RainTomorrow)
RF.accuracy2<-cm.RF$overall["Accuracy"]
RF.accuracy2





#TASK 6

# DECISION TREE
# CONFIDENCE, ROC & AUC for decision tree
WAUS.pred.tree = predict(WAUS.tree, WAUS.test, type = "vector")
WAUS.pred.tree
# ROC
WAUS.tree.prediction <- prediction( WAUS.pred.tree[,2], WAUS.test$RainTomorrow)
WAUS.tree.performance <- performance(WAUS.tree.prediction,"tpr","fpr")
plot(WAUS.tree.performance, main="ROC Curve")
abline(0,1)
#AUC
tree.AUC <- performance(WAUS.tree.prediction,"auc")
tree.AUC.value <- as.numeric(tree.AUC@y.values)
print(tree.AUC.value)




# NAIVE BAYES
# CONFIDENCE, ROC & AUC for Naive Bayes
WAUS.pred.NB = predict(WAUS.naivebayes, WAUS.test, type = 'raw')
WAUS.pred.NB
# ROC
WAUS.NB.prediction <- prediction( WAUS.pred.NB[,2], WAUS.test$RainTomorrow)
WAUS.NB.performance <- performance(WAUS.NB.prediction,"tpr","fpr")
plot(WAUS.NB.performance, add=TRUE, col = "purple")
#AUC
NB.AUC <- performance(WAUS.NB.prediction,"auc")
NB.AUC.value <- as.numeric(NB.AUC@y.values)
print(NB.AUC.value)



# BAGGING
# CONFIDENCE, ROC & AUC for Bagging
WAUS.predBag$prob
# ROC
WAUS.bag.prediction <- prediction( WAUS.predBag$prob[,2], WAUS.test$RainTomorrow)
WAUS.bag.performance <- performance(WAUS.bag.prediction,"tpr","fpr")
plot(WAUS.bag.performance, add=TRUE, col = "blue")
#AUC
bag.AUC <- performance(WAUS.bag.prediction,"auc")
bag.AUC.value <- as.numeric(bag.AUC@y.values)
print(bag.AUC.value)



# BOOSTING
# CONFIDENCE, ROC & AUC for Boosting
WAUS.predBoost$prob
# ROC
WAUS.boost.prediction <- prediction( WAUS.predBoost$prob[,2], WAUS.test$RainTomorrow)
WAUS.boost.performance <- performance(WAUS.boost.prediction,"tpr","fpr")
plot(WAUS.boost.performance, add=TRUE, col = "red")
#AUC
boost.AUC <- performance(WAUS.boost.prediction,"auc")
boost.AUC.value <- as.numeric(boost.AUC@y.values)
print(boost.AUC.value)



# RANDOM FOREST
#CONFIDENCE, ROC & AUC for Random Forest
WAUS.pred.RF <- predict(WAUS.RF, WAUS.test, type="prob")
WAUS.pred.RF
#ROC
WAUS.RF.prediction <- prediction( WAUS.pred.RF[,2], WAUS.test$RainTomorrow)
WAUS.RF.performance <- performance(WAUS.RF.prediction,"tpr","fpr")
plot(WAUS.RF.performance, add=TRUE, col = "green")
#AUC
RF.AUC <- performance(WAUS.RF.prediction,"auc")
RF.AUC.value <- as.numeric(RF.AUC@y.values)
print(RF.AUC.value)



# ADD LEGEND TO THE PLOT
legend("bottomright", title="Classifier", legend = c("Decision Tree","Naive Bayes",
       "Bagging", "Boosting", "Random Forest"), col=c("black","purple","blue","red","green"), lty = 1, cex = 0.8)






# TASK 7
# Create a table with confusion matrix accuracy values, and AUC values for each classifier
comparison.table <- matrix(c(tree.accuracy,NB.accuracy,Bagging.accuracy,Boosting.accuracy,RF.accuracy,tree.AUC.value,
                  NB.AUC.value,bag.AUC.value,boost.AUC.value,RF.AUC.value),ncol=2)
# Add row and col names
colnames(comparison.table) <- c("Accuracy Value","AUC Value")
rownames(comparison.table) <- c("Decision Tree","Naive Bayes","Bagging","Boosting","Random Forest")
# Covert to table
comparison.table <- as.table(comparison.table)
# Output the table
comparison.table
# Comparison explained in report





#TASK 8
#Attribute importance
cat("\n#Decision Tree Attribute Importance\n")
print(summary(WAUS.tree))
#Not needed for naive bayes
cat("\n#Baging Attribute Importance\n")
print(WAUS.bag$importance)
importanceplot(WAUS.bag)
cat("\n#Boosting Attribute Importance\n")
print(WAUS.boost$importance)
importanceplot(WAUS.boost)
cat("\n#Random Forest Attribute Importance\n")
print(WAUS.RF$importance)
varImpPlot(WAUS.RF)



#Remove un-important variables
WAUS.test <- subset(WAUS.test,select = c(Humidity3pm, 
            Sunshine, Pressure3pm, Pressure9am, WindDir3pm,
            WindDir9am, WindGustDir, WindGustSpeed, RainTomorrow))

WAUS.train<- subset(WAUS.train,select = c(Humidity3pm, 
            Sunshine, Pressure3pm, Pressure9am, WindDir3pm,
            WindDir9am, WindGustDir, WindGustSpeed, RainTomorrow))




#TASK 9

#Use updated test and training datasets

#ATTEMPT TO IMPROVE DECISION TREE

#Re-fit tree model on training data
WAUS.tree=tree(RainTomorrow~., data = WAUS.train)
summary(WAUS.tree)
#Plot  tree
plot(WAUS.tree)
text(WAUS.tree, pretty = 0)

#Run prediction and get accuracy
#Confusion matrix
WAUS.predtree = predict(WAUS.tree, WAUS.test, type = "class")
tree.confmatrix=table(Predicted_Class = WAUS.predtree, Actual_Class = WAUS.test$RainTomorrow)
cat("\n#Decision Tree Confusion\n")
print(tree.confmatrix)
#Calculate accuracy using formula manually
tree.accuracy<-(tree.confmatrix[1,1]+tree.confmatrix[2,2])/(sum(tree.confmatrix))
tree.accuracy

# Use CV and pruning on decision tree
test.fit <- cv.tree(WAUS.tree, FUN=prune.misclass)
print(test.fit)

pruned.tree <- prune.misclass(WAUS.tree, best=8)
print(summary(pruned.tree))
plot(pruned.tree)
text(pruned.tree, pretty=0)

predict.pruned.tree <- predict(pruned.tree, WAUS.test, type = "class" )
pruned.confmatrix <- table(predicted = predict.pruned.tree, actual= WAUS.test$RainTomorrow)
print(pruned.confmatrix)

#Calculate accuracy using formula manually
pruned.tree.accuracy<-(pruned.confmatrix[1,1]+pruned.confmatrix[2,2])/(sum(pruned.confmatrix))
#Accuracy was found to be higher
pruned.tree.accuracy

#Find confidence, ROC, and AUC
WAUS.pred.tree = predict(pruned.tree, WAUS.test, type = "vector")
WAUS.pred.tree
# ROC
WAUS.tree.prediction <- prediction( WAUS.pred.tree[,2], WAUS.test$RainTomorrow)
WAUS.tree.performance <- performance(WAUS.tree.prediction,"tpr","fpr")
plot(WAUS.tree.performance, main="ROC Curve")
abline(0,1)
#AUC
tree.AUC <- performance(WAUS.tree.prediction,"auc")
tree.AUC.value <- as.numeric(tree.AUC@y.values)
# AUC WAS FOUND TO BE MUCH HIGHER
print(tree.AUC.value)

pruned.tree.accuracy
tree.AUC.value



#ATTEMPT TO IMPROVE BAGGING

#Cross validation for bagging
#May take awhile to run
cv.bag<-bagging.cv(RainTomorrow~., data = WAUS.train, v=10, mfinal=10, control=rpart.control(cp=0.01))
cv.bag

#Calculate accuracy using formula manually
bag.accuracy<-(cv.bag$confusion[1,1]+cv.bag$confusion[2,2])/(sum(cv.bag$confusion))
#ACCURACY DOES NOT IMPROVE
bag.accuracy



#ATTEMPT TO IMPROVE BOOSTING

#Cross validation for boosting
#May take awhile to run
cv.boost<-boosting.cv(RainTomorrow~., data = WAUS.train, v=10, boos=TRUE, mfinal=50,	coeflearn =	"Breiman", control=rpart.control(cp=0.01))
cv.boost

#Calculate accuracy using formula manually
boost.accuracy<-(cv.boost$confusion[1,1]+cv.boost$confusion[2,2])/(sum(cv.boost$confusion))
#ACCURACY DOES NOT IMPROVE
boost.accuracy


#ATTEMPT TO IMPROVE RANDOM FOREST

data <- subset(WAUS.train, select=-c(RainTomorrow))
x<-rfcv(data, WAUS.train$RainTomorrow, cv.fold=5)
x
WAUS.RF <- randomForest(RainTomorrow~., data = WAUS.train, na.action = na.exclude, ntree=500, mtry=6)
# Confusion matrix
WAUS.predRF <- predict(WAUS.RF, WAUS.test)
RF.confmatrix=table(Predicted_Class = WAUS.predRF, Actual_Class = WAUS.test$RainTomorrow)
cat("\n#Random Forest Confusion\n")
print(RF.confmatrix)


#Calculate accuracy using formula manually
RF.accuracy<-(RF.confmatrix[1,1]+RF.confmatrix[2,2])/(sum(RF.confmatrix))
RF.accuracy






# TASK 10

#Call libraries
library(car)
library(neuralnet)

#Pre-process data

#Get data
WAUS <- read.csv("WAUS2020.csv")
L <- as.data.frame(c(1:49))
set.seed(29625564) # Your Student ID is the random seed
L <- L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS <- WAUS[(WAUS$Location %in% L),]
WAUS <- WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample 2000 rows

task10.WAUS <- subset(WAUS, select = c(Humidity3pm, Sunshine, Pressure3pm, Pressure9am, WindDir3pm,
                                       WindDir9am, WindGustDir, WindGustSpeed, RainTomorrow))

# remove NA
task10.WAUS <- task10.WAUS[complete.cases(task10.WAUS),]

str(task10.WAUS)

# Recode the output class to numeric
task10.WAUS$RainTomorrow <- recode(task10.WAUS$RainTomorrow, " 'Yes' = '1';'No'= '0' ")
task10.WAUS$RainTomorrow <- as.numeric(as.character(task10.WAUS$RainTomorrow))


# Recode the other attributes to numeric

# For WindDir3pm
task10.WAUS$WindDir3pm <- recode(task10.WAUS$WindDir3pm, " 'E' = '0'; 'ENE' = '1'; 'ESE' = '2'; 'N' = '3';
                                   'NE' = '4'; 'NNE' = '5'; 'NNW' = '6'; 'NW' = '7'; 'S' = '8'; 'SE' = '9'; 'SSE' = '10';
                                   'SSW' = '11'; 'SW' = '12'; 'W' = '13'; 'WSW' = '14'; 'WNW' = '15' ")
task10.WAUS$WindDir3pm <- as.numeric(as.character(task10.WAUS$WindDir3pm))

# For WindDir9am
task10.WAUS$WindDir9am <- recode(task10.WAUS$WindDir9am, " 'E' = '0'; 'ENE' = '1'; 'ESE' = '2'; 'N' = '3';
                                   'NE' = '4'; 'NNE' = '5'; 'NNW' = '6'; 'NW' = '7'; 'S' = '8'; 'SE' = '9'; 'SSE' = '10';
                                   'SSW' = '11'; 'SW' = '12'; 'W' = '13'; 'WSW' = '14'; 'WNW' = '15' ")
task10.WAUS$WindDir9am <- as.numeric(as.character(task10.WAUS$WindDir9am))


# For WindGustDir
task10.WAUS$WindGustDir <- recode(task10.WAUS$WindGustDir, " 'E' = '0'; 'ENE' = '1'; 'ESE' = '2'; 'N' = '3';
                                   'NE' = '4'; 'NNE' = '5'; 'NNW' = '6'; 'NW' = '7'; 'S' = '8'; 'SE' = '9'; 'SSE' = '10';
                                   'SSW' = '11'; 'SW' = '12'; 'W' = '13'; 'WSW' = '14'; 'WNW' = '15' ")
task10.WAUS$WindGustDir <- as.numeric(as.character(task10.WAUS$WindGustDir))

# Double check the data structure
str(task10.WAUS)

#Normalize the data by scaling
#Scale all including RainTomorrow
task10.WAUS <- as.data.frame(scale(task10.WAUS[c(1,2,3,4,5,6,7,8,9)]))

str(task10.WAUS)

# Create training and test sets
train.row = sample(1:nrow(task10.WAUS), 0.7*nrow(task10.WAUS))
task10.WAUS.train = task10.WAUS[train.row,]
task10.WAUS.test = task10.WAUS[-train.row,]

#Fit the network
WAUS.nn = neuralnet(RainTomorrow ~ Humidity3pm + Sunshine + Pressure3pm + Pressure9am + 
                      WindDir3pm + WindDir9am + WindGustDir + WindGustSpeed, task10.WAUS.train,
                      hidden=6,linear.output = FALSE)

#Plot the network
plot(WAUS.nn)


WAUS.pred = compute(WAUS.nn, task10.WAUS.test[c(1, 2, 3, 4, 5, 6, 7, 8)])
prob <- WAUS.pred$net.result
prob
#Round
pred <- ifelse(prob>0.5, 1, 0)
task10.WAUS.test$RainTomorrow
#Confusion matrix
nn.confmatrix<-table(observed = task10.WAUS.test$RainTomorrow, predicted = pred)

#Calculate accuracy using formula manually
nn.accuracy<-(nn.confmatrix[1,1]+nn.confmatrix[2,2])/(sum(nn.confmatrix))
nn.accuracy

#END OF CODE
