library(readxl)
library(caret)

rm(list=ls())  #Read in the hs0 data over the internet using the read.table() function.

data <- read_excel("C:/Users/ABHI/desktop/sem1/data mining/assignment 3/mnplr.xlsx",sheet=5)
# data <- sample_data
full_data <- read_excel("C:/Users/ABHI/desktop/sem1/data mining/assignment 3/mnplr.xlsx",sheet=4)

data$Manipulator <- as.factor(data$Manipulator)
full_data$Manipulater <- as.factor(full_data$Manipulater)

data$`C-MANIPULATOR` <- as.factor(data$`C-MANIPULATOR`)
full_data$`C-MANIPULATOR` <- as.factor(full_data$`C-MANIPULATOR`)


# install.packages("unbalanced")

################################################################################################################
# Q2
# SMOTE method - Sample dataset
set.seed(1000)
library(unbalanced)

n <- ncol(data)
output <- data$`C-MANIPULATOR`
output <- as.factor(output)
input <- data [ ,-n]
# View(input)

sample_data <- ubBalance(X= input, Y=output, type="ubSMOTE", percOver=300, percUnder=150)
# View(data)

balanced_sample_Data <- cbind(sample_data$X,sample_data$Y)
# View(balanced_sample_Data)

names(balanced_sample_Data)[11] <- "new_C_Manipulator"
table(balanced_sample_Data$new_C_Manipulator)


#SMOTE method - Complete dataset
library(unbalanced)

n1<-ncol(full_data)
output1<- full_data$`C-MANIPULATOR`
output1<-as.factor(output1)
input1<- full_data [ ,-n1]
# View(input1)

data1<-ubBalance(X= input1, Y=output1, type="ubSMOTE",percOver=700, percUnder=350)
# View(data1)

balanced_full_data<-cbind(data1$X,data1$Y)
# View(balancedData_full)

names(balanced_full_data)[11] <- "new_C_Manipulator"
table(balanced_full_data$new_C_Manipulator)


## Rename the response variable name and remove the Company ID
colnames(data)[ncol(data)] <- "Flag"
data <- data[,-1]

colnames(full_data)[ncol(full_data)] <- "Flag"
full_data <- full_data[,-1]

colnames(balanced_sample_Data)[ncol(balanced_sample_Data)] <- "Flag"
balanced_sample_Data <- balanced_sample_Data[,-1]

colnames(balanced_full_data)[ncol(balanced_full_data)] <- "Flag"
balanced_full_data <- balanced_full_data[,-1]



#################################################################################################################
# Creating training and test dataset using stratified sampling
set.seed(1000)
train.index <- createDataPartition(balanced_full_data$Flag, p = .6, list = FALSE)
balanced_full_data_train <- balanced_full_data[ train.index,]
balanced_full_data_test  <- balanced_full_data[-train.index,]
# index <- sample(2, nrow(balanced_full_data), replace= TRUE, prob=c(0.6,0.4))
# balanced_full_data_train <- balanced_full_data[index==1,]
# balanced_full_data_test <- balanced_full_data[index==2,]

table(balanced_full_data$Flag)
table(balanced_full_data_train$Flag)
table(balanced_full_data_test$Flag)



#################################################################################################################
# Q3, Q4, Q5, Q6

### Run the logistic regressions

## The 4 datasets are 
# 1) data
# 2) full_data
# 3) balanced_sample_Data
# 4) balanced_full_data

# Sample data unbalanced

data_used <- "Sample data unbalanced"
technique <- "Logistic regression"

set.seed(1000)
fit <- glm(Flag ~ . -Manipulator , data = data, family = "binomial")

train_predict <- predict(fit, data, type = "response")
train_predict <- data.frame(train_predict)
colnames(train_predict)[1] <- "V1"

table(train_predict$V1, data$Flag)

df <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())
t_s <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())

for(i in seq(0, 1, by=0.001))
{
  
  y_predict <- data.frame(ifelse(train_predict$V1<i,0,1))
  
  colnames(y_predict)[1] <- "V1"
  
  #Create confusion matrix
  conf_data <- data.frame(cbind(y_predict, data$Flag))
  
  t <- table(conf_data$V1,conf_data$data.Flag)
  t1 <- data.frame(table(conf_data$V1, conf_data$data.Flag))
  
  chk <- confusionMatrix(conf_data$V1,conf_data$data.Flag, positive='1')      # --> To calculate accuracy
  
  df[1,1] <- i
  df[1,2] <- chk$overall["Accuracy"]
  df[1,3] <- chk$byClass["Sensitivity"]
  df[1,4] <- 1 - chk$byClass["Specificity"]
  df[1,5] <- df[1,3] - df[1,4]
  t_s <- rbind(t_s,df)
  
}

plot(t_s, ty='b')
summary(t_s)

select_df <- subset(t_s, YI==max(YI))
result_df <- data.frame(colMeans(select_df))
result_df <- data.frame(t(result_df))
result_df <- cbind(result_df, Dataset = data_used, Method = technique)

final_df <- NULL
final_df <- rbind(final_df,result_df)


#*****************************************************
# Creating the M-score

manipulated_data <- data
manipulated_data$DEPI <- 0
manipulated_data$SGAI <- 0
manipulated_data$LEVI <- 0

# M_score <- data.frame(predict(fit, manipulated_data, type = "response"))
M_score <- data.frame(predict(fit, manipulated_data))
M_threshold <- log(result_df$Threshold/(1-result_df$Threshold))




#################################################################################################################
# Q7

data_used <- "Sample data unbalanced"
technique <- "CART tree"

set.seed(1000)
library(rpart)
library(rpart.plot)
sample_tree <- rpart(Flag ~ .- Manipulator, data = data, method="class", parms = list(split="gini"))
summary(sample_tree)
printcp(sample_tree)

#plotting
rpart.plot(sample_tree)


df <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())
t_s <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())


pred_train <- predict(sample_tree,data,type = "class")

# conf_mat <- table(pred_train,data$Flag)
# temp1 <- data.frame(conf_mat)

conf_data <- data.frame(cbind(data.frame(pred_train), data$Flag))
chk <- confusionMatrix(conf_data$pred_train,conf_data$data.Flag, positive='1')    # --> To calculate accuracy and sensitivity


df[1,1] <- "Not applicable"
df[1,2] <- chk$overall["Accuracy"]
df[1,3] <- chk$byClass["Sensitivity"]
df[1,4] <- 1 - chk$byClass["Specificity"]
df[1,5] <- df[1,3] - df[1,4]

result_df <- df
result_df <- cbind(result_df, Dataset = data_used, Method = technique)

final_df <- rbind(final_df,result_df)

# Accuracy = 91.81



#################################################################################################################
# Q8

# install.packages("splitstackshape")
library(splitstackshape)

data_used <- "Full data balanced"
technique <- "Logistic regresion"

# Full data balanced
set.seed(1000)

fit <- glm(Flag ~ . -Manipulater , data = balanced_full_data_train, family = "binomial")
summary(fit)

test_predict <- predict.glm(fit, balanced_full_data_test, type = "response")
test_predict <- data.frame(test_predict)
colnames(test_predict)[1] <- "V1"

table(test_predict$V1, balanced_full_data_test$Flag)

df <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())
t_s <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())

for(i in seq(0, 1, by=0.001))
{
  
  y_predict <- data.frame(ifelse(test_predict$V1<i,0,1))
  
  colnames(y_predict)[1] <- "V1"
  
  
  #Create confusion matrix
  conf_data <- data.frame(cbind(y_predict, balanced_full_data_test$Flag))
  
  t <- table(conf_data$V1,conf_data$balanced_full_data_test.Flag)
  t1 <- data.frame(table(conf_data$V1, conf_data$balanced_full_data_test.Flag))
  
  chk <- confusionMatrix(conf_data$V1,conf_data$balanced_full_data_test.Flag, positive='1')      # --> To calculate accuracy
  
  df[1,1] <- i
  df[1,2] <- chk$overall["Accuracy"]
  df[1,3] <- chk$byClass["Sensitivity"]
  df[1,4] <- 1 - chk$byClass["Specificity"]
  df[1,5] <- df[1,3] - df[1,4]
  t_s <- rbind(t_s,df)
  
}

plot(t_s, ty='b')
summary(t_s)

select_df <- subset(t_s, YI==max(YI))
result_df <- data.frame(colMeans(select_df))
result_df <- data.frame(t(result_df))
result_df <- cbind(result_df, Dataset = data_used, Method = technique)

final_df <- rbind(final_df,result_df)


#*****************************************************
# Creating the M-score

manipulated_data <- balanced_full_data_test
manipulated_data$SGAI <- 0
manipulated_data$LEVI <- 0

# M_score <- data.frame(predict(fit, manipulated_data, type = "response"))
M_score <- data.frame(predict(fit, manipulated_data))
M_threshold <- log(result_df$Threshold/(1-result_df$Threshold))




#################################################################################################################
# Q9


#***************************************************************************************************************
### 1st part
## Creating CART tree for full balanced data

data_used <- "Full data balanced"
technique <- "CART tree"

# Full data balanced
set.seed(1000)



library(rpart)
library(rpart.plot)
full_balanced_tree <- rpart(Flag ~ .- Manipulater, data = balanced_full_data_train, method="class", parms = list(split="gini"))
summary(full_balanced_tree)
printcp(full_balanced_tree)

#plotting
rpart.plot(full_balanced_tree)

df <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())
t_s <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())

pred_test <- predict(full_balanced_tree, balanced_full_data_test, type = "class")

# conf_data <- data.frame(cbind(data.frame(pred_test), balanced_full_data_test$Flag))
conf_data <- data.frame(pred_test, balanced_full_data_test$Flag)
chk <- confusionMatrix(conf_data$pred_test,conf_data$balanced_full_data_test.Flag, positive='1')    # --> To calculate accuracy and sensitivity


df[1,1] <- "Not applicable"
df[1,2] <- chk$overall["Accuracy"]
df[1,3] <- chk$byClass["Sensitivity"]
df[1,4] <- 1 - chk$byClass["Specificity"]
df[1,5] <- df[1,3] - df[1,4]

result_df <- df
result_df <- cbind(result_df, Dataset = data_used, Method = technique)

final_df <- rbind(final_df,result_df)


#***************************************************************************************************************
### 2nd part
### Creating Random Forest ensemble for full balanced data

# install.packages("randomForest")
# install.packages("caTools")
# install.packages("ROCR")

library(randomForest)
library(caTools)
library(ROCR)

data_used <- "Full data balanced"
technique <- "Random Forest"

# Full data balanced
set.seed(1000)
mtry_rf=if (!is.null(balanced_full_data_train$Flag) && !is.factor(balanced_full_data_train$Flag)) max(floor(ncol(balanced_full_data_train)/3), 1) else floor(sqrt(ncol(balanced_full_data_train))) 
manipulation_rf = randomForest(Flag ~ . -Manipulater , data = balanced_full_data_train, ntree = 100, mtry = mtry_rf, proximity =TRUE, replace = TRUE, sampsize = nrow(balanced_full_data_train), importance = TRUE )


### results- different functions to view results
print(manipulation_rf)
attributes(manipulation_rf)

### To access the error rate 
plot(manipulation_rf)
manipulation_rf$err.rate
rndF1.legend <- if (is.null(manipulation_rf$balanced_full_data_test$err.rate)) {colnames(manipulation_rf$err.rate)} else {colnames(manipulation_rf$balanced_full_data_test$err.rate)}
legend("top", cex =0.5, legend=rndF1.legend, lty=c(1,2,3), col=c(1,2,3), horiz=T)

### Variable importance
importance(manipulation_rf)
dim(importance(manipulation_rf))
# importance(manipulation_rf)[2:4,5]



# Plot variable importance
varImpPlot(manipulation_rf)

### Get accuracy of prediction
# Predicting on training data
table(predict(manipulation_rf), balanced_full_data_train$Flag) #balanced_full_data_train

df <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())
t_s <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())

# Predicting on test data
Flag_pred <-  predict(manipulation_rf, balanced_full_data_test)

conf_data <- data.frame(cbind(data.frame(Flag_pred), balanced_full_data_test$Flag))
chk <- confusionMatrix(conf_data$Flag_pred,conf_data$balanced_full_data_test.Flag, positive='1')    # --> To calculate accuracy and sensitivity


df[1,1] <- "Not applicable"
df[1,2] <- chk$overall["Accuracy"]
df[1,3] <- chk$byClass["Sensitivity"]
df[1,4] <- 1 - chk$byClass["Specificity"]
df[1,5] <- df[1,3] - df[1,4]

result_df <- df
result_df <- cbind(result_df, Dataset = data_used, Method = technique)

final_df <- rbind(final_df,result_df)



#***************************************************************************************************************
### 3rd part  ---> Boosting using adaboost
## Creating adaptive bossting ensemble for full balanced data

data_used <- "Full data balanced"
technique <- "Adaptive bossting - adabag package"

# install.packages("adabag")
library(adabag)
df <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())
t_s <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())


set.seed(1000)
v.adaboost <- boosting(Flag ~ . -Manipulater , data = balanced_full_data_train, mfinal = 40, control = rpart.control(maxdepth = 2))

# barplot(v.adaboost$imp[order(v.adaboost$imp, decreasing = TRUE)], ylim = c(0, 100), main = "Variables Relative Importance", col = "lightblue")
# 
# v.adaboost$trees

Flag_boost_pred <- predict(v.adaboost, balanced_full_data_test)

conf_matrix <- Flag_boost_pred$confusion
# conf_data <- data.frame(cbind(data.frame(Flag_pred), balanced_full_data_test$Flag))
chk <- confusionMatrix(Flag_boost_pred$confusion, positive='1')    # --> To calculate accuracy and sensitivity


df[1,1] <- "Not applicable"
df[1,2] <- chk$overall["Accuracy"]
df[1,3] <- chk$byClass["Sensitivity"]
df[1,4] <- 1 - chk$byClass["Specificity"]
df[1,5] <- df[1,3] - df[1,4]

result_df <- df
result_df <- cbind(result_df, Dataset = data_used, Method = technique)

final_df <- rbind(final_df,result_df)


#***************************************************************************************************************
### 4th part  ---> Boosting using mboost on Full balanced data

data_used <- "Full data balanced"
technique <- "Adaptive bossting - mboost package"

library(mboost)

df <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())
t_s <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())

set.seed(1000)

temp <- balanced_full_data_train[-9]

v.adaboost <- mboost(Flag ~ .  , data = temp
                     ,control = boost_control(mstop = 300, nu = 0.5,
                                              risk = c("inbag", "oobag", "none"), stopintern = FALSE,
                                              center = TRUE, trace = FALSE)
                     , family= Binomial(type = c("adaboost", "glm"),
                                        link = c("logit"))
                     , baselearner = c("bbs", "bols", "btree", "bss", "bns")
                     
)

# barplot(v.adaboost$imp[order(v.adaboost$imp, decreasing = TRUE)], ylim = c(0, 100), main = "Variables Relative Importance", col = "lightblue")
# 
# v.adaboost$trees

# Flag_boost_pred <- predict(v.adaboost, balanced_full_data_test, type = "response")
Flag_boost_pred <- predict(v.adaboost, balanced_full_data_test, type = "class")

conf_data <- data.frame(cbind(data.frame(Flag_boost_pred), balanced_full_data_test$Flag))
chk <- confusionMatrix(conf_data$Flag_boost_pred,conf_data$balanced_full_data_test.Flag, positive='1')    # --> To calculate accuracy and sensitivity


df[1,1] <- "Not applicable"
df[1,2] <- chk$overall["Accuracy"]
df[1,3] <- chk$byClass["Sensitivity"]
df[1,4] <- 1 - chk$byClass["Specificity"]
df[1,5] <- df[1,3] - df[1,4]

result_df <- df
result_df <- cbind(result_df, Dataset = data_used, Method = technique)

final_df <- rbind(final_df,result_df)



#----------X--------------X--------------X--------------X--------------X--------------X--------------X----------
#----------X--------------X--------------X--------------X--------------X--------------X--------------X----------

# In the code above, the data was first balanced and then stratified sampling was employed. This method causes
# the test data to be balanced as well. This is not the correct approach because in practice, we do not have 
# access to the test data.
# To remedy this, we should first divide the data using stratified sampling and then balance only the train data
# The following code uses this procedure

#----------X--------------X--------------X--------------X--------------X--------------X--------------X----------
#----------X--------------X--------------X--------------X--------------X--------------X--------------X----------


rm(list=ls())  #Read in the hs0 data over the internet using the read.table() function.

data <- read_excel("E:/UIC/Study/IDS 572/Assignments/3/IMB579-XLS-ENG.xlsx",sheet=5)
# data <- sample_data
full_data <- read_excel("E:/UIC/Study/IDS 572/Assignments/3/IMB579-XLS-ENG.xlsx",sheet=4)

data$Manipulator <- as.factor(data$Manipulator)
full_data$Manipulater <- as.factor(full_data$Manipulater)

# install.packages("unbalanced")


## Rename the response variable name
colnames(data)[ncol(data)] <- "Flag"
data <- data[,-1]

colnames(full_data)[ncol(full_data)] <- "Flag"
full_data <- full_data[,-1]





#################################################################################################################
# Creating training and test dataset using stratified sampling --> This is being done on the unbalanced data
set.seed(1000)
train.index <- createDataPartition(full_data$Flag, p = .6, list = FALSE)
full_data_train <- full_data[ train.index,]
full_data_test  <- full_data[-train.index,]
# index <- sample(2, nrow(full_data), replace= TRUE, prob=c(0.6,0.4))
# full_data_train <- full_data[index==1,]
# full_data_test <- full_data[index==2,]

table(full_data$Flag)
table(full_data_train$Flag)
table(full_data_test$Flag)


##################################################
#SMOTE method - training dataset
library(unbalanced)

n1<-ncol(full_data_train)
output1<- full_data_train$Flag
output1<-as.factor(output1)
input1<- full_data_train [ ,-n1]
# View(input1)

data1<-ubBalance(X= input1, Y=output1, type="ubSMOTE",percOver=700, percUnder=350)
# View(data1)

balanced_full_data_train<-cbind(data1$X,data1$Y)
# View(balancedData_full)

# Removing the company ID and renaming the response variable
colnames(balanced_full_data_train)[ncol(balanced_full_data_train)] <- "Flag"
# balanced_full_data_train <- balanced_full_data_train[,-1]


### The datasets that we will be using are:-
# 1) fUll_data
# 2) balanced_full_data_train
# 3) full_data_test


#################################################################################################################
# Q3, Q4, Q5, Q6

### Run the logistic regressions

# Sample data unbalanced

data_used <- "Sample data unbalanced"
technique <- "Logistic regression"

set.seed(1000)
fit <- glm(Flag ~ . -Manipulator , data = data, family = "binomial")

train_predict <- predict(fit, data, type = "response")
train_predict <- data.frame(train_predict)
colnames(train_predict)[1] <- "V1"

table(train_predict$V1, data$Flag)

df <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())
t_s <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())

for(i in seq(0, 1, by=0.001))
{
  
  y_predict <- data.frame(ifelse(train_predict$V1<i,0,1))
  
  colnames(y_predict)[1] <- "V1"
  
  #Create confusion matrix
  conf_data <- data.frame(cbind(y_predict, data$Flag))
  
  t <- table(conf_data$V1,conf_data$data.Flag)
  t1 <- data.frame(table(conf_data$V1, conf_data$data.Flag))
  
  chk <- confusionMatrix(conf_data$V1,conf_data$data.Flag, positive='1')      # --> To calculate accuracy
  
  df[1,1] <- i
  df[1,2] <- chk$overall["Accuracy"]
  df[1,3] <- chk$byClass["Sensitivity"]
  df[1,4] <- 1 - chk$byClass["Specificity"]
  df[1,5] <- df[1,3] - df[1,4]
  t_s <- rbind(t_s,df)
  
}

plot(t_s, ty='b')
summary(t_s)

select_df <- subset(t_s, YI==max(YI))
result_df <- data.frame(colMeans(select_df))
result_df <- data.frame(t(result_df))
result_df <- cbind(result_df, Dataset = data_used, Method = technique)

final_df <- NULL
final_df <- rbind(final_df,result_df)



#*****************************************************
# Creating the M-score

manipulated_data <- data
manipulated_data$DEPI <- 0
manipulated_data$SGAI <- 0
manipulated_data$LEVI <- 0

# M_score <- data.frame(predict(fit, manipulated_data, type = "response"))
M_score <- data.frame(predict(fit, manipulated_data))
M_threshold <- log(result_df$Threshold/(1-result_df$Threshold))




#################################################################################################################
# Q7

data_used <- "Sample data unbalanced"
technique <- "CART tree"

set.seed(1000)
library(rpart)
library(rpart.plot)
sample_tree <- rpart(Flag ~ .- Manipulator, data = data, method="class", parms = list(split="gini"))
summary(sample_tree)
printcp(sample_tree)

#plotting
rpart.plot(sample_tree)


df <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())
t_s <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())


pred_train <- predict(sample_tree,data,type = "class")

# conf_mat <- table(pred_train,data$Flag)
# temp1 <- data.frame(conf_mat)

conf_data <- data.frame(cbind(data.frame(pred_train), data$Flag))
chk <- confusionMatrix(conf_data$pred_train,conf_data$data.Flag, positive = '1')    # --> To calculate accuracy and sensitivity


df[1,1] <- "Not applicable"
df[1,2] <- chk$overall["Accuracy"]
df[1,3] <- chk$byClass["Sensitivity"]
df[1,4] <- 1 - chk$byClass["Specificity"]
df[1,5] <- df[1,3] - df[1,4]

result_df <- df
result_df <- cbind(result_df, Dataset = data_used, Method = technique)

final_df <- rbind(final_df,result_df)



#################################################################################################################
# Q8

# install.packages("splitstackshape")
library(splitstackshape)

data_used <- "Full data balanced"
technique <- "Logistic regresion"

# Full data balanced
set.seed(1000)

fit <- glm(Flag ~ . -Manipulater , data = balanced_full_data_train, family = "binomial")
summary(fit)

test_predict <- predict.glm(fit, full_data_test, type = "response")
test_predict <- data.frame(test_predict)
colnames(test_predict)[1] <- "V1"

table(test_predict$V1, full_data_test$Flag)

df <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())
t_s <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())

for(i in seq(0, 1, by=0.001))
{
  
  y_predict <- data.frame(ifelse(test_predict$V1<i,0,1))
  
  colnames(y_predict)[1] <- "V1"
  
  
  #Create confusion matrix
  conf_data <- data.frame(cbind(y_predict, full_data_test$Flag))
  
  t <- table(conf_data$V1,conf_data$full_data_test.Flag)
  t1 <- data.frame(table(conf_data$V1, conf_data$full_data_test.Flag))
  
  chk <- confusionMatrix(conf_data$V1,conf_data$full_data_test.Flag, positive='1')      # --> To calculate accuracy
  
  df[1,1] <- i
  df[1,2] <- chk$overall["Accuracy"]
  df[1,3] <- chk$byClass["Sensitivity"]
  df[1,4] <- 1 - chk$byClass["Specificity"]
  df[1,5] <- df[1,3] - df[1,4]
  t_s <- rbind(t_s,df)
  
}

plot(t_s, ty='b')
summary(t_s)

select_df <- subset(t_s, YI==max(YI))
result_df <- data.frame(colMeans(select_df))
result_df <- data.frame(t(result_df))
result_df <- cbind(result_df, Dataset = data_used, Method = technique)

final_df <- rbind(final_df,result_df)



#*****************************************************
# Creating the M-score

manipulated_data <- full_data_test
manipulated_data$SGAI <- 0
manipulated_data$LEVI <- 0

# M_score <- data.frame(predict(fit, manipulated_data, type = "response"))
M_score <- data.frame(predict(fit, manipulated_data))
M_threshold <- log(result_df$Threshold/(1-result_df$Threshold))


#################################################################################################################
# Q9


#***************************************************************************************************************
### 1st part
## Creating CART tree for full balanced data

data_used <- "Full data balanced"
technique <- "CART tree"

# Full data balanced
set.seed(1000)



library(rpart)
library(rpart.plot)
full_balanced_tree <- rpart(Flag ~ .- Manipulater, data = balanced_full_data_train, method="class", parms = list(split="gini"))
summary(full_balanced_tree)
printcp(full_balanced_tree)

#plotting
rpart.plot(full_balanced_tree)

df <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())
t_s <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())

pred_test <- predict(full_balanced_tree, full_data_test, type = "class")

conf_data <- data.frame(cbind(data.frame(pred_test), full_data_test$Flag))
chk <- confusionMatrix(conf_data$pred_test,conf_data$full_data_test.Flag, positive='1')    # --> To calculate accuracy and sensitivity


df[1,1] <- "Not applicable"
df[1,2] <- chk$overall["Accuracy"]
df[1,3] <- chk$byClass["Sensitivity"]
df[1,4] <- 1 - chk$byClass["Specificity"]
df[1,5] <- df[1,3] - df[1,4]

result_df <- df
result_df <- cbind(result_df, Dataset = data_used, Method = technique)

final_df <- rbind(final_df,result_df)


#***************************************************************************************************************
### 2nd part
### Creating Random Forest ensemble for full balanced data

# install.packages("randomForest")
# install.packages("caTools")
# install.packages("ROCR")

library(randomForest)
library(caTools)
library(ROCR)

data_used <- "Full data balanced"
technique <- "Random Forest"

# Full data balanced
set.seed(1000)
mtry_rf=if (!is.null(balanced_full_data_train$Flag) && !is.factor(balanced_full_data_train$Flag)) max(floor(ncol(balanced_full_data_train)/3), 1) else floor(sqrt(ncol(balanced_full_data_train))) 
manipulation_rf = randomForest(Flag ~ . -Manipulater , data = balanced_full_data_train, ntree = 100, mtry = mtry_rf, proximity =TRUE, replace = TRUE, sampsize = nrow(balanced_full_data_train), importance = TRUE )


### results- different functions to view results
print(manipulation_rf)
attributes(manipulation_rf)

### To access the error rate 
plot(manipulation_rf)
manipulation_rf$err.rate
rndF1.legend <- if (is.null(manipulation_rf$full_data_test$err.rate)) {colnames(manipulation_rf$err.rate)} else {colnames(manipulation_rf$full_data_test$err.rate)}
legend("top", cex =0.5, legend=rndF1.legend, lty=c(1,2,3), col=c(1,2,3), horiz=T)

### Variable importance
importance(manipulation_rf)
dim(importance(manipulation_rf))
# importance(manipulation_rf)[2:4,5]



# Plot variable importance
varImpPlot(manipulation_rf)

### Get accuracy of prediction
# Predicting on training data
table(predict(manipulation_rf), balanced_full_data_train$Flag) #balanced_full_data_train

df <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())
t_s <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())

# Predicting on test data
Flag_pred <-  predict(manipulation_rf, full_data_test)

conf_data <- data.frame(cbind(data.frame(Flag_pred), full_data_test$Flag))
chk <- confusionMatrix(conf_data$Flag_pred,conf_data$full_data_test.Flag, positive = '1')    # --> To calculate accuracy and sensitivity


df[1,1] <- "Not applicable"
df[1,2] <- chk$overall["Accuracy"]
df[1,3] <- chk$byClass["Sensitivity"]
df[1,4] <- 1 - chk$byClass["Specificity"]
df[1,5] <- df[1,3] - df[1,4]

result_df <- df
result_df <- cbind(result_df, Dataset = data_used, Method = technique)

final_df <- rbind(final_df,result_df)



#***************************************************************************************************************
### 3rd part  ---> Boosting using adaboost
## Creating adaptive bossting ensemble for full balanced data

data_used <- "Full data balanced"
technique <- "Adaptive bossting - adabag package"

# install.packages("adabag")
library(adabag)
df <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())
t_s <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())


set.seed(1000)
v.adaboost <- boosting(Flag ~ . -Manipulater , data = balanced_full_data_train, mfinal = 40, control = rpart.control(maxdepth = 2))

# barplot(v.adaboost$imp[order(v.adaboost$imp, decreasing = TRUE)], ylim = c(0, 100), main = "Variables Relative Importance", col = "lightblue")
# 
# v.adaboost$trees

Flag_boost_pred <- predict(v.adaboost, data.frame(full_data_test), type="class")

conf_matrix <- Flag_boost_pred$confusion
# conf_data <- data.frame(cbind(data.frame(Flag_pred), full_data_test$Flag))
chk <- confusionMatrix(Flag_boost_pred$confusion, positive='1')    # --> To calculate accuracy and sensitivity


df[1,1] <- "Not applicable"
df[1,2] <- chk$overall["Accuracy"]
df[1,3] <- chk$byClass["Sensitivity"]
df[1,4] <- 1 - chk$byClass["Specificity"]
df[1,5] <- df[1,3] - df[1,4]

result_df <- df
result_df <- cbind(result_df, Dataset = data_used, Method = technique)

final_df <- rbind(final_df,result_df)


#***************************************************************************************************************
### 4th part  ---> Boosting using mboost on Full balanced data

data_used <- "Full data balanced"
technique <- "Adaptive bossting - mboost package"

library(mboost)

df <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())
t_s <- data.frame(Threshold=double(), Accuracy=double(), Sensitivity=double(), FPR=double(), YI=double())

set.seed(1000)

temp <- balanced_full_data_train[-9]

v.adaboost <- mboost(Flag ~ .  , data = temp
                     ,control = boost_control(mstop = 300, nu = 0.5,
                                              risk = c("inbag", "oobag", "none"), stopintern = FALSE,
                                              center = TRUE, trace = FALSE)
                     , family= Binomial(type = c("adaboost", "glm"),
                                        link = c("logit"))
                     , baselearner = c("bbs", "bols", "btree", "bss", "bns")
                     
)


# barplot(v.adaboost$imp[order(v.adaboost$imp, decreasing = TRUE)], ylim = c(0, 100), main = "Variables Relative Importance", col = "lightblue")
# 
# v.adaboost$trees

# Flag_boost_pred <- predict(v.adaboost, full_data_test, type = "response")
Flag_boost_pred <- predict(v.adaboost, data.frame(full_data_test), type = "class")

conf_data <- data.frame(cbind(data.frame(Flag_boost_pred), full_data_test$Flag))
chk <- confusionMatrix(conf_data$Flag_boost_pred,conf_data$full_data_test.Flag, positive='1')    # --> To calculate accuracy and sensitivity


df[1,1] <- "Not applicable"
df[1,2] <- chk$overall["Accuracy"]
df[1,3] <- chk$byClass["Sensitivity"]
df[1,4] <- 1 - chk$byClass["Specificity"]
df[1,5] <- df[1,3] - df[1,4]

result_df <- df
result_df <- cbind(result_df, Dataset = data_used, Method = technique)

final_df <- rbind(final_df,result_df)





