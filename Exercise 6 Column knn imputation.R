#Assignment 3: Decision Tree
#Course: DATA 630 9040 Machine Learning
#Professor: Dr. Firdu Bati
#Created by: Laura Ellis
#Date: October 31, 2019

#This script creates a neural network for classification of the column dataset

#Loading Data in R
#Set working directory and read the data
setwd("C:/Users/Laura/OneDrive/UMUC/Data Analytics/DATA 630/Module 5/Exercise 6")

#Import data
df <- read.csv(file='column.csv', head=TRUE, sep=',')
df$classf <- as.factor(df$class)
levels(df$classf) <- c("normal","abnormal")

#Load packages
library(neuralnet) #neural network model
library(Amelia) #missing values
library(ggplot2) #plotting
library(corrplot) #correlation
library(data.table) #detect and replace outliers
library(DMwR) #knn imputation for missing values

#Rename variables
names(df)<- c("PI", "PT", "LL", "SS", "PR", "DS", "class", "classf")

#Explore
head(df)
summary(df)

#Scale variables
df[1:6] <-scale(df[1:6])
summary(df)

#Check for outliers
boxplot(df$PI, df$PT, df$LL, df$SS, df$PR, df$DS, main="Identify Outliers", names=c("PI", "PT", "LL", "SS", "PR", "DS"))

#Create outlier function
outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}

#PI
outlier_values <- boxplot.stats(df$PI)$out
boxplot(df$PI, main="pelvic incidence", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
outlierReplace(df, "PI", which(df$PI > 3.2), NA)
#PT
outlier_values <- boxplot.stats(df$PT)$out
boxplot(df$PT, main="pelvic tilt", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
outlierReplace(df, "PT", which(df$PT > 1.92), NA)
outlierReplace(df, "PT", which(df$PT < -2.3), NA)
#LL
outlier_values <- boxplot.stats(df$LL)$out
boxplot(df$LL, main="lumbar lordosis", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
outlierReplace(df, "LL", which(df$LL > 3.9), NA)
#outlierReplace(df, "LL", which(df$LL < ), NA)
#SS
outlier_values <- boxplot.stats(df$SS)$out
boxplot(df$SS, main="sacral slope", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
outlierReplace(df, "SS", which(df$SS > 5.8), NA)
#outlierReplace(df, "SS", which(df$SS < ), NA)
#PR
outlier_values <- boxplot.stats(df$PR)$out
boxplot(df$PR, main="pelvic radius", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
outlierReplace(df, "PR", which(df$PR > 2.2), NA)
outlierReplace(df, "PR", which(df$PR < -2.1), NA)
#DS
outlier_values <- boxplot.stats(df$DS)$out
boxplot(df$DS, main="degree spondylyolisthesis", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
outlierReplace(df, "DS", which(df$DS > 1.97), NA)
#Check for outliers
boxplot(df$PI, df$PT, df$LL, df$SS, df$PR, df$DS, main = "Outliers Removed", names=c("PI", "PT", "LL", "SS", "PR", "DS"))

#Check for missing values
colSums(is.na(df)) 
#visualize the missing data
missmap(df)

#Impute missing values
df <- knnImputation(df,k=10,scale=T,meth="weighAvg")

#Check for missing values
colSums(is.na(df)) #None
#visualize the missing data
missmap(df)

#Correlation
dfcor.df <- subset(df, select=-c(8))
dfcor.df$class <- as.numeric(dfcor.df$class)
dfcor <- round(cor(dfcor.df, method=c("pearson")),3)
corrplot(dfcor, type = "upper", order = "hclust", t1.col="black", tl.srt=45)

#Plot dependent variable
tbl <- with(df, table(class))
ggplot(as.data.frame(tbl), aes(class, Freq, fill = class)) +     
  geom_col(position = 'dodge')
#Plot independent variables
par(mfrow = c(2,1))
#PT
ggplot(data = df, mapping = aes(x = PT, colour = classf)) +
  geom_freqpoly(binwidth = 0.1)
ggplot(data = df, mapping = aes(x = df$classf, y = PT)) +
  geom_boxplot()
#PR
ggplot(data = df, mapping = aes(x = PR, colour = classf)) +
  geom_freqpoly(binwidth = 0.1)
ggplot(data = df, mapping = aes(x = df$classf, y = PR)) +
  geom_boxplot()
#DS
ggplot(data = df, mapping = aes(x = DS, colour = classf)) +
  geom_freqpoly(binwidth = 0.1)
ggplot(data = df, mapping = aes(x = df$classf, y = DS)) +
  geom_boxplot()
#PI
ggplot(data = df, mapping = aes(x = PI, colour = classf)) +
  geom_freqpoly(binwidth = 0.1)
ggplot(data = df, mapping = aes(x = df$classf, y = PI)) +
  geom_boxplot()
#SS
ggplot(data = df, mapping = aes(x = SS, colour = classf)) +
  geom_freqpoly(binwidth = 0.1)
ggplot(data = df, mapping = aes(x = df$classf, y = SS)) +
  geom_boxplot()
#LL
ggplot(data = df, mapping = aes(x = LL, colour = classf)) +
  geom_freqpoly(binwidth = 0.1)
ggplot(data = df, mapping = aes(x = df$classf, y = LL)) +
  geom_boxplot()

#Split data (reproducable)
set.seed(12345)
ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.7, 0.3))
train <- df[ind == 1, ]
test <- df[ind == 2, ]

#Specify model
model <- class~PI+PT+LL+SS+PR+DS

#Build model
nn <- neuralnet(model, data=train, hidden=2, err.fct="ce", linear.output = FALSE)
#Availabe properties
names(nn)

#Network properties
nn$call                  # the command we ran to generate the model
nn$response[1:10]        # actual values of the dependent variable for first 10 records
nn$covariate [1:12,]     # input variables that were used to build the model for first 12 records
nn$model.list            # list dependent and independent variables in the model
nn$net.result[[1]][1:10] # display the first 10 predicted probabilities
nn$weights               # network weights after the last method iteration
nn$startweights          # weights on the first method iteration
nn$result.matrix         # number of trainings steps, the error, and the weights 

#VISUALIZE
plot(nn)

#EVALUATE
predict<-neuralnet::compute(nn, nn$covariate)$net.result
predict<-apply(predict, c(1), round)
predict [1:10]

#CONFUSION MATRIX - TRAIN
table(predict, train$class, dnn =c("Predicted", "Actual"))
mean(predict==train$class)

#CONFUSION MATRIX - TEST
testPred <- neuralnet::compute(nn, test[, 0:6])$net.result
testPred<-apply(testPred, c(1), round)
table(testPred, test$class, dnn =c("Predicted", "Actual"))
mean(testPred==test$class)

#ALTERNATE INPUTS
#Specify model
model <- class~PT+LL+SS+PR+DS
#Build model
nn <- neuralnet(model, data=train, hidden=2, err.fct="ce", linear.output = FALSE)
#VISUALIZE
plot(nn)
#EVALUATE
predict<-neuralnet::compute(nn, nn$covariate)$net.result
predict<-apply(predict, c(1), round)
predict [1:10]
#CONFUSION MATRIX - TRAIN
table(predict, train$class, dnn =c("Predicted", "Actual"))
mean(predict==train$class)
#CONFUSION MATRIX - TEST
testPred <- neuralnet::compute(nn, test[, 0:6])$net.result
testPred<-apply(testPred, c(1), round)
table(testPred, test$class, dnn =c("Predicted", "Actual"))
mean(testPred==test$class)

#DECISION TREE
#Load package
library("party")

#Copy clean data
df.t <- subset(df, select=-c(7))

#Split to train/text
train.t <- df.t[ind == 1, ]
test.t <- df.t[ind == 2, ]

#Build tree
tree <- ctree(classf~., data = train.t)

#visualize the tree
nodes(tree, 2)
plot(tree)
plot(tree, type="simple")

#confusion matrix - train
table(predict(tree), train.t$classf)
prop.table(table(predict(tree), train.t$classf))

#evaluate the model on a test data
testPred.t <- predict(tree, newdata = test.t)
table (testPred.t, test.t$classf)

#Tree predictions
# predict class
test.t$predClass = predict(tree, newdata=test.t, type="response")  
# probability
test.t$predProb = sapply(predict(tree, newdata=test.t,type="prob"),'[[',2) 
# predicted node
test.t$predNode = predict(tree, newdata=test.t, type="node")



