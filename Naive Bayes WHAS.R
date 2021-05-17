#This script uses Naive Bayes to predict survival and place of death for heart attack data WHAS500

#Loading Data in R
#Set working directory and read the data
setwd("C:/Users/Laura/OneDrive/UMUC/Data Analytics/DATA 630/Module 3/Assignment 2")

#Load packages
library(ggplot2)
library(arules)
library(e1071)
library(stringi)

#Read the data into a data frame
whas500 <- read.csv("whas500.csv")
df <- whas500

#Correlation
dfcor.df <- subset(whas500, select=-c(1,16:18))
dfcor <- round(cor(dfcor.df, method=c("pearson")),2)
dfcor
library(corrplot)
corrplot(dfcor, type = "upper", order = "hclust", t1.col="black", tl.srt=45)

#View data
describe(df)
head(df)
summary(df)
str(df)

#Assign outcome
df$outcome <- NA
df$outcome <- paste(df$dstat,df$fstat)
df$outcome <- factor(df$outcome, labels = c("Alive", "Dead at Follow Up", "Dead at Hospital"))
summary(df$outcome)
str(df$outcome)

#Discretize los, lenfol
df$los <- discretize(df$los, method="frequency", breaks=5)
df$lenfol <- NULL
plot(df$los)

#Discretize blood pressure
df$sysbp <- discretize(df$sysbp, method="fixed", breaks=c(0, 120, 130, 140, 180, Inf), labels=c("0-120","120-130","130-140","140+","180+"))
df$diasbp <- discretize(df$diasbp, method="fixed", breaks=c(0, 80, 90, 120, Inf), labels=c("0-80","80-90","90+","120+"))
#Assign blood pressure categories
df$bp <- NA
df$bp <- paste(df$sysbp,df$diasbp)
df$bp <- factor(df$bp)
levels(df$bp)
levels(df$bp)<-c("Normal", "HT Stage 1", "HT Stage 2", "Elevated",
                 "HT Stage 1", "HT Stage 2", "HT Stage 1", "HT Stage 1",
                 "HT Stage 2", "HT Stage 2", "HT Crisis", "HT Stage 2",
                 "HT Stage 2", "HT Crisis", "HT Crisis", "HT Crisis",
                 "HT Crisis")
summary(df$bp)

#Convert to categorical predictors
df$gender <- factor(df$gender, levels= c(0,1), labels=c("male", "female"))
df$cvd <- factor(df$cvd, levels=c(0,1), labels=c("No", "Yes"))
df$afb <- factor(df$afb, levels=c(0,1), labels=c("No", "Yes"))
df$sho <- factor(df$sho, levels=c(0,1), labels=c("No", "Yes"))
df$chf <- factor(df$chf, levels=c(0,1), labels=c("No", "Yes"))
df$av3 <- factor(df$av3, levels=c(0,1), labels=c("No", "Yes"))
df$miord <- factor(df$miord, levels=c(0,1), labels=c("First", "Recurrent"))
df$mitype <- factor(df$mitype, levels=c(0,1), labels=c("non Q-wave", "Q-wave"))

#View data
head(df)
summary(df)
str(df)

#Remove id, sysbp, diasbp, admitdate, disdate, fdate, dstat, fstat
df <- subset(df, select = -c(1,5:6,16:18,20:21))

#Save copy of cleaned data for bayes
bayes_df <- df

#check for missing values
colSums(is.na(df)) #None
#visualize the missing data
library(Amelia)
missmap(df)

#Set seed to ensure result is reproducable
set.seed(1234)

#Split data into training and test
ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.7, 0.3))
train.df <- df [ind == 1, ]
test.df <- df [ind == 2, ]

#Percentages of split
prop.table(table(train.df$outcome))
prop.table(table(test.df$outcome))

#Build model
library(e1071)
#model <- naiveBayes(outcome~., train.df) #model 1
#model <- naiveBayes(outcome~age+hr+sho+av3+year+los+bp, train.df) #model 2
model <- naiveBayes(outcome~age+gender+hr+bmi+cvd+afb+sho+chf+year+los+bp, train.df) #model 3


#Evaluate the model on a training set
predict(model, train.df, type="raw")   #This command returns the predicted probabilities for each class
predict(model, train.df, type="class") #This command returns the predicted class
table(predict(model, train.df, type="class"), train.df$outcome, dnn=c("predicted", "actual"))
#classification accuracy and error for the training data
mean(predict (model, train.df, type="class")== train.df$outcome)
mean(predict (model, train.df, type="class")!= train.df$outcome)

#Evaluate the model on a test set
predict(model, test.df, type="raw")    #This command returns the predicted probabilities for each class
predict(model, test.df, type="class") #This command returns the predicted class
table(predict(model, test.df, type="class"), test.df$outcome, dnn=c("predicted", "actual"))
#classification accuracy  and error for the testing data
mean(predict (model, test.df, type="class")== test.df$outcome)
mean(predict (model, test.df, type="class")!= test.df$outcome)

#Klar to plot variables
#Plot variables
library(klaR)
mn <- NaiveBayes(outcome~age+gender+hr+bmi+cvd+afb+sho+chf+year+los+bp, data=test.df)
plot(mn)

#Caret for model statistics and variable importance
train.df <- df [ind == 1, ]
#Set y as predicted
y=train.df$outcome
#Make model after removing unnecessary variables
training.outcome <- subset(train.df, select=c(14))
training <- subset(train.df, select=(-c(9,10,11,14)))
testing <- test.df
prop.table(table(y))
x=training
#Create model
model=train(x,y,'nb',trControl=trainControl(method='cv',number=10))
model
train.df <- df [ind == 1, ]
#Predict training
predict <-predict(model,newdata=train.df)
#Confusion matrix
confusionMatrix(predict, train.df$outcome)
#Predict testing
predict <-predict(model,newdata=test.df)
#Confusion matrix
confusionMatrix(predict, test.df$outcome)
X<-varImp(model)
plot(X)
