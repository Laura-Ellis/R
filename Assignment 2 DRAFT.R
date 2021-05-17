#Assignment 2: Logistic Regression
#Course: DATA 630 9040 Machine Learning
#Professor: Dr. Firdu Bati
#Created by: Laura Ellis
#Date: October 4, 2019

#Loading Data in R
#Set working directory and read the data
setwd("C:/Users/Laura/OneDrive/UMUC/Data Analytics/DATA 630/Module 3/Assignment 2")

#Load packages
library(aod)
library(ggplot2)
library(arules)
library(e1071)
library(gapminder)
library(mlr)
#library(blorr)
#library(tidymodels)

#Read the data into a data frame
whas500 <- read.csv("whas500.csv")
df <- whas500
attach(df)

#View data
head(df)
summary(df)
str(df)

#Remove patients died at hospital
#df <- subset(df, dstat==0)

#Assign outcome
df$outcome <- NA
df$outcome <- paste(df$dstat,df$fstat)
df$outcome <- factor(df$outcome, labels = c("Alive", "Dead at Hosp", "Dead FollowUp"))
summary(df$outcome)

#Remove id, admitdate, disdate, fdate
df <- subset(df, select = -c(1,16:18,20,22))

#Discretize numeric predictors
df$age <- discretize(df$age, method="fixed", breaks=c(0, 50, 60, 70, 80, 90, Inf), labels=c("under 50", "50-60", "60-70", "70-80", "80-90", "over 90"))
df$sysbp <- discretize(df$sysbp, method="fixed", breaks=c(0, 120, 130, 140, 180, Inf), labels=c("0-120","120-130","130-140","140+","180+"))
df$diasbp <- discretize(df$diasbp, method="fixed", breaks=c(0, 80, 90, 120, Inf), labels=c("0-80","80-90","90+","120+"))
df$hr <- discretize(df$hr, method="fixed", breaks=c(0, 60, 100, Inf), labels=c("Slow","Normal","Fast"))
df$bmi <- discretize(df$bmi, method="fixed", breaks=c(0, 18.5, 25, 30, Inf), labels=c("Underweight", "Normal", "Overweight", "Obese"))
df$los <- discretize(df$los, method="fixed", breaks=c(0,3,7,14,21,28,Inf), labels=c("<3 days", "<1 week", "1-2 weeks", "2-3 weeks", "3-4 weeks", ">4 weeks"))
df$lenfol <- discretize(df$lenfol, method="fixed", breaks=c(0,365,1095,1826,Inf), labels=c("<1 year", "1-3 years", "3-5 years", "5+ years"))
summary(df)

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
df$year <- factor(df$year, levels=c(1,2,3), labels=c("1997", "1999", "2001"))
#df$dstat <- factor(df$dstat, levels=c(0,1), labels=c("Alive", "Dead"))
#df$fstat <- factor(df$fstat, levels=c(0,1), labels=c("Alive", "Dead"))

#View data
head(df)
summary(df)
str(df)

#Save copy of cleaned data
clean_df <- df

#check for missing values
colSums(is.na(df)) #None

#Set seed to ensure result is reproducable
set.seed(1234)

#Split data into training and test
ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.7, 0.3))
train.df <- df [ind == 1, ]
test.df <- df [ind == 2, ]

#Build model using glm
model<-naiveBayes(outcome~., train.df)
model
bayes.table <- prop.table(table(train.df$outcome))  # Returns the same output as A-priori probabilitys in the model

#Evaluate the model on a training set
predict(model, train.df, type="raw")   #This command returns the predicted probabilities for each class
predict(model, train.df, type="class") #This command returns the predicted class
bayes.table.pred <- table(predict(model, train.df, type="class"), train.df$outcome, dnn=c("predicted", "actual"))
#classification accuracy for the training data
bayes.train.acc <- mean(predict (model, train.df, type="class")== train.df$outcome)
#Classification error for the training data
bayes.train.err <- mean(predict (model, train.df, type="class")!= train.df$outcome)

#Evaluate the model on a test set
predict(model, test.df, type="raw")    #This command returns the predicted probabilities for each class
predict(model, test.df, type="class") #This command returns the predicted class
table(predict(model, test.df, type="class"), test.df$outcome, dnn=c("predicted", "actual"))
#classification accuracy for the training data
mean(predict (model, test.df, type="class")== test.df$outcome)
#Classiication error for the training data
mean(predict (model, test.df, type="class")!= test.df$outcome)























#model <-glm(outcome~., data=train.df, family=binomial())
#summary(model)



library(ROCR)
library(effects)
model2<-step(model)
summary(model2)
plot(allEffects(model2))
#Classification accuracy and confusion matrix for reduced model
mypredictions<-round(predict (model2, test.data, type="response")) 
table (mypredictions, test.data$LOW, dnn=c("predicted", "actual"))
mean(round(predict (model2, test.data, type="response"))== test.data$LOW)




model               # Output the coefficients and intercept
summary(model)      # Output the p value for each coefficient
coef(model)         # To display the model coefficients only
model$coefficients  # Another way to display the model coefficients only
model$residuals     # Output the residuals
residuals(model)    # Another way to output the residuals
exp(coef(model))    # Odds ratios
confint(model)      # Confidence intervals
