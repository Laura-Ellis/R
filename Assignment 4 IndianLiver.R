#Assignment 4: Neural Network
#Course: DATA 630 9040 Machine Learning
#Professor: Dr. Firdu Bati
#Created by: Laura Ellis
#Date: November 9, 2019

#This script creates a neural network for the Indian Liver data set

#SOURCES
#https://rpubs.com/bpoulin-CUNY/338004 (Exploratory)
#https://rstudio-pubs-static.s3.amazonaws.com/344010_1f4d6691092d4544bfbddb092e7223d2.html
#http://www.sthda.com/english/wiki/colors-in-r
#https://edu.kpfu.ru/pluginfile.php/419285/mod_resource/content/2/neural-networks-r.pdf
#https://www.datacamp.com/community/tutorials/pca-analysis-r

#SET WORKING DIRECTORY
setwd("C:/Users/Laura/OneDrive/UMUC/Data Analytics/DATA 630/Module 5/Assignment 4")

#LOAD PACKAGES
library(imputeTS) #Imputation
library(RColorBrewer) #Plot colors
library(reshape2) #Plot histograms (melt function)
library(ggplot2) #Plots
library(corrplot) #Plot correlation
library(tibble)
library(magrittr)
library(caret)
library(mlbench)
library(neuralnet)
library(NeuralNetTools)
library(gmodels)
library(e1071)
library(ggbiplot)
library(dplyr)

#IMPORT DATA
liver <- read.csv(file='IndianLiver.csv', head=TRUE, sep=',')
#Copy to working dataframe
df <- liver
#Rename columns
colnames(df) <- c("Age", "Gender", "Tot_Bil", "Dir_Bil", "Alkphos", "Alamine", 
                          "Aspartate", "Tot_Prot", "Albumin", "A_G_Ratio", "Group")

#Convert patient group
df$Group <- as.numeric(ifelse(df$Group == 2, 0, 1))  #converted to zeros and ones
df$Gender <- as.numeric(ifelse(df$Group == 2, 0, 1))  #converted to zeros and ones
df$Sex <- 0
df$Sex[df$Gender == 'Male'] <- 1
t1 <- df[1]
t2 <- df[3:11]
t3 <- df[12]
df <- cbind(t1,t3,t2)

#View data
summary(df)
str(df)

#Check for missing values
colSums(is.na(df))
df <- na_mean(df) #mean imputation 4 values A_G_Ratio
colSums(is.na(df))

#View data
summary(df)
str(df)

#Scale data - min/max method
input<-df[,1:11]
indx <- sapply(input, is.factor)
input <- as.data.frame(lapply(input, function(x) as.numeric(as.character(x))))
max_data <- apply(input, 2, max)
min_data <- apply(input, 2, min)
input_scaled <- as.data.frame(scale(input,center = min_data, scale = max_data - min_data))
df1 <- input_scaled
summary(df1)
str(df1)

#EXPLORATORY ANALYSIS
#Dependent variable
group <- table(df1$Group)
colors <- c("gray", "lightgreen") 
group.prop <- prop.table(group)
group.prop.df <- as.data.frame(group.prop)
pielabels <- sprintf("%s - %1.3f", group.prop.df[,1], group.prop, "%")
pie(group.prop,
    labels=pielabels,  
    clockwise=TRUE,
    col=colors,
    border="black",
    radius=0.8,
    cex=0.8, 
    main="Patients/Non-Patients")

#+++++++++++++++++++++++++++++++++++++FIX
#Independent Variables
#Histograms
ggplot(data = melt(df1[3:11], id.var = "Group"), mapping = aes(x = value)) + 
  geom_histogram(bins = 10, aes(fill=Group), alpha=0.5) + facet_wrap(~variable, scales = 'free_x')

#CORRELATION
# calculate collinearity
corr <- cor(df1[,1:10])
corrplot(corr, order = "hclust", tl.cex = 0.7)
#remove highly correlated
highlyCor <- colnames(df1)[findCorrelation(corr, cutoff = 0.9, verbose = TRUE)]
highlyCor

#PRINCIPLE COMPONENT ANALYSIS
df.pca <- prcomp(df1[1:10], center=TRUE, scale=FALSE)
summary(df.pca)
plot(df.pca, type="l", main='')
grid(nx = 10, ny = 14)
title(main = "Principal Components Weight", sub = NULL, xlab = "Components")
box()
# Calculate the proportion of variance explained
pca_var <- df.pca$sdev^2
pve_df <- pca_var / sum(pca_var)
cum_pve <- cumsum(pve_df)
pve_table <- tibble(comp = seq(1:ncol(df1 %>% select(-Group))), pve_df, cum_pve)
#Plot proportion of variance explained
ggplot(pve_table, aes(x = comp, y = cum_pve)) + 
  geom_point() + 
  geom_abline(intercept = 0.95, color = "red", slope = 0) +
  geom_abline(intercept = 0.99, color = "darkred", slope = 0)
#THRESHOLD 95%
#Set PCA threshold .95
pca <- prcomp(df1[1:10], center=TRUE, scale=TRUE, rank=6) #rank=6 for .95 threshold
#Plot PC1 and PC2
pca_df <- as.data.frame(pca$x)
df2 <- cbind(pca_df,df2$Group)
names(df2)[7] <- "Group"


ggplot(df2, aes(x=PC1, y=PC2, col=Group)) + geom_point(alpha=0.5)
autoplot(pca, data = df1,  colour = 'Group',
         loadings = FALSE, loadings.label = TRUE, loadings.colour = "black")
summary(pca)
df_pcs <- cbind(tibble::enframe(df2$Group), tibble::enframe(pca$x))
GGally::ggpairs(df_pcs, columns = 2:4, ggplot2::aes(color = value))

#THRESHOLD 99%
#Set PCA threshold .99
pca1 <- prcomp(df1[1:10], center=TRUE, scale=TRUE, rank=8) #rank=8 for .99 threshold
#Plot PC1 and PC2
pca_df1 <- as.data.frame(pca1$x)
df3 <- cbind(pca_df,df3$Group)
names(df3)[7] <- "Group"


#SET SEED for DATA SPLIT
#make sure that the result is reproducible
set.seed(1234)
#split the data into a training and test set
ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.7, 0.3))

#NEURAL NETWORK NO PCA
#CREATE TRAIN AND TEST DATA SETS
train1 <- df1[ind == 1, ]
test1 <- df1[ind == 2, ]
#Get variable names and create model
n1 = names(df1[1:10])
model1 = as.formula(paste("Group ~", paste(n1, collapse = " + ")))
#nn1 = neuralnet(model1,data=train1,hidden=2,linear.output=FALSE)
nn1 = neuralnet(model1,data=train1,hidden=6,linear.output=FALSE)
#NEURAL NET 1 DETAILS
#Network properties
nn1$call                  # the command we ran to generate the model
nn1$response[1:10]        # actual values of the dependent variable for first 10 records
nn1$covariate [1:12,]     # input variables that were used to build the model for first 12 records
nn1$model.list            # list dependent and independent variables in the model
nn1$net.result[[1]][1:10] # display the first 10 predicted probabilities
nn1$weights               # network weights after the last method iteration
nn1$startweights          # weights on the first method iteration
nn1$result.matrix         # number of trainings steps, the error, and the weights 
#Plot model
plot(nn1)
#EVALUATE
predict<-neuralnet::compute(nn1, nn1$covariate)$net.result
predict<-apply(predict, c(1), round)
table(predict, train1$Group, dnn =c("Predicted", "Actual"))
mean(predict==train1$Group)
#TEST
testPred <- neuralnet::compute(nn1, test1[, 1:10])$net.result
testPred<-apply(testPred, c(1), round)
table(testPred, test1$Group, dnn =c("Predicted", "Actual"))
mean(testPred==test1$Group)
garson(nn1)
plotnet(nn1)

#NEURAL NETWORK .95
#CREATE TRAIN AND TEST DATA SETS
train2 <- df2[ind == 1, ]
test2 <- df2[ind == 2, ]
#Get variable names and create model
n2 = names(df2[1:6])
model2 = as.formula(paste("Group ~", paste(n2, collapse = " + ")))
nn2 = neuralnet(model2,data=train2,hidden=2,linear.output=FALSE)
#nn2 = neuralnet(model2,data=train2,hidden=6,linear.output=FALSE)
#NEURAL NET 1 DETAILS
#Network properties
nn1$call                  # the command we ran to generate the model
nn1$response[1:10]        # actual values of the dependent variable for first 10 records
nn1$covariate [1:12,]     # input variables that were used to build the model for first 12 records
nn1$model.list            # list dependent and independent variables in the model
nn1$net.result[[1]][1:10] # display the first 10 predicted probabilities
nn1$weights               # network weights after the last method iteration
nn1$startweights          # weights on the first method iteration
nn1$result.matrix         # number of trainings steps, the error, and the weights 
#Plot model
plot(nn2)
#EVALUATE
predict<-neuralnet::compute(nn2, nn2$covariate)$net.result
predict<-apply(predict, c(1), round)
table(predict, train2$Group, dnn=c("Predicted", "Actual"))
mean(predict==train2$Group)
#TEST
testPred <- neuralnet::compute(nn1, test1[, 1:6])$net.result
testPred<-apply(testPred, c(1), round)
table(testPred, test1$Group, dnn =c("Predicted", "Actual"))
mean(testPred==test1$Group)
garson(nn2)
plotnet(nn1)


library(NeuralNetTools)
library(nnet)
nn1 <- nnet(model, data=df, size=16, ring=.1, decay=5e-2, maxit=5000)
plotnet(nn1)
garson(nn1)