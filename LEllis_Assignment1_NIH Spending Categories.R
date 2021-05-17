#Assignment 1: Association Rules Analysis
#Course: DATA 630 9040 Machine Learning
#Professor: Dr. Firdu Bati
#Created by: Laura Ellis
#Date: September 28, 2019

#This script uses association rules to find relationships between spending categories for NIH grants. 
#The data is downloaded from the NIH Project Reporter https://projectreporter.nih.gov/reporter.cfm and 
  #uses the field for spending category. 

#Load packages
library(arules)
library(arulesViz)

#Import data file
trans <- read.transactions("NIH2018SC.txt", format = c("basket"), 
                           header = FALSE, sep = ";", 
                           cols = NULL, rm.duplicates = FALSE, 
                           quote = "\"'", skip = 0, 
                           encoding = "unknown")
df <- as(trans,"transactions")
summary(df)

#Plot frequent spending categories
itemFrequencyPlot(df,support=0.2, main = "Top 5 Spending Categories 2018")
itemFrequencyPlot(df,support=0.1, main = "Top 12 Spending Categories 2018")

#Read the CSV file.  
df1 <- read.csv(file="NIH 2018 Top SCs Cost.csv", head=TRUE, sep=",", na.strings=c("","NA"))

#View data
summary(df1)
str(df1)

#View amounts awarded
summary(df1$AMOUNT)
plot(df1$AMOUNT, ylim=c(0, 20000000), main = "Grants awarded by amount", xlab = "observation number", ylab = "Award amount (extreme outliers not shown)")

#Factor nominal variables
df1$ID <- factor(df1$ID)
df1$IC <- factor(df1$IC)
df1$Biotechnology <- factor(df1$Biotechnology)
df1$Cancer <- factor(df1$Cancer)
df1$Clinical.Research <- factor(df1$Clinical.Research)
df1$Genetics <- factor(df1$Genetics)
df1$Neurosciences <- factor(df1$Neurosciences)

#Discretize numeric
df1$AMOUNT<-discretize(df1$AMOUNT, method="frequency", breaks=4, labels= c("Low","Low-Mid","Mid-High", "High"))

#View data
summary(df1)
str(df1)



#Rules for spending categories
rules<-apriori(df)
# preview the first 10 rules
#inspect(rules[1:2])    0 rules for default parameters
#Run the summary command on rules
#summary(rules)

#Change support and confidence options
rules <- apriori(df, parameter= list(minlen=1, supp=0.05, conf=0.80))
inspect(rules[1:10]) #Better, 11 rules 
summary(rules)


#Change support and confidence options
rules <- apriori(df, parameter= list(minlen=2, supp=0.01, conf=0.80))
inspect(rules[1:10]) #Much better, 398 rules
summary(rules)

#Sort by lift
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)



#Pruning
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
length(rules.sorted)

#Inspect pruned rules
rules.pruned <- rules.sorted[!redundant]
rules.pruned <- sort(rules, by="confidence")
inspect(rules.pruned)

#how many rules remain after pruning
length(rules.pruned)
summary(rules.pruned)
inspect(rules.pruned)

#Subset Biotech rules
Biotech.rules <- subset(rules.pruned, items %in% c("Biotechnology"))
inspect(Biotech.rules)

#Subset Cancer rules
Cancer.rules <- subset(rules.pruned, items %in% c("Cancer"))
inspect(Cancer.rules)

#Subset Clinical Research rules
ClResearch.rules <- subset(rules.pruned, items %in% c("Clinical Research"))
inspect(ClResearch.rules)

#Subset Genetics rules
Genetics.rules <- subset(rules.pruned, items %in% c("Genetics"))
inspect(Genetics.rules)

#Subset Neuro rules
Neuro.rules <- subset(rules.pruned, items %in% c("Neurosciences"))
inspect(Neuro.rules)

#pull the rules with the lift above 50
rules.subset<-subset(rules.pruned, lift>50)
inspect(rules.subset)
summary(rules.subset)

#Interactive plot
plot(rules.pruned, engine='interactive', shading="lift",  measure="support", jitter=0)

goodrules <- rules.pruned[quality(rules.pruned)$lift > 5]
inspect(goodrules)
#Saltz, J. S., & Stanton, J. M. (2018). An introduction to data science. Los Ángeles: SAGE.
plot(goodrules, method = "grouped", main = "")
plot(goodrules, method = "grouped", control = list(k = 15))
sel <- plot(rules, method = "grouped", interactive = TRUE)
subrules2 <- head(rules, n = 20, by = "confidence")
subrules2 <- head(rules, n = 20, by = "lift")
plot(subrules2, method = "graph")
saveAsGraph(head(rules, n = 1000, by = "lift"), file = "rules.graphml")
plot(subrules2, method = "paracoord")
plot(subrules2, method = "paracoord", reorder=true)
oneRule <- sample(rules, 1)
inspect(oneRule)
plot(oneRule, method = "doubledecker", data = df)
#Hahsler M, Chelluboina S. Visualizing Association Rules: Introduction to the R-extension Package arulesViz. R project module. 2011

#Second file
Run the method with default parameters
rules1<-apriori(df1)
# preview the first 10 rules
inspect(rules1[1:2]) #Only 2 rules
#Run the summary command on rules
summary(rules1)

#support and confidence options
rules1 <- apriori(df1, parameter= list(minlen=2, supp=0.01, conf=0.80))
inspect(rules1[1:10])
summary(rules1)
rules1.sorted <- sort(rules1, by="confidence")
inspect(rules1.sorted)

#Pruning
rules1.sorted <- sort(rules1, by="lift")
inspect(rules1.sorted)
subset.matrix1 <- is.subset(rules1.sorted, rules.sorted)
subset.matrix1[lower.tri(subset.matrix1, diag=T)] <- F
redundant1 <- colSums(subset.matrix1, na.rm=T) >= 1
which(redundant1)
length(rules1.sorted)

#Inspect pruned rules
rules1.pruned <- rules1.sorted[!redundant1]
inspect(rules1.pruned)

#Subset Biotech rules
Biotech.rules1 <- subset(rules1.pruned, items %in% c("Biotechnology=1"))
inspect(Biotech.rules1)

#Subset Cancer rules
Cancer.rules1 <- subset(rules1.pruned, items %in% c("Cancer=1"))
inspect(Cancer.rules)

#Subset Clinical Research rules
ClResearch.rules1 <- subset(rules1.pruned, items %in% c("Clinical.Research=1"))
inspect(ClResearch.rules1)

#Subset Genetics rules
Genetics.rules1 <- subset(rules1.pruned, items %in% c("Genetics=1"))
inspect(Genetics.rules1)

#Subset Neuro rules
Neuro.rules1 <- subset(rules1.pruned, items %in% c("Neurosciences=1"))
inspect(Neuro.rules1)

#Plot
plot(rules1.pruned, engine='interactive', shading="lift",  measure="support", jitter=0)
goodrules1 <- rules1.pruned[quality(rules1.pruned)$lift > 2]
inspect(goodrules1)
#Saltz, J. S., & Stanton, J. M. (2018). An introduction to data science. Los Ángeles: SAGE.
plot(goodrules1, method = "grouped", main = "")
plot(goodrules1, method = "grouped", control = list(k = 15))
sel <- plot(rules1, method = "grouped", interactive = TRUE)
subrules3 <- head(rules1, n = 20, by = "confidence")
subrules3 <- head(rules1, n = 20, by = "lift")
plot(subrules3, method = "graph")
saveAsGraph(head(rules1, n = 1000, by = "lift"), file = "rules.graphml")
plot(subrules3, method = "paracoord")
plot(subrules3, method = "paracoord", reorder=true)
oneRule <- sample(rules1, 1)
inspect(oneRule)
plot(oneRule, method = "doubledecker", data = df)

