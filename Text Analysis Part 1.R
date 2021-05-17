#This script is for text analysis of Amazon book reviews. In the notes corpus refers
#to the current data set being used (ex. if df1 is specified as the file to work with
#in line 40, corpus refers to The Martian).

#Places where something needs to be changed depending on the file used will be indicated
#by a line of ++++++++++++++++++++++++++++++++++++++++++++ before and after. 

#Sections will be separated by a line of =========================================

#SOURCES: 
# 
#Term frequency: https://www.sciencedirect.com/topics/computer-science/inverse-document-frequency

#SETUP============================================================================
# Install all required packages.
#install.packages(c("stringr", ggplot2", "e1071", "caret", "quanteda",  "irlba", "randomForest", "stringi", "grepl", "tidyverse", "sqldf"))

#Set working directory
setwd("C:/Users/Laura/OneDrive/UMUC/Data Analytics/DATA 630/Group Project/AmazonReviews")

#SET OPTIONS
options(stringsAsFactors=F) #Strings are not considered factors
Sys.setlocale('LC_ALL','C') #Set system locale - deal with unrecognized characters

#IMPORT DATA
#This assumes the text files are in your working directory set on line 9.
#Read the data into a data frame
df1 <- read.csv(file="Andy-Weir-The-Martian.csv", header=FALSE, sep="\t")
df2 <- read.csv(file="Donna-Tartt-The-Goldfinch.csv", header=FALSE, sep="\t")
df3 <- read.csv(file="EL-James-Fifty-Shades-of-Grey.csv", header=FALSE, sep="\t")
df4 <- read.csv(file="Fillian_Flynn-Gone_Girl.csv", header=FALSE, sep="\t")
df5 <- read.csv(file="John-Green-The-Fault-in-our-Stars.csv", header=FALSE, sep="\t")
df6 <- read.csv(file="Laura-Hillenbrand-Unbroken.csv", header=FALSE, sep="\t")
df7 <- read.csv(file="Paula_Hawkins-The-Girl-On-The-Train.csv", header=FALSE, sep="\t")
df8 <- read.csv(file="Suzanne-Collins-The-Hunger-Games.csv", header=FALSE, sep="\t")
#dfc <- read.csv(file="combined.csv", header=FALSE, sep="\t")
info <- read.csv(file="titles.csv", header=TRUE, sep=",")

#Specify file to work with+++++++++++++++++++++++++++++++++++
df <- df1 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Preprocessing and exploratory=====================================================

#Split ASIN and ID
library(stringi)
library(stringr)
df$doc_id <- stri_sub(df$V2, 22, -17)
df$asin <- stri_sub(df$V2,-10)
df$rating <- as.numeric(df$V1)
df$reviewtitle <- df$V3
df$text <- stri_sub(df$V4, 39)
df <- subset(df, select=-c(1:4))
head(df)

#Add bookinfo+++++++++++++++++++++++++++++++++++++++++++++++
df$title <- "The Martian"            #1
df$author <- "Andy Weir"
#df$title <- "The Goldfinch"          #2
#df$author <- "Donna Tartt"
#df$title <- "Fifty Shades of Grey"   #3
#df$author <- "EL James"
#df$title <- "Gone Girl"              #4
#df$author <- "Fillian Flynn"
#df$title <- "The Fault in our Stars" #5
#df$author <- "John Green"
#df$title <- "Unbroken"               #6
#df$author <- "Laura Hillenbrand"
#df$title <- "The Girl On the Train"  #7
#df$author <- "Paula Hawkins"
#df$title <- "The Hunger Games"       #8
#df$author <- "Suzanne Collins"
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Rename columns (title, author, and asin refer to the book, 
#id, rating, head, and body refer to the review)
names(df) <- c("id", "asin", "rating", "head", "body", "title", "author")

#Remove breaks, span
df$head <- str_replace_all(df$head, "[\\r\\n\\t]+", " ")
df$head <- str_replace_all(df$head, "</span>", " ")
df$head <- str_replace_all(df$head, "<br/><br/>", " ")
#df$head <- str_replace_all(df$head, "???T", "'")
df$body <- str_replace_all(df$body, "[\\r\\n\\t]+", " ")
df$body <- str_replace_all(df$body, "</span>", " ")
df$body <- str_replace_all(df$body, "<br/><br/>", " ")
#df$body <- str_replace_all(df$body, "â???T", "'")

#Check data to see if there are missing values.
length(which(!complete.cases(df)))

#Convert stars to factor
df$stars <- as.factor(df$stars)

#Distribution of asin by rank      #Can be done for all books using combined
library(RColorBrewer)
# Stacked Bar Plot with Colors and Legend
#counts <- table(df$rating, df$title)
#barplot(counts, main="Review Distribution by Book and Rating",
#        xlab="Books", col=brewer.pal(9,"Blues"), cex.names=0.7,
#        legend = rownames(counts))

#Lengths of reviews
df$head.len <- nchar(df$head)
summary(df$head.len)
df$body.len <- nchar(df$body)
summary(df$body.len)

#Preprocessing: Tokenize=====================================================

#Tokenize
library(quanteda)
df.tokens <- tokens(df$body, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)
df.tokens[1:10]

# Lower case the tokens.
df.tokens <- tokens_tolower(df.tokens)
df.tokens[50:90]

#Build stopword list
df.tokens <- tokens_select(df.tokens, stopwords(), 
                         selection = "remove")
#Preprocessing: Stemming=====================================================

#Stem
df.tokens <- tokens_wordstem(df.tokens, language = "english")

#Preprocessing: word frequency=====================================================

#Word frequency: single words=====================================================
#Create bag of words
df.tokens.dfm <- dfm(df.tokens, tolower = FALSE)

#Transform -> matrix
df.tokens.matrix <- as.matrix(df.tokens.dfm)
df.tokens.matrix[1:20, 1:10] # 20 df$body x 10 words
dim(df.tokens.matrix) #Dimensions of matrix 1) count of reviews, 2) count of unique words

#Effects of stemming
colnames(df.tokens.matrix)[1:100] #View first 100 stemmed words

#Create functions
term.frequency <- function(row) {row/sum(row)} #term frequency (TF)
inverse.doc.freq <- function(col) {
  corpus.size <- length(col)
  doc.count <- length(which(col > 0))
  log10(corpus.size / doc.count)}              #inverse document frequency (IDF)
tf.idf <- function(x, idf) {x * idf}           #TF - IDF
#Normalize with TF
#Number of reviews in the corpus where the term is used.
df.tokens.df <- apply(df.tokens.matrix, 1, term.frequency)
dim(df.tokens.df)
View(df.tokens.df[1:20, 1:10])
#Calculate IDF
#Inverse document frequency refers to the number of times a word appears in the corpus. 
#IDF = 0 if the term is in every review.
#IDF is high when term is infrequent; low when term is frequent.
df.tokens.idf <- apply(df.tokens.matrix, 2, inverse.doc.freq)
str(df.tokens.idf)
#Calculate TF-IDF
#TF-IDF measures the importance of a term in the corpus.
df.tokens.tfidf <-  apply(df.tokens.df, 2, tf.idf, idf = df.tokens.idf)
dim(df.tokens.tfidf)
View(df.tokens.tfidf[1:25, 1:25])
#Transpose
df.tokens.tfidf <- t(df.tokens.tfidf)
dim(df.tokens.tfidf)
View(df.tokens.tfidf[1:25, 1:25])
#Check for incomplete
incomplete.cases <- which(!complete.cases(df.tokens.tfidf))
df$Text[incomplete.cases]
#Fix incomplete
df.tokens.tfidf[incomplete.cases,] <- rep(0.0, ncol(df.tokens.tfidf))
dim(df.tokens.tfidf)
sum(which(!complete.cases(df.tokens.tfidf)))

#Reduce to 300 columns with SVD
library(irlba)
df.irlba <- irlba(t(df.tokens.tfidf), nv = 300, maxit = 600)
View(df.irlba$v)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Make data frame
df1.tf <- cbind(df, data.frame(df.tokens.tfidf)) #Change dataframe number to df_.tf
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Export data frame
write.csv(df1.tf, 'df1tf.csv')

#End of part 1. Restart R (CTRL+SHIFT+F10)




#Word frequency: bigrams=====================================================
#Add bigrams to matrix
df.tokens <- tokens_ngrams(df.tokens, n = 1:2)
#Dfm and matrix
df.tokens.dfm <- dfm(df.tokens, tolower = FALSE)
df.tokens.matrix <- as.matrix(df.tokens.dfm)
#Normalize
df.tokens.df <- apply(df.tokens.matrix, 1, term.frequency)
