remove.packages(c("ggplot2", "data.table"))
remove.packages("arulesViz")
remove.packages("arules")
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)
install.packages("wordcloud")
install.packages("tm")
install.packages('SnowballC')
install.packages("slam")
install.packages("quanteda")
install.packages('proxy')
install.packages('factoextra')
install.packages('arules')
install.packages('arulesViz')
library(wordcloud)
library(tm)
# ONCE: install.packages("Snowball")
##library("Snowball")
library(slam)
library(quanteda)
library(SnowballC)
library(arules)
library(proxy)
library(ggplot2)
library(tidyverse)
library(arules)
library(arulesViz)
library(factoextra)


## Initial processing
# Set working directory
myfile <- "Kickstarter_projects-1.csv"
mydf <- read.csv(myfile, header = TRUE, stringsAsFactors = FALSE)
str(a_mydf)
mydf <- na.omit(mydf)
summary(a_mydf$usd_pledged)
hist(a_mydf$usd_pledged)
# data cleaning
a_mydf <- subset(mydf,mydf$goal_usd<100000)
a_mydf <- subset(mydf,mydf$usd_pledged<500000)
# discretize numeric variables
#cut(a_mydf$goal_usd, breaks=c(33000, 66000, 100000), labels=c("1-33K", "33K-66K", "66K-100K"))
a_mydf$goal_usd <- cut(a_mydf$goal_usd, breaks = 4, labels = c("1","2","3","4"))
a_mydf$usd_pledged <- cut(a_mydf$usd_pledged, breaks = 4, labels = c("1","2","3","4"))
a_mydf$duration <- cut(a_mydf$duration, breaks = 3, labels = c("1","2","3"))
a_mydf <- a_mydf[,-9]
a_mydf$start_month <- factor(a_mydf$start_month)
a_mydf$end_month <- factor(a_mydf$end_month)

## Model
rules <- apriori(a_mydf, parameter = list(support = 0.01, confidence = 0.8))

# run apriori
rules <- arules::apriori(a_mydf, parameter = list(support=0.2, confidence=0.6, minlen=2))
peprules <- subset( rules, subset = rhs %pin% "status")
summary(peprules)
inspect(sort(peprules[1:10], by="lift", decreasing=TRUE))

# sort by confidence
sortByConfidence <- sort(peprules, by="confidence", decreasing=TRUE)
inspect(subset(sortByConfidence[1:5]))

# sort by lift
subsetLift <- sort(peprules, by="lift", decreasing=TRUE)
inspect(subsetLift[1:5])


## Visualizations 
plot(peprules)
plot(peprules, method="graph")

