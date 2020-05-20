library(dplyr)        # Data wrangling tasks
library(tidyr)        # Data wrangling tasks
library(ggplot2)      # Plotting/ Data visualization tasks
library(lubridate)    # Date/ Time manipulation
library(magrittr)     # Pipe operator
library(corrplot)     # Correlation function
install.packages("formattable")
library(formattable)  # Data Preview section
library(knitr)        # Data Preview section
library(broom)        # Glance function
library(boot)         # Bootstrapping function
library(glmnet)       # Cross validation function
library(mgcv)         # GAM function
install.packages("verification")
library("verification") # ROC plots
library(rpart)        # Classification Tree
library(rpart.plot)   # Classification Tree plot
library(caret)        # Confusion Matrix function
library(randomForest) # Random Forest function

ks =file.choose()
ks =read.csv(ks)
str(ks)

##data type conversion
#Data type conversion was done to relevant columns such as ID, Name that were converted to 'Character' datatype 
#and Deadline, Launched date that were converted to 'Date' datatype.
ks$id <- as.character(ks$id)
ks$name <- as.character(ks$name)
ks$deadline <- as.Date(ks$deadline)
ks$launched_at <- as.Date(ks$launched_at)


##2.Re-ordering the dataset variables to a meaningful order
ks <- ks[, c(1:4, 12, 8, 6, 5, 7, 9, 11, 13:15, 10)]
View(ks)


#3. Data sub-setting:
ggplot(ks, aes(status)) +
  geom_bar() +
  ylab("# of Projects") + xlab("Final status") +
  ggtitle("Final Status of the Kickstarter projects")

summary(ks$status)

##4. Check for duplicates:
  
  

##5. Check for missing values:

sum(is.na(ks)) 
colSums(is.na(ks))
colnames(ks)
  
  
##6. Feature Creation
#The dataset contains many categorical variables with multiple levels. 
#Many of those levels have too few observations within the level. 
#In order to reduce the number of parameters
#in the predictive analysis, such levels were consolidated.


#a. Country:
#removed the requirement of "state" and focused on country. 
#We will keep a separate dataset with states within each country for further analysis 

ks.proj<-ks[,c(-11, -15)]
plot(ks.proj$country)

ggplot(ks.proj, aes(country)) +
  geom_bar() +
  ylab("# of Projects") + xlab("Countries") +
  ggtitle("Initial Survey: Countries ")

View(ks.proj)

ks.proj$country <- as.character(ks.proj$country)
ks.proj$country <- as.factor(ks.proj$country)
# Reducing levels in Country
ks.proj$country <- as.character(ks.proj$country)
ks.proj$country[ks.proj$country %in% c("JP", "LU", "AT", "HK", "SG", "BE", "CH", "IE", "NO", "DK", 
                                       "MX", "NZ", "SE", "ES", "IT", "NL", "FR", "DE")] <- "Other"
ks.proj$country <- as.factor(ks.proj$country)

levels(ks.proj$country) # 5 levels
sort(round(prop.table(table(ks.proj$country)),2))

str(ks.proj)

ggplot(ks.proj, aes(country)) +
  geom_bar() +
  ylab("# of Projects") + xlab("Countries") +
  ggtitle("Post Survey: Countries ")



##b. Launched Year:


ks.proj <- ks.proj %>% 
  separate(col = "deadline", into = c("deadline_year", "deadline_month", "deadline_day"), sep = "-") %>%
  separate(col = "launched_at", into = c("launched_year", "launched_month", "launched_day"), sep = "-")

str(ks.proj)

ggplot(ks.proj, aes(launched_year)) +
  geom_bar() +
  ylab("# of Projects") + xlab("year launched") +
  ggtitle("Initial Survey: Launched_year ")



levels(ks.proj$launched_year) # 10 levels
round(prop.table(table(ks.proj$launched_year)),2)
# Reducing levels in Launched Year
ks.proj$launched_year <- as.character(ks.proj$launched_year)
ks.proj$launched_year[ks.proj$launched_year %in% c("2009", "2010", "2011")] <- "Before 2012"
ks.proj$launched_year <- as.factor(ks.proj$launched_year)

ggplot(ks.proj, aes(launched_year)) +
  geom_bar() +
  ylab("# of Projects") + xlab("year launched") +
  ggtitle("Post Survey: Launched_year ")




##c. Currency:
levels(ks.proj$currency)

ggplot(ks.proj, aes(currency)) +
  geom_bar() +
  ylab("# of Projects") + xlab("currency type") +
  ggtitle("Initial Survey: Currency ")




sort(round(prop.table(table(ks.proj$currency)),2))
# Reducing levels in Country
ks.proj$currency <- as.character(ks.proj$currency)
ks.proj$currency[ks.proj$currency %in% c("JPY", "HKD", "SGD", "CHF", "NOK", "DKK", "MXN", "NZD", 
                                         "SEK")] <- "Other"
ks.proj$currency <- as.factor(ks.proj$currency)

ggplot(ks.proj, aes(currency)) +
  geom_bar() +
  ylab("# of Projects") + xlab("currency type") +
  ggtitle("Post Survey: Currency ")


str(ks.proj)

