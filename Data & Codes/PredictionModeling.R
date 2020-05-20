# create Training - Test and Validation set (75 split) 

split <- sample.split(ks.proj$status, SplitRatio = 0.75)

#get training and test data
kicktrain <- subset(ks.proj, split == TRUE)
kicktest <- subset(ks.proj, split == FALSE)


summary(logitmodel1)

#####1. Logistic Regression



#  model selection 

library("InformationValue")
install.packages("InformationValue")

library("ROCR")

model.glm   <- glm(status ~ main_category + blurb_length + name_length, data = train.data, family = "binomial")

summary(model.glm)


# Logistic Regression - Parameter Tuning
# CV to choose cut-off probability
searchgrid = seq(0.4, 0.7, 0.02)
result = cbind(searchgrid, NA)
cost1 <- function(r, pi) {
  weight1 = 1
  weight0 = 1
  c1 = (r == 1) & (pi < pcut)  #logical vector - true if actual 1 but predict 0 (False Negative)
  c0 = (r == 0) & (pi > pcut)  #logical vector - true if actual 0 but predict 1 (False Positive)
  return(mean(weight1 * c1 + weight0 * c0))
}

for (i in 1:length(searchgrid)) {
  pcut <- result[i, 1]
  result[i, 2] <- cv.glm(data = train.data, glmfit = model.glm, cost = cost1, K = 3)$delta[2]
}

plot(result, ylab = "CV Cost",main = "Optimal cut-off probability identification")





par(mfrow = c(1,2))
# In-sample Prediction
tree.predict.in <- predict(tree.model, train.data, type = "class")
tree.pred.in <- predict(tree.model, train.data, type = "prob")
confusionMatrix(train.data$status, tree.predict.in)


roc.plot(train.data$status == "successful", pred.in, main = "In-sample ROC")$roc.vol

# Model selection - Validation data
pred.val <- predict(model.glm, newdata = validation.data, type = "response")
prediction.val <- ifelse(pred.val < 0.64,0,1)
table(as.factor(validation.data$status), prediction.val)
roc.plot(validation.data$status == "successful", pred.val, main = "Validation ROC")$roc.vol





###classication tre####


tree.model <- rpart(status ~ main_category + name_length + launched_year + blurb_length + duration + goal_usd, data = train.data, method = "class")
rpart.plot(tree.model)
#plotcp = cp values plotted agaisnt geometric mean
#to depict the deviation until the minimum value is 
#reached. . 
##Look into setting minsplot = 10


##Tune decision  tree: 

##DT Tuned Model 1 
#minsplit = 10, maxdepth=1
kick_tuned1<-rpart(status ~ main_category + name_length + launched_year + blurb_length + duration + goal_usd, data = train.data, method = 'class',
                   control = rpart.control( minsplit=5, maxdepth = 5 ))
rpart.plot(kick_tuned1) 



##inspect Tuned Model 1 
summary(kick_tuned1) #inspect tree
printcp(kick_tuned1) 
plotcp(kick_tuned1) #visualize cross-validation results 


##DT Tuned Model 2 
#minsplit = 10, maxdepth=5

kick_tuned2<-rpart(status ~ main_category + name_length + launched_year + blurb_length + duration + goal_usd, data = train.data, method = 'class',
                   control = rpart.control( minsplit=100, maxdepth = 30))

rpart.plot(kick_tuned2) 

##inspect Tuned Model 2
summary(kick_tuned2) #inspect tree
printcp(kick_tuned2) 
plotcp(kick_tuned2) #visualize cross-validation results 




###Testing:#####

kick_test<-data.frame(predict(tree.model, newdata = test.data))
#fed_test<-data.frame(predict(Fed_tuned2, newdata = test))

kick_test

kick_test<-kick_test %>% mutate(results = ifelse(successful==0, 'successful', ifelse(failed==1, 'failed', 'Successful')))
results<-kick_test %>% mutate(results = ifelse(successful==0, 'successful', ifelse(failed==1, 'failed', 'Successful')))

row.names(test.data)<- NULL
kick_test1<-data.frame(test.data %>% bind_cols(results))
View(kick_test1)
str(kick_test1)
kick_test1$results<-as.factor(kick_test1$results)


confusionMatrix(kick_test1$results)

###################PART 3###################
################### Prediction ###################

kick_pred<-data.frame(predict(tree.model, newdata = test.data))
kick_pred
plot(kick_pred)

 

