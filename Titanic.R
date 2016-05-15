# set path
# can I use github here?
setwd("H:/R/ML_Titanic")
# import training and test data
# strings are read as Factors, else use:
# train <- read.csv("H:/R/ML_Titanic/train.csv",stringsAsFactors=FALSE
train <- read.csv("H:/R/ML_Titanic/train.csv")
View(train)
test <- read.csv("H:/R/ML_Titanic/test.csv")
View(test)
# asume nobody survived in test-set
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "onlymaleperish.csv", row.names = FALSE)
View(submit)
# now commit this small change
train$Child <- 0
train$Child[train$Age < 18] <- 1
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "betterdata.csv", row.names = FALSE)
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
if(!require('rattle')){
  install.packages('rattle')
  library(rattle)
}
if(!require('rpart.plot')){
  install.packages('rpart.plot')
  library(rpart.plot)
}
if(!require('RColorBrewer')){
  install.packages('RColorBrewer')
  library(RColorBrewer)
}

fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(xval=20))
#new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)
# remove columns created earlier, test & train should have same contents
test$Survived <- NA
train$Child <- NULL
train$Fare2 <- NULL
combi <- rbind(train, test)
View(combi)
combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
combi$Title[combi$Title %in% c('Master', 'Miss', 'Ms', 'Mlle')] <- 'Unmarried'
combi$Title[combi$Title %in% c('Mme', 'Mr', 'Mrs')] <- 'Married'
combi$Title[combi$Title %in% c('Capt', 'Col', 'Don', 'Dr', 'Jonkheer', 'Major', 'Sir',   'Dona', 'Lady', 'the Countess')] <- 'High'
combi$Title <- factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
train <- combi[1:891,]
test <- combi[892:1309,]
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize,
             data=train, method="class")
fancyRpartPlot(new.fit)
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "features.csv", row.names = FALSE)
