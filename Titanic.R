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
fancyRpartPlot(fit)
# remove columns created earlier, test & train should have same contents
test$Survived <- NA
train$Child <- NULL
train$Fare2 <- NULL
combi <- rbind(train, test)
View(combi)
combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
#combi$Title[combi$Title %in% c('Master', 'Miss', 'Ms', 'Mlle')] <- 'Unmarried'
#combi$Title[combi$Title %in% c('Mme', 'Mr', 'Mrs')] <- 'Married'
#combi$Title[combi$Title %in% c('Capt', 'Col', 'Don', 'Dr', 'Jonkheer', 'Major', 'Sir',   'Dona', 'Lady', 'the Countess')] <- 'High'
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'
combi$Title <- factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)
train <- combi[1:891,]
test <- combi[892:1309,]
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize #+ FamilyID
             ,data=train, method="class")
fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "features.csv", row.names = FALSE)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)
if(!require('randomForest')){
  install.packages('randomForest')
  library(randomForest)
}
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
                      FamilyID2, data=combi[1:891,], importance=TRUE, ntree=2000)
varImpPlot(fit)
Prediction <- predict(fit, combi[892:1309,])
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)
if(!require('party')){
  install.packages('party')
  library(party)
}

#todo:
# ticket price is a total - use ticketID, count # of tickets per ticketID and divide price by #oftickets
# find relation between cabinID and dead? Location....
temp <- table(combi$Ticket)
combi <- merge(combi, temp, by.x='Ticket', by.y='Var1')
combi$Tprice <- combi$Fare/combi$Freq
combi <- combi[order(combi$PassengerId),]
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Tprice + Embarked + Title + FamilySize + FamilyID,
               data = combi[1:891,], controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, combi[892:1309,], OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "partyforest.csv", row.names = FALSE)