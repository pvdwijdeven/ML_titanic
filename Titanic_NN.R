# load libraries
if(!require('rpart.plot')){
  install.packages('rpart.plot')
  library(rpart.plot)
}
if(!require('party')){
  install.packages('party')
  library(party)
}
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
# set path
setwd("H:/R/ML_Titanic")

# import training and test data
# strings are read as Factors, else use:
# train <- read.csv("H:/R/ML_Titanic/train.csv",stringsAsFactors=FALSE
train <- read.csv("H:/R/ML_Titanic/train.csv")
View(train)
test <- read.csv("H:/R/ML_Titanic/test.csv")
test$Survived <- NA
View(test)

# create combined overview
combi <- rbind(train, test)
View(combi)
combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)

# for now the title-separation below seems to work worse than te original one...
#combi$Title[combi$Title %in% c('Master', 'Miss', 'Ms', 'Mlle')] <- 'Unmarried'
#combi$Title[combi$Title %in% c('Mme', 'Mr', 'Mrs')] <- 'Married'
#combi$Title[combi$Title %in% c('Capt', 'Col', 'Don', 'Dr', 'Jonkheer', 'Major', 'Sir',   'Dona', 'Lady', 'the Countess')] <- 'High'

# so keep using the original one...
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

# fill in ages by making use of age fitting via rpart
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

# fill in other empty columns
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# create a more reliable familyID
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

#Now cabin number. For now we will use only deck
combi$Cabin <- as.character(combi$Cabin)
combi$Deck <- "U"
combi$Deck <- substring(combi$Cabin, 1, 1)
combi$Deck[combi$Deck==""] <- "U"
combi$Deck <- factor(combi$Deck)
combi$Cabin <- factor(combi$Cabin)
Deckfit <- rpart(Deck ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[(combi$Deck!="U"),], method="anova")
fancyRpartPlot(Deckfit)
combi$Deck[(combi$Deck=="U")] <- predict(Deckfit, combi[(combi$Deck=="U"),])

# ticket price is a total - use ticketID, count # of tickets per ticketID and divide price by #oftickets
temp <- table(combi$Ticket)
combi <- merge(combi, temp, by.x='Ticket', by.y='Var1')
combi$Tprice <- combi$Fare/combi$Freq
# sort on PassengerID again
combi <- combi[order(combi$PassengerId),]

#now fit the data!
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Tprice + Embarked + Title + FamilySize + FamilyID + Deck,
               data = combi[1:891,], controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, combi[892:1309,], OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "NN.csv", row.names = FALSE)
