# set path
# can I use github here?
setwd("H:/R/Titanic")
# import training and test data
# strings are read as Factors, else use:
# train <- read.csv("H:/R/Titanic/train.csv",stringsAsFactors=FALSE
train <- read.csv("H:/R/Titanic/train.csv")
View(train)
test <- read.csv("H:/R/Titanic/test.csv")
View(test)
# asume nobody survived in test-set
test$Survived <- rep(0,418)
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)
View(submit)
# now commit this small change