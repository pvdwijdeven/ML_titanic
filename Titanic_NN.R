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
if(!require('mcclust')){
  install.packages('mcclust')
  library(mcclust)
}
if(!require('dplyr')){
  install.packages('dplyr')
  library(dplyr)
}
if(!require('tidyr')){
  install.packages('tidyr')
  library(tidyr)
}
if(!require('neuralnet')){
  install.packages('neuralnet')
  library(neuralnet)
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
combi$Deck <- NA
combi$Deck <- as.numeric(combi$Deck)
combi$Deck[substring(combi$Cabin, 1, 1) %in% c("A","T")] <- 1
combi$Deck[substring(combi$Cabin, 1, 1)=="B"] <- 2
combi$Deck[substring(combi$Cabin, 1, 1)=="C"] <- 3
combi$Deck[substring(combi$Cabin, 1, 1)=="D"] <- 4
combi$Deck[substring(combi$Cabin, 1, 1)=="E"] <- 5
combi$Deck[substring(combi$Cabin, 1, 1)=="F"] <- 6
combi$Deck[substring(combi$Cabin, 1, 1)=="G"] <- 7
#Deck = matrix(ncol=9,nrow=1309)
Deckfit <- rpart(Deck ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Deck),], method="anova")
combi$Deck[is.na(combi$Deck)] <- predict(Deckfit, combi[is.na(combi$Deck),])
fancyRpartPlot(Deckfit)
combi$Cabin <- factor(combi$Cabin)

# ticket price is a total - use ticketID, count # of tickets per ticketID and divide price by #oftickets
temp <- table(combi$Ticket)
combi <- merge(combi, temp, by.x='Ticket', by.y='Var1')
combi$Tprice <- combi$Fare/combi$Freq
# sort on PassengerID again
combi <- combi[order(combi$PassengerId),]

set.seed(415)
#fit <- cforest(as.factor(Survived) ~ Deck + Pclass + Sex + Age + SibSp + Parch + Tprice + Embarked + Title + FamilySize + FamilyID,
#               data = combi[1:891,], controls=cforest_unbiased(ntree=2000, mtry=3))
#Prediction <- predict(fit, combi[892:1309,], OOB=TRUE, type = "response")
#submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
#write.csv(submit, file = "partyforest.csv", row.names = FALSE)

#now fit the data with neural network
# make name length a feature
tr1<-combi[1:891,] %>%
  mutate(lname=sapply(as.character(Name),nchar)) %>%
  dplyr::select(-Ticket,-Cabin,-Name,-Fare,-Freq,-FamilyID,-FamilyID2,-Surname,-Title)

bins<-tr1 %>% # piped functions follow
  
  # make it narrow, don't touch numeric variables and IDs
  gather(catnames,catvalues,-PassengerId,-Survived,-SibSp,
         -Parch,-Tprice, -Age,-lname,-Deck,-FamilySize) %>%
  
  # make single column out of them
  unite(newfactor,catnames,catvalues,sep=".") %>%
  
  # add a new column - it's "1" for every record
  mutate( is = 1) %>%
  
  # create a column from each factor, and where there's no record, add "0"
  spread(newfactor, is, fill = 0)

seed<-2
##prepare list for neuralnet() call
#cat(paste0(names(bins),sep="+"))
bins.nn<-function(df,rep=1,hidden=c(1),threshold=0.1) {
  set.seed(seed)
  nn.obj<-neuralnet(Survived ~ SibSp+ Parch+Tprice+ Age+ lname+ Deck+ FamilySize+ Embarked.C+ Embarked.Q+ Embarked.S+ Pclass.1+ Pclass.2+ Pclass.3+ Sex.female+ Sex.male,
                    data=df,
                    hidden=hidden,
                    lifesign="full",
                    lifesign.step=2000,
                    threshold=threshold,
                    rep=rep)
  return(nn.obj)}

# clean up results from NAs and 2s
cleanup<-function(vect){
  sapply(vect,function(x){
    if(is.na(x)) 0
    else if(x>0) 1
    else 0})}

qualify<-function(real,guess){
  check<-table(real,guess)
  if(sum(dim(check)==c(2,2))!=2) percentage<-0
  else{
    good.ones<-check[1,1]+check[2,2]
    bad.ones<-check[1,2]+check[2,1]
    #paste0(as.character()),'%')
    percentage<-round(100*good.ones/(good.ones+bad.ones))
  }#if(is.na(percentage)) percentage<-0
  return(percentage)
}

# In internal tests, split train set to train/test part and check how
# the selected algorithm works
## BEGIN internal tests
if(FALSE){
  
  
  #nr<-dim(bins)[1] # number of observations
  #share<-0.8 # this is our 80% parameter
  #set.seed(seed)
  #trainset<-sample.int(nr,round(share*nr))
  
  #trainers<-bins[trainset,]
  #testers<-bins[-trainset,]
  trainers<-bins
  
  nfeat<-dim(bins)[2] 
  
  #n.testers<-bins.nn(trainers,rep=3,hidden=c(4),threshold=0.25)
  
  #res.testers<-neuralnet::compute(n.testers,testers[,3:nfeat])
  #qualify(cleanup(round(res.testers$net.result)),testers$Survived)
  
  mult<-list()
  eff<-list()
  tries=50
  for(i in 1:tries){
    cat("Iteration #",i,"/",tries,"\n")
    set.seed(i)
    r<- 3#as.integer(runif(1,5,10))
    h<- 5#as.integer(runif(1,5,10))
    t<- 0.2#as.integer(runif(1,5,10))
    nr1<-dim(trainers)[1]
    ttrainset<-sample.int(nr1,round(0.9*nr1))
    ttrainers<-trainers[ttrainset,]
    ttesters<-trainers[-ttrainset,]
    mult[[i]]<-bins.nn(ttrainers,rep=r,hidden=h,threshold=t)
    
    res<-neuralnet::compute(mult[[i]],ttesters[,3:nfeat])
    eff[[i]]<-qualify(cleanup(round(res$net.result)),
                      ttesters$Survived)
    print(eff[[i]])
  }
  
  pult<-matrix(NA, nrow=dim(bins.test)[1])
  alltries<-1:tries
  mineff<-mean(unlist(eff))#85##########################
  goodtries<-alltries[unlist(eff)>40]
  for(i  in goodtries){
    res<-neuralnet::compute(mult[[i]],bins.test[,2:nfeat.test])#testers[,3:nfeat])
    pult<-cbind(pult,cleanup(round(res$net.result)))                           
  }
  pult<-dplyr::select(as.data.frame(pult),-V1) # drop NA column
  predi<-rowSums(pult)
  cu<-mean(predi[predi!=0]) ###############
  #cu<-0.5*max(predi)
  ppredi<-sapply(predi,function(x)if(x>cu) 1 else 0)
  #qualify(ppredi,testers$Survived)
}
## END internal tests

# Train with full train set
#n.full<-bins.nn(bins,rep=5,hidden=c(4),threshold=0.25)

##### Now test data



ts1<-combi[892:1309,] %>%
  mutate(lname=sapply(as.character(Name),nchar)) %>%

  dplyr::select(-Ticket,-Cabin,-Name,-Fare,-Freq,-FamilyID,-FamilyID2,-Surname,-Title)
bins.test<-ts1 %>% # piped functions follow
  
  # make it narrow, don't touch numeric variables and IDs
  gather(catnames,catvalues,-PassengerId,-Survived,-SibSp,
         -Parch,-Tprice, -Age,-lname,-Deck,-FamilySize) %>%
  
  # make single column out of them
  unite(newfactor,catnames,catvalues,sep=".") %>%
  
  # add a new column - it's "1" for every record
  mutate( is = 1) %>%
  
  # create a column from each factor, and where there's no record, add "0"
  spread(newfactor, is, fill = 0)

nfeat.test<-dim(bins.test)[2] 

#res<-neuralnet::compute(n.full,bins.test[,2:nfeat.test])
#upload<-cleanup(round(res$net.result))

trainers<-bins

nfeat<-dim(bins)[2] 

#n.testers<-bins.nn(trainers,rep=3,hidden=c(4),threshold=0.25)

#res.testers<-neuralnet::compute(n.testers,testers[,3:nfeat])
#qualify(cleanup(round(res.testers$net.result)),testers$Survived)

mult<-list()
eff<-list()
tries=200
for(i in 1:tries){
  cat("Iteration #",i,"/",tries,"\n")
  set.seed(i)
  r<- 3#as.integer(runif(1,5,10))
  h<- 5#as.integer(runif(1,5,10))
  t<- 0.25#as.integer(runif(1,5,10))
  nr1<-dim(trainers)[1]
  ttrainset<-sample.int(nr1,round(0.9*nr1))
  ttrainers<-trainers[ttrainset,]
  ttesters<-trainers[-ttrainset,]
  mult[[i]]<-bins.nn(ttrainers,rep=r,hidden=h,threshold=t)
  
  res<-neuralnet::compute(mult[[i]],ttesters[,3:nfeat])
  eff[[i]]<-qualify(cleanup(round(res$net.result)),
                    ttesters$Survived)
  print(eff[[i]])
}

pult<-matrix(NA, nrow=dim(bins.test)[1])
alltries<-1:tries
mineff<-90##########################
goodtries<-alltries[unlist(eff)>mineff]
for(i  in goodtries){
  res<-neuralnet::compute(mult[[i]],bins.test[,2:nfeat.test])#testers[,3:nfeat])
  pult<-cbind(pult,cleanup(round(res$net.result)))                           
}
pult<-dplyr::select(as.data.frame(pult),-V1) # drop NA column
predi<-rowSums(pult)
cu<-mean(predi[predi!=0]) ###############
#cu<-0.5*max(predi)
ppredi<-sapply(predi,function(x)if(x>cu) 1 else 0)

upload<-ppredi
names(upload)<-c("Survived")
upload1<-data.frame(cbind(bins.test$PassengerId,upload))
names(upload1)<-c("PassengerId","Survived")
write.csv(upload1,file="res.csv",row.names=FALSE,quote=FALSE)