splitName <- function(dataSet){
 
  dataSet$Title <- gsub('(.*, )|(\\..*)', '', dataSet$Name)
  
  spTitle <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                  'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
  
  
  dataSet$Title[dataSet$Title == 'Mlle']        <- 'Miss' 
  dataSet$Title[dataSet$Title == 'Ms']          <- 'Miss'
  dataSet$Title[dataSet$Title == 'Mme']         <- 'Mrs' 
  dataSet$Title[dataSet$Title %in% spTitle]  <- 'Special Title'
  dataSet$Title <- factor(dataSet$Title)

  dataSet$Surname <- sapply(dataSet$Name,  
                         function(x) strsplit(x, split = '[,.]')[[1]][1])
  return(dataSet)
}


groupFamily <- function(dataSet){

  dataSet$Fsize <- dataSet$SibSp + dataSet$Parch + 1

  dataSet$Family <- paste(dataSet$Surname, dataSet$Fsize, sep='_')
  
  dataSet$FsizeD[dataSet$Fsize == 1] <- 'Alone'
  dataSet$FsizeD[dataSet$Fsize > 1] <- 'Family'
  
  return(dataSet)
}

fillMissing <- function(dataSet){

  factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                   'Title','Surname','Family','Fsize')
  
  dataSet[factor_vars] <- lapply(dataSet[factor_vars], function(x) as.factor(x))
  
  set.seed(129)
  mice_mod <- mice(dataSet[, !names(dataSet) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 
  mice_output <- complete(mice_mod)
  
  dataSet$Age <- mice_output$Age
  
  # Discretize age size
  dataSet$AgeD[dataSet$Age >= 0 & dataSet$Age < 18] <- '0-18'
  dataSet$AgeD[dataSet$Age >= 18 & dataSet$Age < 30] <- '18-30'
  dataSet$AgeD[dataSet$Age >= 30 & dataSet$Age < 45] <- '30-45'
  dataSet$AgeD[dataSet$Age >= 45 & dataSet$Age < 65] <- '45-65'
  dataSet$AgeD[dataSet$Age >= 65] <- '65-up'
  
  dataSet$Fare[is.na(dataSet$Fare)] <- median(dataSet$Fare, na.rm=TRUE)
  
  dataSet$Embarked[dataSet$Embarked==""] <- "S"
  
  return(dataSet)
}


person <- function(dataSet){
  dataSet$Person <- 'Male'
  dataSet$Person[dataSet$Age < 18] <- 'Child'
  dataSet$Person[dataSet$Age >= 18 & dataSet$Sex == "female"] <- 'Female'
  dataSet$Person <- factor(dataSet$Person)
  return(dataSet)
}


