train <- read.csv("D:/practice/zs/7e2ae14e-8-dataset/dataset/train.csv")
train$CASE_SUBMITTED_DAY = as.Date(paste(train$CASE_SUBMITTED_DAY,"-",train$CASE_SUBMITTED_MONTH,"-",train$CASE_SUBMITTED_YEAR, sep=""), format = "%d-%m-%Y")
train$DECISION_DAY = as.Date(paste(train$DECISION_DAY,"-",train$DECISION_MONTH,"-",train$DECISION_YEAR, sep = ""), format = "%d-%m-%Y")

#converting wages to per year
#---------------------------------------------------------------------------
train$sequence <- 1:nrow(train) 

train_new = data.frame()
df=train[train$PW_UNIT_OF_PAY=="Week",]
df$PREVAILING_WAGE=df$PREVAILING_WAGE*52
train_new = rbind(train_new,df)
df=train[train$PW_UNIT_OF_PAY=="Hour",]
df$PREVAILING_WAGE=df$PREVAILING_WAGE*9*(365-102)
train_new = rbind(train_new,df)
df=train[train$PW_UNIT_OF_PAY=="Month",]
df$PREVAILING_WAGE=df$PREVAILING_WAGE*12
train_new = rbind(train_new,df)
df=train[train$PW_UNIT_OF_PAY=="Bi+AC0-Weekly",]
df$PREVAILING_WAGE=df$PREVAILING_WAGE*26
train_new = rbind(train_new,df)
df=train[train$PW_UNIT_OF_PAY=="Year",]
train_new = rbind(train_new,df)
train_updated = train_new[order(train_new$sequence),]

train_new = data.frame()
df=train[train$WAGE_UNIT_OF_PAY=="Week",]
df$WAGE_RATE_OF_PAY_FROM=df$WAGE_RATE_OF_PAY_FROM*52
df$WAGE_RATE_OF_PAY_TO=df$WAGE_RATE_OF_PAY_TO*52
train_new = rbind(train_new,df)
df=train[train$WAGE_UNIT_OF_PAY=="Hour",]
df$WAGE_RATE_OF_PAY_FROM=df$WAGE_RATE_OF_PAY_FROM*9*(365-102)
df$WAGE_RATE_OF_PAY_TO=df$WAGE_RATE_OF_PAY_TO*9*(365-102)
train_new = rbind(train_new,df)
df=train[train$WAGE_UNIT_OF_PAY=="Month",]
df$WAGE_RATE_OF_PAY_FROM=df$WAGE_RATE_OF_PAY_FROM*12
df$WAGE_RATE_OF_PAY_TO=df$WAGE_RATE_OF_PAY_TO*12
train_new = rbind(train_new,df)
df=train[train$WAGE_UNIT_OF_PAY=="Bi+AC0-Weekly",]
df$WAGE_RATE_OF_PAY_FROM=df$WAGE_RATE_OF_PAY_FROM*26
df$WAGE_RATE_OF_PAY_TO=df$WAGE_RATE_OF_PAY_TO*26
train_new = rbind(train_new,df)
df=train[train$WAGE_UNIT_OF_PAY=="Year",]
train_new = rbind(train_new,df)
train = train_new[order(train_new$sequence),]
train$PROCESS_TIME = as.numeric(train$DECISION_DAY - train$CASE_SUBMITTED_DAY)

v=c(-1,-2,-5,-9,-10,-13,-17,-18,-19,-20,-22,-23,-27,-29)
train = train[,v]

#--------------------------------------------------------------------------------------

train = train[complete.cases(train),]

#Add a factor a others to unnecessary data
#--------------------------------------------------------------------------------------
removeBlankLevelsInDataFrame <- function(dataframe) {
  for (i in 1:ncol(dataframe)) {
    levels <- levels(dataframe[, i])
    if (!is.null(levels) && levels[1] == "") {
      levels(dataframe[,i])[1] = "?"
    }
  }
  dataframe
}

removeBlankLevelsInVector <- function(vector) {
  levels <- levels(vector)
  if (!is.null(levels) && levels[1] == "") {
    levels(vector)[1] = "?"
  }
  vector
}

train = removeBlankLevelsInDataFrame(train)


set.seed(123)

train = train[c(-7,-14)]
train$CASE_SUBMITTED_MONTH = as.factor(as.character(train$CASE_SUBMITTED_MONTH))
train$CASE_SUBMITTED_YEAR = as.factor(as.character(train$CASE_SUBMITTED_YEAR))
train$DECISION_MONTH = as.factor(as.character(train$DECISION_MONTH))
train$DECISION_YEAR = as.factor(as.character(train$DECISION_YEAR))
#library(C50)
#model_decTree = C50::C5.0(train[-15], train$CASE_STATUS, trails = 1)
#------------------------------------------------------------------------------------
#build model
library(randomForest)
model_rf = randomForest(CASE_STATUS ~ ., data = train)
save(model_rf,file = "D:/practice/zs/rf_model.rda")

#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#prediction, preparing test data

test <- read.csv("D:/practice/zs/7e2ae14e-8-dataset/dataset/test.csv")
test$CASE_SUBMITTED_DAY = as.Date(paste(test$CASE_SUBMITTED_DAY,"-",test$CASE_SUBMITTED_MONTH,"-",test$CASE_SUBMITTED_YEAR, sep=""), format = "%d-%m-%Y")
test$DECISION_DAY = as.Date(paste(test$DECISION_DAY,"-",test$DECISION_MONTH,"-",test$DECISION_YEAR, sep = ""), format = "%d-%m-%Y")

test$sequence <- 1:nrow(test) 

test_new = data.frame()
df=test[test$PW_UNIT_OF_PAY=="Week",]
df$PREVAILING_WAGE=df$PREVAILING_WAGE*52
test_new = rbind(test_new,df)
df=test[test$PW_UNIT_OF_PAY=="Hour",]
df$PREVAILING_WAGE=df$PREVAILING_WAGE*9*(365-102)
test_new = rbind(test_new,df)
df=test[test$PW_UNIT_OF_PAY=="Month",]
df$PREVAILING_WAGE=df$PREVAILING_WAGE*12
test_new = rbind(test_new,df)
df=test[test$PW_UNIT_OF_PAY=="Bi+AC0-Weekly",]
df$PREVAILING_WAGE=df$PREVAILING_WAGE*26
test_new = rbind(test_new,df)
df=test[test$PW_UNIT_OF_PAY=="Year",]
test_new = rbind(test_new,df)
test_updated = test_new[order(test_new$sequence),]

test_new = data.frame()
df=test[test$WAGE_UNIT_OF_PAY=="Week",]
df$WAGE_RATE_OF_PAY_FROM=df$WAGE_RATE_OF_PAY_FROM*52
df$WAGE_RATE_OF_PAY_TO=df$WAGE_RATE_OF_PAY_TO*52
test_new = rbind(test_new,df)
df=test[test$WAGE_UNIT_OF_PAY=="Hour",]
df$WAGE_RATE_OF_PAY_FROM=df$WAGE_RATE_OF_PAY_FROM*9*(365-102)
df$WAGE_RATE_OF_PAY_TO=df$WAGE_RATE_OF_PAY_TO*9*(365-102)
test_new = rbind(test_new,df)
df=test[test$WAGE_UNIT_OF_PAY=="Month",]
df$WAGE_RATE_OF_PAY_FROM=df$WAGE_RATE_OF_PAY_FROM*12
df$WAGE_RATE_OF_PAY_TO=df$WAGE_RATE_OF_PAY_TO*12
test_new = rbind(test_new,df)
df=test[test$WAGE_UNIT_OF_PAY=="Bi+AC0-Weekly",]
df$WAGE_RATE_OF_PAY_FROM=df$WAGE_RATE_OF_PAY_FROM*26
df$WAGE_RATE_OF_PAY_TO=df$WAGE_RATE_OF_PAY_TO*26
test_new = rbind(test_new,df)
df=test[test$WAGE_UNIT_OF_PAY=="Year",]
test_new = rbind(test_new,df)
test = test_new[order(test_new$sequence),]
test$PROCESS_TIME = as.numeric(test$DECISION_DAY - test$CASE_SUBMITTED_DAY)

v=c(3,4,6,7,8,11,12,14,15,16,21,24,25,26,29)
test = test[,v]
#test$DECISION_MONTH = as.factor(as.character(test$DECISION_MONTH))
#--------------------------------------------------------------------------------------



#--------------------------------------------------------------------------------------
removeBlankLevelsInDataFrame <- function(dataframe) {
  for (i in 1:ncol(dataframe)) {
    levels <- levels(dataframe[, i])
    if (!is.null(levels) && levels[1] == "") {
      levels(dataframe[,i])[1] = "?"
    }
  }
  dataframe
}

removeBlankLevelsInVector <- function(vector) {
  levels <- levels(vector)
  if (!is.null(levels) && levels[1] == "") {
    levels(vector)[1] = "?"
  }
  vector
}

test = removeBlankLevelsInDataFrame(test)
test$CASE_SUBMITTED_MONTH = as.factor(as.character(test$CASE_SUBMITTED_MONTH))
levels(test$CASE_SUBMITTED_MONTH) = levels(train$CASE_SUBMITTED_MONTH)
test$CASE_SUBMITTED_YEAR = as.factor(as.character(test$CASE_SUBMITTED_YEAR))
levels(test$CASE_SUBMITTED_YEAR) = levels(train$CASE_SUBMITTED_YEAR )
test$DECISION_MONTH = as.factor(as.character(test$DECISION_MONTH))
levels(test$DECISION_MONTH) = levels(train$DECISION_MONTH)
test$DECISION_YEAR = as.factor(as.character(test$DECISION_YEAR))
levels(test$DECISION_YEAR) = levels(train$DECISION_YEAR)

test= test[c(-7,-14)]
levels(test$EMPLOYER_COUNTRY) = levels(train$EMPLOYER_COUNTRY)
levels(test$FULL_TIME_POSITION) = levels(train$FULL_TIME_POSITION)

results = predict(model_rf, test, type="class")

df <- read.csv("D:/practice/zs/7e2ae14e-8-dataset/dataset/test.csv")
df <- cbind(df,results)
df <- data.frame(df$CASE_NO, df$results)
colnames(df) = c("CASE_NO","CASE_STATUS")
