library(randomForest)

prediction = function(){
  

load("rf_model.rda")
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



test= test[c(-7,-14)]
levels(test$EMPLOYER_COUNTRY) = c("AUSTRALIA","CANADA","CHINA","UNITED STATES OF AMERICA")
levels(test$FULL_TIME_POSITION) = c("?", "N", "Y")

results = predict(model_rf, test, type="class")

df <- read.csv("D:/practice/zs/7e2ae14e-8-dataset/dataset/test.csv")
df <- cbind(df,results)
df <- data.frame(df$CASE_NO, df$results)
colnames(df) = c("CASE_NO","CASE_STATUS")
return(df)

}

res = prediction()
