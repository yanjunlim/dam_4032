library(stringi)
library(nnet)
library(boot)
library(plyr)
library(ROCR)

#-Load the "churn" dataset
churndata <- read.csv("Telco-Customer-Churn.csv", header = TRUE)
str(churndata)
dim(churndata) # 7043   21
summary(churndata)
#--Observed : 11 NA's in TotalCharges

#-Handling Missing Data
churndata_new <- churndata

#--Replacing Rows with NA's with the Median
churndata_new$TotalCharges[which(is.na(churndata_new$TotalCharges))] <- # Set : Rows in the Missing (Those that have NA)
  median(churndata_new$TotalCharges, na.rm=TRUE)                        # From: Middle "Value" of the Rating
dim(churndata_new)      #No rows removed
summary(churndata_new)  #NA's is removed from TotalCharges

churndata_new$CustomerID <- NULL
testdata <- churndata_new

for (t in 1:ncol(testdata)){
  if (is.factor(testdata[,t])){
    lev = levels(testdata[,t])
    testdata[,t] <- as.character(testdata[,t])
    for(l in 1:length(lev)){
      if (lev[l] == "Yes" & l != 2){
        testdata[,t][which(testdata[,t] == "1")] <- l-1
        testdata[,t][which(testdata[,t] == "Yes")] <- 1
      }else{
        testdata[,t][which(testdata[,t] == lev[l])] <- l-1
      }
    }
    testdata[,t] <- as.integer(testdata[,t])
  }
}
churndata_new<-testdata
churndata_new$Churn<-as.factor(churndata_new$Churn)
accuracy<-c()
area<-c()

k<-10
pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k)
{
  index <- sample(1:nrow(churndata_new),round(0.7*nrow(churndata_new)))
  train <- churndata_new[index,]
  test <- churndata_new[-index,]
  
  #using a single layer neural network, with the nodes = 2/3 of inputs
  model <- nnet(Churn~.-TotalCharges-Gender-Partner-PhoneService-PaymentMethod-Dependents-StreamingTV-StreamingMovies-DeviceProtection, data=train,size=6,maxit=1e7,decay=.001)
  p2<-predict(model,newdata=test,type="class")
  #actual vs predicted
  table(test$Churn,p2)
  results<-data.frame(actual=test$Churn,prediction=p2)
  attach(results)
  count=0
  for(h in 1:nrow(test))
  {
    if(actual[h]==prediction[h])
    {
      count<-count+1
    }
  }
  #accuracy of the model against actual data
  accuracy[i] = count/nrow(test)
  
  #calculating true-positive vs false-positive
  p1 <- predict(model,newdata=test,type="raw")
  pred <- prediction(p1,test$Churn)
  perf = performance(pred,"tpr","fpr")
  plot(perf,lwd=2,col="blue",main="ROC-Neural Network on Churning")
  
  #getting Area Under Curve
  auc<-performance(pred,measure="auc")
  auc<-auc@y.values[[1]]
  area[i]<-auc
  
  pbar$step()
}

print(paste('Mean Actual Accuracy: ',mean(accuracy)))
print(paste('Mean Area Under Curve: ',mean(area)))