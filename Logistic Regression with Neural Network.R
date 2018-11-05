library(DMwR)
library(tseries)
library(usdm)
library(olsrr)
library(pROC)
library(ROCR)
library(ROSE)
library(stringi)
library(nnet)

#-Load the "churn" dataset
churndata <- read.csv("Telco-Customer-Churn.csv", header = TRUE)
str(churndata)
dim(churndata) # 7043   21
summary(churndata)
#--Observed : 11 NA's in TotalCharges


#-Handling Missing Data
cnew <- churndata

#--Replacing Rows with NA's with the Median
cnew$TotalCharges[which(is.na(cnew$TotalCharges))] <- # Set : Rows in the Missing (Those that have NA)
  median(cnew$TotalCharges, na.rm=TRUE)                        # From: Middle "Value" of the Rating
dim(cnew)      #No rows removed
summary(cnew)  #NA's is removed from TotalCharges

#-Clean of Columns (to Factor)
cnew$A <- "A" #Temporary Column

#--CustomerID
cnew$CustomerID <- NULL #no use

#--TotalCharges
cnew$A <- cnew$MonthlyCharges * cnew$Tenure
cor.test(~ A + TotalCharges, cnew) #0.9992631
cnew$TotalCharges <- NULL
#---TotalCharges is closely related to MonthlyCharges and Tenure, thus it is removed.

#--Senior Citizen
cnew$A[which(cnew$SeniorCitizen == 1)] <- "Yes"
cnew$A[which(cnew$SeniorCitizen == 0)] <- "No"
cnew$SeniorCitizen <- as.factor(cnew$A)

#--Tenure
cnew$A[which(                   cnew$Tenure <= 12)] <- "0~12"
cnew$A[which(cnew$Tenure > 12 & cnew$Tenure <= 24)] <- "12~24"
cnew$A[which(cnew$Tenure > 24 & cnew$Tenure <= 36)] <- "24~36"
cnew$A[which(cnew$Tenure > 36 & cnew$Tenure <= 48)] <- "36~48"
cnew$A[which(cnew$Tenure > 48 & cnew$Tenure <= 60)] <- "48~60"
cnew$A[which(cnew$Tenure > 60                    )] <- "60~"
cnew$Tenure <- as.factor(cnew$A)

#--MonthlyCharges
cnew$A[which(                           cnew$MonthlyCharges <= 20 )] <- "0~20"
cnew$A[which(cnew$MonthlyCharges > 20 & cnew$MonthlyCharges <= 40 )] <- "20~40"
cnew$A[which(cnew$MonthlyCharges > 40 & cnew$MonthlyCharges <= 60 )] <- "40~60"
cnew$A[which(cnew$MonthlyCharges > 60 & cnew$MonthlyCharges <= 80 )] <- "60~80"
cnew$A[which(cnew$MonthlyCharges > 80 & cnew$MonthlyCharges <= 100)] <- "80~100"
cnew$A[which(cnew$MonthlyCharges > 100                            )] <- "100~"
cnew$MonthlyCharges <- as.factor(cnew$A)

cnew$A <- NULL #Remove Temporary Column
str(cnew) #Verify factors

#-Takes a random 70% of data
sample <- sample(nrow(cnew), 0.7*nrow(cnew), replace = FALSE)
train <- cnew[sample,]      # Store the 70% data in train
valid <- cnew[-sample,]     # Store the remaining 30% data in valid

#-Balancing the train data
#-Takes a random 70% of data
#sample <- sample(nrow(churndata_new), 0.7*nrow(churndata_new), replace = FALSE)
#train <- churndata_new[sample,]      # Store the 70% data in train
#valid <- churndata_new[-sample,]     # Store the remaining 30% data in valid


#churndata_new<-testdata
#churndata_new$Churn<-as.factor(churndata_new$Churn)
accuracy<-c()
area<-c()

k<-5
pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k)
{
  index <- sample(1:nrow(cnew),round(0.7*nrow(cnew)))
  train <- cnew[index,]
  test <- cnew[-index,]
  
  #No:3624 Yes:1306
  train <- SMOTE(Churn~.-Gender-Partner-Dependents-OnlineBackup-DeviceProtection-PhoneService-SeniorCitizen-TechSupport,k=5,train,perc.over=400,perc.under=125)
  
  #using a single layer neural network, with the nodes = 2/3 of inputs
  model <- nnet(Churn~.-Gender-Partner-Dependents-OnlineBackup-DeviceProtection-PhoneService-SeniorCitizen-TechSupport, data=train,size=6,maxit=1e7,decay=.001)
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