library(DMwR)
library(plyr)
library(gbm)
library(caret)
library(randomForest)
library(tree)
library(stringi)
library(ISLR)

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

# ================= Classification Tree pruning =================

ltreeFit <- tree(Churn ~ ., data = train, 
                 split = "deviance",
                 method = "recursive.partition",
                 control = tree.control(nobs = nrow(train),
                                        mincut = 10,
                                        minsize = 20,
                                        mindev = 0))
plot(ltreeFit)
cvTree <- cv.tree(ltreeFit, FUN = prune.misclass, K = 10) # K-fold Cross-Validation
cbind(cvTree$size, cvTree$dev)                            # check deviance vs size
plot(cvTree$size, cvTree$dev, type="b",                   # plot deviance vs size
     xlab = "Number of Leaves", ylab = "Deviance")                   

bestSize <- 4     #bias is at the maximum on this point with 753 variance
ptreeFit <- prune.misclass(ltreeFit, best = bestSize)     # prune tree to best size
plot(ptreeFit)
text(ptreeFit, pretty = FALSE)

predTrain <- predict(ptreeFit, train, type = "class")  # prediction on train set
mean(predTrain == train$Churn)                        # classification accuracy
#accuracy = 0.784

# ================= Prediction and Classification =================

# Load and examine the dataset

rf_churn <- as.data.frame(train)
str(rf_churn)

#No = 5174, yes = 1869
table(rf_churn$Churn)


rfFit <- randomForest(Churn ~ .,                      # formula
                      data = rf_churn,                    # data set
                      ntree = 100,                       # number of trees
                      mtry = 4,                          # variables for split
                      importance = TRUE)                 # importance recorded                 
rfFit
#Accuracy = 0.79
varImpPlot(rfFit, type = 1)
#Tenure tops the chart, hence most important.


#====================under sampling ====================

k<-5
accuracy<-c()
pbar <- create_progress_bar('text')
pbar$init(k)
#i<-1
for(i in 1:k){
  #split data 70-30
  sample <- sample(nrow(cnew), 0.7*nrow(cnew), replace=FALSE)
  train <- cnew[sample,]      # Store the 70% data in train
  test <- cnew[-sample,]     # Store the remaining 30% data in valid
  
  #No:3624 Yes:1306
  train <- SMOTE(Churn~.,k=5,train,perc.over=50,perc.under=300)
  
  
  x1<-train$Gender*train$Partner*train$Dependents*train$PhoneService*train$DeviceProtection*train$StreamingMovies*train$StreamingTV*train$PaymentMethod
  fitcontrol<-trainControl(method="repeatedcv",number = 4,repeats = 4)
  
  gbm1<-train(Churn~.-Gender-Partner-Dependents-PhoneService-DeviceProtection-StreamingMovies-StreamingTV-PaymentMethod,data=train,method="gbm",trControl=fitcontrol,verbose=FALSE)
  train$Churn<-ifelse(train$Churn=="Yes",1,0)
  
  #training model
  gbm.lfp<-gbm(Churn~., distribution = 'bernoulli',data=train,n.trees = 100,interaction.depth = 10,shrinkage=.01,n.minobsinnode = 5)
  
  #400, 1, 0.01, 3 => 0.704
  #400, 1, 0.01, 10 => 0.7037
  #100, 10, 0.01, 3 => 0.715
  #100, 10, 0.01, 5 => 0.721
  #100, 10, 0.01, 5 => 0.721
  
  
  gbm.lfp.test<-predict(gbm.lfp,newdata = test,type = 'response', n.trees = 400)
  gbm.class<-ifelse(gbm.lfp.test<0.5,'No','Yes')
  
  results<-data.frame(actual=test$Churn,prediction=gbm.class)
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
  
  
  pbar$step()
}

print(paste('Mean Actual Accuracy: ',mean(accuracy)))



#====================balanced sampling ====================

k<-5
accuracy<-c()
pbar <- create_progress_bar('text')
pbar$init(k)

x<-Churn~.-Gender-Partner-Dependents-PhoneService-DeviceProtection-StreamingMovies-StreamingTV-PaymentMethod
#i<-1
for(i in 1:k){
  #split data 70-30
  sample <- sample(nrow(cnew), 0.7*nrow(cnew), replace=FALSE)
  train <- cnew[sample,]      # Store the 70% data in train
  test <- cnew[-sample,]     # Store the remaining 30% data in valid
  
  #No:3624 Yes:1306
  train <- SMOTE(Churn~.,k=5,train,perc.over=200,perc.under=150)
  
  
  x1<-train$Gender*train$Partner*train$Dependents*train$PhoneService*train$DeviceProtection*train$StreamingMovies*train$StreamingTV*train$PaymentMethod
  fitcontrol<-trainControl(method="repeatedcv",number = 4,repeats = 4)
  
  gbm1<-train(Churn~.-Gender-Partner-Dependents-PhoneService-DeviceProtection-StreamingMovies-StreamingTV-PaymentMethod,data=train,method="gbm",trControl=fitcontrol,verbose=FALSE)
  train$Churn<-ifelse(train$Churn=="Yes",1,0)
  
  #training model
  gbm.lfp<-gbm(Churn~., distribution = 'bernoulli',data=train,n.trees = 400,interaction.depth = 10,shrinkage=.01,n.minobsinnode = 5)
  
  #k=5
  #100, 1, 0.01, 3 => 0.658
  #100, 5, 0.01, 3 => 0.65
  #400, 5, 0.01, 3 => 0.7433
  #400, 5, 0.01, 5 => 0.7515
  #400, 10, 0.01, 5 => 0.777 ***
  #400, 15, 0.01, 5 => 0.769
  #400, 10, 0.01, 10 => 0.763
  #600, 10, 0.01, 10 => 0.767
  #600, 10, 0.01, 5 => 0.768
  #500, 10, 0.01, 3 => 0.763
  #400, 12, 0.01, 5 => 0.776
  #400, 12, 0.01, 7 => 0.769
  #400, 11, 0.01, 5 => 0.765
  #450, 10, 0.01, 5 => 0.757
  #400, 10, 0.005, 5 => 0.744
  
  #k=10
  #400, 10, 0.01, 5 => 0.763
  #600, 10, 0.01, 5 => 0.762
  
  help(gbm)
  gbm.lfp.test<-predict(gbm.lfp,newdata = test,type = 'response', n.trees = 400)
  gbm.class<-ifelse(gbm.lfp.test<0.5,'No','Yes')
  
  results<-data.frame(actual=test$Churn,prediction=gbm.class)
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
  
  
  pbar$step()
}

print(paste('Mean Actual Accuracy: ',mean(accuracy)))



#====================over sampling ====================
k<-5
accuracy<-c()
pbar <- create_progress_bar('text')
pbar$init(k)
#i<-1
for(i in 1:k){
  #split data 70-30
  sample <- sample(nrow(cnew), 0.7*nrow(cnew), replace=FALSE)
  train <- cnew[sample,]      # Store the 70% data in train
  test <- cnew[-sample,]     # Store the remaining 30% data in valid
  
  #No:3624 Yes:1306
  train <- SMOTE(Churn~.,k=5,train,perc.over=400,perc.under=125)
  
  
  
  x1<-train$Gender*train$Partner*train$Dependents*train$PhoneService*train$DeviceProtection*train$StreamingMovies*train$StreamingTV*train$PaymentMethod
  fitcontrol<-trainControl(method="repeatedcv",number = 4,repeats = 4)
  
  gbm1<-train(Churn~.-Gender-Partner-Dependents-PhoneService-DeviceProtection-StreamingMovies-StreamingTV-PaymentMethod,data=train,method="gbm",trControl=fitcontrol,verbose=FALSE)
  train$Churn<-ifelse(train$Churn=="Yes",1,0)

  
  #training model
  gbm.lfp<-gbm(Churn~., distribution = 'bernoulli',data=train,n.trees = 400,interaction.depth = 1,shrinkage=.01,n.minobsinnode = 3)
  
  gbm.lfp.test<-predict(gbm.lfp,newdata = test,type = 'response', n.trees = 400)
  gbm.class<-ifelse(gbm.lfp.test<0.5,'No','Yes')
  
  results<-data.frame(actual=test$Churn,prediction=gbm.class)
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
  
  
  pbar$step()
}

print(paste('Mean Actual Accuracy: ',mean(accuracy)))