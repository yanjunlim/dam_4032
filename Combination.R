library(DMwR)
library(plyr)
library(gbm)
library(caret)
library(randomForest)
library(tree)
library(stringi)
library(plotly)
library(ggplot2)
library(RColorBrewer)
library(ISLR)
library(dummies)
library(ggfortify)
library(rpart)
library(tseries)
library(usdm)
library(olsrr)
library(pROC)
library(ROCR)
library(ROSE)
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


new_my_data <- dummy.data.frame(churndata_new, names = c("Gender", "Partner","Dependents","PhoneService",
                                                         "MultipleLines","InternetService","OnlineSecurity",
                                                         "OnlineBackup","DeviceProtection","TechSupport","StreamingTV",
                                                         "StreamingMovies","Contract","PaperlessBilling","PaymentMethod"))

summary(new_my_data)

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

#Based on percentage, the difference of churning and not churning is 5% hence we remove the 3 columns
cnew$Gender<-NULL
cnew$PhoneService<-NULL
cnew$MultipleLines<-NULL



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


area<-c()
accuracyrf<-c()
accuracypca<-c()
areaundercurve<-c()
misClasificError<-c()
a1<-c()

k<-10
pbar <- create_progress_bar('text')
pbar$init(k)
#i<-1
for(i in 1:k){
  
  #split data 70-30
  sample <- sample(nrow(cnew), 0.7*nrow(cnew), replace=FALSE)
  train <- cnew[sample,]      # Store the 70% data in train
  test <- cnew[-sample,]     # Store the remaining 30% data in valid
  
  #No:3624 Yes:1306
  train <- SMOTE(Churn~.,k=5,train,perc.over=200,perc.under=150)
  #================= Gradient Boosting with Binary Logistic Rgression =======================
  x1<-train$Gender*train$Partner*train$Dependents*train$PhoneService*train$DeviceProtection*train$StreamingMovies*train$StreamingTV*train$PaymentMethod
  fitcontrol<-trainControl(method="repeatedcv",number = 4,repeats = 4)
  
  #using Gradient Boosting model to strengthen the model
  gbm1<-train(Churn~.-Partner-Dependents-DeviceProtection-StreamingMovies-StreamingTV-PaymentMethod,data=train,method="gbm",trControl=fitcontrol,verbose=FALSE)
  gbmtest<-predict(gbm1,test,type="prob")[,2]
  test$Churn<-ifelse(test$Churn=="Yes",1,0) #classify Yes as 1, No as 0
  pr1<-prediction(gbmtest,test$Churn) #test predicted model against the actual data
  prf1<-performance(pr1,measure="tpr",x.measure="fpr")
  plot(prf1,lwd=2,col="blue", main="Binary Logistic Regression with GBM")
  a1[i]<-auc(test$Churn,gbmtest) #accuracy of Gradient Boosting Model
  
  #=============================== Neural Network =============================================
  #using a single layer neural network, with the nodes = 2/3 of inputs
  nmodel <- nnet(Churn~.-Partner-Dependents-DeviceProtection-StreamingMovies-StreamingTV-PaymentMethod, data=train,size=6,maxit=1e7,decay=.001)
  
  #calculating true-positive vs false-positive
  p2 <- predict(nmodel,newdata=test,type="raw")
  pred <- prediction(p2,test$Churn)
  perf = performance(pred,"tpr","fpr")
  plot(perf,lwd=2,col="blue",main="ROC-Neural Network on Churning")
  
  #getting Area Under Curve
  auc<-performance(pred,measure="auc")
  auc<-auc@y.values[[1]]
  area[i]<-auc
  
  #====================== Binary Logistic Regression (Normal) =================================
  #doing logistic linear regression
  model<-glm(Churn~.,family = binomial(link='logit'),data=train)
  summary(model)
  
  #based on prediction, test on actual
  p<-predict(model,newdata=test,type="response")
  pr<-prediction(p,test$Churn)
  #plot true-positive to false-positive
  prf<-performance(pr,measure="tpr",x.measure="fpr")
  plot(prf,lwd=2,col="blue", main="Binary Logistic Regression")
  
  #finding Area Under Curve
  auc <- performance(pr, measure="auc")
  auc <- auc@y.values[[1]]
  #AUC (best) = 1, AUC(coin toss)=0.5
  areaundercurve[i]<-auc
  
  #=============================== Random Forest ======================================================
  train$Churn<-ifelse(train$Churn=="Yes",1,0)
  #training model for Random
  gbm.lfp<-gbm(Churn~., distribution = 'bernoulli',data=train,n.trees = 400,interaction.depth = 10,shrinkage=.01,n.minobsinnode = 5)
  test$Churn<-ifelse(test$Churn==1,"Yes","No")
  #testing model against actual data
  gbm.lfp.test<-predict(gbm.lfp,newdata = test,type = 'response', n.trees = 400)
  gbm.class<-ifelse(gbm.lfp.test<0.5,'No','Yes')
  
  results<-data.frame(actual=as.factor(test$Churn),prediction=gbm.class)
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
  accuracyrf[i] = count/nrow(test)
  
  #========================= Principle Component Analysis ====================================
  #-Takes a random 70% of data
  sample <- sample(nrow(new_my_data), 0.7*nrow(new_my_data), replace = FALSE)
  trainpca <- new_my_data[sample,]      # Store the 70% data in train
  testpca <- new_my_data[-sample,]
  
  #SMOTE
  trainpca <- SMOTE(Churn~.,k=5,trainpca,perc.over=200,perc.under=150)
  trainpca$Churn<-ifelse(trainpca$Churn=="Yes",1,0)
  
  #compute PCs
  prin_comp <- prcomp(trainpca[,c(2:47)], scale. = T)
  names(prin_comp)
  prin_comp$rotation[1:46,1:46]
  dim(prin_comp$x)
  biplot(prin_comp, scale = 0)
  std_dev <- prin_comp$sdev
  pr_var <- std_dev^2
  pr_var[1:12]
  prop_varex <- pr_var/sum(pr_var)
  prop_varex[1:20]
  
  #plot variation graphs
  plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")
  
  plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
  
  trainpca$Churn<-ifelse(trainpca$Churn==1,"Yes","No")
  train.data <- data.frame(Churn = as.factor(trainpca$Churn), prin_comp$x)
  train.data <- train.data[,1:21]
  
  #run a decision tree
  rpart.model <- rpart(Churn ~ .,data = train.data, method = "class")
  rpart.model
  
  testpca$Churn<-ifelse(testpca$Churn=="Yes",1,0)
  #transform test into PCA
  test.data <- predict(prin_comp, newdata = testpca)
  test.data <- as.data.frame(test.data)
  
  #select the first 30 components
  test.data <- test.data[,1:21]
  
  #make prediction on test data
  rpart.prediction <- predict(rpart.model, test.data)
  
  final.sub <- data.frame(Churn = rpart.prediction)
  pca.class<-ifelse(rpart.prediction<0.5,'No','Yes')
  
  testpca$Churn<-ifelse(testpca$Churn==1,"Yes","No")
  results<-data.frame(actual=testpca$Churn,prediction=pca.class)
  attach(results)
  count=0
  for(h in 1:nrow(testpca))
  {
    if(actual[h]==prediction.Yes[h])
    {
      count<-count+1
    }
  }
  #accuracy of the model against actual data
  accuracypca[i] = count/nrow(testpca)
  
  
  pbar$step()
}

print(paste('Mean Area Under Curve (Binary Logistic Regression) : ',mean(areaundercurve)))
print(paste('Mean Area Under Curve ( Binary Logistic Regression with GBM ) : ',mean(a1)))
print(paste('Mean Area Under Curve (Neural Network): ',mean(area)))
print(paste('Mean Actual Accuracy (Random Forest): ',mean(accuracyrf)))
print(paste('Mean Actual Accuracy (Principle Component Analysis): ',mean(accuracypca)))