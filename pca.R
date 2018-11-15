library(stringi)
library(dummies)
library(DMwR)
library(ggfortify)

k<-5
accuracy<-c()


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

new_my_data <- dummy.data.frame(churndata_new, names = c("Gender", "Partner","Dependents","PhoneService",
                                                         "MultipleLines","InternetService","OnlineSecurity",
                                                         "OnlineBackup","DeviceProtection","TechSupport","StreamingTV",
                                                         "StreamingMovies","Contract","PaperlessBilling","PaymentMethod"))


summary(new_my_data)

i<-1
for(i in 1:k){

  #-Takes a random 70% of data
  sample <- sample(nrow(new_my_data), 0.7*nrow(new_my_data), replace = FALSE)
  train <- new_my_data[sample,]      # Store the 70% data in train
  test <- new_my_data[-sample,]
  
  #SMOTE
  train <- SMOTE(Churn~.,k=5,train,perc.over=200,perc.under=150)
  train$Churn<-ifelse(train$Churn=="Yes",1,0)

  #compute PCs
  prin_comp <- prcomp(train[,c(2:47)], scale. = T)
  names(prin_comp)
  prin_comp$rotation[1:16,1:16]
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

  train$Churn<-ifelse(train$Churn==1,"Yes","No")
  train.data <- data.frame(Churn = as.factor(train$Churn), prin_comp$x)
  train.data <- train.data[,1:47]

  #run a decision tree
  library(rpart)
  rpart.model <- rpart(Churn ~ .,data = train.data, method = "class")
  rpart.model

  test$Churn<-ifelse(test$Churn=="Yes",1,0)
  #transform test into PCA
  test.data <- predict(prin_comp, newdata = test)
  test.data <- as.data.frame(test.data)

  #select the first 30 components
  test.data <- test.data[,1:46]

  #make prediction on test data
  rpart.prediction <- predict(rpart.model, test.data)

  final.sub <- data.frame(Churn = rpart.prediction)
  pca.class<-ifelse(rpart.prediction<0.5,'No','Yes')
  
  test$Churn<-ifelse(test$Churn==1,"Yes","No")
  results<-data.frame(actual=test$Churn,prediction=pca.class)
  attach(results)
  count=0
  for(h in 1:nrow(test))
  {
    if(actual[h]==prediction.Yes[h])
    {
      count<-count+1
    }
  }
  #accuracy of the model against actual data
  accuracy[i] = count/nrow(test)

}

print(paste('Mean Actual Accuracy: ',mean(accuracy)))