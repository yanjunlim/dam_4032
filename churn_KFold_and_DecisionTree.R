library(stringi)

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

#---churndata dataset is cleaned up.


#---Swaping Yes -> 1, No -> 0, ~Service -> -1
#---  In: Partner, Dependents, PhoneService, MultipleLines, OnlineSecurity, OnlineBackup, DeviceProtection, 
#---      TechSupport, StreamingTV, StreamingMovies, PaperlessBilling, Churn
for (c in 1:ncol(churndata_new)){
  if ("Yes" %in% churndata_new[,c]){
    churndata_new[,c] <- as.character(churndata_new[,c])
    churndata_new[,c][which(stri_cmp_eq(churndata_new[,c],"Yes"))]  <- 1
    churndata_new[,c][which(stri_cmp_eq(churndata_new[,c],"No"))]   <- 0
    churndata_new[,c][which(stri_detect_fixed(churndata_new[,c],"service"))]<- 0
    churndata_new[,c] <- as.integer(churndata_new[,c])
  }
}


#churndata <- churndata_new
#remove(churndata_new)


#lib for sampling function
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
set.seed(123)

#--Option 1 3/4 Sample split
#--sample size s at 75%
#s<- floor(0.75* nrow(churndata))

#train_index <- sample(seq_len(nrow(churndata)), size = s)

#--Training & Testing models
#churn_train<-churndata[train_index,]
#churn_test<- churndata[-train_index,]
#--end of option 1


#--option 2 K Fold CV
churndata1<-churndata
#--Start test
churndata1<-churndata1[sample(nrow(churndata1)),]
#--Create 10 equally size folds
folds <- cut(seq(1,nrow(churndata1)),breaks=10,labels=FALSE)
#--Perform 10 fold cross validation
for(i in 1:10){
  #--Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- churndata1[testIndexes, ]
  trainData <- churndata1[-testIndexes, ]
  #--Use the test and train data partitions
}




#--Decision Tree using regression part

#-- grow tree with all data(not all category are usable)
#tree <- rpart(Churn~., churn_train, method="class")

#--grow tree with Payment category
tree<- rpart(Churn~ PaymentMethod + MonthlyCharges +TotalCharges, trainData, method="class")

#--grow tree with Services category
#tree<- rpart(Churn~ InternetService + Contract + Tenure, churn_train, method="class")


#tree<- rpart(Churn~ InternetService + Contract + Tenure + PaymentMethod + MonthlyCharges +TotalCharges, churn_train, method="class")

#--result of rpart plot
printcp(tree) #--display the results 
plotcp(tree) #--visualize cross-validation results 
summary(tree) #--detailed summary of splits
rpart.plot(tree, type=4, extra = 101)#--Detailed plot


#--Run Prediction against test data
p<-predict(tree, testData, type = "class")
table(testData[,21], p)
