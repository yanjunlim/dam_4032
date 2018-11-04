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
#churndata <- churndata_new
#remove(churndata_new)

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

#--Decision Tree using rpart
#--added library
library(rpart)
library(rpart.plot)

#sample size
s<- sample(7043, 5000)
#Training & Testing
churn_training<-churndata_new[s,]
churn_testing<- churndata_new[-s,]



# grow tree with all data(not all category are usable)
#tree <- rpart(Churn~., churndata_new, method="class")

#--grow tree with Payment category
tree<- rpart(Churn~ PaymentMethod + MonthlyCharges +TotalCharges, churndata_new, method="class")

#--grow tree with Services category
#tree<- rpart(Churn~ InternetService + Contract + Tenure, churndata_new, method="class")

#--result of plot
printcp(tree) # display the results 
plotcp(tree) # visualize cross-validation results 
summary(tree) # detailed summary of splits
rpart.plot(tree, type=4, extra = 101)#Detailed plot

#--Run against test data
p<-predict(tree, churn_testing, type = "class")
table(churn_testing[,21], p)
