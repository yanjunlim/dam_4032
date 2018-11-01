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
    churndata_new[,c][which(stri_detect_fixed(churndata_new[,c],"service"))]<- -1
    churndata_new[,c] <- as.integer(churndata_new[,c])
  }
}

library(ggplot2)
#Internet Service
ggplot(churndata_new, aes(x=MonthlyCharges, y=Tenure, color=InternetService, shape=InternetService)) + geom_point()
#Multiple lines
ggplot(churndata_new, aes(x=MonthlyCharges, y=Tenure, color=MultipleLines)) + geom_point()


#kmeans clustering
churndata_new$CustomerID<- as.numeric (churndata_new$CustomerID)
churndata_new$Gender<- as.numeric (churndata_new$Gender)
churndata_new$SeniorCitizen<- as.numeric (churndata_new$SeniorCitizen)
churndata_new$Partner<- as.numeric (churndata_new$Partner)
churndata_new$Dependents<- as.numeric (churndata_new$Dependents)
churndata_new$Tenure<- as.numeric (churndata_new$Tenure)
churndata_new$PhoneService<- as.numeric (churndata_new$PhoneService)
churndata_new$MultipleLines<- as.numeric (churndata_new$MultipleLines)
churndata_new$InternetService<- as.numeric (churndata_new$InternetService)
churndata_new$OnlineSecurity<- as.numeric (churndata_new$OnlineSecurity)
churndata_new$OnlineBackup<- as.numeric (churndata_new$OnlineBackup)
churndata_new$DeviceProtection<- as.numeric (churndata_new$DeviceProtection)
churndata_new$TechSupport<- as.numeric (churndata_new$TechSupport)
churndata_new$StreamingTV<- as.numeric (churndata_new$StreamingTV)
churndata_new$StreamingMovies<- as.numeric (churndata_new$StreamingMovies)
churndata_new$Contract<- as.numeric (churndata_new$Contract)
churndata_new$PaperlessBilling<- as.numeric (churndata_new$PaperlessBilling)
churndata_new$PaymentMethod<- as.numeric (churndata_new$PaymentMethod)
churndata_new$MonthlyCharges<- as.numeric (churndata_new$MonthlyCharges)
churndata_new$TotalCharges<- as.numeric (churndata_new$TotalCharges)
churndata_new$Churn<- as.numeric (churndata_new$Churn)

set.seed(20)
clusters <- kmeans(churndata_new[,7:15],5)
ggplot(data=churndata_new, aes(x=MonthlyCharges, y=Tenure ,color=clusters$cluster ))+ geom_point()
