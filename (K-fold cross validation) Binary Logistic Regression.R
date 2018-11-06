library(DMwR)
library(tseries)
library(usdm)
library(olsrr)
library(pROC)
library(ROCR)
library(ROSE)
library(stringi)
library(caret)
library(gbm)

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

areaundercurve<-c()
misClasificError<-c()
a1<-c()
#Find out about distribution
#jarque.bera.test(churndata_new$Tenure)
# data:  churndata$Tenure
# X-squared = 632.07, df = 2, p-value < 2.2e-16
#jarque.bera.test(churndata_new$MonthlyCharges)
# data:  churndata$MonthlyCharges
# X-squared = 520.9, df = 2, p-value < 2.2e-16

# means both are not normally distributed


str(train)
#do collinearity test
#lm.fit<-glm(Churn~.,family = binomial(link='logit'),data=train)
#summary(lm.fit)

# Call:
#   lm(formula = Churn ~ . - TotalCharges - Gender - Partner - PhoneService - 
#        PaymentMethod - Dependents - StreamingTV - StreamingMovies - 
#        DeviceProtection, data = churndata_new)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.72291 -0.28009 -0.08661  0.33796  1.15049 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       0.3377494  0.0197722  17.082  < 2e-16 ***
#   SeniorCitizen     0.0555374  0.0128788   4.312 1.64e-05 ***
#   Tenure           -0.0049550  0.0002895 -17.117  < 2e-16 ***
#   MultipleLines     0.0450734  0.0076807   5.868 4.60e-09 ***
#   InternetService   0.1385550  0.0094652  14.638  < 2e-16 ***
#   OnlineSecurity   -0.0796207  0.0110131  -7.230 5.35e-13 ***
#   OnlineBackup     -0.0451829  0.0104748  -4.314 1.63e-05 ***
#   TechSupport      -0.0748200  0.0109589  -6.827 9.37e-12 ***
#   Contract         -0.0405995  0.0083804  -4.845 1.30e-06 ***
#   PaperlessBilling  0.0535578  0.0100398   5.335 9.88e-08 ***
#   MonthlyCharges    0.0011313  0.0002400   4.714 2.48e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3784 on 7032 degrees of freedom
# Multiple R-squared:  0.2667,	Adjusted R-squared:  0.2657 
# F-statistic: 255.7 on 10 and 7032 DF,  p-value: < 2.2e-16


#a <-lm(Churn ~ .-CustomerID-Gender-Partner-MultipleLines-PaymentMethod-Dependents, churndata_new)
#summary(a)
#ols_vif_tol(lm.fit)
# # A tibble: 10 x 3
# Variables        Tolerance   VIF
# <chr>                <dbl> <dbl>
# 1 SeniorCitizen        0.902  1.11
# 2 Tenure               0.402  2.49
# 3 MultipleLines        0.801  1.25
# 4 InternetService      0.417  2.40
# 5 OnlineSecurity       0.264  3.79
# 6 OnlineBackup         0.306  3.27
# 7 TechSupport          0.267  3.74
# 8 Contract             0.416  2.40
# 9 PaperlessBilling     0.835  1.20
#10 MonthlyCharges       0.390  2.56

#IF VIF>10, means high collinearity ( represented by other variables)
#if VIF>4, means need to investigate
#best if VIF close to 1

library(plyr) 
k<-5
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
  gbmtest<-predict(gbm1,test,type="prob")[,2]
  test$Churn<-ifelse(test$Churn=="Yes",1,0)
  pr1<-prediction(gbmtest,test$Churn)
  prf1<-performance(pr1,measure="tpr",x.measure="fpr")
  plot(prf1)
  a1[i]<-auc(test$Churn,gbmtest)
  
  #doing logistic linear regression
  model<-glm(Churn~.,family = binomial(link='logit'),data=train)
  summary(model)
  
  #regression(anova)
  #anova(model,test="Chisq")
  
  #prediction
  #p<-predict(model,type="response")
  #p<-ifelse(p>0.5,1,0)
  #misClasificError[i] <- mean(p!=test$Churn)
  #accuracy of prediction
  #print(paste('Accuracy',1-misClasificError[i]))
  
  #based on prediction, test on actual
  p<-predict(model,newdata=test,type="response")
  pr<-prediction(p,test$Churn)
  #plot true-positive to false-positive
  prf<-performance(pr,measure="tpr",x.measure="fpr")
  plot(prf)
  
  #finding Area Under Curve
  auc <- performance(pr, measure="auc")
  auc <- auc@y.values[[1]]
  #AUC (best) = 1, AUC(coin toss)=0.5
  areaundercurve[i]<-auc
  pbar$step()
}

#print(paste('Mean Prediction Accuracy: ',1-mean(misClasificError)))
print(paste('Mean Area Under Curve: ',mean(areaundercurve)))
print(paste('Mean Area Under Curve ( GBM ) : ',mean(a1)))
