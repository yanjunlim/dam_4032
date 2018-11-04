library(stringi)
library(tseries)
library(usdm)
library(olsrr)
library(pROC)
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
areaundercurve<-c()
misClasificError<-c()
#Find out about distribution
#jarque.bera.test(churndata_new$Tenure)
# data:  churndata$Tenure
# X-squared = 632.07, df = 2, p-value < 2.2e-16
#jarque.bera.test(churndata_new$MonthlyCharges)
# data:  churndata$MonthlyCharges
# X-squared = 520.9, df = 2, p-value < 2.2e-16

# means both are not normally distributed



#do collinearity test
lm.fit<-lm(Churn~.-TotalCharges-Gender-Partner-PhoneService-PaymentMethod-Dependents-StreamingTV-StreamingMovies-DeviceProtection, data=churndata_new)
summary(lm.fit)

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
ols_vif_tol(lm.fit)
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
k<-10
pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k){
#split data 70-30
index <- sample(1:nrow(churndata_new),round(0.7*nrow(churndata_new)))
train <- churndata_new[index,]
test <- churndata_new[-index,]
#doing logistic linear regression
model<-glm(Churn~.-TotalCharges-Gender-Partner-PhoneService-PaymentMethod-Dependents-StreamingTV-StreamingMovies-DeviceProtection,family = binomial(link='logit'),data=train)
summary(model)

#regression(anova)
anova(model,test="Chisq")

#prediction
p<-predict(model,type="response")
p<-ifelse(p>0.5,1,0)
misClasificError[i] <- mean(p!=test$Churn)
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

print(paste('Mean Prediction Accuracy: ',1-mean(misClasificError)))
print(paste('Mean Area Under Curve: ',mean(areaundercurve)))
