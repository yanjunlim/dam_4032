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
  median(churndata_new$TotalCharges, na.rmR=TRUE)                        # From: Middle "Value" of the Rating
dim(churndata_new)      #No rows removed
summary(churndata_new)  #NA's is removed from TotalCharges



# ================= Prediction and Classification =================

# Load and examine the dataset
library(ISLR)
rf_churn <- as.data.frame(churndata_new)
str(rf_churn)

#No = 5174, yes = 1869
table(rf_churn$Churn)

# Fit a random forest
library(randomForest)
rfFit <- randomForest(Churn ~ .-CustomerID,                      # formula
                      data = rf_churn,                    # data set
                      ntree = 100,                       # number of trees
                      mtry = 4,                          # variables for split
                      importance = TRUE)                 # importance recorded                 
rfFit
help(randomorest)
#Accuracy = 0.79
varImpPlot(rfFit, type = 1)
#Tenure tops the chart, hence most important.

# -------------------------------------------------------
# Weighted Training

library(caret)

# Set up the formula ------------------------------------ ?
trainFormula <- (Churn~.-CustomerID-TotalCharges-Gender-Partner-PhoneService-PaymentMethod-Dependents-StreamingTV-StreamingMovies-DeviceProtection)

# Set up observation weights for training
table(rf_churn$Churn)

#Churn no = 0.734, Churn yes = 0.265
prop.table(table(rf_churn$Churn))
obsWeights <- ifelse(rf_churn$Churn == "No",
                     (0.5/table(rf_churn$Churn)[1]),
                     (0.5/table(rf_churn$Churn)[2]))

# cross-validation
trainCtrl <- trainControl(method = "repeatedcv",              # repeated cross-validation
                          number = 10,                         # number of folds for k-CV
                          repeats = 1,                        # number of repeats for CV
                          summaryFunction = twoClassSummary,  # allows metric = ROC in train
                          classProbs = TRUE)                  # probability (not just class)

rfFit <- train(trainFormula,                                  # formula
               data = rf_churn,                                # dataset
               method = "rf",                                 # model
               weights = obsWeights,                          # observation weights
               metric = "ROC",                                # tuning metric
               trControl = trainCtrl)                         # train controls

rfFit$finalModel

# Prediction
Prediction <- predict(rfFit, rf_churn)
mean(rf_churn$Churn==Prediction)
#accuracy 0.80

