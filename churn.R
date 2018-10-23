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
# for (c in 1:ncol(churndata_new)){
#   if ("Yes" %in% churndata_new[,c]){
#     churndata_new[,c] <- as.character(churndata_new[,c])
#     churndata_new[,c][which(stri_cmp_eq(churndata_new[,c],"Yes"))]  <- 1
#     churndata_new[,c][which(stri_cmp_eq(churndata_new[,c],"No"))]   <- 0
#     churndata_new[,c][which(stri_detect_fixed(churndata_new[,c],"service"))]<- -1
#     churndata_new[,c] <- as.integer(churndata_new[,c])
#   }
# }



#--------------Regression tree------------

#Prepare training and validation data for regression tree 70-30
trainData <- sample(nrow(churndata_new), 0.7*nrow(churndata_new), replace = FALSE)
trainingChurn<-churndata_new[trainData,]
validationChurn <-churndata_new[-trainData,]
summary(trainingChurn)
summary(validationChurn)

#install.packages("tree")
library(tree)


# Build a "default" classification Tree
treeFit <- tree(Churn ~.-CustomerID, data = trainingChurn)
plot(treeFit)
text(treeFit, pretty = FALSE)

# Predict using the tree model
trainingPrediction<-predict(treeFit,trainingChurn, type = "class") #training data prediction
mean(trainingPrediction == trainingChurn$Churn)

validationPrediction<-predict(treeFit,validationChurn,type="class") #validation data prediction
mean(validationPrediction ==validationChurn$Churn)


# Build a "large" Regression Tree
ltreeFit <- tree(Churn ~ .-CustomerID, data = trainingChurn, 
                 split = "deviance",
                 method = "recursive.partition",
                 control = tree.control(nobs = nrow(trainingChurn),  # number of sample points
                                        mincut = 10,             # minimum points in each child
                                        minsize = 20,            # minimum points in each parent
                                        mindev = 0))            # minimum information gain to split
plot(ltreeFit)
predTrain <- predict(ltreeFit, trainingChurn, type = "class")  # prediction on train set
mean(predTrain == trainingChurn$Churn)                        # classification accuracy
predValid <- predict(ltreeFit, validationChurn, type = "class")  # prediction on validation set
mean(predValid == validationChurn$Churn)                        # classification accuracy


# Build a "small" Regression Tree
streeFit <- tree(Churn ~ .-Churn-CustomerID, data = trainingChurn, 
                 split = "deviance",
                 method = "recursive.partition",
                 control = tree.control(nobs = nrow(trainingChurn),
                                        mincut = 0.1 * nrow(trainingChurn),
                                        minsize = 0.2 * nrow(trainingChurn),
                                        mindev = 0.02))

plot(streeFit)
text(streeFit,pretty=FALSE)

predTrain <- predict(streeFit, trainingChurn, type = "class")  # prediction on train set
mean(predTrain == trainingChurn$Churn)                        # classification accuracy
predValid <- predict(streeFit, validationChurn, type = "class")  # prediction on validation set
mean(predValid == validationChurn$Churn)                        # classification accuracy
 
# Build a "pruned" Regression Tree (cross-validated)
# Start with a large tree (the largest, if possible)
ltreeFit <- tree(Churn ~ .- Churn-CustomerID, data = churndata_new,
                 split = "deviance",
                 method = "recursive.partition",
                 control = tree.control(nobs = nrow(churndata_new),
                                        mincut = 10,
                                        minsize = 20,
                                        mindev = 0))


plot(ltreeFit)

cvTree <- cv.tree(ltreeFit, FUN = prune.misclass, K = 10)     # K-fold Cross-Validation
cbind(cvTree$size, cvTree$dev)                            # check cvTree output
plot(cvTree$size, cvTree$dev, type="b", xlab = "No. of Leaves", ylab = "Deviance")        # plot deviance vs size
 
# 4 is the best size because it has the least number of leaves and a high deviance
bestSize <- 4
ptreeFit <- prune.misclass(ltreeFit, best = bestSize)         # prune tree to best size
plot(ptreeFit)
text(ptreeFit, pretty = FALSE)

# Predict using the tree model on the full data
predTrain <- predict(ptreeFit, churndata_new, type = "class")
mean(predTrain == churndata_new$Churn)


#--------------Random Forest------------
#install.packages("randomForest")
library(randomForest)
bagFit <- randomForest(Churn ~ .-CustomerID,                    # formula
                       data = churndata_new,                  # data set
                       ntree = 500,                      # number of trees
                       mtry = 19,                         # variables for split
                       importance = TRUE)                # importance recorded
bagFit

predTrain <- predict(bagFit, churndata_new, type = "class")   # prediction on train set
mean(predTrain == churndata_new$Churn)                    # classification accuracy

#There's an improvement in accuracy.

# Bagging gives you variable importance for free, depending on the splits
importance(bagFit)        # importance of the variables in the model (values)
varImpPlot(bagFit)        # importance of the variables in the model (visual)

#Contract is the most important

# -------------------------------------------------------
# Multiple Random Forest Model on train set

rfFit <- randomForest(Churn ~ .-CustomerID,                     # formula
                      data = churndata_new,                   # data set
                      ntree = 500,                       # number of trees
                      mtry = 5,                          # variables for split
                      importance = TRUE)                 # importance recorded                 
rfFit

predTrain <- predict(rfFit, churndata_new, type = "class")    # prediction on train set
mean(predTrain == churndata_new$Churn)  

#There is a sligh drop in accuracy.Hence, increase the number of trees.

rfFit <- randomForest(Churn ~ .-CustomerID,                     # formula
                      data = churndata_new,                   # data set
                      ntree = 100,                       # number of trees
                      mtry = 5,                          # variables for split
                      importance = TRUE)                 # importance recorded                 
rfFit

predTrain <- predict(rfFit, churndata_new, type = "class")    # prediction on train set
mean(predTrain == churndata_new$Churn) 

# Random Forest also gives variable importance for free, depending on splits
importance(rfFit)       
varImpPlot(rfFit)      

#Tenure became the most important variable

#In conclusion, bagfit is more important as "contract" takes every variable into consideration