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

for (i in 1:ncol(churndata_new))
{
  churndata_new[,i]<-as.numeric(churndata_new[,i])
}

index <- sample(1:nrow(churndata_new),round(0.9*nrow(churndata_new)))
train <- churndata_new[index,]
test <- churndata_new[-index,]

## Scale data for neural network

ta<-apply(churndata_new,2,function(x) sum(is.na(x)))
lm.fit <- glm(MonthlyCharges~Tenure+Contract, data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$MonthlyCharges)^2)/nrow(test)

maxs <- apply(churndata_new, 2, max) 
mins <- apply(churndata_new, 2, min)
scaled <- as.data.frame(scale(na.omit(churndata_new), center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("MonthlyCharges~Tenure+Contract"))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=F)
plot(nn)

testn<-NULL
testn <- cbind(testn,test_$Tenure)
testn <- cbind(testn,test_$Contract)

pr.nn <- compute(nn,testn)
pr.nn_ <- pr.nn$net.result*(max(churndata_new$MonthlyCharges)-min(churndata_new$MonthlyCharges))+min(churndata_new$MonthlyCharges)
test.r <- (test_$MonthlyCharges)*(max(churndata_new$MonthlyCharges)-min(churndata_new$MonthlyCharges))+min(churndata_new$MonthlyCharges)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

print(paste(MSE.lm,MSE.nn))
plot(test$MonthlyCharges,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$MonthlyCharges,pr.lm,col='blue',pch=18,cex=0.7)
#abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))

library(boot)
set.seed(200)
lm.fit <- glm(MonthlyCharges~Tenure+Contract,data=churndata_new)
cv.glm(churndata_new,lm.fit,K=10)$delta[1]

set.seed(450)
cv.error <- NULL
k <- 5

library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k){
  index <- sample(1:nrow(churndata_new),round(0.9*nrow(churndata_new)))
  maxs1 <- apply(churndata_new, 2, max) 
  mins1 <- apply(churndata_new, 2, min)
  scaled1 <- as.data.frame(scale(na.omit(churndata_new), center = mins1, scale = maxs1 - mins1))
  train.cv <- scaled1[index,]
  test.cv <- scaled1[-index,]
  nn <- neuralnet(f,data=train.cv,hidden=c(5,3),linear.output=F)
  testn<-NULL
  testn <- cbind(testn,test.cv$Tenure)
  testn <- cbind(testn,test.cv$Contract)
  pr.nn <- compute(nn,testn)
  pr.nn <- pr.nn$net.result*(max(churndata_new$MonthlyCharges)-min(churndata_new$MonthlyCharges))+min(churndata_new$MonthlyCharges)
  test.cv.r <- (test.cv$MonthlyCharges)*(max(churndata_new$MonthlyCharges)-min(churndata_new$MonthlyCharges))+min(churndata_new$MonthlyCharges)   
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)    
  pbar$step()
}

mean(cv.error)
boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)
