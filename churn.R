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

churndata_modified <-churndata_new
keeps<-c("MonthlyCharges","Tenure","Contract")
churndata_modified<-churndata_modified[keeps]
index <- sample(1:nrow(churndata_modified),round(0.9*nrow(churndata_modified)))
train <- churndata_modified[index,]
test <- churndata_modified[-index,]

## Scale data for neural network

ta<-apply(churndata_modified,2,function(x) sum(is.na(x)))
lm.fit <- glm(MonthlyCharges~Tenure+Contract, data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$MonthlyCharges)^2)/nrow(test)

maxs <- apply(churndata_modified, 2, max) 
mins <- apply(churndata_modified, 2, min)
scaled <- as.data.frame(scale(na.omit(churndata_modified), center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("MonthlyCharges~Tenure+Contract"))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T, stepmax=1e7)
plot(nn)

testn<-test_
keeps<-c("MonthlyCharges","Tenure","Contract")
testn<-testn[keeps]

pr.nn <- neuralnet::compute(nn,testn[,2:3])

results<-data.frame(actual=testn$MonthlyCharges,prediction=pr.nn$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdtf=data.frame(roundedresults)
attach(roundedresultsdtf)
count=0
for(h in 1:nrow(testn))
{
  if(actual[h]==prediction[h])
  {
    count<-count+1
  }
}
confidence = count/nrow(testn)

pr.nn_ <- pr.nn$net.result*(max(churndata_modified$MonthlyCharges)-min(churndata_modified$MonthlyCharges))+min(churndata_modified$MonthlyCharges)
test.r <- (test_$MonthlyCharges)*(max(churndata_modified$MonthlyCharges)-min(churndata_modified$MonthlyCharges))+min(churndata_modified$MonthlyCharges)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

print(paste(MSE.lm,MSE.nn))
plot(test$MonthlyCharges,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$MonthlyCharges,pr.lm,col='blue',pch=18,cex=0.7)
#abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))

library(boot)
set.seed(200)
lm.fit <- glm(MonthlyCharges~Tenure+Contract,data=churndata_modified)
cv.glm(churndata_modified,lm.fit,K=2)$delta[1]

set.seed(450)
cv.error <- NULL
k <- 5

library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k){
  index <- sample(1:nrow(churndata_modified),round(0.9*nrow(churndata_modified)))
  maxs1 <- apply(churndata_modified, 2, max) 
  mins1 <- apply(churndata_modified, 2, min)
  scaled1 <- as.data.frame(scale(na.omit(churndata_modified), center = mins1, scale = maxs1 - mins1))
  train.cv <- scaled1[index,]
  test.cv <- scaled1[-index,]
  nn <- neuralnet(f,data=train.cv,hidden=c(5,3),linear.output=T, stepmax=1e7)
  testn<-test_
  keeps<-c("MonthlyCharges","Tenure","Contract")
  testn<-testn[keeps]
  pr.nn <- neuralnet::compute(nn,testn[,2:3])
  
  results<-data.frame(actual=testn$MonthlyCharges,prediction=pr.nn$net.result)
  roundedresults<-sapply(results,round,digits=0)
  roundedresultsdtf=data.frame(roundedresults)
  attach(roundedresultsdtf)
  count=0
  for(h in 1:nrow(testn))
  {
    if(actual[h]==prediction[h])
    {
      count<-count+1
    }
  }
  confidence[i] = count/nrow(testn)
  
  pr.nn <- pr.nn$net.result*(max(churndata_modified$MonthlyCharges)-min(churndata_modified$MonthlyCharges))+min(churndata_modified$MonthlyCharges)
  test.cv.r <- (test.cv$MonthlyCharges)*(max(churndata_modified$MonthlyCharges)-min(churndata_modified$MonthlyCharges))+min(churndata_modified$MonthlyCharges)   
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)    
  pbar$step()
}
mean(confidence)
mean(cv.error)
boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)
