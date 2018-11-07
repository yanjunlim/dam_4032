# library(DMwR)
# library(tseries)
# library(usdm)
# library(olsrr)
# library(pROC)
# library(ROCR)
# library(ROSE)
# library(stringi)
# library(caret)
# library(gbm)
library(plotly)
library(ggplot2)
library(RColorBrewer)

#-Load the "churn" dataset
churndata <- read.csv("Telco-Customer-Churn.csv", header = TRUE)
str(churndata)
dim(churndata) # 7043   21
summary(churndata)
#--Observed : 11 NA's in TotalCharges


#-Handling Missing Data
c.new <- churndata

#--Replacing Rows with NA's with the Median
c.new$TotalCharges[which(is.na(c.new$TotalCharges))] <-
  # Set : Rows in the Missing (Those that have NA)
  median(c.new$TotalCharges, na.rm = TRUE)                # From: Middle "Value" of the Rating
dim(c.new)      #No rows removed
summary(c.new)  #NA's is removed from TotalCharges

#-Clean of Columns (to Factor)
c.new$A <- "A" #Temporary Column

#--CustomerID
c.new$CustomerID <- NULL #no use

#--TotalCharges
c.new$A <- c.new$MonthlyCharges * c.new$Tenure
ggplot(c.new, aes(c.new$A, c.new$TotalCharges)) + geom_point(color = brewer.pal(n = 3, name = 'Set3')[1]) +
  labs(x = "Monthly Charges*Tenure", y = "Total Charges")

cor.test(~ A + TotalCharges, c.new) #0.9992631
c.new$TotalCharges <- NULL
#---TotalCharges is closely related to MonthlyCharges and Tenure, thus it is removed.

#--Senior Citizen
c.new$A[which(c.new$SeniorCitizen == 1)] <- "Yes"
c.new$A[which(c.new$SeniorCitizen == 0)] <- "No"
c.new$SeniorCitizen <- as.factor(c.new$A)

#--Tenure
c.new$A[which(c.new$Tenure <= 12)] <- "0-12"
c.new$A[which(c.new$Tenure > 12 & c.new$Tenure <= 24)] <- "12-24"
c.new$A[which(c.new$Tenure > 24 & c.new$Tenure <= 36)] <- "24-36"
c.new$A[which(c.new$Tenure > 36 & c.new$Tenure <= 48)] <- "36-48"
c.new$A[which(c.new$Tenure > 48 & c.new$Tenure <= 60)] <- "48-60"
c.new$A[which(c.new$Tenure > 60)] <- "gt.60"
c.new$Tenure <- as.factor(c.new$A)

#--MonthlyCharges
c.new$A[which(c.new$MonthlyCharges <= 20)] <- "0-20"
c.new$A[which(c.new$MonthlyCharges > 20 &
                c.new$MonthlyCharges <= 40)] <- "20-40"
c.new$A[which(c.new$MonthlyCharges > 40 &
                c.new$MonthlyCharges <= 60)] <- "40-60"
c.new$A[which(c.new$MonthlyCharges > 60 &
                c.new$MonthlyCharges <= 80)] <- "60-80"
c.new$A[which(c.new$MonthlyCharges > 80 &
                c.new$MonthlyCharges <= 100)] <- "80-100"
c.new$A[which(c.new$MonthlyCharges > 100)] <- "gt.100"
c.new$MonthlyCharges <- as.factor(c.new$A)

c.new$A <- NULL #remove temporary column
str(c.new) #verify factors

#-Pie Charts
t.table <- table(c.new$Churn)
t.percent <- round(100 * t.table / sum(t.table), 1)
t.color <- brewer.pal(n = length(t.table), name = 'Set3')
pie(
  t.table,
  labels = paste(t.percent, "%"),
  main = paste(t.df$Churn[1], ":", labels(t.df[t.col])[[2]]),
  col = t.color
)

c.split <- split(c.new, c.new$Churn)
par(mfrow = c(1, length(c.split)), mar = c(0, 0, 3, 0)) #split based on churn

for (t.col in 1:(ncol(c.new) - 1)) {
  for (t.len in 1:length(c.split)) {
    t.df <- c.split[[t.len]]
    t.table <- table(t.df[[t.col]])
    t.percent <- round(100 * t.table / sum(t.table), 1)
    t.color <- brewer.pal(n = length(t.table), name = 'Set3')
    pie(
      t.table,
      labels = paste(t.percent, "%"),
      main = paste(t.df$Churn[1], ":", labels(t.df[t.col])[[2]]),
      col = t.color
    )
  }
  legend("topleft",
         levels(t.df[[t.col]]),
         cex = 0.8,
         fill = t.color)
}
remove(t.col, t.len, t.df, t.table, t.percent, t.color)
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1) #revert

#-Takes a random 70% of data
sample <- sample(nrow(c.new), 0.7 * nrow(c.new), replace = FALSE)
train <- c.new[sample,]   #atore the 70% data in train
valid <- c.new[-sample,]  #store the remaining 30% data in valid
remove(sample)
