#-Load the "playstore" dataset
playstore <- read.csv("googleplaystore.csv", header = TRUE)
str(playstore)
dim(playstore)
summary(playstore)
#--Observed :    1 NaN          in Type           0.00%
#--Observed :    1 Blank        in Content.Rating 0.00%
#--Observed :    2 NaN, 1 Blank in Android.Ver    0.00%
#--Observed : 1474 NA's         in Rating         13.5%

#-Handling Missing Data
playstore_new <- playstore

#--Removing Rows with Blanks and NaN (Less than 1%)
playstore_new <- playstore_new[playstore_new$Type!="NaN" & 
                               playstore_new$Content.Rating!="" & 
                               playstore_new$Android.Ver!="NaN" & 
                               playstore_new$Android.Ver!="",]
dim(playstore_new)  #4 rows removed
summary(playstore)  #Blanks and NaN is removed from Type, Content.Rating, and Android.Ver

#--Replacing Rows with NA's with the Median
playstore_new$Rating[which(is.na(playstore_new$Rating))] <- # Set : Rows in the Missing (Those that have NA)
  median(playstore_new$Rating, na.rm=TRUE)                  # From: Middle "Value" of the Rating
dim(playstore_new)  #No rows removed
summary(playstore)  #Na's is removed from Rating

#---GooglePlayStore dataset is cleaned up.
playstore <- playstore_new
remove(playstore_new)

