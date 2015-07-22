library('dplyr')

#creation of a working directory
str <- paste('C:',Sys.getenv('HOMEPATH'))
str <- gsub(" ", "", str)
setwd(str)
if (!file.exists("GettingData")) {
  dir.create("GettingData")
}
str <- paste('C:',Sys.getenv('HOMEPATH'),"\\GettingData")
str <- gsub(" ", "", str)
setwd(str)
#download and unziping commented out 
#the donwloaded directory (UCI HAR Dataset) should be in the working directory(%HOME%\GettingData in windows) fixed above or elsewhere but you should modify the working directory accordingly.
# fileURL <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
# download.file(fileURL, destfile = './data/Dataset.zip', method = 'curl', extra ='--insecure')
# dateDownloaded <- date()
# unzip('data/Dataset.zip')
#test data
subject.test    <- read.table("UCI HAR Dataset\\test\\subject_test.txt")
test.x          <- read.table("UCI HAR Dataset\\test\\X_test.txt")
activity.test.y <- read.table("UCI HAR Dataset\\test\\y_test.txt")
test.data       <- cbind(subject.test, activity.test.y, test.x)
#training data
subject.train   <- read.table("UCI HAR Dataset\\train\\subject_train.txt")
train.x         <- read.table("UCI HAR Dataset\\train\\X_train.txt")
activity.train.y<- read.table("UCI HAR Dataset\\train\\y_train.txt")
train.data      <- cbind(subject.train, activity.train.y, train.x)
#now join train and test data
data            <- rbind(test.data,train.data)
#use the features table to get the column names
features        <-  read.table("UCI HAR Dataset\\features.txt") 
colnamesData    <- c("subject","activity",gsub('-','.',as.character(features$V2)))
##parenthesis are not doing well with R, so remove them
colnamesData    <- gsub('\\(\\)','', colnamesData)
colnames(data)  <- colnamesData
##select mean and std as required
usefulCols     <- grep("(mean)|(std)|(subject)|(activity)", colnames(data))
data1          <- data[,usefulCols]
##a last step I am not sure of, is meanFreq a mean ?, in doubt I remove
data1          <- select(data1,-contains("meanFreq"))
#replacing number by explicit names in the activity column
data1[,2] <- gsub(1 , "WALKING",data1[,2])
data1[,2] <- gsub(2 , "WALKING_UPSTAIRS",data1[,2])
data1[,2] <- gsub(3 , "WALKING_DOWNSTAIRS",data1[,2])
data1[,2] <- gsub(4 , "SITTING", data1[,2])
data1[,2] <- gsub(5 , "STANDING",data1[,2])
data1[,2] <- gsub(6 , "LAYING", data1[,2])
#the dplyr part 
data2 <- group_by(data1, subject, activity)
data3 <- summarize(
  data2,tBodyAcc.X    = mean(tBodyAcc.mean.X) ,
  tBodyAcc.mean.Y     = mean(tBodyAcc.mean.Y),
  tBodyAcc.mean.Z     = mean(tBodyAcc.mean.Z),
  tBodyAcc.std.X      = mean(tBodyAcc.std.X),
  tBodyAcc.std.Y      = mean(tBodyAcc.std.Y),
  tBodyAcc.std.Z      = mean(tBodyAcc.std.Z),
  tGravityAcc.mean.X  = mean(tGravityAcc.mean.X),
  tGravityAcc.mean.Y  = mean(tGravityAcc.mean.Y),
  tGravityAcc.mean.Z  = mean(tGravityAcc.mean.Z),
  tGravityAcc.std.X   = mean(tGravityAcc.std.X),
  tGravityAcc.std.Y   = mean(tGravityAcc.std.Y),
  tGravityAcc.std.Z   = mean(tGravityAcc.std.Z),
  tBodyAccJerk.mean.X = mean(tBodyAccJerk.mean.X),
  tBodyAccJerk.mean.Y = mean(tBodyAccJerk.mean.Y),
  tBodyAccJerk.mean.Z = mean(tBodyAccJerk.mean.Z),
  tBodyAccJerk.std.X  = mean(tBodyAccJerk.std.X),
  tBodyAccJerk.std.Y  = mean(tBodyAccJerk.std.Y),
  tBodyAccJerk.std.Z  = mean(tBodyAccJerk.std.Z),
  tBodyGyro.mean.X    =  mean(tBodyGyro.mean.X),
  tBodyGyro.mean.Y    =  mean(tBodyGyro.mean.Y),
  tBodyGyro.mean.Z    =  mean(tBodyGyro.mean.Z),
  tBodyGyro.std.X     =  mean(tBodyGyro.std.X),
  tBodyGyro.std.Y     =  mean(tBodyGyro.std.Y),
  tBodyGyro.std.Z     =  mean(tBodyGyro.std.Z),
  tBodyGyroJerk.mean.X=  mean(tBodyGyroJerk.mean.X),
  tBodyGyroJerk.mean.Y=  mean(tBodyGyroJerk.mean.Y),
  tBodyGyroJerk.mean.Z=  mean(tBodyGyroJerk.mean.Z),
  tBodyGyroJerk.std.X =  mean(tBodyGyroJerk.std.X),
  tBodyGyroJerk.std.Y =  mean(tBodyGyroJerk.std.Y),
  tBodyGyroJerk.std.Z =  mean(tBodyGyroJerk.std.Z),
  tBodyAccMag.mean    =  mean(tBodyAccMag.mean),
  tBodyAccMag.std     =  mean(tBodyAccMag.std),
  tGravityAccMag.mean =  mean(tGravityAccMag.mean),
  tGravityAccMag.std  =  mean(tGravityAccMag.std),
  tBodyAccJerkMag.mean=  mean(tBodyAccJerkMag.mean),
  tBodyAccJerkMag.std =  mean(tBodyAccJerkMag.std),
  tBodyGyroMag.mean   =  mean(tBodyGyroMag.mean),
  tBodyGyroMag.std    =  mean(tBodyGyroMag.std),
  tBodyGyroJerkMag.mean =  mean(tBodyGyroJerkMag.mean),
  tBodyGyroJerkMag.std  =  mean(tBodyGyroJerkMag.std),
  fBodyAcc.mean.X      =  mean(fBodyAcc.mean.X),
  fBodyAcc.mean.Y      =  mean(fBodyAcc.mean.Y),
  fBodyAcc.mean.Z      =  mean(fBodyAcc.mean.Z),
  fBodyAcc.std.X       =  mean(fBodyAcc.std.X),
  fBodyAcc.std.Y       =  mean(fBodyAcc.std.Y),
  fBodyAcc.std.Z       =  mean(fBodyAcc.std.Z),
  fBodyAccJerk.mean.X  =  mean(fBodyAccJerk.mean.X),
  fBodyAccJerk.mean.Y  =  mean(fBodyAccJerk.mean.Y),
  fBodyAccJerk.mean.Z  =  mean(fBodyAccJerk.mean.Z),
  fBodyAccJerk.std.X   =  mean(fBodyAccJerk.std.X),
  fBodyAccJerk.std.Y   = mean(fBodyAccJerk.std.Y),
  fBodyAccJerk.std.Z   =  mean(fBodyAccJerk.std.Z),
  fBodyGyro.mean.X     =  mean(fBodyGyro.mean.X),
  fBodyGyro.mean.Y     =  mean(fBodyGyro.mean.Y),
  fBodyGyro.mean.Z     =  mean(fBodyGyro.mean.Z),
  fBodyGyro.std.X      =  mean(fBodyGyro.std.X),
  fBodyGyro.std.Y      =  mean(fBodyGyro.std.Y),
  fBodyGyro.std.Z      =  mean(fBodyGyro.std.Z),
  fBodyAccMag.mean     =  mean(fBodyAccMag.mean),
  fBodyAccMag.std      =  mean(fBodyAccMag.std),
  fBodyBodyAccJerkMag.mean  =  mean(fBodyBodyAccJerkMag.mean),
  fBodyBodyAccJerkMag.std   =  mean(fBodyBodyAccJerkMag.std),
  fBodyBodyGyroMag.mean     =  mean(fBodyBodyGyroMag.mean),
  fBodyBodyGyroMag.std      =  mean(fBodyBodyGyroMag.std),
  fBodyBodyGyroJerkMag.mean =  mean(fBodyBodyGyroJerkMag.mean),
  fBodyBodyGyroJerkMag.std  =  mean(fBodyBodyGyroJerkMag.std)
  )
  #final touch
write.table(data3,"tidy_data.txt", row.names= FALSE)
