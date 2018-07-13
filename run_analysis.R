#Cleaning the Environment and Variables

rm(list=ls())

#Download the source data zip file to a folder ./CourseraData

if(!file.exists("./CourseraData")){dir.create("./CourseraData")}

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(fileUrl,destfile="./CourseraData/Dataset.zip")

# Unzip dataSet to CourseraData directory

unzip(zipfile="./CourseraData/Dataset.zip",exdir="./CourseraData")

# This script requires dplyr package to be installed
# The following script will install dplyr if it is not already installed

if(!require(dplyr))
{
  install.packages("dplyr")
}

library(dplyr)

# Main Body of the Analysis Script

# Read the data sets

# Reading training files:
  tbl_x_train <- read.table("./CourseraData/UCI HAR Dataset/train/X_train.txt")
  tbl_y_train <- read.table("./CourseraData/UCI HAR Dataset/train/y_train.txt")
  tbl_subject_train <- read.table("./CourseraData/UCI HAR Dataset/train/subject_train.txt")

# Reading testing files:
  tbl_x_test <- read.table("./CourseraData/UCI HAR Dataset/test/X_test.txt")
  tbl_y_test <- read.table("./CourseraData/UCI HAR Dataset/test/y_test.txt")
  tbl_subject_test <- read.table("./CourseraData/UCI HAR Dataset/test/subject_test.txt")

# Reading features file:
  tbl_features <- read.table('./CourseraData/UCI HAR Dataset/features.txt')

# Reading activity labels:
  tbl_activityLabels <- read.table('./CourseraData/UCI HAR Dataset/activity_labels.txt')

# Assigning column names to tables created above
  colnames(tbl_x_train) <- tbl_features[,2] 
  colnames(tbl_y_train) <-"activityId"
  colnames(tbl_subject_train) <- "subjectId"
  colnames(tbl_x_test) <- tbl_features[,2] 
  colnames(tbl_y_test) <- "activityId"
  colnames(tbl_subject_test) <- "subjectId"
  colnames(tbl_activityLabels) <- c('activityId','activityType')

#Requirement 1: Merges the training and the test sets to create one data set.
  tbl_train <- cbind(tbl_y_train, tbl_subject_train, tbl_x_train)
  tbl_test <- cbind(tbl_y_test, tbl_subject_test, tbl_x_test)
  tbl_merged <- rbind(tbl_train, tbl_test)

#Requirement 2: Extracts only the measurements on the mean and standard deviation for each measurement

# Substep: Read the column names from the merged table
  col_merged <- colnames(tbl_merged)

# Subsetp: Filter the two ID columns, and any columns related to mean or standard deviation
  vector_extract <-
    (
      grepl("activityId" , col_merged) |
        grepl("subjectId" , col_merged) |
        grepl("mean.." , col_merged) | grepl("std.." , col_merged)
    )
  
# Subtep: Create the extacted subset
  tbl_extracted <- tbl_merged[,vector_extract == TRUE]

# Requirement 3: Uses descriptive activity names to name the activities in the data set
# This code mrgres the extracted table and the activity lable tables to add the Activity description at the very end of the file
  tbl_activityNames <- merge(tbl_extracted, tbl_activityLabels, by='activityId',all.x = TRUE)

# Requirement 4: Appropriately labels the data set with descriptive variable names.
  names(tbl_activityNames)<-gsub("std()", "StandardDeviation", names(tbl_activityNames))
  names(tbl_activityNames)<-gsub("mean()", "Mean", names(tbl_activityNames))
  names(tbl_activityNames)<-gsub("^t", "Time", names(tbl_activityNames))
  names(tbl_activityNames)<-gsub("^f", "Frequency", names(tbl_activityNames))
  names(tbl_activityNames)<-gsub("Acc", "Accelerometer", names(tbl_activityNames))
  names(tbl_activityNames)<-gsub("Gyro", "Gyroscope", names(tbl_activityNames))
  names(tbl_activityNames)<-gsub("Mag", "Magnitude", names(tbl_activityNames))
  names(tbl_activityNames)<-gsub("BodyBody", "Body", names(tbl_activityNames))
  
# Requirement 5: From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
  
  tbl_tidyData<-aggregate(tbl_activityNames[,3:81], by = list(activity = tbl_activityNames$activityId, subject = tbl_activityNames$subjectId), FUN = mean)
  tbl_tidyData<-merge(tbl_tidyData, tbl_activityLabels, by.x=1, by.y=1, all.x = TRUE)
  tbl_tidyData<-arrange(tbl_tidyData, activity, subject)
  tbl_tidyData<-select(tbl_tidyData,activityType,subject,`TimeBodyAccelerometer-Mean()-X`:`FrequencyBodyGyroscopeJerkMagnitude-MeanFreq()`)
  View(tbl_tidyData)
  write.table(tbl_tidyData, file = "tbl_tidyData.txt",row.name=FALSE,quote = FALSE, sep = '\t')
  
# End of Program
  
# Last Update Made on Friday July 6 2018 17:20 CST
  