## Paige Fairchild
## Course Project for Coursera "Getting and Cleaning Data"
## September 27, 2023


# Instructions ------------------------------------------------------------
##You should create one R script called run_analysis.R that does the following

## 1. Merges the training and the test sets to create one data set.

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

## 3. Uses descriptive activity names to name the activities in the data set

## 4. Appropriately labels the data set with descriptive variable names. 

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


# Load packages -----------------------------------------------------------


pacman::p_load(
  rio, # importing data
  here, # for relative filepaths
  skimr, # for reviewing the data
  janitor, # for cleaning the data
  lubridate, # for date cleaning
  tidyverse, # for data management and visualization
  )


# Import data -------------------------------------------------------------
features <- import("C:/Users/pfairchild/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt", col.names = c("n", "functions"))

activities <- import("C:/Users/pfairchild/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

training_set <- import("C:/Users/pfairchild/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt", col.names = features$functions )

training_labels <- import("C:/Users/pfairchild/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt", col.names = "code")
  
test_set <- import("C:/Users/pfairchild/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
  
test_labels <- import("C:/Users/pfairchild/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt", col.names = "code")

subject_train <- import("C:/Users/pfairchild/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

subject_test <- import("C:/Users/pfairchild/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt", col.names = "subject")

# Merge data --------------------------------------------------------------
set <- rbind(training_set, test_set)
labels <- rbind(training_labels, test_labels)
subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(subject, labels, set)


# Extract mean and SD for each measurement --------------------------------

TidyData <- Merged_Data %>% 
  select(subject, code, contains("mean"), contains("std"))


# Add activity names ------------------------------------------------------

TidyData$code <- activities[TidyData$code, 2]


# Label data set with descriptive variable names --------------------------
names(TidyData)[2] = "activity"
names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))
names(TidyData)<-gsub("^t", "Time", names(TidyData))
names(TidyData)<-gsub("^f", "Frequency", names(TidyData))
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))
names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("angle", "Angle", names(TidyData))
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))


# Tidy Data Set with Avg of each activity and subject ---------------------

FinalData <- TidyData %>% 
  group_by(subject, activity) %>% 
  summarise_all(funs(mean))
write.table(FinalData, "FinalData.txt", row.names = FALSE)
