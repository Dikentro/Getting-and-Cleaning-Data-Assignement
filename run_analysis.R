#Preparation

#Step 1: Activate the packages that will be needed for this assignement

library(dplyr)
library(data.table)
        
#Step 2: Assign all data frames

features <- read.table("C:/Users/diken/OneDrive/Desktop/R/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("C:/Users/diken/OneDrive/Desktop/R/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("C:/Users/diken/OneDrive/Desktop/R/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("C:/Users/diken/OneDrive/Desktop/R/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("C:/Users/diken/OneDrive/Desktop/R/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("C:/Users/diken/OneDrive/Desktop/R/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("C:/Users/diken/OneDrive/Desktop/R/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("C:/Users/diken/OneDrive/Desktop/R/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt", col.names = "code")

#Note: For some reason the X_test and X_train tables could not be read (error: more columns than column names), 
#so I imported both files in RStudio. 

#Assignement: You should create one R script called run_analysis.R that does the following.

#Step 1: Merges the training and the test sets to create one data set.

#In this step we will bind the content of the 3 different group files based on the table rows.

X <- rbind(x_train, x_test) 
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)

#The last function is to unite all data in one table. 
Merged_Data <- cbind(Subject, Y, X) 

#Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.

#This function will be based on the table columns. 

TidyData <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))

#Step 3: Uses descriptive activity names to name the activities in the data set

#In this step we replace numbers of the "code" column from the "TidyData" set by the corresponding activity of 
#the 2nd column of the "activities" variable.

TidyData$code <- activities[TidyData$code, 2]

#Step 4: Appropriately labels the data set with descriptive variable names.

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
        
#Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable 
#for each activity and each subject.

#The FinalData is the summary of TidyData when: 
        #1. The contents are groupped by subject and activity
        #2. One takes the means of each variable for each activity and each subject

FinalData <- TidyData %>%
        group_by(subject, activity) %>%
        summarise_all(funs(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE) #Extracts the FinalData.txt
