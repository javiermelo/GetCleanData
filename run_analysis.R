##run_analysis.R
##
## Getting and Cleaning Data project
## Create a second, independent tidy data set with the average of each variable
## for each activity and each subject
##
## 1. Merges the training and the test sets to create one data set
##
## Initializes file handlers
## Data Source:
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles
##         %2FUCI%20HAR%20Dataset.zip
## It is assumed that the zip file was unpacked into "data" directory
## which is under working directory

## verify existence of files in the right directory
fileFeatures <- "./data/UCI HAR Dataset/features.txt"
if(!file.exists(fileFeatures)){
        stop(paste("Required file does not exist:",
                   fileFeatures), call.= FALSE)
}
fileActLabels <- "./data/UCI HAR Dataset/activity_labels.txt"
if(!file.exists(fileActLabels)){
        stop(paste("Required file does not exist: ",
                   fileActLabels), call.= FALSE)
}
fileSubjectTrain <- "./data/UCI HAR Dataset/train/subject_train.txt"
if(!file.exists(fileSubjectTrain)){
        stop(paste("Required file does not exist: ",
                   fileSubjectTrain), call.= FALSE)
}
fileSubjectTest <- "./data/UCI HAR Dataset/test/subject_test.txt"
if(!file.exists(fileSubjectTest)){
        stop(paste("Required file does not exist: ",
                   fileSubjectTest), call.= FALSE)
}
fileXtrain <- "./data/UCI HAR Dataset/train/X_train.txt"
if(!file.exists(fileXtrain)){
        stop(paste("Required file does not exist: ",
                   fileXtrain), call.= FALSE)
}
fileYtrain <- "./data/UCI HAR Dataset/train/y_train.txt"
if(!file.exists(fileYtrain)){
        stop(paste("Required file does not exist: ",
                   fileYtrain), call.= FALSE)
}
fileXtest <- "./data/UCI HAR Dataset/test/X_test.txt"
if(!file.exists(fileXtest)){
        stop(paste("Required file does not exist: ",
                   fileXtest), call.= FALSE)
}
fileYtest <- "./data/UCI HAR Dataset/test/y_test.txt"
if(!file.exists(fileYtest)){
        stop(paste("Required file does not exist: ",
                   fileYtest), call.= FALSE)
}

## read files
library(data.table)
Features <- fread(fileFeatures)
ActLabels <- fread(fileActLabels)
SubjectTrain <- fread(fileSubjectTrain)
SubjectTest <- fread(fileSubjectTest)
Xtrain <- read.table(fileXtrain, header = FALSE)
Ytrain <- fread(fileYtrain)
Xtest <- read.table(fileXtest, header = FALSE)
Ytest <- fread(fileYtest)


## add column "subjectId" into  Xtrain from SubjectTrain

Xtrain$subjectId <- SubjectTrain$V1

## add a column "subjectId" into  Xtest from SubjectTest

Xtest$subjectId <- SubjectTest$V1

## add column "activityId" into Xtrain from Ytrain

Xtrain$activityId <- Ytrain$V1

## add column "activityId" into Xtest from Ytest

Xtest$activityId <- Ytest$V1

## combine Xtrain and Xtest into Xtt to create one data set

Xtt <- rbind(Xtrain, Xtest)

## 2. Extracts only the measurements on the mean and standard deviation
## for eachmeasurement.
##
##  select only columns names containing "-mean()" or "-std()" and 
## combine with the last 2 columns added into colsel vector;
## subset columns based on the colsel vector
##
toMatch <- c("-mean\\(\\)", "-std\\(\\)")
colsel <- union(grep(paste(toMatch,collapse="|"),Features$V2, value=FALSE),
                (562:563))
Xtt <- Xtt[ , colsel] 

## 3. Uses descriptive activity names to name  the activities in the data set
##  I selected lowerUpperCamel convention for the variable names because
## if I left them only in lower case, it is going to be harder to read
## punctuation is removed from the labels
ActLabels$V2 <- gsub("[[:punct:]]", " ", ActLabels$V2)
setnames(ActLabels,  names(ActLabels),  c("activityId", "activity"))

## factor is created with the activity descriptive names 
activityF <- factor(Xtt$activityId, labels=ActLabels$activity)
Xtt$activityId <- activityF


## 4. Appropriately labels the data set with descriptive variable names

variables <- union(grep(paste(toMatch,collapse="|"),Features$V2, value=TRUE),
                   c("subjectId", "activity"))
## remove dashes and parentheses 
variables <- gsub("[[:punct:]]", "", variables)
## substitute "mean" and "std" to comply with lowerUpperCamel notation
variables <- gsub("mean","Mean", variables)
variables <- gsub("std","Std", variables)
setnames(Xtt, variables)

## 5. Creates a second, indepedent tidy data set with the average of each
##    variable for each activity and each subject

library(reshape2)
Xtt <- melt(Xtt,id=c("activity", "subjectId"),
                measure.vars=variables[1:66], as.is=TRUE)
## cast to summarize with mean accross the features in variable
XttWide <- dcast(Xtt,activity+subjectId~variable, mean)
## write the table into a file without row names and default separator
write.table(XttWide,"./data/Xttwide.txt",row.names=FALSE)

