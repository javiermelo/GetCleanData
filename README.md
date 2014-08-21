##README
##Getting and Cleaning Data Project | Coursera
##======================================


###Purpose

The purpose of this data collection and cleaning data project is to create a summarized data set based on the "Human Activity Recognition Using SmartPhones Dataset Version 1.0" (Anguita et al., 2012).

###Input Files
  This dataset is available at:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

And a full description of data at:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

###Output Files

run_analysis.R : R script that creates a tidy dataset  with the average of selected features for each activity and each subject.

CodeBook.md : describes in detail the structure of the dataset file

XttWide.txt : tydy dataset with the average of calculated features that used mean and std functions and arranged by activity and subject.

README.md:  This file with instructions, explanation of process and analysis files.

###How to run the script

1.  Download the compressed dataset from the link mentioned above and unzip the file in your working directory. The structure of directories must be kept to allow the script to find  the files. If files are not found, error messages are displayed.

2.  Run in R under your working directory: 
   ```
	> source("run_analysis.R")
   ```
3.  After a few senconds "data.table 1.9.2  For help type: help("data.table")" messages is displayed. Wait some seconds until the prompt appears again indicating that the script finished.

4.  XttWide.txt can be found from your working directory as "./data/XttWide.txt"

###How to read the file

Use R Console: 
   ```
   > mytidy <- read.table("XttWide.txt", header=TRUE, as.is=TRUE)
   ```

###run_analysis.R Explained

It will be listed below the 5 major steps as specified in the project instructions. Then under each step, the sequence of explanations about the R code will be included.

1.  Merges the training and the test sets to create one data set
  1.  Initialize file handlers, and verify the existence of the files.  Script stops with message if a file does not exist.
  2.  Read the files and creates the corresponding data.table for each file
  3. Xtrain.txt has all the variables for each subject and activity,but they are not included in the file as columns. The subject is found in subject_train.txt and the activiy id in ytrain.txt. The relation among the files is the order of the records. The idea is to add the subjectID and activityId as columns to the Xtrain dataset.  
      ```
      ## add column "subjectId" into  Xtrain from SubjectTrain

      Xtrain$subjectId <- SubjectTrain$V1

      ## add a column "subjectId" into  Xtest from SubjectTest

      Xtest$subjectId <- SubjectTest$V1
      ```
  4. The same is done to Xtest.txt data because it has the same structure.
  5.  Combine both data.tables with rbind because they have the same number and type of columns into the Xtt data.table.
2.  Extracts only the measurements on the mean and standard deviation for each measurement.
  1. Only variables containing "-mean()", or  "-std()" will be extracted, so the objective is to build a regular expression 
`[-mean()|-std()]` and extract the columns numbers from features. Also you need to keep the columns added in 1 (562:563)
      ```

      toMatch <- c("-mean\\(\\)", "-std\\(\\)")
      colsel <- union(grep(paste(toMatch,collapse="|"),Features$V2,     value=FALSE),(562:563))
      Xtt <- Xtt[ , colsel]
      ```
3.  Uses descriptive activity names to name the activities in the data set.
  1. lowerUpperCamel naming convention is followed for the name of all columns in the data set from now on.  This is because names of the variables are too long and lowerUpperCamel will make them easier to read. Activity Labels are modified by removing punctuation and spaces
      ```

      ActLabels$V2 <- gsub("[[:punct:]]", " ", ActLabels$V2)
      ```
  2. change the the column name in action labels data.table.
      ```

      setnames(ActLabels,  names(ActLabels),  c("activityId", "activity"))
      ```
   3. build the factors and replace the activity column values from acitvityId to activities with descriptive names:
      ```

      activityF <- factor(Xtt$activityId, labels=ActLabels$activity)
      Xtt$activityId <- activityF
      ```
4. Appropriately labels the data set with descriptive variable names.
  1. Build the variables vector with the names 
      ```

      variables <- union(grep(paste(toMatch,collapse="|"),Features$V2, value=TRUE),
                   c("subjectId", "activity"))
      ```
  2.  remove dashes and parentheses 
      ```

      variables <- gsub("[[:punct:]]", "", variables)
      ```
  3. substitute "mean" and "std" to comply with lowerUpperCamel notation and assign the names to the Xtt data.table
      ```

      variables <- gsub("mean","Mean", variables)
      variables <- gsub("std","Std", variables)
      setnames(Xtt, variables)
      ```
5.  Creates a second, independent tidy data set with the average of each variable for each activity and each subject 
   1.  Using functionality of reshape2 package, we melt the data frame  into a narrow dataset without any summary across the variables by activity and subjectId.  It was followed here the order given in the instructions "for each activity and each subject".  Note also that as.is is used to keep the data types unmodified.
      ```

      library(reshape2)
      Xtt <- melt(Xtt,id=c("activity", "subjectId"),
                measure.vars=variables[1:66], as.is=TRUE)
      ```
  2. The tidy data set is created by casting Xtt across the variables and summarizing with the mean by activity and subjectId
      ```

      XttWide <- dcast(Xtt,activity+subjectId~variable, mean)
      ```
  3.  Creates the file with default field separator 
      ```

      write.table(XttWide,"./data/Xttwide.txt",row.names=FALSE)
      ```
 





###References

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

