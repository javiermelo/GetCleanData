##Getting and Cleaning Data Project | Coursera
##======================================


###Study Design

The purpose of this data collection and cleaning data project is to create a summarized data set based on the "Human Activity Recognition Using SmartPhones Dataset Version 1.0" (Anguita et al., 2012).  This dataset is available at:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

And a full description of data at:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The data set result was generated trough the creation of a R script called run_analysis.R that does the following: 

1. Merges the training and the test sets to create one data set
2. Extracts only the measurements on the mean and standard deviation for each measurement
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names 
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject 


> **DATASET FORMAT:**
> - File Name: XttWide.txt 
> - Record lenght: variable
> - Separator: space **" "**
> - Records: 180
> - Header: yes
> - Variables: 68
> - Character variables quoted: yes

### Codebook


Variable  | Position | Description                    
--------- | :--:  | -----
**activity**   |      1  |Activity performed by the volunteer|
|              |         | Values:
|              |         | WALKING
|              |         |WALKING UPSTAIRS
|              |         |WALKING DOWNSTAIRS
|              |         |SITTING
|              |         |STANDING
|              |         |LAYING
**subjectId**  |      2 |Sequential identifying the volunteer
|              |         | Values:
|              |         | (1<= subjectId <= 30)
**tBodyAccMeanX**|      3 |Average of  time domain horizontal body acceleration
**tBodyAccMeanY**|      4 |Average of  time domain vertical body acceleration
**tBodyAccMeanZ**|      5 |Average of  time domain transverse body acceleration
**tBodyAccStdX**|      6 |Standard deviation of  time domain horizontal body acceleration
**tBodyAccStdY**|      7 |Standard deviation of  time domain vertical body acceleration
**tBodyAccStdZ**|      8 |Standard deviation of  time domain transverse body acceleration
**tGravityAccMeanX**|      9 |Average of  time domain horizontal gravity acceleration
**tGravityAccMeanY**|      10 |Average of  time domain vertical gravity acceleration
**tGravityAccMeanZ**|      11 |Average of  time domain transverse gravity acceleration
**tGravityAccStdX**|     12 |Standard deviation of  time domain horizontal gravity acceleration
**tGravityAccStdY**|     13 |Standard deviation of  time domain vertical gravity acceleration
**tGravityAccStdZ**|      14 |Standard deviation of  time domain transverse gravity acceleration
**tBodyAccJerkMeanX**|  15 |Average of  time domain horizontal body jerk 
**tBodyAccJerkMeanY**|  16 |Average of  time domain vertical body jerk
**tBodyAccJerkMeanZ**|  17 |Average of  time domain transverse body jerk
**tBodyAccJerkStdX**|     18 |Standard deviation of  time domain horizontal body jerk
**tBodyAccJerkStdY**|     19 |Standard deviation of  time domain vertical body jerk
**tBodyAccJerkStdZ**|     20 |Standard deviation of  time domain transverse body jerk
**tBodyGyroMeanX**|     21 |Average of  time domain horizontal body angular velocity
**tBodyGyroMeanY**|     22 |Average of  time domain vertical body acceleration
**tBodyGyroMeanZ**|     23 |Average of  time domain transverse body acceleration
**tBodyGyroStdX**|     24 |Standard deviation of time domain horizontal body angular velocity
**tBodyGyroStdY**|     25 |Standard deviation of  time domain vertical body angular velocity
**tBodyGyroStdZ**|     26 |Standard deviation of  time domain transverse body angular velocity
**tBodyGyroJerkMeanX**|     27 |Average of  time domain horizontal body angular jerk
**tBodyGyroJerkMeanY**|     28 |Average of  time domain vertical body angular jerk
**tBodyGyroJerkMeanZ**|     29 |Average of  time domain transverse body angular jerk
**tBodyGyroJerkStdX**|     30 |Standard deviation of time domain horizontal body angular jerk
**tBodyGyroJerkStdY**|     31 |Standard deviation of  time domain vertical body angular jerk
**tBodyGyroJerkStdZ**|     32 |Standard deviation of  time domain 
**tBodyAccMagMean**|     33 |Average of  time domain vector magnitude body acceleration 
**tBodyAccMagStd**|     34 |Standard deviation of  time domain vector magnitude body acceleration 
**tGravityAccMagMean**|     35 |Average of  time domain gravity acceleration vector magnitude 
**tGravityAccMagStd**|     36 |Standard deviation of time domain gravity acceleration vector magnitude
**tBodyAccJerkMagMean**|     37 |Average of  time domain body acceleration jerk vector magnitude 
**tBodyAccJerkMagStd**|     38 |Standard deviation of  time domain body acceleration jerk vector magnitude 
**tBodyGyroMagMean**|     39 |Average of time domain angular velocity vector magnitude 
**tBodyGyroMagStd**|     40 |Standard deviation of time domain angular velocity vector magnitude
**tBodyGyroJerkMagMean**|     41 |Average of time domain body angular jerk vector magnitude 
**tBodyGyroJerkMagStd**|     42 |Standard deviation of  time domain body angular jerk vector magnitude 
**fBodyAccMeanX**|     43 |Average of  frequency domain horizontal body acceleration
**fBodyAccMeanY**|     44 |Average of  frequency domain vertical body acceleration
**fBodyAccMeanZ**|     45 |Average of  frequency domain transverse body acceleration
**fBodyAccStdX**|     46 |Standard deviation of  frequency domain horizontal body acceleration
**fBodyAccStdY**|     47 |Standard deviation of  frequency domain vertical body acceleration
**fBodyAccStdZ**|     48 |Standard deviation of  frequency domain transverse body acceleration
**fBodyAccJerkMeanX**|  49 |Average of  frequency domain horizontal body jerk 
**fBodyAccJerkMeanY**|  50 |Average of  frequency domain vertical body jerk
**fBodyAccJerkMeanZ**|  51 |Average of  frequency domain transverse body jerk
**fBodyAccJerkStdX**|     52 |Standard deviation of  frequency domain horizontal body jerk
**fBodyAccJerkStdY**|     53 |Standard deviation of  frequency domain vertical body jerk
**fBodyAccJerkStdZ**|     54 |Standard deviation of  frequency domain transverse body jerk
**fBodyGyroMeanX**|     55 |Average of  frequency domain horizontal body angular velocity
**fBodyGyroMeanY**|     56 |Average of  frequency domain vertical body acceleration
**fBodyGyroMeanZ**|     57 |Average of  frequency domain transverse body acceleration
**fBodyGyroStdX**|     58 |Standard deviation of frequency domain horizontal body angular velocity
**fBodyGyroStdY**|     59 |Standard deviation of  frequency domain vertical body angular velocity
**fBodyGyroStdZ**|     60 |Standard deviation of  frequency domain transverse body angular velocity
**fBodyAccMagMean**|    61 |Average of  frequency domain vector magnitude body acceleration 
**fBodyAccMagStd**|     62 |Standard deviation of  frequency domain vector magnitude body acceleration 
**fBodyAccJerkMagMean**|     63 |Average of  frequency domain body acceleration jerk vector magnitude 
**fBodyAccJerkMagStd**|     64 |Standard deviation of  frequency domain body acceleration jerk vector magnitude 
**fBodyGyroMagMean**|     65 |Average of frequency domain angular velocity vector magnitude 
**fBodyGyroMagStd**|     66 |Standard deviation of frequency domain angular velocity vector magnitude
**fBodyGyroJerkMagMean**|     67 |Average of frequency domain body angular jerk vector magnitude 
**fBodyGyroJerkMagStd**|     68 |Standard deviation of  frequency domain body angular jerk vector magnitude 
| | |**Note 1: Variables from postion 3 to 68 are normalized and bounded within [-1,1]**| |
| | |**Note 2: Values of the variables are actually averages of the calculated features for each activity performed by each volunteer**


###References

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

