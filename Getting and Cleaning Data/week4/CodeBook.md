---
title: "CodeBook"
output: html_document
---

This CodeBook describes the variables, the data, and any transformations or work that has been performed to clean up the data.

## Source

A full description is available at the site: <http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones>.

The data was obtained from: 
<https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>.


## Data Set Information

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

## Files

* README.txt
* features_info.txt - contains information about the features.
* features.txt - lists all features.
* activity_labels.txt - links the activity Ids with their names.
For the train and test data sets:
* train/X_train.txt - training/test data set.
* train/y_train.txt - training/test labels.
* train/ubject_train.txt - Id of the subject.
The data files in the "Inertial Signals" folders were ignored.

## Preprocessing

1. The training and the test sets were merged to create one data set.
2. Only the measurements on the mean and standard deviation for each measurement were extracted.
3. Descriptive activity names were used to name the activities in the data set.
4. The data set was appropriately labeled with descriptive variable names.
5. From the data set in step 4, a second, independent tidy data set was created swith the average of each variable for each activity and each subject.