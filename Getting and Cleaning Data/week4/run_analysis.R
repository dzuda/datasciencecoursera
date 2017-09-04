library(data.table)
library(plyr)

# Read data
testSet <- read.table("UCI HAR Dataset/test/X_test.txt")
testLabels <- read.table("UCI HAR Dataset/test/y_test.txt")
testSubject <- read.table("UCI HAR Dataset/test/subject_test.txt")

trainingSet <- read.table("UCI HAR Dataset/train/X_train.txt")
trainingLabels <- read.table("UCI HAR Dataset/train/y_train.txt")
trainingSubject <- read.table("UCI HAR Dataset/train/subject_train.txt")

features <- read.table("UCI HAR Dataset/features.txt")[,2]
activityLabels <- read.table('UCI HAR Dataset/activity_labels.txt')[,2]

# Appropriately labels the data set with descriptive variable names
names(testSet) <- features
names(testLabels) <- "activityId"
names(testSubject) <- "subjectId"

names(trainingSet) <- features
names(trainingLabels) <- "activityId"
names(trainingSubject) <- "subjectId"

# Uses descriptive activity names to name the activities in the data set
testLabels[,2] = activityLabels[testLabels[,1]]
names(testLabels) <- c('activityId','activityType')

trainingLabels[,2] = activityLabels[trainingLabels[,1]]
names(trainingLabels) <- c('activityId','activityType')

# Extracts only the measurements on the mean and standard deviation for each measurement
featuresExtracted <-grepl("mean|std", features)

testSet <- testSet[, featuresExtracted]
trainingSet <- trainingSet[, featuresExtracted]

# mergedData <- cbind(testExtracted, trainingExtracted)
testData <- cbind(as.data.table(testSubject), testLabels, testSet)
trainingData <- cbind(as.data.table(trainingSubject), trainingLabels, trainingSet)

# Merges the training and the test sets to create one data set
data <- rbind(testData, trainingData)

tidyData <- ddply(data, c("subjectId","activityId"), numcolwise(mean))
write.table(tidyData,file="tidydata.txt", row.names=FALSE)
