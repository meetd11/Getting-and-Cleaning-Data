featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)


#Merges the training and the test sets to create one data set.

subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

colnames(features) <- t(featureNames[2])

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)


#Extracts only the measurements on the mean and standard deviation for each measurement.

columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)

extractedData <- completeData[,requiredColumns]
dim(extractedData)


#Uses descriptive activity names to name the activities in the data set

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

extractedData$Activity <- as.factor(extractedData$Activity)

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)


#From the data set, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "tidy.txt", row.names = FALSE, quote = FALSE)