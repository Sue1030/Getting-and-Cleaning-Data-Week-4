#setting working directory

setwd('Week4/')

#Reading data from the files
xTrain <- read.table("X_train.txt")
yTrain <- read.table("Y_train.txt")
features <- read.table("features.txt")
activityType <- read.table("activity_labels.txt")
subjectTrain <- read.table("subject_train.txt")

#Assigning column names to the data imported above
colnames(activityType) <- c('activityID', 'activityType');
colnames(subjectTrain) <- "subjectId";
colnames(xTrain) <- features[,2];
colnames(yTrain) <- "activityId";

#Creating the final training data set by combining the yTrain, subjectTrain, and the xTrain
trainingData <- cbind(yTrain,subjectTrain,xTrain);

#Reading in the test data
subjectTest <- read.table ('subject_test.txt')
yTest <- read.table('Y_test.txt')
xTest <- read.table('X_test.txt')

#Assigning column names to the test data
colnames(subjectTest) <- "subjectId";
colnames(xTest) <- features[,2];
colnames(yTest) <- "activityId";

#Creating final test data set by combining the xTest, yTest and subjectTest data
testData <- cbind(yTest,subjectTest,xTest);

#Combining training and test data to create the final data set
finalData <- rbind(trainingData,testData);

colNames <- colnames(finalData);

#Extract only the measurements of mean and standard deviation for each measurement
#Create a logical expression that contains true values for the ID, mean and standard deviation and false for other values

logicalVector = (grepl("activity..",colNames) | gerpl("subject..",colNames) | grepl("mean..",colNames) & !grepl("meanFreq..",colNames))

#Subset the final data table based on teh logical expression above to keep only the desired columns
finalData <- merge(finalData, activityType, by='activityId', all.x=TRUE);
colNames <- colnames(finalData);

#Cleaning up the variable names and labeling the data with descriptive activity names
for (i in 1:length(colNames))
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bbody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude ",colNames[i])
  colNames[i] = gsub("Jerkmag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])

};

#Reassigning the new column names to the finalData data set
colnames(finalData) = colNames;

#Creating the tidy and cleaned up data set with the average of each varaibale for each activity
finalDataNoActivityType <- finalData[,names(finalData) !='activityType'];

#Summarizing the finalDataNoActivityType table to include just the mean
tidyData <- aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId', 'subjectId')], by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId), mean);

#Merging tidy data with activity type
tidyData <- merge(tidyData,activityType,by='activityId',all.x=TRUE);

#Exporting the tidyData data set
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');





                   