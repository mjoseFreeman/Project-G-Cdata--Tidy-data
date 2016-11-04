
fileURL <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL,destfile = "./Dataset.zip", method ="curl")
unzip(zipfile="./Dataset.zip",exdir="./Projectw4")

#set working directory to the location where the UCI HAR Dataset was unzipped
setwd("/Users/Joey/data/Projectw4/UCI HAR Dataset/")
#get the list of files on UCI HAR Dataset
files <-list.files("/Users/Joey/data/Projectw4/UCI HAR Dataset/", recursive = TRUE)
files

#1.Merges the training and the test sets to create one data set.
#1.1 Reading the data from the files 
features<- read.table('./features.txt',header=FALSE); #imports features.txt
activity <-read.table("./activity_labels.txt",header=FALSE)
subjectTrain <-read.table("./train/subject_train.txt",header=FALSE)
trainx <-read.table("./train/x_train.txt",header=FALSE)
trainy <-read.table("./train/y_train.txt",header=FALSE)
subjectTest <-read.table("./test/subject_test.txt",header=FALSE)
testx <-read.table("./test/x_test.txt",header=FALSE)
testy <-read.table("./test/y_test.txt",header=FALSE)

#1.2 Giving variable names to the data
colnames(activity) = c("activityID", "activityType")
colnames(trainx) = features[,2] 
colnames(trainy) = "activityID" 
colnames(subjectTrain) = "subjectID"
colnames(testx) = features[,2] 
colnames(testy) = "activityID" 
colnames(subjectTest) = "subjectID"

#1.3 Merge  training set by merging subjectTrain, trainy, and trainx
trainingData = cbind(subjectTrain,trainy,trainx)
# Merge  test set by merging subjectTest, testy, and testx
testData = cbind(subjectTest,testy,testx)

#1.4 Merge train and test data to create a final data set
allData = rbind(trainingData,testData)

#2. Extract only the measurements on the mean and standard deviation for each measurement.

#2.1 Create a vector for the column names from the finalData, which will be used to select 
#the desired mean() & std() columns
colNames  = colnames(allData) 

#2.2 We are going to find all the columns that have mean, std, activityID and subjectID 
#and create a logical vector
mean_and_std <- (grepl("activityID" , colNames) | 
                         grepl("subjectID" , colNames) | 
                         grepl("mean" , colNames) | 
                         grepl("std" , colNames))

#2.3 Subset allData based on the logicalVector to keep only desired columns
allData = allData[, mean_and_std==TRUE]
#2.4 Checkint the data
str(allData)

#3. Use descriptive activity names to name the activities in the data set
allData = merge(allData, activity, by ="activityID", all=TRUE)
#3.1 Checking if it added the activity type name
str(allData)
#3.2 Checking that it has the 6 levels showed previously
levels(allData$activityType)

#4. Appropriately label the data set with descriptive variable names
names(allData) <-gsub("^t", "time", names(allData))
names(allData)<-gsub("^f", "frequency", names(allData))
names(allData)<-gsub("Acc", "Accelerometer", names(allData))
names(allData)<-gsub("Gyro", "Gyroscope", names(allData))
names(allData)<-gsub("Mag", "Magnitude", names(allData))
names(allData)<-gsub("BodyBody", "Body", names(allData))
#Check if it did it
names(allData)

#5.From the data set in step 4, create a second, independent tidy data set with the 
#average of each variable for each activity and each subject.

library(plyr)
Data2<-aggregate(. ~subjectID + activityType, allData, mean)
Data2<-Data2[order(Data2$subjectID,Data2$activityType),]
write.table(Data2, file = "tidydata.txt",row.name=FALSE)
