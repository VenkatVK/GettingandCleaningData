# Getting and Cleaning Data Course Project
require("data.table")
require("reshape2")
require("plyr")

# 1. Merges the training and the test sets to create one data set.
# Read Test Data Files
subjtest <- read.table('./UCI HAR Dataset/test/subject_test.txt')
x_test <- read.table('./UCI HAR Dataset/test/x_test.txt')
y_test <- read.table('./UCI HAR Dataset/test/y_test.txt')

# create a set column for DF
set <- 'test'
#merge first two dt's and identify set. using cbind so rows remain in original sort
testbind <- cbind(subjtest,set,y_test)
#merge third dt's
test <- cbind(testbind,x_test)

# Read Train Data Files
subjecttrain <- read.table('./UCI HAR Dataset/train/subject_train.txt')
x_train <- read.table('./UCI HAR Dataset/train/x_train.txt')
y_train <- read.table('./UCI HAR Dataset/train/y_train.txt')

# create a set column for DF
set <- 'train'
#merge first two dt's and identify set. using cbind so rows remain in original sort
trainbind <- cbind(subjecttrain,set,y_train)
#merge third dt's
train<- cbind(trainbind,x_train)

#merge the test and train data tables
testtrain <- rbind(test,train)
# clean up the environment
rm(test,subjtest,y_test,x_test,testbind,train,subjecttrain,y_train,x_train,trainbind)

# Extracts only the measurements on the mean and standard deviation for each measurement.

#read feature column  names
colNames <- read.table('./UCI HAR Dataset/features.txt',stringsAsFactors=FALSE)[[2]]

colNames <- c("subject","set","activity", colNames)

#set remaining column names from features.txt
names(testtrain) <- colNames

# discard data columns without "mean' or "std"
colNames <- names(testtrain)
selectedCols <- c("subject" ,"set", "activity", grep("-(mean|std)\\(\\)", colNames, value=TRUE))
testtrain <- testtrain[,selectedCols]

# Uses descriptive activity names to name the activities in the data set
# replace value in the activity column with their text equivelents from activity_labels.txt
testtrain$activity[testtrain$activity==1] <- "walking"
testtrain$activity[testtrain$activity==2] <- "walking upstairs"
testtrain$activity[testtrain$activity==3] <- "walking downstairs"
testtrain$activity[testtrain$activity==4] <- "sitting"
testtrain$activity[testtrain$activity==5] <- "standing"
testtrain$activity[testtrain$activity==6] <- "laying"

# Appropriately labels the data set with descriptive activity names.

# translate column names
colNames <- names(testtrain)
colNames <- gsub(pattern="^t",replacement="time",x=colNames)
colNames <- gsub(pattern="^f",replacement="freq",x=colNames)
colNames <- gsub(pattern="-?mean[(][)]-?",replacement="Mean",x=colNames)
colNames <- gsub(pattern="-?std[()][)]-?",replacement="Std",x=colNames)
colNames <- gsub(pattern="-?meanFreq[()][)]-?",replacement="MeanFreq",x=colNames)
colNames <- gsub(pattern="BodyBody",replacement="Body",x=colNames) 
names(testtrain) <- colNames

# Creates a second, independent tidy data set with the average of 
#            each variable for each activity and each subject.

# calc average mean and export tidy data set

id_labels   = c("subject","set","activity")
data_labels = setdiff(colnames(testtrain), id_labels)
melt_data      = melt(testtrain, id = id_labels, measure.vars = data_labels)

tidy_data   = dcast(melt_data, subject + activity ~ variable, mean)
write.table(tidy_data, file =  "./tidy_data.txt")
