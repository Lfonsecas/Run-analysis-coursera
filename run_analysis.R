
## R script called run_analysis.R that does the following:

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average 
# of each variable for each activity and each subject.


rm(list=ls())

install.packages("data.table")
install.packages("reshape2")

library(data.table)
library(reshape2)

if(!file.exists("./data")){dir.create("./data")}

## Loading the features and activity labels

features<-read.table("./data/UCI HAR Dataset/features.txt", sep="", header = FALSE)

activity_labels<-read.table("./data/UCI HAR Dataset/activity_labels.txt", sep="", header = FALSE)

extract_features <- grepl("mean|std", features[,2])

activityLabels<-function(x){
  if(x==1){
    x<-activity_labels$V2[1]
  } else if (x==2) {x<-activity_labels$V2[2]
  } else if (x==3) {x<-activity_labels$V2[3]
  } else if (x==4) {x<-activity_labels$V2[4]
  } else if (x==5) {x<-activity_labels$V2[5]
    } else if (x==6) {x<-activity_labels$V2[6]}
}

## Organizing the test dataset

X_test<-read.table("./data/UCI HAR Dataset/test/X_test.txt", sep="", header = FALSE)

y_test<-read.table("./data/UCI HAR Dataset/test/y_test.txt", sep="", header = FALSE)

y_test1<-sapply(y_test[,1], activityLabels)

y_test<-cbind(y_test, y_test1)

subject_test<-read.table("./data/UCI HAR Dataset/test/subject_test.txt", sep="", header = FALSE)

colnames(X_test)<- features[,2]

X_test<-X_test[, extract_features]

Data_test<-cbind(subject_test, y_test, X_test)

colnames(Data_test)[1:3]<-c("subject", "Activity_ID", "Activity_label")


## Organizing the train dataset

X_train<-read.table("./data/UCI HAR Dataset/train/X_train.txt", sep="", header = FALSE)

y_train<-read.table("./data/UCI HAR Dataset/train/y_train.txt", sep="", header = FALSE)

y_train1<-sapply(y_train[,1], activityLabels)

y_train<-cbind(y_train, y_train1)

subject_train<-read.table("./data/UCI HAR Dataset/train/subject_train.txt", sep="", header = FALSE)

colnames(X_train) <- features[,2]

X_train<-X_train[, extract_features]

Data_train<-cbind(subject_train, y_train, X_train)

colnames(Data_train)[1:3]<-c("subject", "Activity_ID", "Activity_label")

## Merging both datasets

Data<-rbind(Data_test, Data_train)

## Tidy data set with the average of each variable for each activity and each subject

idl = c("subject", "Activity_ID", "Activity_label")
labels = setdiff(colnames(Data), idl)
melt_data = melt(Data, id = idl, measure.vars = labels)
tidy_data = dcast(melt_data, subject + Activity_label ~ variable, mean) 

write.table(tidy_data, file = "./data/tidy_data.txt", row.name = FALSE)





