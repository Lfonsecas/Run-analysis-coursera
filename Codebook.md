
CODEBOOK

## Process

The R script called run_analysis.R that does the following:

1. Merges the training and the test sets to create one data set.

2. Extracts only the measurements on the mean and standard deviation for each measurement.

3. Uses descriptive activity names to name the activities in the data set

4. Appropriately labels the data set with descriptive variable names.

5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Variables created in the process for both the test and the train sets

features: name of the columns of the experiment data

activity_labels: labels of activities considered in the experiment dataset.

extract_features: variable created to extract only the mean and the standard deviation from the data.

activityLabels: Function created to match the id of the activities and the label of each activity.

## Variables created for the test set

y_test1: variable created to match the id of activities and the label of activities in the test set. Created by applying the activityLabels function on the variable y_test.

Data_test: dataset for the test set. Concatenates the following objects: the vector with the ID of each activity (subject_test), the label of each activity (y_test1) and the experiment dataset (X_test).

## Variables created for the train set

y_train1: variable created to match the id of activities and the label of activities in the train set. Created by applying the activityLabels function on the variable y_train.

Data_train: concatenated dataset for the train set. Concatenates the following objects: the vector with the ID of each activity (subject_train), the vector with the label of each activity (y_traain1), and the experiment dataset (X_train).

## Output of the script

Data: Merged dataset. Merges the dataset for the test set with the dataset for the train set. 

tidy_data: tidy dataset - takes the average of each variable for each activity and each subject.

