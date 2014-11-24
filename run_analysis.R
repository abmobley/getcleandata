run_analysis <- function () {
  #load necessary libraries
  library(dplyr)
  
  #read features and make them unique
  features <- read.table("features.txt", stringsAsFactors = FALSE)
  features <- make.unique(features$V2,"_")
  
  #get the test set and set unique feature names
  #keep only features with mean() and std()
  x_test <- read.table("test/X_test.txt")
  names(x_test) <- features
  x_test <- select(x_test, contains("mean()"), contains("std()"))
  
  #get subjects and activity labels and bind to x_test
  subjects_test <- read.table("test/subject_test.txt")
  names(subjects_test) <- "subject"
  y_test <- read.table("test/y_test.txt")
  names(y_test) <- "activity"
  x_test <- cbind(x_test,subjects_test,y_test)
  
  #get the training set and set unique feature names
  #keep only features with mean() and std()
  x_train <- read.table("train/X_train.txt")
  names(x_train) <- features
  x_train <- select(x_train, contains("mean()"), contains("std()"))
  
  #get subjects and activity labels and bind to x_train
  subjects_train <- read.table("train/subject_train.txt")
  names(subjects_train) <- "subject"
  y_train <- read.table("train/y_train.txt")
  names(y_train) <- "activity"
  x_train <- cbind(x_train,subjects_train,y_train)
  
  #combine training and test sets into one data set
  data <- rbind(x_test,x_train)
  
  #give activities descriptive names instead of numbers
  activity_labels <- read.table("activity_labels.txt",stringsAsFactors = FALSE)
  data$activity <- factor(data$activity, labels = activity_labels$V2)
  
  #group by subject and activity and get mean of each variable
  data <- summarise_each(group_by(data,subject,activity), funs(mean))
  
  #sort data by subject and then activity
  arrange(data, subject, activity)
}
