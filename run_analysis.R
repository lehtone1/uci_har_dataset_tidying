#' Create two tidy version of the UCI HAR Dataset
#' for human activity recognition
run_analysis <- function() {
  library(dplyr)
  x_merged <- x_merge()
  y_merged <- y_merge()
  subject_merged <- subject_merge()
  x_mean_sd <- extract_mean_sd(x_merged)
  y_labeled <- add_activities(y_merged)
  tidy_set <- cbind(subject_merged, y_labeled, x_mean_sd)
  tidy_set2 <- summarize_activity_subject(tidy_set)
  return(list(tidy_set=tidy_set,tidy_set2=tidy_set2))
}

#' Merges the x values for the  training- and 
#' testingset to create one dataset.Adds column 
#' headers for the data.
x_merge <- function() {
  # Load files
  x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
  x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
  feature_names <- read.table("UCI HAR Dataset/features.txt")
  # Add names
  names(x_train) <- feature_names$V2
  names(x_test) <- feature_names$V2
  # Merge
  x_merged <- rbind(x_train, x_test)
  return(x_merged)
}

#' Merges the y values for the  training and 
#' testingset to create one dataset.
y_merge <- function() {
  # Load files
  y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
  y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
  # Merge
  y_merged <- rbind(y_train, y_test)
  names(y_merged) <- c("activity")
  return(y_merged)
}

#' Merges the subject ID values for the  training 
#' and testingset to create one dataset
subject_merge <-function() {
  train_sub_id <- read.table("UCI HAR Dataset/train/subject_train.txt")
  test_sub_id <- read.table("UCI HAR Dataset/test/subject_test.txt")
  subject_merged <- rbind(train_sub_id, test_sub_id)
  names(subject_merged) <- c("subject")
  return(subject_merged)
}

#' extracts the x columns, that use the mean() or
#' std() function.
#' 
#' @param dataset A dataframe that contains all the x values from the
#' dataset.
extract_mean_sd <- function(dataset) {
  mean_sd_ind <- grep("mean()|std()|subject", names(dataset))
  return(dataset[,mean_sd_ind])
}

#' Changes the numbered activities to textual values
#' 
#' @param dataset  A dataframe that contains all the y values from the
#' dataset.
add_activities <- function(dataset) {
  labels <- read.table("UCI HAR Dataset/activity_labels.txt")
  for(i in 1:dim(labels)[1]) {
    number <- labels[i,]$V1
    word <- labels[i,]$V2
    dataset$activity[dataset$activity== number] = as.character(word)
  }
  return(dataset)
}

#' Creates the tidy_set2 dataset, where the 
#' column values are average values for the 
#' concerned variable for a subject and activity 
#' pair.
#' 
#' @param dataset a dataframe that contains the first tidied version
#' of the UCI HAR Dataset
summarize_activity_subject <- function(dataset) {
  grouped <- group_by(dataset, subject, activity)
  return(summarise_all(.tbl=grouped,.funs=mean))
}