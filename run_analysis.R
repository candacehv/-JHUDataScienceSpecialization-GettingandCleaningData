# ==============================
# Version: 1.0
# Author: Candacehv
# Date: Jan 9, 2020

# Description: 
# Demonstrate data collection, manipulation, and cleaning of a data set.

# Initiate by typing main() into the console

# Steps: 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each +
#   measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with +
#   the average of each variable for each activity and each subject.
# ===============================


library(readr)
library(dplyr)
library(data.table)
library(purrr)

# readMergeData() loads in the UCI HAR datasets. Then merges all datasets into a
# single broad tidy dataset. 

readMergeData <- function() {
  # training data
  xtrain <- read.table("X_train.txt")
  ytrain <- read.table("y_train.txt")
  subject_train <- read.table("subject_train.txt")
  
  # testing data
  xtest <- read.table("X_test.txt")
  ytest <- read.table("y_test.txt")
  subject_test <- read.table("subject_test.txt")
  
  train <- cbind(subject_train, ytrain, xtrain)
  test <- cbind(subject_test,  ytest, xtest)
  data <- rbind(train, test)
  
  # Return dataset
  data
}


# renameVars() function renames all variables with descriptive names from the 
# 'features.txt' file.
renameVars <- function(data){
  # descriptive labels for variables
  features <-  read.table("features.txt")
  features <- unlist(features[2], use.names = F)
  
  setnames(data, 1:2, c("SubjectID", "Activity"))
  setnames(data, 3:563, features)
  
  # Return dataset
  data
}


# subsetCols() uses regex to find mean and std cols only and creates temporary
# df with only relevant variables
subsetCols <- function(data){
  means <- grep("\\bmean()\\b", colnames(data))
  stds <- grep("\\bstd()\\b", colnames(data))
  meansandstds <- c(unlist(means, use.names = F), unlist(stds, use.names = F))
  meansandstds <- sort(meansandstds)
  
  # create new df with only relevant columns (means and stds)
  relcols <- c(1, 2, meansandstds)
  reldata <- data[, relcols]
  
  #return relevant data dataset
  reldata
}

# renameActivity() replaces non-descriptive Activity factors with descriptive names
renameActivity <- function(reldata) {
  # descriptive labels for activities measured
  actdata <- read.table("activity_labels.txt")
  
  reldata$Activity <- gsub("1", actdata[1,2], reldata$Activity)
  reldata$Activity <- gsub("2", actdata[2,2], reldata$Activity)
  reldata$Activity <- gsub("3", actdata[3,2], reldata$Activity)
  reldata$Activity <- gsub("4", actdata[4,2], reldata$Activity)
  reldata$Activity <- gsub("5", actdata[5,2], reldata$Activity)
  reldata$Activity <- gsub("6", actdata[6,2], reldata$Activity)
  
  #return reldata with updated activity categorical data descriptions
  reldata
}

# createFinal <-summarizes the data set: Group by SubjectID and Activity, then get the mean of
# each of these groupings for all variables. 
createFinal <- function(reldata) {
  finalData <- reldata %>% 
    group_by(SubjectID, Activity) %>%
    summarize_all("mean")
  
  # Return final dataset
  finalData
}

# saveData() writes the data to file. 
saveData <- function(finalData){
  print(finalData)
  write.table(finalData, "final_data.txt", append = FALSE, sep = ",", dec = ".",
              row.names = F, col.names = TRUE)
}

main <- function(){
  allData <- readMergeData()
  allData <- renameVars(allData)
  tempData <- subsetCols(allData)
  tempData <- renameActivity(tempData)
  finalDataset <- createFinal(tempData)
  saveData(finalDataset)
}
