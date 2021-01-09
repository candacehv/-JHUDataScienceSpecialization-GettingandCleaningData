library(readr)
library(dplyr)
library(data.table)
library(purrr)

# Read in data
# xtrain <- read.table("C:/Users/orhol/Desktop/Candace_School/MOOCS/Johns Hopkins Data Science/Getting and Cleaning Data/Week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
# ytrain <- read.table("C:/Users/orhol/Desktop/Candace_School/MOOCS/Johns Hopkins Data Science/Getting and Cleaning Data/Week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
# subject_train <- read.table("C:/Users/orhol/Desktop/Candace_School/MOOCS/Johns Hopkins Data Science/Getting and Cleaning Data/Week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
# xtest <- read.table("C:/Users/orhol/Desktop/Candace_School/MOOCS/Johns Hopkins Data Science/Getting and Cleaning Data/Week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
# ytest <- read.table("C:/Users/orhol/Desktop/Candace_School/MOOCS/Johns Hopkins Data Science/Getting and Cleaning Data/Week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
# subject_test <- read.table("C:/Users/orhol/Desktop/Candace_School/MOOCS/Johns Hopkins Data Science/Getting and Cleaning Data/Week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
# actdata <- read.table("C:/Users/orhol/Desktop/Candace_School/MOOCS/Johns Hopkins Data Science/Getting and Cleaning Data/Week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")

features <-  read.table("C:/Users/orhol/Desktop/Candace_School/MOOCS/Johns Hopkins Data Science/Getting and Cleaning Data/Week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt")
features <- unlist(features[2], use.names = F)

# Data exploration
# print(head(xtrain))
# print(head(ytrain))
# print(head(subject_train))

# Merge data into one tidy dataset
train <- cbind(subject_train, ytrain, xtrain)
test <- cbind(subject_test,  ytest, xtest)
data <- rbind(train, test)

# Show column names of newly merged data sets
# print(colnames(data))

# Update col names to be more descriptive, using features list
setnames(data, 1:2, c("SubjectID", "Activity"))
setnames(data, 3:563, features)


# Use regex to find mean and std cols only
means <- grep("\\bmean()\\b", colnames(data))
stds <- grep("\\bstd()\\b", colnames(data))
meansandstds <- c(unlist(means, use.names = F), unlist(stds, use.names = F))
meansandstds <- sort(meansandstds)
# Validation test: print(meansandstds)

# create new df with only relevant columns (means and stds)
relcols <- c(1, 2, meansandstds)
reldata <- data[, relcols]
tmp <- reldata$Activity


# Replace non-descriptive Activity factors with descriptive names
# TODO: Replace this with apply function
reldata$Activity <- gsub("1", actdata[1,2], reldata$Activity)
reldata$Activity <- gsub("2", actdata[2,2], reldata$Activity)
reldata$Activity <- gsub("3", actdata[3,2], reldata$Activity)
reldata$Activity <- gsub("4", actdata[4,2], reldata$Activity)
reldata$Activity <- gsub("5", actdata[5,2], reldata$Activity)
reldata$Activity <- gsub("6", actdata[6,2], reldata$Activity)


# Summarize the data set: Group by SubjectID and Activity, then get the mean of
# each of these groupings for all variables
finalData <- reldata %>% 
  group_by(SubjectID, Activity) %>%
  summarize_all("mean")


print(finalData)
write.table(finalData, "final_data.txt", append = FALSE, sep = ",", dec = ".",
            row.names = F, col.names = TRUE)
