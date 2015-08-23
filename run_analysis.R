# This R script performs some cleaning on human activity data, recorded
# from subjects performing activities while carrying a Galaxy smartphone.
# The full description of the experiment and data set is available at:
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones


library(plyr)
library(reshape)

download.activity.data = function() {
 
        if (!file.exists("UCI HAR Dataset")) {
                # download the data
                fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
                zipfile="UCI_HAR_data.zip"
                message("Downloading activity data")
                download.file(fileURL, destfile=zipfile, method="curl")
                unzip(zipfile)
        }
}

merge.datasets = function() {
        "Step 1: Merges the training and the test sets to create one data set."
        # Read data
        training.x <- read.table("UCI HAR Dataset/train/X_train.txt")
        training.y <- read.table("UCI HAR Dataset/train/y_train.txt")
        training.subject <- read.table("UCI HAR Dataset/train/subject_train.txt")
        test.x <- read.table("UCI HAR Dataset/test/X_test.txt")
        test.y <- read.table("UCI HAR Dataset/test/y_test.txt")
        test.subject <- read.table("UCI HAR Dataset/test/subject_test.txt")
        # Merge
        merged.x <- rbind(training.x, test.x)
        merged.y <- rbind(training.y, test.y)
        merged.subject <- rbind(training.subject, test.subject)
        # merge train and test datasets and return list
        list(x=merged.x, y=merged.y, subject=merged.subject)
}

extract.mean.and.std = function(df) {
        # Step 2: Extract only the measurements on the mean and 
        # standard deviation for each measurement.
        
        # Read the feature list file
        features <- read.table("UCI HAR Dataset/features.txt")
        # Find the mean and std columns
        mean.col <- sapply(features[,2], function(x) grepl("mean()", x, fixed=T))
        std.col <- sapply(features[,2], function(x) grepl("std()", x, fixed=T))
        # Extract them from the data
        limited.df <- df[, (mean.col | std.col)]
        # Step 4: Label the data set with descriptive variable names.
        colnames(limited.df) <- features[(mean.col | std.col), 2]
        limited.df
}

name.activities = function(df) {
        # Step 3: Add descriptive activity names to name the activities 
        # in the data set
        colnames(df) <- "activity"
        df$activity[df$activity == 1] = "Walking"
        df$activity[df$activity == 2] = "WalkingUpstairs"
        df$activity[df$activity == 3] = "WalkingDownstairs"
        df$activity[df$activity == 4] = "Sitting"
        df$activity[df$activity == 5] = "Standing"
        df$activity[df$activity == 6] = "Laying"
        df
}

bind.data <- function(x, y, subjects) {
        # Combine mean-std values (x), activities (y) and subjects into
        # one data frame.
        cbind(x, y, subjects)
}

rename.features <- function(col) {
        col <- gsub("tBody", "Time.Body", col)
        col <- gsub("tGravity", "Time.Gravity", col)
        
        col <- gsub("fBody", "FFT.Body", col)
        col <- gsub("fGravity", "FFT.Gravity", col)
        
        col <- gsub("\\-mean\\(\\)\\-", ".Mean.", col)
        col <- gsub("\\-std\\(\\)\\-", ".Std.", col)
        
        col <- gsub("\\-mean\\(\\)", ".Mean", col)
        col <- gsub("\\-std\\(\\)", ".Std", col)
        
        return(col)
}

create.tidy.dataset = function(df) {
        # Given X values, y values and subjects, create an independent tidy dataset
        # with the average of each variable for each activity and each subject.
        tidy <- ddply(df, .(subject, activity), function(x) colMeans(x[,1:60]))
        tidy
}

clean.activity.data = function() {
        # Download activity data, if it does not already exist
        download.activity.data()
        # merge training and test datasets. merge.datasets function returns a list
        # of three dataframes: X, y, and subject
        merged <- merge.datasets()
        # Extract only the measurements of the mean and standard deviation for each
        # measurement
        cx <- extract.mean.and.std(merged$x)
        # Name activities
        cy <- name.activities(merged$y)
        # Use descriptive column name for subjects
        colnames(merged$subject) <- c("subject")
        # Combine data frames into one
        combined <- bind.data(cx, cy, merged$subject)
        # Create tidy dataset
        tidy <- create.tidy.dataset(combined)
        
        ## Create a second, independent tidy data set with the average of each variable for each activity and each subject.
        
        tidy.mean <- ddply(melt(tidy, id.vars=c("subject", "activity")), .(subject, activity), summarise, MeanSamples=mean(value))
        write.csv(tidy.mean, file = "tidy.mean.txt",row.names = FALSE)
        
        # Write tidy dataset as txt file       
        write.table(tidy, "UCI_HAR_tidy.txt", row.names=FALSE)
}