#
# Create a data directory to contain the original data files.
#
create_data_directory <- function() {
    if (!file.exists("data")) {
        dir.create(path = "data")
    } 
}

#
# Download and extract the data from a given URL.
#
download_and_unzip <- function(url, destfile) {
    if (!file.exists(destfile)) {
        download.file(url = url, destfile = destfile, method = "curl")
    }
    unzip(zipfile = destfile, exdir = "data")
}

#
# Downloads the original data, creates a new tidy data set, and writes the tidy data set to a file.
#
run_analysis <- function() {
    create_data_directory()
    download_and_unzip(
        url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
        destfile = "data/getdata-projectfiles-UCI HAR Dataset.zip")

    features <- read.table(
        file = "data/UCI HAR Dataset/features.txt",
        col.names = c("ID", "Feature"))
    activity_labels <- read.table(
        file = "data/UCI HAR Dataset/activity_labels.txt",
        col.names = c("ID", "Activity"))

    training_set <- create_tidy_data_set(
        data_file = "data/UCI HAR Dataset/train/X_train.txt",
        features = features$Feature,
        data_labels_file = "data/UCI HAR Dataset/train/y_train.txt",
        subject_file = "data/UCI HAR Dataset/train/subject_train.txt",
        activity_factors = activity_labels)
    test_set <- create_tidy_data_set(
        data_file = "data/UCI HAR Dataset/test/X_test.txt",
        features = features$Feature,
        data_labels_file = "data/UCI HAR Dataset/test/y_test.txt",
        subject_file = "data/UCI HAR Dataset/test/subject_test.txt",
        activity_factors = activity_labels)
    # Recombine the training and test data sets.
    data_set <- rbind(training_set, test_set)

    # Write the tidy data.
    write_data_set_file(
        data_set = data_set,
        file = "HAR-dataset.txt")

    # Create summary data set
    means_by_subject_activity <- create_tidy_summary_data_set(data_set)

    # Write the summary data set
    write_data_set_file(
        data_set = means_by_subject_activity,
        file = "HAR-summary.txt")
}

#
# Create a tidy data set from the given input files.
#
create_tidy_data_set <- function(data_file, features, data_labels_file, subject_file, activity_factors) {
    data_set <- read.table(file = data_file)
    names(data_set) <- features
    data_set = subset_on_mean_and_std_columns(data_set)
    names(data_set) <- transform_feature_names(names(data_set))

    subject_column <- read.table(
        file = subject_file,
        col.names = c("Subject"))

    activity_column <- create_activity_column(
        data_set_labels = read.table(
            file = data_labels_file,
            col.names = c("ID")),
        activity_factors = activity_factors)

    cbind(Subject = subject_column, Activity = activity_column, data_set)
}

#
# Filter the data set columns to only consider mean and standard deviation variables.
#
subset_on_mean_and_std_columns <- function(data_set) {
    means_and_std_column_names_logical <- grepl(pattern = "(mean|std)", x = names(data_set), ignore.case = TRUE, perl = TRUE)
    subset(data_set, select = names(data_set)[means_and_std_column_names_logical])
}

#
# Converts the list of activity IDs into human-friendly names.
#
create_activity_column <- function(data_set_labels, activity_factors) {
    # Add a column to help restore ordering after the merge.
    data_set_labels <- cbind(1:nrow(data_set_labels), data_set_labels)
    names(data_set_labels) <- c("Index", "ID")

    # merge() reorders the records based on the "by" column, which invalidates the mapping to the original data set.
    data_set <- merge(   
        x = data_set_labels,
        y = activity_factors,
        by = "ID")
    # Restore the correct ordering, using the fake ID values.
    data_set <- arrange(df = data_set, Index)
    # Only take the human-friendly activity descriptions.
    data_set[, c("Activity")]
}

#
# Reformat feature names to be more descriptive.
#
transform_feature_names <- function(feature_names) {
    feature_names <- lapply(X = feature_names, FUN = sub, pattern = "^t", replacement = "Time")
    feature_names <- lapply(X = feature_names, FUN = sub, pattern = "^f", replacement = "Frequency")

    feature_names <- lapply(X = feature_names, FUN = sub, pattern = "BodyBody", replacement = "Body")
    feature_names <- lapply(X = feature_names, FUN = sub, pattern = "BodyAcc", replacement = "BodyAcceleration")
    feature_names <- lapply(X = feature_names, FUN = sub, pattern = "GravityAcc", replacement = "GravityAcceleration")
    feature_names <- lapply(X = feature_names, FUN = sub, pattern = "gravity", replacement = "Gravity")
    feature_names <- lapply(X = feature_names, FUN = sub, pattern = "Mag", replacement = "Magnitude")

    feature_names <- lapply(X = feature_names, FUN = sub, pattern = "BodyGyro", replacement = "BodyGyroscopeAcceleration")

    feature_names <- lapply(X = feature_names, FUN = sub, pattern = "-mean\\(\\)", replacement = "Mean")
    feature_names <- lapply(X = feature_names, FUN = sub, pattern = "-meanFreq\\(\\)", replacement = "MeanFrequency")
    feature_names <- lapply(X = feature_names, FUN = sub, pattern = "-std\\(\\)", replacement = "StandardDeviation")

    feature_names <-lapply(X = feature_names, FUN = sub, pattern = "-X$", replacement = "OnXAxis")
    feature_names <-lapply(X = feature_names, FUN = sub, pattern = "-Y$", replacement = "OnYAxis")
    feature_names <-lapply(X = feature_names, FUN = sub, pattern = "-Z$", replacement = "OnZAxis")

    feature_names <-lapply(X = feature_names, FUN = sub, pattern = "^angle\\((\\w+),(\\w+)\\)", replacement = "AngleBetween\\1And\\2")
    # Handle corruption in the data where we see an extra close parenthesis in the form "angle(X), y)"
    feature_names <-lapply(X = feature_names, FUN = sub, pattern = "^angle\\((\\w+)\\),(\\w+)\\)", replacement = "AngleBetween\\1And\\2")

    unlist(feature_names)
}

#
# Aggregate the given data set by Subject and Activity.
#
create_tidy_summary_data_set <- function(data_set) {
    aggregate(
        . ~ Activity + Subject,
        data = data_set,
        FUN = mean,
        na.rm = TRUE)
}

#
# Write the given data set to a file.
#
write_data_set_file <- function(data_set, file) {
    write.table(
        x = data_set,
        row.names = FALSE,
        quote = FALSE,
        file = file)
}