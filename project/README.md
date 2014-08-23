Human Activity Recognition Using Smartphones
============================================

# Description
The `run_analysis.R` script combines training and test data sets from UCI's [Human Activity Recognition Using Smartphones Data Set] (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip) into a single tidy data set. The newly created data set is written to a file named `HAR-dataset.txt` along with a summary data set file, named `HAR-summary.txt`, containing mean values for each variable. Only mean and standard deviation variables from the original data set are considered.

# Usage
``` splus
source('run_analysis.R')
run_analysis()
```

# Output
1. [HAR-dataset.txt] (https://github.com/mrcodeninja/coursera-getting-and-cleaning-data/blob/master/project/HAR-dataset.txt)

...[Code Book] (https://github.com/mrcodeninja/coursera-getting-and-cleaning-data/blob/master/project/CodeBook.md)
...Tidy data set created using the [Human Activity Recognition Using Smartphones Data Set] (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip) from the [UCI Machine Learning Repository] (http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones).

2. [HAR-summary.txt] (https://github.com/mrcodeninja/coursera-getting-and-cleaning-data/blob/master/project/HAR-summary.txt)

...Aggregated data set containing the mean values of each variable for each activity and each subject.