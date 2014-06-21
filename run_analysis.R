# The filePath for the data located.
# The fileNameSuffix indicates test or training file.
# This function do :
# 1.read the datafile
# 2.extract the data
# 3.add ActivityID/SubjectID columns
readDataAndExtractData <- function(fileNameSuffix, filePath) {
    
    
    #read y_data
    fileName <- file.path(filePath, paste0("y_", fileNameSuffix, ".txt"))
    y_data <- read.table(fileName, header=F, col.names=c("ActivityID"))
    
    #read subjects
    fileName <- file.path(filePath, paste0("subject_", fileNameSuffix, ".txt"))
    subject_data <- read.table(fileName, header=F, col.names=c("SubjectID"))
    
    # read the column names
    data_cols <- read.table("features.txt", header=F, as.is=T, col.names=c("MeasureID", "MeasureName"))
    
    # read the x data file
    fileName <- file.path(filePath, paste0("X_", fileNameSuffix, ".txt"))
    print(paste("    Read the data file:" ,fileName) )
    data <- read.table(fileName, header=F, col.names=data_cols$MeasureName)
    
    # column names needed for extract the subset
    meanAndStdCols <- grep(".*mean\\(\\)|.*std\\(\\)", data_cols$MeasureName)
    
    # subset the data (done early to save memory)
    data <- data[,meanAndStdCols]
    
    # append the activity id and subject id columns
    data$ActivityID <- y_data$ActivityID
    data$SubjectID <- subject_data$SubjectID
    
    # return the data
    data
}



# call readDataAndExtractData to read test & train data, and merger it.
readAndMergeData <- function() {
    rbind(readDataAndExtractData("test","test"), readDataAndExtractData("train","train"))
}

# change some column name, make it more readable.
applyAppropriatelyLabel<-function(data) {
    
    cnames <- colnames(data)
    cnames <- gsub("\\.+mean\\.+", cnames, replacement="Mean")
    cnames <- gsub("\\.+std\\.+",  cnames, replacement="StandardDeviation")
    colnames(data) <- cnames
    data
}

# Add the activity names as another column
applyActivityLabel <- function(data) {
    actlabels <- read.table("activity_labels.txt", header=F, as.is=T, col.names=c("ActivityID", "ActivityName"))
    actlabels$ActivityName <- as.factor(actlabels$ActivityName)
    dataWithLabel <- merge(data, actlabels)
    dataWithLabel
}


# Create a tidy data set that has the average of each variable for each activity and each subject.
getTidyData <- function(data) {
    library(reshape2)
    
    # melt the dataset
    id_vars = c("ActivityID", "ActivityName", "SubjectID")
    measure_vars = setdiff(colnames(data), id_vars)
    melted_data <- melt(data, id=id_vars, measure.vars=measure_vars)
    
    # recast 
    dcast(melted_data, ActivityName + SubjectID ~ variable, mean)    
}

# Create the tidy data set and save it on to the named file
SaveTidyDataToFile <- function(data,fileName) {
    write.table(data, fileName)
}




# this is where I put the data file
#setwd("C:/R/UCI HAR Dataset")

##  You should create one R script called run_analysis.R that does the following. 
##  1. Merges the training and the test sets to create one data set.
##  2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#extradcting is done while read the data.
print("1/2. read and merge data")
data<-readAndMergeData()

##  3. Uses descriptive activity names to name the activities in the data set
print("3.applyActivityLabel()")
data<-applyActivityLabel(data)

##  4. Appropriately labels the data set with descriptive variable names. 
print("4.applyAppropriatelyLabel()")
data<-applyAppropriatelyLabel(data)

##  5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
print("5.getTidyData()/Save data to file:TidyData.txt")
data<-getTidyData(data)
SaveTidyDataToFile(data,"TidyData.txt")

print("Done.")
