#set the working directory
setwd("C:/Users/Ethan/Documents/Spring 2017/DS4100/HW2")

## Function: ImportData
###### checks if data has been imported yet and if it hasn't imports delay data into a dataframe
## Args: NA
## Returns: a message saying whether or not the data has already been imported
ImportData <- function() {
  if (!exists("acquisitions")) {
    acquisitions <<- read.csv("acquisitions.csv", header=TRUE)
    print("Data imported")
  } else {
    print("The data has already been imported")
  }
}

## Function: leastInterval()
###### finds the smallest interval between successive investments
## Args: None
## Returns: smallest interval as a time difference in days
leastInterval <- function() {
  dates <- as.Date(acquisitions$Date, format="%m/%d/%Y")
  result <- min(diff(dates))
  return(result)
}