setwd("C:/Users/Ethan/Documents/Spring 2017/DS4100/HW4")
library(openxlsx)

## Function: importData
###### checks if data has been imported yet and if it hasn't imports delay data into a dataframe
## Args: NA
## Returns: a message saying whether or not the data has already been imported
importData <- function() {
  if (!exists("farmers_markets")) {
    farmers_markets <<- read.xlsx("Farmers_markets.xlsx", startRow=3)
    print("Data imported")
  } else {
    print("The data has already been imported")
  }
}

#import the data
importData()

# create dataframe with only non-NA values for Season1Date
non_na <- farmers_markets[which(!is.na(farmers_markets["Season1Date"])),]

# three types of date formats:            RE:
# 1  June to September                     ^(\D+)(\s)to(\s)(\D+)
# 2  05/11/2013 to 06/15/2013              ^(\d+)/(\d+)/(\d+)(\s)to(\s)(\d+)/(\d+)/(\d+)
# 3  June 17, 2012 to October 28, 2012     ^(\D+)(\s)(\d+),(\s)(\d+)(\s)to(\s)(\D+)(\s)(\d+),(\s)(\d+)

# create dataframe without rows missing end dates
valid_ranges <- non_na[grep("^(.+)to(\\s)(.+)", non_na[["Season1Date"]], perl=TRUE, value=FALSE),]

# find indexs for each of the types of dates
indx1 <- grepl("^(\\D+)(\\s)to(\\s)(\\D+)", valid_ranges$Season1Date)
indx2 <- grepl("^(\\d+)/(\\d+)/(\\d+)(\\s)to(\\s)(\\d+)/(\\d+)/(\\d+)", valid_ranges$Season1Date)
indx3 <- grepl("^(\\D+)(\\s)(\\d+),(\\s)(\\d+)(\\s)to(\\s)(\\D+)(\\s)(\\d+),(\\s)(\\d+)", valid_ranges$Season1Date)

# split up valid ranges
valid_ranges$Season1Date <- strsplit(valid_ranges$Season1Date, " to ")

# create an array of months
months <- c("January", "February", "March", "April", "May", "June",
            "July", "August", "September", "October", "November", "December")


## Function: acceptsWIC
###### find all the markets which accept WIC
## Args: NA
## Returns: a dataframe containing all of the markets which accept WIC
acceptsWIC <- function() {
  result <- farmers_markets[which(farmers_markets["WIC"] == "Y"),]
  return(result)
}