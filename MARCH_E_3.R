setwd("C:/Users/Ethan/Documents/Spring 2017/DS4100/HW3")
library(lubridate)

## Function: importData
###### checks if data has been imported yet and if it hasn't imports delay data into a dataframe
## Args: NA
## Returns: a message saying whether or not the data has already been imported
importData <- function() {
  if (!exists("bird_strikes")) {
    bird_strikes <<- read.csv("Bird Strikes.csv", header=TRUE, stringsAsFactors=FALSE)
    print("Data imported")
  } else {
    print("The data has already been imported")
  }
}

# import the data
importData()

# replace convert FlightDate entries to POSIXct
birdstrikes$FlightDate <- as.POSIXct(birdstrikes$FlightDate, format="%m/%d/%Y")

## Function: mostStrikesInAYear
###### finds the year with the most bird strike incidents
## Args: NA
## Returns: the year in which the most bird strike incidents occurred
mostStrikesInAYear <- function() {
  strikes <- as.data.frame(table(year(bird_strikes$FlightDate)))
  result <- strikes[which(strikes$Freq == max(strikes$Freq)), 1]
  result <- as.numeric(as.character(result))
  return(result)
}

## Function: strikesByYear
###### creates a dataframe containing for each year, the number of bird strike incidents
## Args: NA
## Returns: a dataframe of years and bird strike incidents
strikesByYear <- function() {
  result <- as.data.frame(table(year(bird_strikes$FlightDate)))
  names(result)[1] <- "Year"
  return(result)
}

## Function: strikesByAirline
###### creates a dataframe containing the number of bird strike incidents per airline
## Args: NA
## Returns: a dataframe with columns airline and frequency
strikesByAirline <- function() {
  temp <- as.data.frame(table(bird_strikes$Aircraft..Airline.Operator), stringsAsFactors=FALSE)
  names(temp)[1] <- "Airline"
  temp <- temp[which(temp["Airline"] != "BUSINESS" &
                     temp["Airline"] != "MILITARY"),]
  AirlineStrikes <<- temp
  return(AirlineStrikes)
}

## Function: mostStrike
###### finds the airline that has the most bird strike incidents
## Args:
###### AirlineStrikes - a dataframe containing airlines and the number of incidents they've had
## Returns: the name of the airline with the most incidents
mostStrikes <- function(airline_strikes) {
  no_unknown <- AirlineStrikes[which(AirlineStrikes["Airline"] != "UNKNOWN"),]
  result <- no_unknown[which(no_unknown["Freq"] == max(no_unknown["Freq"])),1]
  return(result)
}