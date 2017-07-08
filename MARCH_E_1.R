# set working directory
setwd("C:/Users/Ethan/Documents/Spring 2017/DS4100/HW1")

## Function: ImportData
###### checks if data has been imported yet and if it hasn't imports delay data into a dataframe
## Args: NA
## Returns: a message saying whether or not the data has already been imported
ImportData <- function() {
  if (!exists("delays_df")) {
    delays_df <<- read.table("AirlineDelays.txt", header=TRUE, sep=",")
    print("Data imported")
  } else {
    print("The data has already been imported")
  }
}

# Run the import data function
ImportData()

# remove the last column since it does not contain any data
delays_df <- delays_df[-c(14)]

# omit rows with missing data
delays_df <- na.omit(delays_df)

## Function: TotalNumDelays
###### finds and returns total number of delays of a carrier
## Args:
###### carrier (character) - The two-letter abbreviation of the airline who's delays you want to check
## Returns:
###### a numeric describing a carriers total number of delays
TotalNumDelays <- function(carrier) {
  carrier_flights <- delays_df[which(delays_df["CARRIER"] == carrier),]
  result <- length(which(carrier_flights["DEP_DELAY"] > 0 |
                         carrier_flights["ARR_DELAY"] > 0 |
                         carrier_flights["CARRIER_DELAY"] > 0 |
                         carrier_flights["WEATHER_DELAY"] > 0 |
                         carrier_flights["NAS_DELAY"] > 0 |
                         carrier_flights["SECURITY_DELAY"] > 0 |
                         carrier_flights["LATE_AIRCRAFT_DELAY"] > 0))
  return(result)
}

## Function: TotalDelaysByOrigin
###### finds total number of delays from a particular airport
## Args:
###### origin (character) - the 3-letter abbreviation of the airport of origin
## Return:
###### a numeric describing the number of delays from the specified airport
TotalDelaysByOrigin <- function(origin) {
  origin_flights <- delays_df[which(delays_df["ORIGIN"] == origin),]
  result <- length(which(origin_flights["DEP_DELAY"] > 0 |
                         origin_flights["ARR_DELAY"] > 0 |
                         origin_flights["CARRIER_DELAY"] > 0 |
                         origin_flights["WEATHER_DELAY"] > 0 |
                         origin_flights["NAS_DELAY"] > 0 |
                         origin_flights["SECURITY_DELAY"] > 0 |
                         origin_flights["LATE_AIRCRAFT_DELAY"] > 0))
  return(result)
}

## Function: AvgDelay
###### calculates the average delay of a carrier flying into a specific airport
## Args:
###### carrier (charater) - The two-letter abbreviation of the airline who's delays you want to check
###### dest (character) - The 3-letter abbreviation of the destination airport
## Returns:
###### a numeric describing the average arrival delay  minutes of specified carrier into specified airport
AvgDelay <- function(carrier, dest) {
  carrier_arrivals <- delays_df[which(delays_df["CARRIER"] == carrier),]
  results <- mean(carrier_arrivals$ARR_DELAY)
  return(results)
}
