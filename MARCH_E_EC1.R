require(XML)
require(plyr)
require(lubridate)

# parse the xml file
xmlfile <- xmlTreeParse("http://aiweb.cs.washington.edu/research/projects/xmltk/xmldata/data/auctions/ebay.xml")

# set the root of the file
root <- xmlRoot(xmlfile)

# get the id's of the sales and store them in a vector
id <- c()
for (i in 1:5) {
  id[i] <- as.numeric(xmlValue(root[[i]][[5]][[11]]))
}

# get the number of bids on each sale and store them in a vector
num_bids <- c()
for (i in 1:5) {
  num_bids[i] <- as.numeric(xmlValue(root[[i]][[5]][[5]]))
}

# combine the two into a data frame
bids <- data.frame(id,num_bids)

# Function: moreFiveBids()
# Counts how many auctions have more than 5 bids
# Args: NA
# Returns: number of auction with more than 5 bids (numeric)
moreFiveBids <- function() {
  result <- length(which(bids$num_bids > 5))
  return(result)
}

# import and clean data
trade_data <- xmlToDataFrame("http://www.barchartmarketdata.com/data-samples/getHistory15.xml")
trade_data <- trade_data[-1, -c(1,2,3)]
rownames(trade_data) <- NULL
trade_data$timestamp <- strptime(trade_data$timestamp, "%Y-%m-%dT%H:%M:%S-05:00")

# Function: highestClosingPrice
# Finds the highest closing price of the security
# Args: NA
# Returns: highest closing price (numeric)
highestClosingPrice <- function() {
  result <- max(trade_data$close)
  return(result)
}

# Function: totalVolume()
# Calculates total volume traded
# Args: NA
# Returns: total volume traded (numeric)
totalVolume <- function() {
  result <- sum(as.numeric(trade_data$volume))
  return(result)
}

# Function: averageVolume()
# Calculates average volume traded each hour
# Args: NA
# Returns: a dataframe of hours and the average volume traded that hour
averageVolume <- function() {
  avg_per_hour <- data.frame(0:23)
  names(avg_per_hour) <- c("hour")
  for (i in 0:23) {
    trades <- trade_data[which(hour(trade_data$timestamp) == i),]
    avg <- mean(as.numeric(trades$volume))
    avg_per_hour$volume[i+1] <- avg
  }
  return(avg_per_hour)
}
