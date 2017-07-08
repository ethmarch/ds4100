library(XML)

## Function: importData
###### checks if data has been imported yet and if it hasn't imports delay data into a dataframe
## Args: NA
## Returns: a message saying whether or not the data has already been imported
importData <- function() {
  if (!exists("farmers_markets")) {
    url <- "http://ds4100.weebly.com/uploads/8/6/5/9/8659576/senators.xml"
    senators_df <<- xmlToDataFrame(url)
    print("Data imported")
  } else {
    print("The data has already been imported")
  }
}

## Function: senatorName
###### gets the names of senators from a given state
## Args:
###### state - the two letter state abbreviation
## Returns: a vector containing names of senators for the given state
senatorName <- function(state) {
  senators = c()
  for(i in 1:nrow(senators_df)) {
    if (senators_df[i, "state"] == state) {
      first <- senators_df[i, "first_name"]
      last <- senators_df[i, "last_name"]
      senators <- c(senators, paste(first, last, sep =" "))
    }
  }
  return(senators)
}

## Function: senatorPhone 
###### gets the phone number for a given senator
## Args:
###### first - the first name of the senator
###### last - the last name of the senator
## Returns: the phone number of the entered senator
senatorPhone <- function(first, last) {
  for(i in 1:nrow(senators_df)) {
    if (senators_df[i, "first_name"] == first &
        senators_df[i, "last_name"] == last) {
      results <- senators_df[i, "phone"]
    }
  }
  return(results)
}  


