setwd("C:/Users/Ethan/Documents/Spring 2017/DS4100/HW8")
library("RMySQL")

## Function: importData
###### checks if data has been imported yet and if it hasn't imports delay data into a dataframe
## Args: NA
## Returns: a message saying whether or not the data has already been imported
importData <- function() {
  if (!exists("birdstrikes")) {
    birdstrikes <<- read.csv("Bird Strikes.csv", header=TRUE)
    print("Data imported")
  } else {
    print("The data has already been imported")
  }
}

# import the data
importData()

# convert date column to POSIX format
birdstrikes$FlightDate <- as.POSIXct(birdstrikes$FlightDate, format="%m/%d/%Y")

# create the aircraft table
type_id <- 1:length(levels(birdstrikes$Aircraft..Type))
type <- levels(birdstrikes$Aircraft..Type)
aircraft <- data.frame(type_id, type)
aircraft$type[aircraft$type==""] <- NA

# create the airports table
origins <- birdstrikes[,c("Airport..Name", "Origin.State")]
origins$Airport..Name[origins$Airport..Name=="UNKNOWN"] <- NA
origins$Airport..Name[origins$Airport..Name==""] <- NA
origins <- unique(origins)
origins$origin_id <- 1:nrow(origins)


# create the operators table
op_id <- 1:length(levels(birdstrikes$Aircraft..Airline.Operator))
op_name <- levels(birdstrikes$Aircraft..Airline.Operator)
operators <- data.frame(op_id, op_name)

# create the effects table
effect_code <- 1:length(levels(birdstrikes$Effect..Impact.to.flight))
description <- levels(birdstrikes$Effect..Impact.to.flight)
effects <- data.frame(effect_code, description)
effects$description[effects$description==""] <- NA

# create the incidents table
incidents <- birdstrikes
incidents$Aircraft..Type <- aircraft$type_id[match(incidents$Aircraft..Type, aircraft$type)]
incidents$Effect..Impact.to.flight <- effects$effect_code[match(incidents$Effect..Impact.to.flight, effects$description)]
incidents$Aircraft..Airline.Operator <- operators$op_id[match(incidents$Aircraft..Airline.Operator, operators$op_name)]

getId <- function(x,y) {
  if (is.na(x)) {
    id <- origins$origin_id[which(is.na(origins$Airport..Name) & origins$Origin.State == y)]
  } else {
    id <- origins$origin_id[which(origins$Airport..Name == x & origins$Origin.State == y)]
  }
  id <- as.numeric(id)
  return(id)
}

incidents$Airport..Name[incidents$Airport..Name=="UNKNOWN"] <- NA
incidents$Airport..Name[incidents$Airport..Name==""] <- NA
incidents$origin <- mapply(getId, incidents$Airport..Name, incidents$Origin.State)
incidents <- incidents[-c(2,7)]
incidents$Feet.above.ground[incidents$Feet.above.ground==""] <- NA

write.csv(aircraft,"aircraft.csv", col.names=TRUE, row.names=FALSE, na="NULL", quote=TRUE)
write.csv(effects, "effects.csv",  col.names=TRUE, row.names=FALSE, na="NULL", quote=TRUE)
write.csv(operators, "operators.csv",  col.names=TRUE, row.names=FALSE, na="NULL", quote=TRUE)
write.csv(origins, "origins.csv",  col.names=TRUE, row.names=FALSE, na="NULL", quote=TRUE)
write.csv(incidents, "incidents.csv",  col.names=TRUE, row.names=FALSE, na="NULL", quote=TRUE)

mydb <- dbConnect(MySQL(), user="ds4100", password="homework8", dbname="birdstrikes", host="localhost")

#3
# a)
rsa <- dbGetQuery(mydb, "SELECT count(*) FROM incidents")
a_data <- fetch(rsa, n=-1)

# b)
rsb <- dbGetQuery(mydb, "SELECT airline_name, count(*)
                          FROM operators, incidents
                          WHERE incidents.operator = operators.op_id
                          GROUP BY airline_name")
# c)
rsc <- dbGetQuery(mydb, "SELECT count(*)
                          FROM incidents i, origins o
                          WHERE i.origin = o.origin_id
                            AND o.airport_name = \"LOGAN INTL\"")
# d)
rsd <-dbGetQuery(mydb, "SELECT count(*)
                         FROM incidents
                         WHERE height > 10000")

