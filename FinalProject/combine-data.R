setwd("C:/Users/Ethan/Documents/Spring 2017/DS4100/FinalProject/data")
library(mongolite)

# get list of all the files to be imported
path <- "C:/Users/Ethan/Documents/Spring 2017/DS4100/FinalProject/data"
fnames <- dir(path, pattern="[a-z]{3}1[4|5|6].csv")

# import all the files and combined them into one data frame
for(i in 1:length(fnames)) {
  if (i == 1) {
    full_data <- read.csv(fnames[i], header = TRUE, stringsAsFactors = FALSE)
  }
  else {
    temp <- read.csv(fnames[i], header = TRUE, stringsAsFactors = FALSE)
    full_data <- rbind(full_data, temp)
  }
}

# Change the names of the full data frame
names(full_data) <- c("Number", "Date", "Service", "Component", "Role", "Description")

# Created new data frame contatining only entries with non-empty descriptions
with_description <- full_data[which(full_data$Description != ""),]

# establish connection to MongoDB
c = mongo(collection = "tickets", db = "its")

# insert data into database
c$insert(full_data)

