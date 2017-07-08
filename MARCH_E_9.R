setwd("C:/Users/Ethan/Documents/Spring 2017/DS4100/HW9")
library(openxlsx)

## Function: importData
###### checks if data has been imported yet and if it hasn't imports delay data into a dataframe
## Args: NA
## Returns: a message saying whether or not the data has already been imported
importData <- function() {
  if (!exists("spring15") && !exists("fall15")) {
    fall15 <<- read.xlsx("grades_data.xlsx", sheet = 2, startRow = 2, colNames = TRUE)
    spring15 <<- read.xlsx("grades_data.xlsx", sheet = 3, startRow = 3, colNames = TRUE)
    print("Data imported")
  } else {
    print("The data has already been imported")
  }
}

## create combined dataframe with Score and Excel Quiz
score_excel <- rbind(fall15[c(5,41)],spring15[c(5,26)])

## Function for calculating statistical mode
## Source: https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Fall Stats
mean(fall15$Score)
median(fall15$Score)
range(fall15$Score)
getmode(fall15$Score)
sd(fall15$Score)

# Spring Stats
mean(spring15$Score)
median(spring15$Score)
range(spring15$Score)
getmode(spring15$Score)
sd(spring15$Score)

# Score Histogram
hist(score_excel$Score)

# Excel Quiz Histogram
hist(score_excel$Excel.Quiz)

# Pearson Moment
cor(score_excel$Score, score_excel$Excel.Quiz, use = "complete.obs", method="pearson")

# Spearman Rank
cor(score_excel$Score, score_excel$Excel.Quiz, use = "complete.obs", method="spearman")
