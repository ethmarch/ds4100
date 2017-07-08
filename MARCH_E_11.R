setwd("C:/Users/Ethan/Documents/Spring 2017/DS4100/HW11")
library(dplyr)

## Function: importData
###### checks if data has been imported yet and if it hasn't imports delay data into a dataframe
## Args: NA
## Returns: a message saying whether or not the data has already been imported
importData <- function() {
  if (!exists("titanic")) {
    titanic <<- read.csv("titanic_data.csv", header=TRUE, na.strings = c(""))
    print("Data imported")
  } else {
    print("The data has already been imported")
  }
}

importData()

# check for missing data
sapply(titanic, function(x) sum(is.na(x)))

# take subset of data with columns with too many missing values or irrelevant data removed
titanic2 <- subset(titanic, select=c(2,3,5,6,7,8,10,12))

# split the data into training and test sets
train <- sample_frac(titanic2, 0.5)
sid <- as.numeric(rownames(train))
test <- titanic2[-sid,]

# initial model with all variables
pred_all <- glm(Survived ~ ., family=binomial(link='logit'), data=train)
summary(pred)

# optimized model with only statistically significant parameters
pred_op <- glm(Survived ~ Pclass + Sex + Age + SibSp, family=binomial(link='logit'), data=train)

# Use the model to predict survival in the test data
predictions <- predict(pred_op, test, type='response')

# round all predictions greater than 0.5 up to one and the rest down to 0
predictions <- ifelse(predictions > 0.5, 1, 0)

# calculate the percent accuracy
wrong_class <- mean(predictions != test$Survived, na.rm = TRUE)
accuracy <- 1 - wrong_class

print(paste("The model has an accuracy of ",accuracy,".", sep = ""))


