setwd("C:/Users/Ethan/Documents/Spring 2017/DS4100/FinalProject")
library("mongolite")
library('e1071');
library('SparseM');
library('tm');
library("SnowballC")
library("RTextTools")

# Establish connection to MongoDB
c <- mongo(collection = "tickets", db = "its")

# Import data from MongoDB
data <- c$find('{ "Description": { "$ne": "" } }')

# Create dataframe with only Service and Description
serv_des <- data[c(3,6)]

# Set indices for training data
train_idx <- sample(1:nrow(serv_des), size=0.7*nrow(serv_des))

# Split data into training and test sets
train <- serv_des[train_idx,]
test <- serv_des[-train_idx,]

# Take a smaller sample of the train set to use
train_idx2 <- sample(1:nrow(serve_des), size=0.1*nrow(train))
train <- train[train_idx2,]

# Take a smaller sample of the test set to use
test_idx <- sample(1:nrow(test), size=0.1*nrow(test))
test <- test[test_idx,]

# Create source vectors  
trainvector <- as.vector(train$Description)
testvector <- as.vector(test$Description)

trainsource <- VectorSource(trainvector)
testsource <- VectorSource(testvector)

# Create corpus
traincorpus <- Corpus(trainsource)
testcorpus <- Corpus(testsource)

# Transform the data sets to remove irrelvant things like whitespace, capitalization, and "stop words"
traincorpus <- tm_map(traincorpus,stripWhitespace)
traincorpus <- tm_map(traincorpus,tolower)
traincorpus <- tm_map(traincorpus, removeWords,stopwords("english"))
traincorpus<- tm_map(traincorpus,removePunctuation)

testcorpus <- tm_map(testcorpus,stripWhitespace)
testcorpus <- tm_map(testcorpus,tolower)
testcorpus <- tm_map(testcorpus, removeWords,stopwords("english"))
testcorpus<- tm_map(testcorpus,removePunctuation)

# Create term document matrix
trainmatrix <- t(TermDocumentMatrix(traincorpus))
trainmatrix <- removeSparseTerms(trainmatrix, sparse = 0.998)
testmatrix <- t(TermDocumentMatrix(testcorpus))
testmatrix <- removeSparseTerms(testmatrix, sparse = 0.998)

# Function to convert the word frequencies to yes (presence) and no (absence) labels
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

# Apply the convert_count function to get final training and testing DTMs
trainNB <- apply(trainmatrix, 2, convert_count)
testNB <- apply(testmatrix, 2, convert_count)

# Train naive Bayes model
model <- naiveBayes(trainNB, as.factor(train$Service), laplace=1)

# Use the model to make predictions for the test data
results <- predict(model, testNB)

correct <- c()
for (i in 1:length(results)) {
  correct[i] <- results[i] == test$Service[i]
}

accuracy <- sum(correct)/length(results) * 100

print(paste("Percent correct:", accuracy))
