suppressPackageStartupMessages(library(tidyverse))

data <- read_csv('R/k-nearest-neighbors/breast-cancer-wisconsin.csv', show_col_types = FALSE) # nolint

data <- subset(data, select = -c(id))

data <- as.data.frame(lapply(data, function(x) {
  if(is.factor(x) || is.character(x)) {
    as.numeric(as.character(x))
  } else {
    as.numeric(x)
  }
}))

data[is.na(data)] <- -99999

head(data, 5)

suppressPackageStartupMessages(library(caret))
set.seed(255)

data$class <- factor(data$class, levels = c(2, 4))

index <- createDataPartition(data$class, times = 1, p = 0.8, list = FALSE)

train <- data[index,]
test <- data[-index,]

preprocessing <- preProcess(train, method = c("center", "scale"))
trainTransformed <- predict(preprocessing, train)
testTransformed <- predict(preprocessing, test)

knnModel <- train(
  class ~ ., 
  data = trainTransformed, 
  method = "knn", 
  trControl = trainControl(method = "cv"), 
  tuneGrid = data.frame(k = c(3,5,7))
)

best <- knn(
  class ~ .,
  data = trainTransformed,
  k = knnModel$bestTune$k
)