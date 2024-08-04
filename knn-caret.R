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

index <- createDataPartition(data$class, times = 1, p = 0.2, list = FALSE)

train <- data[index,]
test <- data[-index,]

preprocessing <- preProcess(train, method = c("center", "scale"))
transformed_train <- predict(preprocessing, train)
transformed_test <- predict(preprocessing, test)

model <- train(
  class ~ ., 
  data = transformed_train, 
  method = "knn", 
  trControl = trainControl(method = "cv"), 
  tuneGrid = data.frame(k = c(3,5,7))
)

best <- knn3(
  class ~ .,
  data = transformed_train,
  k = model$bestTune$k
)

predictions <- predict(best, transformed_test, type = "class")

cm <- confusionMatrix(predictions, transformed_test$class)
cm