suppressPackageStartupMessages(library(caret))
set.seed(255)

data$class <- factor(data$class, levels = c(0, 1))

index <- createDataPartition(data$class, times = 1, p = 0.8, list = FALSE)

train <- data[index,]
test <- data[-index,]

preValues <- preProcess(train, method = c("center", "scale"))
trainTransformed <- predict(preValues, train)
testTransformed <- predict(preValues, test)

knnModel <- train(
  not_fully_paid ~ ., 
  data = trainTransformed, 
  method = "knn", 
  trControl = trainControl(method = "cv"), 
  tuneGrid = data.frame(k = c(3,5,7))
)