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


library(caTools)
set.seed(255)

split <- sample.split(data$class, SplitRatio = 0.2)
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

scaled_train <- scale(train[,1:10])
scaled_test <- scale(test[,1:10])

library(class)
predict <- knn(
  train = scaled_train,
  test = scaled_test,
  cl = train$class,
  k=1
)

actual <- test$class
cm <- table(actual,predict)
cm

accuracy <- sum(diag(cm))/length(actual)*100 
accuracy