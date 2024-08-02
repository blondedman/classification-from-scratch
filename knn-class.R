suppressPackageStartupMessages(library(tidyverse))

data <- read_csv('R/k-nearest-neighbors/breast-cancer-wisconsin.csv', show_col_types = FALSE) # nolint
data <- subset(data, select = -c(id))
head(data, 5)

data <- as.data.frame(lapply(data, function(x) {
  if(is.factor(x) || is.character(x)) {
    as.numeric(as.character(x))
  } else {
    as.numeric(x)
  }
}))


library(caTools)
set.seed(255)

split <- sample.split(data$class, SplitRatio = 0.75)
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

scaled_train <- scale(train[-10])
scaled_test <- scale(test[-10])

library(class)
test <- knn(
  train = scaled_train,
  test = scaled_test,
  cl = train$class,
  k=10
)