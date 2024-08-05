suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))


# defining the knn classifier

knn <- function(data, predict, k=3) {
  
  if (length(data) >= k) {
    warning("idiot")
  }
  
  distances <- list()
  for (group in names(data)) {
    for (features in data[[group]]) {
      euclidean_distance <- sqrt(sum((features - predict) ^ 2))
      distances <- append(distances, list(c(euclidean_distance, group)))
    }
  }
  
  distances <- distances %>% 
    do.call(rbind, .) %>% 
    as.data.frame() %>%
    arrange(V1)
  
  votes <- distances[1:k, 2]
  # cat('votes: ', votes, '\n')
  
  result <- names(table(votes))[which.max(table(votes))]
  # cat('result: ', result, '\n')
  
  confidence <- (max(table(votes))) / k
  
  return(list(result = result, confidence = confidence))
}

# loading the dataset

df <- read.csv('R/breast-cancer-wisconsin/breast-cancer-wisconsin.csv',
               stringsAsFactors = FALSE,
               na.strings = "?") # nolint


# preprocessing the data

df <- df %>% 
  replace(is.na(.), -99999) %>%
  select(-id)

df <- df %>%
  mutate_all(as.numeric) %>%
  as.data.frame()

df_list <- split(df, seq(nrow(df)))           # splitting 
df_list <- df_list[sample(length(df_list))]   # shuffling

split <- 0.2 # split ratio
train <- list('2' = list(), '4' = list())
test <- list('2' = list(), '4' = list())

train_data <- df_list[1:(length(df_list) - floor(split * length(df_list)))]
test_data <- df_list[(length(df_list) - floor(split * length(df_list)) + 1):length(df_list)]

# populate the train set

for (i in train_data) {
  train[[as.character(i$class)]] <- append(train[[as.character(i$class)]], list(i[-ncol(i)]))
}

# populate the test set

for (i in test_data) {
  test[[as.character(i$class)]] <- append(test[[as.character(i$class)]], list(i[-ncol(i)]))
}

correct <- 0
total <- 0

for (group in names(test)) {
  for (data in test[[group]]) {
    KNN <- knn(train, unlist(data), k=10)
    RESULT <- KNN$result
    CONFIDENCE <- KNN$confidence
    if (group == RESULT) {
      correct <- correct + 1
    } else {
      cat('confidence: ', CONFIDENCE, '\n')
    }
    total <- total + 1
  }
}

cat('accuracy: ', correct / total, '\n')