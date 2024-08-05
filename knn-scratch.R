suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
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
  result <- as.character(sort(table(votes), decreasing = TRUE)[1])
  confidence <- as.numeric(max(table(votes))) / k
  
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