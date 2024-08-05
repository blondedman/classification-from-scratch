suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(stringr))

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