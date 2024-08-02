suppressPackageStartupMessages(library(tidyverse))

data <- read_csv('R/k-nearest-neighbors/breast-cancer-wisconsin.csv', show_col_types = FALSE) # nolint
data <- subset(data, select = -c(class))
head(data, 5)

library(caTools)
set.seed(255)

split = sample.split(data$cancer, SplitRatio = 0.75)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)
