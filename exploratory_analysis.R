# Import packages
#library(ggplot2)
#library(tibble)
#library(dplyr)
#library(knitr)
library(tidyverse)

# Import data
centers = read_csv("data/fulfilment_center_info.csv")
meals = read_csv("data/meal_info.csv")
sales = read_csv("data/train.csv")

# Exploration of centers csv
head(centers)

for (col in seq(length(colnames(centers)))){
  cat("Unique values in",colnames(centers)[col],": ")
  cat(dim(unique(centers[col]))[1], "\n")
}

hist(centers$op_area, col = "darkred", border = "white",
     xlab = "Operational area size", main = "Histogram", 
     breaks = 7, probability = T)