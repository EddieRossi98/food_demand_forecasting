# Import packages
#library(ggplot2)
#library(tibble)
#library(dplyr)
#library(knitr)
library(tidyverse)

# Import data
centers = read_csv("data/fulfilment_center_info.csv")
meals = read.csv("data/meal_info.csv")
sales = read.csv("data/train.csv")

# Exploration of centers csv
head(centers)
for 

cat(length(unique(centers$center_id)))
cat(length(unique(centers$city_code)))
cat(length(unique(centers$region_code)))
cat(length(unique(centers$center_type)))

hist(centers$op_area, col = "darkred", border = "white",
     xlab = "Operational area size", main = "Histogram", 
     breaks = 7, probability = T)