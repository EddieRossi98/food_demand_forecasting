# Import packages

# Import data
centers = read.csv("data/fulfilment_center_info.csv")
meals = read.csv("data/meal_info.csv")
sales = read.csv("data/train.csv")

# The business problem

"A meal delivery company operates in multiple cities. 
 They have various fulfillment centers in these cities for dispatching meal orders to their customers. 
 We need to provide these centers the demand forecasting for upcoming weeks 
 so that these centers will plan the stock of raw materials accordingly."

# Task

"Predict the demand for the next 10 weeks!"

# Exploration of centers csv
head(centers)
"
- **center_id**: Fulfilment identifier
- **city_code**: City id in which the center is located on
- **region_code**: Region id in which the center is located on
- **center_type**: Type of the center
- **op_area**: Size of the operational area
"
for (col in seq(length(colnames(centers)))){
  cat("Unique values in",colnames(centers)[col],": ")
  cat(dim(unique(centers[col]))[1], "\n")
}

hist(centers$op_area, col = "darkred", border = "white",
     xlab = "Operational area size", main = "Histogram", 
     breaks = 7, probability = T)

# Exploration of meals csv
head(meals)
"
- **meal_id**: Meal identifier
- **category**: Category of food
- **cuisine**: Category of cuisine
"
for (col in seq(length(colnames(meals)))){
  cat("Unique values in",colnames(meals)[col],": ")
  cat(dim(unique(meals[col]))[1], "\n")
}


# Exploration of sales csv
head(sales)
"
- **id**: Id of the single transaction
- **week**: Temporal variable, we have 145 unique weeks 
- **center_id**: Fulfilment identifier
- **meal_id**: Meal identifier
- **checkout_price**: Paid price for the product
- **base_price**: Full price of the product without promotion
- **emailer_for_promotion**: Binomial variable, it's the product sent by a promotion email?
- **homepage_featured**: Binomial variable, it's the product on website's homepage?
- **num_orders**: Number of orders for the meal and center (it's our target!)
"
for (col in seq(length(colnames(sales)))){
  cat("Unique values in",colnames(sales)[col],": ")
  cat(dim(unique(sales[col]))[1], "\n")
}

# Create unique dataset
X = merge(sales, centers, by = "center_id")
X = merge(X, meals,by = "meal_id")
head(X)

# Plot corr matrix
corr = cor(X[, c(4,5,6,9,13)])
library(ggplot2)
library(ggcorrplot)
ggcorrplot(corr, hc.order = TRUE, outline.col = "white", 
           colors = c("blue", "white", "darkred"), lab = TRUE) +
           ggtitle("Correlations between numerical variables")
"Num orders is not highly correlated with other numerical variables"

# Work with time series
aggregate_ts <- function(center, dataset) {
  " Given the center_id and dataset returns the total orders from the center ts"
  t1 = dataset[dataset$center_id==center, c(4,9)]
  t2 = aggregate(num_orders ~ week, t1, sum)
  t3 = t2[order(t2$week),]
  return(t3)
}

head(X[order(X$week),])

X[X$center_id==89, c(4,9)]

ts89 <- aggregate_ts(center = 89, dataset = X)
ts89

unique(X[X$center_id == 89 ,"meal_id"])



ggplot(X[X$meal_id ==1062 & X$center_id == 89,], aes(x=week, y=num_orders)) +
       geom_line() + xlab("week")# + ggtitle("Aggregate data for selected center")



plot(X[X$meal_id ==1062 & X$center_id == 89, "num_orders"], type = "l")
i = 0
for (meal in unique(X[X$center_id == 89 ,"meal_id"])){
  i = i + 1
  lines(X[X$meal_id ==meal & X$center_id == 89, "num_orders"], type = "l", col = i)
  
}

lines(X[X$meal_id ==1062 & X$center_id == 89, "num_orders"], type = "l", col = 2)
