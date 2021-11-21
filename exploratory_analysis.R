# Import packages
library(tidyverse)
library(ggplot2)
library(ggcorrplot)

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

kable(head(centers,10)) 
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


# Exploration of meals csv
kable(head(meals,10)) 
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
kable(head(sales,10)) 
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

# Search for NAs
print(sum(is.na(X))) # There are no NA values in dataset

### Univariate analysis

# Numerical variables

# Boxplots

par(mfrow = c(2,2))
boxplot(X$checkout_price, main = "checkout_p", col = "darkred")
boxplot(X$base_price, main = "base_p", col = "darkred")
boxplot(X$op_area, main = "op_area", col = "darkred")
boxplot(X$num_orders, main = "num_orders", col = "darkred")
par(mfrow = c(1,1))

par(mfrow = c(1,2))
boxplot(log(X$num_orders), main = "log(num_orders)", col = "darkred")
boxplot(X$num_orders, main = "num_orders", col = "darkred")
par(mfrow = c(1,1))
"Maybe the high num_orders are alla related to some specific series from a specific
 center and meal"

# Density Hist

par(mfrow = c(2,2))
hist(X$checkout_price, col = "darkred", border = "white",xlab = "Price paid", 
     main = "Histogram", breaks = 8, probability = T)
hist(X$base_price, col = "darkred", border = "white",xlab = "Initial price ", 
     main = "Histogram", breaks = 8, probability = T)
hist(X$op_area, col = "darkred", border = "white",xlab = "Operational area size", 
     main = "Histogram", breaks = 8, probability = T)
par(mfrow = c(1,1))

# Categorical Variables

# Frequency Hist

ggplot(X, aes(x=center_type)) + ggtitle("center_type relative frequencies") +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(..x..))) + 
  scale_y_continuous(labels=scales::percent) + ylab("relative frequencies") + 
  theme(legend.title = element_blank(), legend.position = "none") +
  geom_text(aes( label=paste(as.character(round((..count..)/sum(..count..)*100)),"%",sep = ""),
                 y= (..count..)/sum(..count..) ), stat= "count", vjust = -.3)

ggplot(X, aes(x=category)) + ggtitle("category relative frequencies") +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(..x..))) + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_y_continuous(labels=scales::percent) + ylab("relative frequencies") + 
  theme(legend.title = element_blank(), legend.position = "none") +
  geom_text(aes( label=paste(as.character(round((..count..)/sum(..count..)*100)),"%",sep = ""),
                 y= (..count..)/sum(..count..) ), stat= "count", vjust = -.3)

ggplot(X, aes(x=cuisine)) + ggtitle("cuisine relative frequencies") +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(..x..))) + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_y_continuous(labels=scales::percent) + ylab("relative frequencies") + 
  theme(legend.title = element_blank(), legend.position = "none") +
  geom_text(aes( label=paste(as.character(round((..count..)/sum(..count..)*100)),"%",sep = ""),
                 y= (..count..)/sum(..count..) ), stat= "count", vjust = -.3)

ggplot(X, aes(x=emailer_for_promotion)) + ggtitle("emailer_for_promotion relative frequencies") +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(..x..))) + 
  scale_y_continuous(labels=scales::percent) + ylab("relative frequencies") + 
  theme(legend.title = element_blank(), legend.position = "none") +
  geom_text(aes( label=paste(as.character(round((..count..)/sum(..count..)*100)),"%",sep = ""),
                 y= (..count..)/sum(..count..) ), stat= "count", vjust = -.3)

ggplot(X, aes(x=homepage_featured)) + ggtitle("homepage_featured relative frequencies") +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(..x..))) + 
  scale_y_continuous(labels=scales::percent) + ylab("relative frequencies") + 
  theme(legend.title = element_blank(), legend.position = "none") +
  geom_text(aes( label=paste(as.character(round((..count..)/sum(..count..)*100)),"%",sep = ""),
                 y= (..count..)/sum(..count..) ), stat= "count", vjust = -.3)

"It seems that there are no categories to delete or unify"





### Multivariate analyis

# Plot corr matrix

corr = cor(X[, c(4,5,6,9,13)])
ggcorrplot(corr, hc.order = TRUE, outline.col = "white", 
           colors = c("blue", "white", "darkred"), lab = TRUE) +
  ggtitle("Correlations between numerical variables")
"Num orders is not highly correlated with other numerical variables"

# Numerical variables

p1 = ggplot(X, aes(x=checkout_price, y=num_orders)) + geom_point(color = "blue") + ggtitle("checkout_price vs num_orders")
p2 = ggplot(X, aes(x=base_price, y=num_orders)) + geom_point(color = "blue") +  ggtitle("base_price vs num_orders")
plot_grid(p1, p2, label_size = 12)
"Maybe if there are discounts there are more orders"

discount <- X$base_price-X$checkout_price
ggplot(X, aes(x=discount, y=num_orders)) + geom_point(color = "blue") + ggtitle("base_price vs num_orders")
"Itseems right"

ggplot(X,aes(x=op_area,y=num_orders))+ggtitle("op_area vs num_orders") + geom_point(color = "blue")

# Categorical variables

ggplot(X, aes(x=center_type, y=log(num_orders), fill=center_type)) + geom_boxplot() + 
  theme(legend.title = element_blank(), legend.position = "none") + ggtitle("center_type vs log(num_orders)")

ggplot(X, aes(x=category, y=log(num_orders), fill=category)) + geom_boxplot() + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme(legend.title = element_blank(), legend.position = "none") + ggtitle("category vs log(num_orders)")

ggplot(X, aes(x=cuisine, y=log(num_orders), fill=cuisine)) + geom_boxplot() + 
  theme(legend.title = element_blank(), legend.position = "none") + ggtitle("cuisine vs log(num_orders)")

X$emailer_for_promotion = as.factor(X$emailer_for_promotion) # convert binomial into factor
X$homepage_featured = as.factor(X$homepage_featured) # convert binomial into factor
p1 = ggplot(X, aes(x=emailer_for_promotion, y=log(num_orders), fill=emailer_for_promotion)) + geom_boxplot() + theme(legend.title = element_blank(), legend.position = "none") +
  ggtitle("emailer_for_promotion vs log(num_orders)")
p2 = ggplot(X, aes(x=homepage_featured, y=log(num_orders), fill = homepage_featured)) + geom_boxplot() + theme(legend.title = element_blank(), legend.position = "none") +
  ggtitle("homepage_featured vs log(num_orders)")
plot_grid(p1, p2, label_size = 12)
"Promotions ad email seem to increase the orders"





######## Extra Work with time series

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
