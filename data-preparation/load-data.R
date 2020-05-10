
#install.packages("lubridate")
install.packages("ggplot2")


library(dplyr)
library(lubridate)
library(ggplot2)

load_data_set <- function(include_item = TRUE) {
  # load sales
  sales <- read.csv("data/sales_train.csv")

  # load sales
  sales$date <- as.Date(sales$date, "%d.%m.%Y")
  sales$year <- year(sales$date)
  sales$year <-  as.factor(sales$year)

  sales$month <- month(sales$date)
  sales$month <- as.factor(sales$month)

  sales$day <- day(sales$date)
  sales$day <- as.factor(sales$day)

  sales$weekdays <-  weekdays(sales$date)
  sales$weekdays <- as.factor(sales$weekdays)

  #sales$shop_id = as.factor(sales$shop_id)
  #sales$item_id =  as.factor(sales$item_id)

  if(include_item) {
    items_sales <- read.csv("data/items.csv")
    sales <- merge(sales,
        items_sales[, c("item_id", "item_category_id")],
        by = "item_id", all.x = T)
    sales$item_category_id <-  as.factor(sales$item_category_id)
  }

  return(sales)
}

data <- load_data_set()

summary(data)

ggplot(data, aes(x = year, y = item_price)) +
    geom_point()