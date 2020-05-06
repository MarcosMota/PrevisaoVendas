
#install.packages('lubridate')
install.packages('ggplot2')

library(dplyr)
library(lubridate)
library(ggplot2)

loadDataSet = function(include.item = TRUE){
  # load sales
  sales = read.csv('data/sales_train.csv')
  
  # load sales
  sales$date = as.Date(sales$date, "%d.%m.%Y")
  sales$year = year(sales$date)
  sales$year =  as.factor(sales$year)
  
  sales$month = month(sales$date)
  sales$month = as.factor(sales$month)
  
  sales$day = day(sales$date)
  sales$day = as.factor(sales$day)
  
  sales$weekdays =  weekdays(sales$date)
  sales$weekdays = as.factor(sales$weekdays)
  
  # sales$shop_id = as.factor(sales$shop_id)
  # sales$item_id =  as.factor(sales$item_id)
  
  if(include.item){
    items_sales = read.csv('data/items.csv')  
    sales = merge(sales, items_sales[,c("item_id", "item_category_id")], by = "item_id", all.x = T)  
    sales$item_category_id =  as.factor(sales$item_category_id)
  }

  return(sales)
}

df <- loadDataSet()

glimpse(df)

sales_shopwise
sales_shopwise = df %>%
    select(shop_id, item_cnt_day) %>%
    group_by(shop_id) %>%
    summarise(item_cnt_day =  sum(item_cnt_day, na.rm = T))

ggplot(data =  sales_shopwise, 
       mapping = aes(x = reorder(shop_id, item_cnt_day), 
                     y = item_cnt_day, 
                     fill = factor(shop_id))) +
    geom_histogram(stat = "identity", color = "yellow") +
    # coord_flip() +
    xlab("Shop ID") + ylab("Sales Count")+
    # geom_label(stat = "identity",position = position_dodge(width = 1),hjust = "center", aes(label = item_cnt_day)) +
    ggtitle(label = "Shop wise sales") +
    theme(
        # get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
        # Change plot and panel background
        plot.background=element_rect(fill = "black"),
        panel.background = element_rect(fill = 'black'),
        # Change legend 
        # legend.position = c(0.6, 0.07),
        # legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(color = "gray", fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        # align title to top center, top ledt is by default.
        plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
        # axis ticks to bold black
        axis.text=element_text(colour = "yellow",face = "bold"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white")
    ) 


