
#data cleaning script - 08112022

#load needed packages
library(tidyverse)
library(tidyr)
library(dplyr)

#set directory
setwd('/users/trevor.mattos/desktop/nycdsa/R')

#read in the csv data file
data <- read.csv('/users/trevor.mattos/desktop/nycdsa/R/phones.csv')

#read in the sellers data
sellers<- read.csv('/users/trevor.mattos/desktop/nycdsa/R/vendors.csv')

# perform the join with the sellers page data
data <- left_join(
  data,
  sellers,
  by=c("seller_id"="url")
)


#be sure to only keep unique page IDs

#check for duplicates
data %>%
  group_by(page_id) %>%
  summarise(n=n()) %>%
  arrange(desc(n))

# DROP DUPLICATES
data <- data %>%
  distinct(page_id, .keep_all=TRUE) 


#clean the phone brand data (keep the original column)

data <-data %>%
  mutate(brand=case_when(
    str_detect(title, 'iPhone') ~ 'iPhone',
    str_detect(title, 'Samsung') ~ 'Samsung',
    str_detect(title, 'Xiaomi|Poco|Redmi') ~ 'Xiaomi',
    str_detect(title, 'Motorola') ~ 'Motorola',
    str_detect(title, 'Realme') ~ 'Realme',
    str_detect(title, 'Honor') ~ 'Honor',
    str_detect(title, 'Nokia') ~ 'Nokia',
    str_detect(title, 'Sony') ~ 'Sony',
    str_detect(title, 'Huawei') ~ 'Huawei',
    str_detect(title, 'Oneplus') ~ 'OnePlus',
    TRUE ~ 'Other'
  ))

# derive the new/used status and then the total sales of the item
data <- data %>%
  mutate(condition=case_when(
    str_detect(newstatus_sales, 'Nuevo') ~ 'new',
    str_detect(newstatus_sales, 'Usado') ~ 'used',
    TRUE ~ 'Other'
  ))

#create new column with numeric value for items sold
data$items_sold <- as.numeric(gsub(".*?([0-9]+).*", "\\1", data$newstatus_sales)) 


# create the proportion of total seller sales from item
data <- data %>%
  mutate(sales_perc = 
           round(items_sold/total_seller_sales, digits=3))

# more data cleaning

# 1. convert price in soles to numeric
data <- data %>% 
  rowwise() %>%
  mutate(price_in_soles=str_extract_all(string=price_in_soles, 
                                        pattern="[0-9]+") %>%
           unlist() %>%
           paste0(collapse='.') %>%
           as.numeric()
  ) 
# 2. create a separate column for price in dollars
data <- data %>%
  mutate(price_in_dollars=round(price_in_soles*.2568, digits=2))

# 3. obtain the number of ratings as numeric
data <- data %>% 
  rowwise() %>%
  mutate(total_product_ratings=str_extract_all(string=total_product_ratings, 
                                               pattern="[0-9]+") %>%
           unlist() %>%
           paste0(collapse='') %>%
           as.numeric()
  ) 

# 4. clean units available and convert to numeric
data <- data %>% 
  rowwise() %>%
  mutate(total_units_available=str_extract_all(string=total_units_available, 
                                               pattern="[0-9]+") %>%
           unlist() %>%
           paste0(collapse='') %>%
           as.numeric()
  ) 


#obtain the name of the seller
data <- data %>% 
  rowwise() %>%
  mutate(vendor=str_extract_all(string=seller_id, 
                                pattern="[A-Z]+") %>%
           unlist() %>%
           paste0(collapse='') %>%
           substr(1,7)
  ) 

# clean the total store reviews number 
data %>% 
  rowwise() %>%
  mutate(store_reviews=str_extract_all(string=store_reviews, 
                                       pattern="[0-9]+") %>%
           unlist() %>%
           paste0(collapse='') %>%
           as.numeric()
  ) 

# get good store reviews as numeric
data <- data %>% 
  rowwise() %>%
  mutate(good_str_reviews=str_extract_all(string=good_str_reviews, 
                                          pattern="[0-9]+") %>%
           unlist() %>%
           paste0(collapse='') %>%
           as.numeric()
  ) 
# create percentage of total store reviews that are good store reviews
data <- data %>%
  mutate(goodreview_perc = good_str_reviews/store_reviews ) %>%
  ungroup()


#number of items sold by vendor
data <-inner_join(
  data,
  data %>%
    group_by(vendor) %>%
    summarise(total_item_sales=sum(items_sold, na.rm=TRUE))%>%
    arrange(desc(total_item_sales)),
  by="vendor")

# create an indicator for active sellers
data %>%
  mutate(active_seller=0) %>%
  mutate(active_seller=replace(active_seller, which(total_item_sales>=1),1)) 
  
#export clean csv
write.csv(data,"/users/trevor.mattos/desktop/nycdsa/r/phones_clean.csv", row.names = FALSE)
  
