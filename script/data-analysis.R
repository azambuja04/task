# This script contains data analysis

library('tidyverse') # Package for data manipulation
library('lubridate') # Manipulate date variables 
library('plotly') # Package for data visualization

# Reading data ----

# Packages - Reading CSV data

packages <- read.csv('data/packages.csv',  na.strings=c("NA","NaN", ""))

# Products - Reading CSV data

products <- read.csv('data/products.csv',  na.strings=c("NA","NaN", ""))

# Shipments - Read CSV data and fix date formats

shipments <- read.csv('data/shipments.csv', na.strings=c("NA","NaN", "")) %>%
  mutate(createdAt = lubridate::date(createdAt),
         deliverDate = lubridate::date(deliverDate),
         estimatedDeliverDate = lubridate::date(estimatedDeliverDate),
         pickupDate = lubridate::date(pickupDate),
         processDate = lubridate::date(processDate),
         deliveryTime = as.numeric(difftime(deliverDate, createdAt, units = "days")), # Create DeliveryTime as the difference in days between createdAt and deliverDate
         predictedDiff = as.numeric(difftime(estimatedDeliverDate, deliverDate, units = "days")), # Time difference between predicted and delivered dates
         delayed = ifelse(deliverDate > estimatedDeliverDate, 'Delayed', 'On Time')) # Create a variable that classifies delivered orders into Delayed or On Time

# Task minimun requirement ----

# 1. Average delivery time per courier ----

shipments %>% 
  filter(!is.na(deliverDate)) %>% # Remove not delivered orders
  group_by(courier) %>%
  summarise(mean = mean(deliveryTime)) %>%
  arrange(desc(mean))

# DeuschePost presents the highest deliveryTime, followed by transaher and fedex.

# 2. Average delivery time per method ----

shipments %>% 
  filter(!is.na(deliverDate)) %>% # Remove not delivered orders
  group_by(method) %>%
  summarise(mean = mean(deliveryTime)) %>%
  arrange(desc(mean))

# "dhl express - gpt - priority (packet tracked)" presents the highest deliveryTime.

# 3. Average products per order ----

shipments_aux <- shipments %>%
  separate_rows(packages) %>% 
  filter(!packages %in% c('oid', ':', '')) # Unnest and create one row per package

packages_aux <- packages %>% 
  separate_rows(products) %>%
  filter(!products %in% c('oid', ':', '')) # Unnest and create on row per product

shipments_aux %>%
  inner_join(packages_aux, by = c('packages' = 'X_id')) %>% # Join Shipments and Packages data
  group_by(X_id) %>%
  summarise(products = n_distinct(products)) %>% # Count # of products whithin each order
  ungroup() %>%
  summarise(products_per_order = mean(products)) 

# In average, each order presents 2.85 products 

# Aditional analysis ----

# 4. Which are the Most popular Courier and Method

shipments %>% 
  group_by(courier) %>%
  summarise(count = n_distinct(X_id)) %>%
  arrange(desc(count))

shipments %>% 
  group_by(method) %>%
  summarise(count = n_distinct(X_id)) %>%
  arrange(desc(count))

# 5. How accurate is the delivery prediction?

shipments %>% 
  filter(!is.na(delayed)) %>% # Remove not delivered orders
  group_by(delayed) %>%
  summarise(count = n_distinct(X_id)) %>%
  arrange(desc(count))

shipments %>% 
  filter(!is.na(delayed)) %>% # Remove not delivered orders
  group_by(delayed, courier) %>%
  summarise(count = n_distinct(X_id),
            predictedDiff = mean(predictedDiff)) %>%
  arrange(desc(count, courier))

shipments %>% 
  filter(delayed == 'Delayed') %>% # Remove not delivered orders
  group_by(predictedDiff) %>%
  summarise(count = n_distinct(X_id)) %>%
  ungroup() %>%
  mutate(perc = count/sum(count)) # Calculating percentage

# 60% of delayed deliveries presents 1 day of delay

# 6. Which packages have more chance of delay?

# 7. Total order fee cost per day

shipments %>% 
  distinct(X_id, createdAt, )