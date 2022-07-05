# Loading packages ----

library(tidyverse)
library(plotly)

# Reading data ----

# Packages - Reading CSV data and unnesting one package per line

packages <- read.csv('data/packages.csv',  na.strings=c("NA","NaN", ""))  %>% 
  separate_rows(products) %>%
  filter(!products %in% c('oid', ':', ''))

# Products - Reading CSV data

products <- read.csv('data/products.csv',  na.strings=c("NA","NaN", ""))

# Shipments - Reading CSV data and unnesting one package per line

shipments <- read.csv('data/shipments.csv', na.strings=c("NA","NaN", ""))  %>% 
  separate_rows(packages) %>%
  filter(!packages %in% c('oid', ':', ''))

# Checking variable names ----

colnames(packages)
colnames(products)
colnames(shipments)

# Ensuring data quality ----

# Packages ----

sapply(packages, function(y) sum(length(which(is.na(y))))) %>% as.data.frame()

# Distributions ----

summary(packages)

# Products ----

sapply(products, function(y) sum(length(which(is.na(y))))) %>% as.data.frame()

# Distributions per column ----

summary(products)

# Vat presents uncommon values, could be outliers. 

# Shipments ----

sapply(shipments, function(y) sum(length(which(is.na(y))))) %>% as.data.frame()

# Distributions per column ----

summary(shipments)

# Is it possible to present NAs on finalweight?

# Exporting final data to /data

saveRDS(packages, 'data/packages.rds')
saveRDS(products, 'data/products.rds')
saveRDS(shipments, 'data/shipments.rds')

