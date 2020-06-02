library(RCurl)
library(rgdax)
library(reticulate)
library(rgdax)
library(dplyr)
library(tidyverse)
library(rlang)

#Get only UsD
coinbase_data <- public_info() %>%
  subset(quote_currency == "USD")

base_data <- NULL
for (i in coinbase_data$id){
print(i)

coin_data <- public_candles(i,granularity = 86400) %>% 
  add_column(symbol = i)

base_data  <- rbind(base_data,coin_data)

Sys.sleep(1)
  }

base_data$symbol <- gsub("-USD","",base_data$symbol)

base_data$time <- as.Date(base_data$time)
base_data <- base_data[order(base_data$symbol,base_data$time),]
base_data <- base_data %>%
  dplyr::rename(
    date = time
  )

write_rds(base_data,"App/data/crypto.RDS")