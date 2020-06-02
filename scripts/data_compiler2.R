library(RCurl)
library(rgdax)
library(reticulate)
library(rgdax)
library(dplyr)
library(tidyverse)
library(rlang)


start_date <- Sys.Date() - 180

#Get seq of dates for tickers
price_range <- seq(start_date, Sys.Date() , "days")

#Get only UsD
coinbase_data <- public_info() %>%
  subset(quote_currency == "USD")

base_data <- NULL
print(length(coinbase_data$id))
for (i in coinbase_data$id){
  print(i)
  for (d in 1:(length(price_range)-1)){
      
    Sys.sleep(5)
    skip <- F
    
    coin_data <- tryCatch({
      public_candles(i,granularity = 300,start = price_range[d],end = price_range[d+1]) %>% 
        add_column(symbol = i)
    }, error = function(e) {
      skip <<- T
      print(paste0("Error: ", e))
    })
    
    if(skip){
      next
    }
    
    base_data  <- rbind(base_data,coin_data)
  }
  
}

base_data$symbol <- gsub("-USD","",base_data$symbol)

base_data$time <- as.Date(base_data$time)
base_data <- base_data[order(base_data$symbol,base_data$time),]
base_data <- rename(base_data,
                    date = time
)
base_data <- unique(base_data)

write_rds(base_data,"data/crypto.RDS")