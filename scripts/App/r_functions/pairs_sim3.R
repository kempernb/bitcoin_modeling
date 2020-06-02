allocate <- function(weights,data,cur_date,cur_portfolio){
  
  #Data prep
  price_data <- data %>%
    subset(date == cur_date)
  
  #order 0 to max then NA
  weights <- weights[,order(colSums(weights))]
  assets <- colnames(weights)

  portfolio_assets <- c(assets,"cash")
  portfolio <- as.data.frame(matrix(ncol = length(portfolio_assets), nrow=1, dimnames = list(NULL,portfolio_assets)))
  
  cur_cash <- tail(cur_portfolio$cash,1)
  spent <- 0
  
  
  for (i in 1:length(assets)){
    
    asset <- assets[i]
    cur_weight <- weights[[asset]]
    cur_price <- price_data[[gsub("_.*","",asset)]]
    #Checks values for weights.
    # >0 buy NA nothing 0 Sell
    #if weight is NA 
    
    if (is.na(cur_weight)){
      
      shairs <- cur_portfolio %>%
        select(asset) %>%
        subset(asset != 0) %>%
        tail(1)
      
      portfolio[[asset]] <- as.numeric(shairs)
      
    } else if (cur_weight != 0){
      
      n_shairs <- ((cur_weight *  cur_cash) / cur_price)
      portfolio[[asset]] <- n_shairs
      
      spent <- spent + n_shairs * cur_price
      
    } else if (cur_weight == 0){
      
      #Get the # of owned shairs
      shairs <- cur_portfolio %>%
        select(asset) %>%
        subset(asset != 0) %>%
        tail(1)
      
      gain <- as.numeric(shairs) * cur_price

      cur_cash <- cur_cash + gain
      portfolio[[asset]] <- 0
    }
  }
  
  # print(cur_cash)
  portfolio[["cash"]] <- cur_cash - spent
  
  portfolio$date <- price_data$date
  
  return(portfolio)
}


#Pairs trading simulation, only will trade 1 preace point at a time
pairs_simulation <- function(pair_data,stock_metadata,start_date,lookback,z_window,starting_cash){
  
  #Subset the metadata & tranlate to wide
  data <- stock_metadata %>%
    subset(date >= (start_date - lookback*2) & ((symbol %in% pair_data$pair1 | symbol %in% pair_data$pair2))) %>%
    select(date,symbol,adjusted) %>%
    spread(symbol,adjusted)
  
  
  if (nrow(data) != nrow(drop_na(data))){
    return("error")
  }
  
  #Empty symbol and weight dataframes
  symbol <- c(pair_data$pair1 , pair_data$pair2)
    #Set up Symbol list
    if (length(symbol) != length(unique(symbol))){
      
      symbol <- generate_names(symbol)
      pair_data$pair1 <- symbol[1:(length(symbol)/2)]
      pair_data$pair2 <- symbol[(length(symbol)/2 + 1):length(symbol)]
  }
  
  weights <- as.data.frame(sapply(symbol, function(symbol) numeric()))
  
  #Set up portfolio
  portfolio <- as.data.frame(matrix(ncol = length(symbol), nrow=1, dimnames = list(NULL,symbol)))
  portfolio[is.na(portfolio)] <- 0
  portfolio$cash <- starting_cash
  portfolio$date <- as_date(start_date)
  
  
  #define constants
  n_pairs <- length(symbol) / 2
  cash <- starting_cash
  dates <- seq(as.Date(start_date),as.Date(Sys.Date()),by = 1)
  
  
  for (i in dates){
    
    #Only run date is in dataframe
    if (!(i %in% data$date)){
      next
    }
    
    #Get current data
    cur_data <- data %>%
      subset(date <= i) %>%
      tail(lookback)
    
    #Set up current weights
    cur_weights <- as.data.frame(matrix(ncol = length(symbol), nrow=1, dimnames = list(NULL,symbol)))
    #Pairs loop
    for (j in 1:nrow(pair_data)){
      
      cur_stocks <- pair_data[j,]

      S1 <- cur_stocks$pair1
      S2 <- cur_stocks$pair2
      
      S1_data <- cur_data[[gsub("_.*","",S1)]]
      S2_data <- cur_data[[gsub("_.*","",S2)]]
      
      reg <- data.frame(S1_data,
                        S2_data,
                        stringsAsFactors = F)
      
      lin_reg <- lm(S1_data~S2_data,data = reg)
      
      b <- lin_reg$coefficients[2]
      
      Z <- S1_data - b * S2_data
      
      if (nrow(cur_data) >= z_window){
        
        z_score <- (tail(Z,1) - mean(Z))/sd(Z)
        
        if (z_score < 0 & cur_stocks$p1){
          pair_data$p2[j] <- F
          pair_data$p1[j] <- F
          
          cur_weights[[S1]] <- 0
          
          next
        }
        
        if (z_score > 0 & cur_stocks$p2){
          pair_data$p2[j] <- F
          pair_data$p1[j] <- F
          
          cur_weights[[S2]] <- 0
          
          next
        }
        
        if (z_score < -1 & !(cur_stocks$p2)){
          pair_data$p1[j] <- T
          pair_data$p2[j] <- F
          
          cur_weights[[S1]] <- 1 / n_pairs
          
          next
        }
        
        if (z_score > 1 & !(cur_stocks$p1)){
          pair_data$p1[j] <- F
          pair_data$p2[j] <- T
          
          cur_weights[[S2]] <- 1 / n_pairs
          
          next
        }
        
      }
    }
    cur_portfolio <- allocate(cur_weights,data,i,portfolio)
    portfolio <- rbind(portfolio,cur_portfolio)
    
    cur_weights$date <- as_date(i)
    weights <- rbind(weights,cur_weights)
  }
  
  #remove first row of portfolio dataframe
  portfolio <- portfolio[-1,]
  
  re_l <- ncol(portfolio)
  shairs <- portfolio[,c(-re_l,-(re_l-1))]
  
  symbols <- (colnames(shairs))
  
  
  for (i in symbols){
    # print(i)
    stock_data <- stock_metadata %>%
      select(symbol,date,adjusted) %>%
      subset(symbol == gsub("_.*","",i))
    
    portfolio_data <- portfolio %>%
      select(i,date)
    
    values <- merge(stock_data,portfolio_data)
    portfolio[[i]]<- values$adjusted * values[[i]]
  }
  
  portfolio$total_value <- portfolio %>%
    select(-"date") %>%
    rowSums()

  outcome <- list(portfolio,weights)
  return(outcome)
}

generate_names <- function(symbols){
  
  for (i in 1:length(symbols)){
    num <- 1
    
    for (j in 1:length(symbols)){
      if (i == j){
        next
      }
      
      if (symbols[i] == symbols[j]){
        
        symbols[j] = paste0(symbols[j],"_",num)
        num <- num +1
      }
      
    }
    
  }
  return(symbols)
}
