library(shiny)
library(data.table)
library(plotly)
library(ggplot2)
library(dplyr)
library(timetk)
library(tidyquant)
library(tidyverse)
library(dplyr)
library(quantmod)
library(reticulate)
library(shinycssloaders)
library(Rtsne)
library(shinybusy)
library(corrplot)
library(DT)
library(shinyjs)

use_virtualenv("r-reticulate")
sklearn <- import("sklearn")
#Source python scripts
source_python('python_scripts/find_cointegrated_pairs.py')
base_data <- readRDS("data/crypto.RDS")
source("r_functions/pairs_sim3.R")

wide_data <- base_data %>%
  select(date,symbol,close) %>%
  pivot_wider(names_from = symbol, values_from = close)
wide_data <- wide_data[ , colSums(is.na(wide_data)) == 0]

base_data <- subset(base_data, symbol %in% colnames(wide_data))

crypto_names <- unique(base_data$symbol)

ui <- fluidPage(
useShinyjs(),
navbarPage("Stock Analysis",
  # Sidebar layout with a input and output definitions ----
  navbarMenu("Modeling",
             tabPanel("Heat Map",
  #TSNE Page ----
  sidebarLayout(
    sidebarPanel(
      br(),
      sliderInput("TSNE_daterange",
                  "Date Range:",
                  min = as.Date("2000-01-01","%Y-%m-%d"),
                  max = as.Date(Sys.Date(),"%Y-%m-%d"),
                  value=as.Date(c(Sys.Date()-2*365,Sys.Date())),
                  timeFormat="%Y-%m-%d"),
      br(),
      sliderInput("perplexity","TSNE Perplexity",0,100,30),
      br()
    ),
    mainPanel(
      fluidRow(
        fluidRow(
          withSpinner(
            column(6,
                   plotOutput("coint_heatmap")
            )
          ),
          column(6,
                 DT::dataTableOutput("pairs_table"),
          )
        )    
      )
    ) 
  )
  ),
  tabPanel("Simulation",
           sidebarLayout(
             
             sidebarPanel(
               br(),
               fluidRow(
                 column(6,
                        dateInput("pairs_start_date","Start Date",value = Sys.Date() - 2 * 365)
                 ),
                 column(6,
                        numericInput("pairs_start_cash","Starting Cash",1000)
                 )
               ),
               br(),
               fluidRow(
                 column(6,
                        numericInput("pairs_lookback_window","Lookback Window",30)
                 ),
                 column(6,
                        numericInput("pairs_z_window","Z Window",30)
                 )
               ),
               br(),
               fluidRow(
                 column(6,
                        # sliderInput("pairs_n_pairs","Number of Pairs",1,20,3)
                        fluidRow(
                          column(6,
                                 actionButton("pairs_add","Add")
                          ),
                          column(6,
                                 actionButton("pairs_delete","Delete")
                          )
                        )
                 ),
                 column(6,align = "center",
                        actionButton("pairs_start","Start Simulation")
                 )
                 
               ),
               br(),
               fluidRow(
                 column(12,align = "center",
                        actionButton("import_pairs","Import Pairs")
                 )
               ),
               fluidRow(
                 column(6,
                        uiOutput("pairs_pairs_1")
                 ),
                 column(6,
                        uiOutput("pairs_pairs_2")
                 ),
               )
             ),
             mainPanel(
               fluidRow(
                 column(12,
                        plotlyOutput("pairs_simulation_output"),
                        plotlyOutput("pairs_simulation_weight_output"),
                        )
               )
             ) 
           )
           )  
           ),
  tabPanel("Price",
           sidebarLayout(
             sidebarPanel(),
             mainPanel(
               plotlyOutput("crypto_prices")
             )
           )
           )
  )
)


server <- function(input, output) {
  
  #TSNE Returns -----
  #story generated TSNE data
  TSNE_data <- reactiveVal()
  selected_pairs <- reactiveVal()
  
  output$coint_heatmap <- renderPlot({
    
    log_returns <- base_data %>%
      select(date,symbol,close) %>%
      group_by(symbol) %>%                         # We are grouping the stocks by the stock symbol
      tq_transmute(select = close,
                   mutate_fun = periodReturn,
                   period = 'daily',
                   type = "log",
                   col_rename = 'returns')%>%     
      pivot_wider(names_from = symbol, values_from = returns)
    
    log_returns <- subset(log_returns,date >= input$TSNE_daterange[1] & date <= input$TSNE_daterange[2])
    log_returns <-log_returns[ , colSums(is.na(log_returns)) == 0]

    results <- find_cointegrated_pairs(log_returns[-1])
    
    pairs <- (results[[3]])
    pairs <- sapply(pairs, paste, collapse=",")
    pairs <- paste(pairs,collapse="\n")
    
    output$pairs_table <- DT::renderDataTable({
      
      pairs <- read.csv(text=pairs,header = F)
      colnames(pairs) <- c("pair1","pair2")
      selected_pairs(pairs)
      
      data <- data.frame(
        pair1 = pairs[1],
        pair2 = pairs[2],
        add_sim = shinyInput(actionButton, nrow(pairs), 'button_', label = "Add", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ),
        stringsAsFactors = FALSE
      )
      
      data
    },server = FALSE, escape = FALSE, selection = 'none')
    
    
    p_vals <- as.matrix(results[[2]])
    colnames(p_vals) <- names(log_returns[-1])
    rownames(p_vals) <- names(log_returns[-1])
    
    
    p_vals[p_vals > 0.05] <- 1
    
    p_vals <- 1 - p_vals
    
    corrplot(p_vals,type = "upper",method = "color")
    
  })
  
  #Pairs Simulation ---
  output$pairs_simulation_output <- renderPlotly({
    req(simulation_outcome())
    
    pairs_outcome <- simulation_outcome()[[1]]

    plot1 <- plot_ly(pairs_outcome, x = ~date, y = ~total_value, type = 'scatter', mode = 'lines',colors = "Set1")
  })
  
  output$pairs_simulation_weight_output <-  renderPlotly({
    req(simulation_outcome())
    
    pairs_outcome <- simulation_outcome()[[1]] %>%
      select(-total_value)

    pairs_outcome_scaled <- pairs_outcome %>%
      select(-date)
    pairs_outcome_scaled <- pairs_outcome_scaled / rowSums(pairs_outcome_scaled)
    pairs_outcome_scaled$date <- pairs_outcome$date
    pairs_outcome_scaled <- gather(pairs_outcome_scaled, "symbol", "price", -date)
    pairs_outcome_scaled <- na_if(pairs_outcome_scaled,0) %>%
      drop_na()
    
    plot1 <- plot_ly(pairs_outcome_scaled, x = ~date, y = ~price,color = ~symbol, type = 'scatter',colors = "Set1")
    
    plot1
  })
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  observeEvent(input$select_button, {
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    
    pair_data <- selected_pairs()[selectedRow,]
    
    add_pairs(pair_data)
  })
  
  
  
  
  simulation_outcome <- reactiveVal(NULL)
  n_pairs <- reactiveVal(0)
  pairs <- reactiveVal(NULL)
  
  observeEvent(input$pairs_add,{
    n_pairs(n_pairs() + 1)
    pair <- data.frame(
      pair1 = "",
      pair2 = "",
      stringsAsFactors = F
    )
    pairs(
      rbind(pairs(),pair)
    )
    })
  
  observeEvent(input$pairs_delete,{
    n_pairs(n_pairs() - 1)
    
    data <- pairs()
    
    data <- data[-nrow(data),]
    pairs(data)
  })
  
  output$pairs_pairs_1 <- renderUI({
    if (n_pairs() <= 0){
      return()
    }
    
    pair_data <- pairs() %>%
      select(pair1)
    
    numIndividuals <- as.integer(n_pairs())
    lapply(1:numIndividuals, function(i) {
      selectizeInput(paste0("pair1_",i),"",crypto_names,selected = as.character(pair_data[i,]))
    })
  })
  
  output$pairs_pairs_2 <- renderUI({
    if (n_pairs() <= 0){
      return()
    }
    
    pair_data <- pairs() %>%
      select(pair2)
    
    names_data <- data.frame(
      crypto_names = crypto_names,
      pair2 = gsub(".*/ ","",crypto_names)
    )
    
    pair_data <- merge(names_data,pair_data)
    
    numIndividuals <- as.integer(n_pairs())
    lapply(1:numIndividuals, function(i) {
      selectizeInput(paste0("pair2_",i),"",crypto_names,selected = pair_data$crypto_names[i])
    })
  })
  
  
  
  to_add_pairs <- reactiveVal()
  add_pairs <- function(pairs){
    
    pair1 <- pairs[1]
    pair2 <- pairs[2]
    
    data <- data.frame(
      pair1 = pair1,
      pair2 = pair2,
      stringsAsFactors = F
    )
    
    to_add_pairs(rbind(
      to_add_pairs(),data
    ))
    
  } 
  
  observeEvent(input$import_pairs,{
    req(to_add_pairs())
    n <- nrow(to_add_pairs())
    n_pairs(n_pairs()+n)
    
    pairs(
      rbind(pairs(),to_add_pairs())
    )
    
    to_add_pairs(NULL)
    
  })
  
  observeEvent(input$pairs_start, {
    show_modal_spinner()
    #Build pairs dataframes
    
    num <- as.integer(n_pairs())
    
    pair1 <- sapply(1:num, function(i) { input[[paste0("pair1_",i)]] })
    pair1 <-  gsub(".*/ ","",pair1)
    
    #Set up pairs   
    pair2 <- sapply(1:num, function(i) { input[[paste0("pair2_",i)]] })
    pair2 <-  gsub(".*/ ","",pair2)
    
    pair_data <- data.frame(
      pair1  = pair1,
      pair2 = pair2,
      stringsAsFactors = F
    )
    pair_data$p1 <- F
    pair_data$p2 <- F
    
    meta_data <- base_data %>%
      select(date,symbol,close) %>%
      rename(adjusted = close)
    
    outcome <- pairs_simulation(pair_data,meta_data,input$pairs_start_date,input$pairs_lookback_window,input$pairs_z_window,input$pairs_start_cash)
    remove_modal_spinner()
    
    simulation_outcome(outcome)
    
  })
  
  #Prices Pages----
  
  output$crypto_prices <- renderPlotly({
    p <- plot_ly(base_data, x = ~date, y = ~close, color = ~symbol)
  })
  
}

shinyApp(ui, server)
