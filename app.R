############   
# 1. Import Libraries
############

## Required packages
packages = c("geckor", "lubridate", "tidyverse", "shiny", "plotly", "httr", "RCurl", "jsonlite")

## Load or install&load
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# remove scientific notation
options(scipen = 100)


############   
# 3. Collect data
############

# check if alive
ping() 

# List of coins
my_coins <- c("terra-luna","cardano", "bitcoin", "metaverse-index", 
             "ethereum","matic-network", "flower-solana", "solana")

# get coin history
many_coins_usd <- coin_history(coin_id = my_coins, 
                          vs_currency = "usd", 
                          days = 300) # use "max" for all history

# get bitcoin
many_coins_bit <- coin_history(coin_id = my_coins, 
                           vs_currency = "btc", 
                           days = 300) %>%
  select(timestamp, coin_id, btc_price = price)

# join df's
many_coins <- left_join(many_coins_usd, many_coins_bit) # ∃ some missing btc values

# add clean date column
many_coins <- many_coins %>%
  mutate(date = date(timestamp))



############   
# 4. Shiny App
############   


### UI
ui <- fluidPage(

  # Application title
  titlePanel("Top 7 trending crypto coins (from CoinGecko API)"),
  hr(),

    # Sidebar for coin select
    sidebarLayout(
      sidebarPanel(
        actionButton("refresh", 
                     "Refresh app"),
        
        br(),br(),
        
        selectInput("coin", 
                        "Select coin", 
                        choices = my_coins, 
                        selected = "bitcoin"),
            hr(),
            
            sliderInput("bins",
                        "Last n days:",
                        min = 1,
                        max = 150,
                        value = 30), # add to max of days
            
            dateInput("dateinput", 
                      "End date:",
                      value = today()),
            
            textInput("editdayslimit", 
                      "New max days limit", 
                      value = 150),
            br(),
            
            checkboxInput("bitcoinprice", 
                         "Display Bitcoin value",
                         value = FALSE),
        ),
      
      
      # Plot
      mainPanel(
        plotlyOutput("plot")
        )
    )
)

### Server
server <- function(input, output, session) {

    output$plot <- renderPlotly({
      
      # interaction with slide and date end
      start_date <- today() - input$bins
      end_date <- input$dateinput
      
      # filter input data accordingly
      input_data <- many_coins %>%
        filter(coin_id %in% input$coin,
               date >= start_date,
               date <= end_date)
      
      # interactive plot
      fig_1 <- plot_ly(input_data, 
                     type = 'scatter', 
                     mode = 'lines',
                     name = 'USD',
                     x = ~date, 
                     y = ~price,
                     hovertemplate = paste('<b>value</b>: $ %{y:.5f}', # 5 decimal places
                                           '<br><b>date</b>: %{x}'),
                     showlegend = T) %>%
            layout(title = paste("Value of", input$coin, "coin (in USD):",sep= " "),
                   xaxis = list(title = 'date'),
                   yaxis = list(title = 'price (USD)'))
      
      if(FALSE %in% input$bitcoinprice) {
        fig_1
      } else {
        fig_2 <- plot_ly(input_data,
                    type = 'scatter', 
                    mode = "lines", 
                    name = 'BTC',
                    x = ~date, 
                    y = ~btc_price,
                    hovertemplate = paste('<b>value</b>: BTC %{y:.5f}', # 5 decimal places
                                          '<br><b>date</b>: %{x}'),
                    showlegend = T)
        
        subplot(fig_1, fig_2, nrows = 2) %>%
          layout(title = paste("Value of", input$coin, "coin (USD & BTC):",sep= " "))
                    
      }
      
    })
    
    # change max limit
    observeEvent(input$editdayslimit, {
      updateSliderInput(session, "bins", max = as.integer(input$editdayslimit))
    })
    
    # refresh view
    observeEvent(input$refresh, {
      session$reload();
    })
    
    # bitcoin line
    observeEvent(input$bitcoinprice, {
      update
      updateSliderInput(session, "bins", max = as.integer(input$editdayslimit))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

