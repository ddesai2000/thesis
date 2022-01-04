library(tidyverse)
library(dplyr)
library(tidyquant)
library(timetk)

companies <- read_csv(file = 'data/master_ESG_companies.csv')

# Setting stock symbols to a variable
tickers <- companies$symbol

# Download the stock price data
multpl_stocks <- tq_get(tickers,
                        from = "2013-01-01",
                        to = "2021-12-31",
                        get = "stock.prices")

# Get annual returns
multpl_stock_annual_returns <- multpl_stocks %>%
  group_by(symbol) %>%                    
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'annually',
               col_rename = 'returns')

# Create year of return variable
multpl_stock_annual_returns <- multpl_stock_annual_returns %>%
  add_column(year = NA)

multpl_stock_annual_returns$year <- 
  substr(multpl_stock_annual_returns$date, 1, 4)
multpl_stock_annual_returns$year <- as.numeric(multpl_stock_annual_returns$year)

# Save return dataset