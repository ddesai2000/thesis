library(tidyverse)
library(dplyr)
library(tidyquant)
library(timetk)

companies <- read_csv(file = 'data/master_ESG_companies.csv')

# Setting our stock symbols to a variable

tickers <- companies$symbol

# Download the stock price data

multpl_stocks <- tq_get(tickers,
                        from = "2013-01-01",
                        to = "2021-12-31",
                        get = "stock.prices")

multpl_stock_annual_returns <- multpl_stocks %>%
  group_by(symbol) %>%                    
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'annually',
               col_rename = 'returns')
