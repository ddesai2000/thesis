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

multpl_stocks_1 <- tq_get(tickers,
                        from = "2000-01-01",
                        to = "2012-12-31",
                        get = "stock.prices")

multpl_stocks_2 <- tq_get(tickers,
                        from = "1977-01-01",
                        to = "1999-12-31",
                        get = "stock.prices")

# Get annual returns
annual_returns <- multpl_stocks %>%
  group_by(symbol) %>%                    
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'annually',
               col_rename = 'returns')

annual_returns_1 <- multpl_stocks_1 %>%
  group_by(symbol) %>%                    
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'annually',
               col_rename = 'returns')

annual_returns_2 <- multpl_stocks_2 %>%
  group_by(symbol) %>%                    
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'annually',
               col_rename = 'returns')

# Join together

master_annual_returns <- rbind(annual_returns, annual_returns_1, annual_returns_2)

# Create year of return variable
master_annual_returns <- master_annual_returns %>%
  add_column(year = NA)

master_annual_returns$year <- 
  substr(master_annual_returns$date, 1, 4)
master_annual_returns$year <- as.numeric(master_annual_returns$year)

# Save return dataset
write_csv(master_annual_returns, file = 'data/master_annual_returns.csv')

