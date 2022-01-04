library(tidyverse)
library(dplyr)
library(janitor)
library(padr)

companies <- read_csv(file = 'data/ESG_US_Demands.csv')

# find unique company names
company_names <- companies[!duplicated(companies$Symbol), ] %>%
  clean_names() %>%
  select(symbol, company, pid, sector, exit_type)

# create function to take characters from value
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# clean names and add column for year invested
companies <- companies %>%
  add_column(invest_year = NA) %>%
  clean_names()

# Create and fill out year invested variable
companies$invest_year <- substrRight(companies$date_invested, 2)
companies$invest_year <- paste0("20", companies$invest_year)
companies$invest_year <- as.numeric(companies$invest_year)

# Edit years from 1900s

companies$invest_year[companies$invest_year == "2099"] <- "1999"
companies$invest_year[companies$invest_year == "2098"] <- "1998"
companies$invest_year[companies$invest_year == "2097"] <- "1997"
companies$invest_year[companies$invest_year == "2094"] <- "1994"
companies$invest_year[companies$invest_year == "2093"] <- "1993"
companies$invest_year[companies$invest_year == "2087"] <- "1987"
companies$invest_year[companies$invest_year == "2080"] <- "1980"

companies$invest_year <- as.numeric(companies$invest_year)

# Get rid of companies without year invested data
companies <- companies[!is.na(companies$invest_year), ]

# Get rid of exact duplicate rows (probably created by mistake)
companies <- companies %>%
  distinct()

# Group once for holding percent of each investor
companies_grouped <- companies %>%
  group_by(symbol, investor_id, invest_year, demand_group) %>%
  summarize(holding_percent = max(current_holding_percent))

# Group again to get holding percent of all investors combined and number of investors
main_companies_grouped <- companies_grouped %>%
  group_by(symbol, invest_year, demand_group) %>%
  summarize(comb_holding_percent = sum(holding_percent), investors = n())

# Split up columns by E, S, and G
new_main_companies_grouped <- main_companies_grouped %>%
  pivot_wider(names_from = "demand_group", 
              values_from = c("invest_year", "comb_holding_percent", "investors"),
              names_repair = "check_unique")

# Unnest vectors into different columns
wide_companies_grouped <- new_main_companies_grouped %>%
  unnest_wider(invest_year_Governance, names_sep = "_") %>%
  unnest_wider(invest_year_Social, names_sep = "_") %>%
  unnest_wider(invest_year_Environmental, names_sep = "_") %>%
  unnest_wider(comb_holding_percent_Governance, names_sep = "_") %>%
  unnest_wider(comb_holding_percent_Social, names_sep = "_") %>%
  unnest_wider(comb_holding_percent_Environmental, names_sep = "_") %>%
  unnest_wider(investors_Governance, names_sep = "_") %>%
  unnest_wider(investors_Social, names_sep = "_") %>%
  unnest_wider(investors_Environmental, names_sep = "_")

# Group once for holding percent of each investor (not including demand group)
general_companies_grouped <- companies %>%
  group_by(symbol, investor_id, invest_year) %>%
  summarize(holding_percent = max(current_holding_percent))

# Group again to get holding percent of all investors combined and number of investors
add_companies_grouped <- general_companies_grouped %>%
  group_by(symbol, invest_year) %>%
  summarize(comb_holding_percent = sum(holding_percent), investors = n()) %>%
  add_column(ESG = "ESG")

# Pivot Wider with overall ESG invest dates
new_add_companies_grouped <- add_companies_grouped %>%
  pivot_wider(names_from = "ESG", 
              values_from = c("invest_year", "comb_holding_percent", "investors"),
              names_repair = "check_unique")

# Unnest vectors into different columns
wide_add_companies_grouped <- new_add_companies_grouped %>%
  unnest_wider(invest_year_ESG, names_sep = "_") %>%
  unnest_wider(comb_holding_percent_ESG, names_sep = "_") %>%
  unnest_wider(investors_ESG, names_sep = "_") 

# Join datasets with E, S, and G separate and ESG together
master_ESG_companies <- wide_add_companies_grouped %>%
  left_join(wide_companies_grouped) %>%
  left_join(., company_names)

# Move important columns to front
master_ESG_companies <- master_ESG_companies %>% 
  relocate(company, .after = symbol) %>%
  relocate(pid, .after = company) %>%
  relocate(sector, .after = pid) %>%
  relocate(exit_type, .after = sector)

# Write master file to csv
write_csv(master_ESG_companies, file = 'data/master_ESG_companies.csv')

# Get years of each firm need data for
companies_all_years <- add_companies_grouped %>% 
  group_by(symbol) %>% 
  complete(invest_year = full_seq((min(invest_year)-3):(max(invest_year) + 4), 1)) %>%
  rename(year = invest_year) %>%
  select(symbol, year) %>%
  add_column(date_form = NA)

companies_all_years$date_form <- 
  as.Date(ISOdate(companies_all_years$year, 12, 31))

# Remove years greater than 2021
companies_all_years <- subset(companies_all_years, year < 2022)

# Add returns data in
master_annual_returns <- read_csv(file = 'data/master_annual_returns.csv')

companies_all_years_returns <- companies_all_years %>%
  left_join(master_annual_returns, by = c('symbol','year')) %>%
  select(-date)

# Add cumulative holdings column for ESG
ESG_cumu_holdings <- add_companies_grouped %>%
  select(-c(investors, ESG))  %>%
  rename(year = invest_year)

ESG_cumu_holdings$date_form <- 
  as.Date(ISOdate(ESG_cumu_holdings$year, 12, 31))

ESG_cumu_holdings <- ESG_cumu_holdings %>% 
  pad(group = "symbol")

ESG_cumu_holdings$comb_holding_percent[is.na(ESG_cumu_holdings$comb_holding_percent)] = 0

ESG_cumu_holdings$cum_holding_sum <- ave(ESG_cumu_holdings$comb_holding_percent, 
                              ESG_cumu_holdings$symbol, FUN=cumsum)

ESG_cumu_holdings <- ESG_cumu_holdings %>%
  select(-c(comb_holding_percent, year))

# Join cumulative ESG holdings column to companies_all_years_returns
companies_all_years_returns_hold <- companies_all_years_returns %>%
  left_join(ESG_cumu_holdings, by = c('symbol','date_form'))

# Add cumulative holdings column for E, S, and G separate
sep_cumu_holdings <- main_companies_grouped %>%
  pivot_wider(names_from = "demand_group", 
              values_from = "comb_holding_percent",
              names_repair = "check_unique")

sep_cumu_holdings$Governance[is.na(sep_cumu_holdings$Governance)] = 0
sep_cumu_holdings$Social[is.na(sep_cumu_holdings$Social)] = 0
sep_cumu_holdings$Environmental[is.na(sep_cumu_holdings$Environmental)] = 0  

sep_cumu_holdings <- sep_cumu_holdings %>%
  group_by(symbol, invest_year) %>%
  summarize(Governance = max(Governance), Social = max(Social), 
            Environmental = max(Environmental)) %>%
  rename(year = invest_year)

sep_cumu_holdings$date_form <- 
  as.Date(ISOdate(sep_cumu_holdings$year, 12, 31))

sep_cumu_holdings <- sep_cumu_holdings %>% 
  pad(group = "symbol")

sep_cumu_holdings$Governance[is.na(sep_cumu_holdings$Governance)] = 0
sep_cumu_holdings$Social[is.na(sep_cumu_holdings$Social)] = 0
sep_cumu_holdings$Environmental[is.na(sep_cumu_holdings$Environmental)] = 0

sep_cumu_holdings$cum_G_holding_sum <- ave(sep_cumu_holdings$Governance, 
                                           sep_cumu_holdings$symbol, FUN=cumsum)
sep_cumu_holdings$cum_S_holding_sum <- ave(sep_cumu_holdings$Social, 
                                           sep_cumu_holdings$symbol, FUN=cumsum)
sep_cumu_holdings$cum_E_holding_sum <- ave(sep_cumu_holdings$Environmental, 
                                           sep_cumu_holdings$symbol, FUN=cumsum)

sep_cumu_holdings <- sep_cumu_holdings %>%
  ungroup() %>%
  select(-c(Environmental, Social, Governance, year))

# Join E, S, and G holdings columns to companies_all_years_returns_hold
companies_all_years_returns_hold <- companies_all_years_returns_hold %>%
  left_join(sep_cumu_holdings, by = c('symbol','date_form'))
  
master_companies_returns_hold <- 
  companies_all_years_returns_hold[!is.na(companies_all_years_returns_hold$symbol), ]

# Write master_companies_returns_hold to csv to have cumulative holdings and stock returns
write_csv(master_companies_returns_hold, file = 'data/master_companies_returns_cumhold.csv')

# Join master ESG file with master company returns + holding file
main_companies_data <- master_companies_returns_hold  %>%
  left_join(master_ESG_companies, by = "symbol")

# Write full data file to csv
write_csv(main_companies_data, file = 'data/main_companies.csv')

