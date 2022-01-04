library(tidyverse)
library(dplyr)
library(janitor)

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

# Add returns data in + Cumulative Holdings
