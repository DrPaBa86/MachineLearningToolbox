# Download stock prices
#
# Patrick Bäurer
# 2020-04-30
#


# load packages ----
library(magrittr)
library(tidyquant)
library(tidyverse)


# download DAX stock prices ----

# DAX symbols: https://de.finance.yahoo.com/quote/%5EGDAXI/components?p=%5EGDAXI
DaxSymbols <- read_rds(paste0(getwd(), "/Datasets/DaxPrices/DaxSymbols.rds"))

# Download data
Stock_prices <- tq_get(DaxSymbols %>% pull(Symbol), get = "stock.prices")

# Visualize single stock
symbol <- "SAP.DE"
Stock_prices %>%
  filter(symbol == !!symbol) %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_point() +
  labs(title = symbol)

# Analyse data
(MinDates <- Stock_prices %>%
  group_by(symbol) %>%
  summarise(MinDate = min(date)) %>%
  arrange(desc(MinDate)))

# Filter to MinDate
MinDate <- MinDates %>% slice(1) %>% pull(MinDate)
Stock_prices %<>%
  filter(date >= MinDate)

# Search for NAs
sum(is.na(Stock_prices$open))
sum(is.na(Stock_prices$high))
sum(is.na(Stock_prices$low))
sum(is.na(Stock_prices$close))
sum(is.na(Stock_prices$volume))
sum(is.na(Stock_prices$adjusted))

# Save data
DaxPrices <- Stock_prices
write_rds(DaxPrices, paste0(getwd(), "/Datasets/DaxPrices/DaxPrices.rds"))

