# Load packages
library(fpp3)

# Global economy

global_economy

global_economy %>%
  select(Year, Country, GDP, Imports, Exports, Population)

## Australian prison data 

prison <- readr::read_csv("data/prison_population.csv") %>%
  mutate(Quarter = yearquarter(date)) %>%
  select(-date) %>%
  as_tsibble(
    index = Quarter,
    key = c(state, gender, legal, indigenous)
  )

##Victorian electricity demand

vic_elec

vic_elec %>% gg_season(Demand)

vic_elec %>% gg_season(Demand, period = "week")

vic_elec %>% gg_season(Demand, period = "day")

## Google stock prices

gafa_stock

google_2015 <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) == 2015) %>%
  select(Date, Close)

google_2015 %>% autoplot(Close)

google_2015 %>%
  ACF(Close, lag_max = 100) %>%
  autoplot()

# Short-term visitors to Australia

austa <- readxl::read_excel("data/340101.xls", sheet = "Data1", skip = 9) %>%
  rename(date = `Series ID`, value = A85375847A) %>%
  select(date, value) %>%
  transmute(
    Month = yearmonth(date),
    Visitors = value / 1e3
  ) %>%
  bind_rows(tibble(
    Month = yearmonth(seq(as.Date("2021-06-01"), by = "1 month", length = 7)),
    Visitors = NA_real_
  )) %>%
  as_tsibble(index = Month) %>%
  filter(Month >= yearmonth("2000 Jan"))

# Fit ETS model
fit <- austa %>%
  filter(Month < yearmonth("2018 Jan")) %>%
  model(ETS(Visitors))

# Produce forecasts
fc <- forecast(fit, h = 48) 
