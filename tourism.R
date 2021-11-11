# Load packages
library(fpp3)

# Australian tourism

tourism <- tsibble::tourism %>%
  mutate(
    State = recode(State,
      "Australian Capital Territory" = "ACT",
      "New South Wales" = "NSW",
      "Northern Territory" = "NT",
      "Queensland" = "QLD",
      "South Australia" = "SA",
      "Tasmania" = "TAS",
      "Victoria" = "VIC",
      "Western Australia" = "WA"
    ),
    Region = stringr::str_remove(Region, "Australia's ")
  )

tourism

holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))

holidays %>% autoplot(Trips) +
  ylab("thousands of trips") +
  ggtitle("Australian domestic holiday nights")

holidays %>% gg_season(Trips) +
  ylab("thousands of trips") +
  ggtitle("Australian domestic holiday nights")

holidays %>% gg_subseries(Trips) +
  ylab("thousands of trips") +
  ggtitle("Australian domestic holiday nights")

holidays %>%
  ACF(Trips) %>%
  autoplot()


tourism %>% features(Trips, feat_stl)

tourism %>%
  features(Trips, feat_stl) %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year, col = Purpose)) +
  geom_point() +
  facet_wrap(vars(State))

most_seasonal <- tourism %>%
  features(Trips, feat_stl) %>%
  filter(seasonal_strength_year == max(seasonal_strength_year))

tourism %>%
  right_join(most_seasonal, by = c("State", "Region", "Purpose")) %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State, Region, Purpose))

most_trended <- tourism %>%
  features(Trips, feat_stl) %>%
  filter(trend_strength == max(trend_strength))

tourism %>%
  right_join(most_trended, by = c("State", "Region", "Purpose")) %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State, Region, Purpose))

# Compute features
tourism_features <- tourism %>%
  features(Trips, feature_set(pkgs = "feasts"))

# Compute PCs
pcs <- tourism_features %>%
  select(-State, -Region, -Purpose) %>%
  prcomp(scale = TRUE) %>%
  broom::augment(tourism_features)

pcs %>% ggplot(aes(x = .fittedPC1, y = .fittedPC2)) +
  geom_point() +
  theme(aspect.ratio = 1)

pcs %>% ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = State)) +
  geom_point() +
  theme(aspect.ratio = 1)

pcs %>% ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Purpose)) +
  geom_point() +
  theme(aspect.ratio = 1)

outliers <- pcs %>%
  filter(.fittedPC1 > 12 | (.fittedPC1 > 10 & .fittedPC2 > 0))

pcs %>% ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Purpose)) +
  geom_point() +
  theme(aspect.ratio = 1) +
  geom_point(data = outliers, aes(x = .fittedPC1, y = .fittedPC2), col = "black", shape = 1, size = 3)

outliers %>%
  left_join(tourism, by = c("State", "Region", "Purpose")) %>%
  mutate(Series = glue::glue("{State}", "{Region}", "{Purpose}", .sep = "\n\n")) %>%
  ggplot(aes(x = Quarter, y = Trips)) + geom_line() +
  facet_grid(Series ~ ., scales = "free_y") +
  ggtitle("Outlying time series in PC space")

holiday_fit <- holidays %>%
  model(
    snaive = SNAIVE(Trips),
    naive = NAIVE(Trips),
    ets = ETS(Trips),
    arima = ARIMA(Trips)
  )

holiday_fit %>%
  filter(State == "VIC") %>%
  select(arima) %>%
  report()

glance(holiday_fit)

tidy(holiday_fit)

augment(holiday_fit)

augment(holiday_fit) %>%
  filter(State == "VIC", .model == "arima") %>%
  features(.resid, ljung_box, dof = 2, lag = 8)

holiday_fit %>%
  filter(State == "VIC") %>%
  select(arima) %>%
  gg_tsresiduals()

holiday_fc <- holiday_fit %>%
  forecast(h = "2 years")

holiday_fc %>%
  filter(State == "VIC") %>%
  autoplot(holidays, level = NULL) +
  labs(title = "Holidays in Victoria", y = "Thousands of visitors") +
  guides(color = guide_legend(title = "Forecast"))

holiday_fc %>%
  filter(State == "VIC", .model == "arima") %>%
  autoplot(holidays) +
  labs(title = "Holidays in Victoria", y = "Thousands of visitors") +
  guides(color = guide_legend(title = "Forecast"))

holiday_fc %>% hilo(level = 95)

vic_holidays <- holidays %>% filter(State == "VIC")
train <- vic_holidays %>% filter(year(Quarter) <= 2015)
vic_fit <- train %>%
  model(
    snaive = SNAIVE(Trips),
    naive = NAIVE(Trips),
    ets = ETS(Trips),
    arima = ARIMA(Trips)
  )
vic_fc <- vic_fit %>%
  forecast(h = 8)
vic_fc %>%
  autoplot(vic_holidays, level = NULL) +
  labs(title = "Holidays in Victoria", y = "Thousands of visitors") +
  guides(color = guide_legend(title = "Forecast"))

accuracy(vic_fc, holidays)


vic_holiday_stretch <- holidays %>%
  filter(State == "VIC") %>%
  stretch_tsibble(.init = 16, .step = 1)


fit_cv <- vic_holiday_stretch %>%
  model(
    ets = ETS(Trips),
    arima = ARIMA(Trips),
    snaive = SNAIVE(Trips)
  )

fc_cv <- fit_cv %>%
  forecast(h = 1)

fc_cv %>%
  select(-.model)

fc_cv %>% accuracy(holidays)

tourism %>%
  aggregate_key(Purpose * (State / Region), Trips = sum(Trips)) %>%
  filter(Quarter == yearquarter("1998 Q1"))

tourism %>%
  aggregate_key(Purpose * (State / Region), Trips = sum(Trips)) %>%
  model(ets = ETS(Trips)) %>%
  reconcile(ets_adjusted = min_trace(ets)) %>%
  forecast(h = 2)

tourism_agg <- tourism %>%
  aggregate_key(Purpose * (State / Region),
    Trips = sum(Trips)
  )
fc <- tourism_agg %>%
  filter_index(. ~ "2015 Q4") %>%
  model(ets = ETS(Trips)) %>%
  reconcile(ets_adjusted = min_trace(ets)) %>%
  forecast(h = "2 years")

fc %>%
  filter(is_aggregated(Purpose) & is_aggregated(State)) %>%
  autoplot(tourism_agg, level = 95)

fc %>%
  filter(is_aggregated(Purpose) & State == "VIC" &
    is_aggregated(Region)) %>%
  autoplot(tourism_agg, level = 95)

fc %>%
  filter(is_aggregated(Purpose) & Region == "Melbourne") %>%
  autoplot(tourism_agg, level = 95)

fc %>%
  filter(is_aggregated(Purpose) & Region == "Snowy Mountains") %>%
  autoplot(tourism_agg, level = 95)

fc %>%
  filter(Purpose == "Holiday" & Region == "Barossa") %>%
  autoplot(tourism_agg, level = 95)

fc %>%
  filter(is_aggregated(Purpose) & Region == "MacDonnell") %>%
  autoplot(tourism_agg, level = 95)

fc <- tourism_agg %>%
  filter_index(. ~ "2015 Q4") %>%
  model(
    ets = ETS(Trips),
    arima = ARIMA(Trips)
  ) %>%
  mutate(
    comb = (ets + arima) / 2
  ) %>%
  reconcile(
    ets_adj = min_trace(ets),
    arima_adj = min_trace(arima),
    comb_adj = min_trace(comb)
  ) %>%
  forecast(h = "2 years")

fc %>%
  accuracy(tourism_agg) %>%
  group_by(.model) %>%
  summarise(MASE = mean(MASE)) %>%
  arrange(MASE)

