library(tsibble)
library(tidyverse)
library(fpp3)


# 9.1 Stationraity and differencing ---------------------------------------

google_2015 <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) == 2015)

google_2015 |> ACF(Close) |>
  autoplot() + labs(subtitle = "Google closing stock price")

google_2015 |> ACF(difference(Close)) |>
  autoplot() + labs(subtitle = "Google closing stock price")


google_2015 |>
  mutate(diff_close = difference(Close)) |>
  features(diff_close, ljung_box, lag = 10)

PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost)/1e6)  |>
  transmute(
    `Sales ($million)` = Cost,
    `Log sales` = log(Cost),
    `Annual change in log sales` = difference(log(Cost), 12),
    `Doubly differenced log sales` =
      difference(difference(log(Cost), 12), 1)
  ) |>
  pivot_longer(-Month, names_to = "Type", values_to = "Sales") |>
  mutate(
    Type = factor(Type, levels = c(
      "Sales ($million)",
      "Log sales",
      "Annual change in log sales",
      "Doubly differenced log sales"))
  ) |>
  ggplot(aes(x = Month, y = Sales)) +
  geom_line() +
  facet_grid(vars(Type), scales = "free_y") +
  labs(title = "Corticosteroid drug sales", y = NULL)



# Unit root tests ---------------------------------------------------------


google_2015 |>
  features(Close, unitroot_kpss)


google_2015 |>
  mutate(diff_close = difference(Close)) |>
  features(diff_close, unitroot_kpss)


google_2015 |>
  features(Close, unitroot_ndiffs)

aus_total_retail <- aus_retail |>
  summarise(Turnover = sum(Turnover))

aus_total_retail |>
  mutate(log_turnover = log(Turnover)) |>
  features(log_turnover, unitroot_nsdiffs)


aus_total_retail |>
  mutate(log_turnover = difference(log(Turnover), 12)) |>
  features(log_turnover, unitroot_ndiffs)
