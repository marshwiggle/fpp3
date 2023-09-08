library(tsibble)
library(fpp3)


# Exercise 2 --------------------------------------------------------------

stl_holiday_features <- tourism |>
  filter(Purpose == "Holiday") |>
  features(Trips, feat_stl)

library(glue)

stl_tourism_features |>
  select_at(vars(contains("season"), spikiness, linearity, curvature)) |>
  mutate(
    # Change period symbols
    seasonal_peak_year = seasonal_peak_year +
      4*(seasonal_peak_year == 0),
    seasonal_trough_year = seasonal_trough_year +
      4*(seasonal_trough_year == 0),
    seasonal_peak_year = glue("Q{seasonal_peak_year}"),
    seasonal_trough_year = glue("Q{seasonal_trough_year}"),
  ) |>
  GGally::ggpairs()
