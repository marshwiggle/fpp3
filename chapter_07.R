library(tsibble)
library(tidyverse)
library(fpp3)


# 7.1 The linear model ----------------------------------------------------


us_change |>
  pivot_longer(c(Consumption, Income), names_to = "Series") |>
  autoplot(value) +
  labs(y = "% change")

us_change |>
  ggplot(aes(x = Income, y = Consumption)) +
  labs(y = "Consumption (quarterly & change)",
     x = "Income (quarterly & change)") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
  

us_change |>
  model(TSLM(Consumption ~ Income)) |>
  report()

us_change |>
  select(-Consumption, -Income) |>
  pivot_longer(-Quarter) |>
  ggplot(aes(Quarter, value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  guides(colour = "none") +
  labs(y = "% change")

us_change |>
  GGally::ggpairs(columns = 2:6)


# 7.2 Least squares estimation --------------------------------------------


fit_consMR <- us_change |>
  model(tslm = TSLM(Consumption ~ Income + Production +
                      Unemployment + Savings))

report(fit_consMR)

augment(fit_consMR) |>
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Percent change in US consumption expenditure"
    ) +
  scale_colour_manual(values = c(Data = "black", Fitted = "#DD5E00")) +
  guides(colour = guide_legend(title = NULL))


augment(fit_consMR) |>
  ggplot(aes(x = Consumption, y = .fitted)) +
  geom_point() +
  labs(
    y = "Fitted (predicted values)",
    x = "Data (acutal values)",
    title = "Percent change in US consumption expenditure"
  ) +
  geom_abline(intercept = 0, slope = 1)


# 7.3 Evaluating the regression model -------------------------------------

fit_consMR |> gg_tsresiduals()

augment(fit_consMR) |>
  features(.innov, ljung_box, lag = 10)

us_change |>
  left_join(residuals(fit_consMR), by = "Quarter") |>
  pivot_longer(Income:Unemployment,
               names_to = "regressor", values_to = "x") |>
  ggplot(aes(x = x, y = .resid)) +
  geom_point() +
  facet_wrap(. ~ regressor, scales = "free_x") +
  labs(y = "Residuals", x = "")

augment(fit_consMR) |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() + labs(x = "Fitted", y = "Residuals")


fit <- aus_airpassengers |>
  filter(Year <= 2011) |>
  left_join(guinea_rice, by = "Year") |>
  model(TSLM(Passengers ~ Production))

report(fit)

fit |> gg_tsresiduals()


# 7.4 Some useful predictors ----------------------------------------------

recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)

recent_production |>
  autoplot(Beer) +
  labs(y = "Megaliters",
       title = "Australian quarterly beer production")


fit_beer <- recent_production |>
  model(TSLM(Beer ~ trend() + season ()))

report(fit_beer)


augment(fit_beer) |>
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  ) +
  labs(y = "Megaliters",
       title = "Australian quarterly beer production") +
  guides(colour = guide_legend(title = "Series"))

augment(fit_beer) |>
  ggplot(aes(x = Beer, y = .fitted,
             colour = factor(quarter(Quarter)))) +
  geom_point() +
  labs(y = "Fitted", x = "Actual values",
       title = "Australian quaterly beer production") +
  geom_abline(intercept = 0, slope = 1) +
  guides(colour = guide_legend(title = "Quarter"))


fourier_beer <- recent_production |>
  model(TSLM(Beer ~ trend() + fourier(K = 2)))

report(fourier_beer)


# 7.5 Selecting predictors ------------------------------------------------


glance(fit_consMR) |>
  select(adj_r_squared, CV, AIC, AICc, BIC)


# 7.6 Forecasting with regression -----------------------------------------

recent_production <- aus_production |>
  filter(year(Quarter) >= 1992) 

fit_beer <- recent_production |>
  model(TSLM(Beer ~ trend() + season()))

fc_beer <- forecast(fit_beer)
fc_beer |>
  autoplot(recent_production) +
  labs(
    title = "Forecasts of beer production using regression",
    y = "megaliters"
  )

fit_consBest <- us_change |>
  model(
    lm = TSLM(Consumption ~ Income + Savings + Unemployment)
  )

future_scenarios <- scenarios(
  Increase = new_data(us_change, 4) |>
    mutate(Income=1, Savings=0.5, Unemployment=0),
  Decrease = new_data(us_change, 4) |>
    mutate(Income=-1, Savings=-0.5, Unemployment=0),
  names_to = "Scenario"
)

fc <- forecast(fit_consBest, new_data = future_scenarios)

us_change |>
  autoplot(Consumption) +
  autolayer(fc) +
  labs(title = "US consumptions", y = "% change")
