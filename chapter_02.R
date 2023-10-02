library(tsibble)
library(fpp3)

# 2.1. tibble objects

y <- tsibble(
  Year = 2015:2019,
  Observation = c(123, 39, 78, 52, 110),
  index = Year
)


olympic_running # from fpp3

olympic_running |> distinct(Sex)

PBS # From tisbbledata package. Sales data on pharmaceutical products in Australia

PBS |>
  filter(ATC2 == "A10")


PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost)


PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost))

PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost)) |>
  mutate(Cost = TotalC/1e6)

PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost)) |>
  mutate(Cost = TotalC/1e6) -> a10

prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")
prison

# 2.2 Time plots

melsyd_economy <- ansett |>
  filter(Airports == "MEL-SYD", Class == "Economy") |>
  mutate(Passengers = Passengers/1000)

autoplot(melsyd_economy, Passengers) +
  labs(title = "Ansett airlines economy class",
       subtitle = "Melbourne-Sydney",
       y = "Passengers ('000)")

autoplot(a10, Cost)  + 
  labs(y = "$ (millions)",
       title = "Australian antidiabetic drug sales")

# 2.4 Seasonal plots

a10 |>
  gg_season(Cost, labels = "both") +
  labs(y = "$ (millions)",
       title = "Seasonal plot: Antidiabetic drug sales")

vic_elec |> gg_season(Demand, period = "day") +
  theme(legend.position = "none") +
  labs(y = "MWh", title = "Electricity demand: Victoria")

vic_elec |> gg_season(Demand, period = "week") +
  theme(legend.position = "none") +
  labs(y = "MWh", title = "Electricity demand: Victoria")



vic_elec |> gg_season(Demand, period = "year") +
  labs(y = "MWh", title = "Electricity demand: Victoria")

# 2.5 Seasonal subseries plots

a10 |>
  gg_subseries(Cost) +
  labs(
    y = "$ (millions)",
    title = "Australian antidiabetic drug sales"
  )

holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  group_by(State) |>
  summarise(Trips = sum(Trips))

holidays

autoplot(holidays, Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")

gg_season(holidays, Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")

holidays |> 
  gg_subseries(Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")

# 2.6 Scatterplots
vic_elec |>
  filter(year(Time) == 2014) |>
  autoplot(Demand) +
  labs(y = "GW",
       title = "Half-hour electricity demand: Victoria")

vic_elec |>
  filter(year(Time) == 2014) |>
  autoplot(Temperature) +
  labs(
    y = "Degrees Celsius",
    title = "Half-hourly temperatures: Melbourne, Australia"
  )

vic_elec |>
  filter(year(Time) == 2014) |>
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  labs(x = "Temperature (degrees Celsius)",
       y = "Electricity demand (GW)")

visitors <- tourism |>
  group_by(State) |>
  summarise(Trips = sum(Trips))

visitors |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() + 
  facet_grid(vars(State), scales = "free_y") +
  labs(title = "Australian domestic tourism",
       y = "Overnight trips ('000)")

visitors |>
  pivot_wider(values_from=Trips, names_from=State) |>
  GGally::ggpairs(columns = 2:9)


# 2.7 Lag plots

recent_production <- aus_production |>
  filter(year(Quarter) >= 2000)

recent_production |>
  gg_lag(Beer, geom = "point") +
  labs(x = "lag(Beer, k)")

recent_production |> ACF(Beer, lag_max = 9)

recent_production |>
  ACF(Beer) |>
  autoplot() + labs(title = "Australian beer productions")

a10 |>
  ACF(Cost, lag_max = 48) |>
  autoplot() +
  labs(title = "Australian antidiabetic drug sales")


# 2.9 White noise

set.seed(30)

y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)

y |> autoplot(wn) + labs(title = "White noise", y = "")

y |>
  ACF(wn) |>
  autoplot() + labs(title="White noise")

