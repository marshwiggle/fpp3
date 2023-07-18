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

# 2.10 Excercises

# Ex.3 

tute1 <- readr::read_csv("tute1.csv")
mytimeseries <- tute1 |>
  mutate(Quarter = yearquarter(Quarter)) |>
  as_tsibble(index = Quarter)

mytimeseries |>
  pivot_longer(-Quarter) |>
  ggplot(aes(x = Quarter, y = value, colour = name)) +
  geom_line() + 
  facet_grid(name ~ ., scales = "free_y")

# Ex.4

library(USgas)

gas_consumption <- us_total |>
  as_tsibble(index = year, key = state)
  
  
gas_consumption |>  
  filter(state %in% c('Maine', 'Vermont', 'New Hampshire', 'Massachusetts', 'Connecticut', 'Rhode Island')) |>
  autoplot()
  

# Ex.5

tourism <- readxl::read_excel('tourism.xlsx') |>
  mutate(Quarter = yearquarter(Quarter)) |>
  as_tsibble(index=Quarter, key= c(Region, State, Purpose))

tourism |> 
  group_by(Region, Purpose) |> 
  summarise(MeanTrips = mean(Trips)) |> 
  ungroup() |> 
  filter(MeanTrips == max(MeanTrips))

tourism |> 
  group_by(State) |>
  summarize(Trips = sum(Trips))


# Ex. 7

set.seed(144)
myseries <- aus_retail |>
  filter(`Series ID`  == sample(aus_retail$`Series ID`, 1))


myseries |>
  autoplot() +
  labs(title = "Supermarkets in Tasmania")

myseries |>
  gg_season() +
  labs(title = "Supermarkets in Tasmania")

myseries |>
  gg_subseries() +
  labs(title = "Supermarkets in Tasmania")

myseries |>
  gg_lag() +
  labs(title = "Supermarkets in Tasmania")

myseries |>
  ACF(Turnover) |>
  autoplot()

# Ex. 8

# 1

total_private <- us_employment |> filter(Series_ID == 'CEU0500000001')

total_private |>
  autoplot()

total_private |>
  gg_season()

total_private |>
  gg_subseries()

total_private |>
  gg_lag()

total_private |>
  ACF(Employed) |>
  autoplot()

# 2

bricks <- aus_production |> select(Bricks)


bricks |>
  autoplot()

bricks |>
  gg_season()

bricks |>
  gg_subseries()

bricks |>
  gg_lag()


bricks |>
  ACF(Bricks) |>
  autoplot()


# 3

hare <- pelt |> select(Hare)

hare |>
  autoplot()


hare |>
  gg_season()


hare |>
  gg_lag()

hare |>
  ACF(Hare) |>
  autoplot()


# 4

# h-zero-2, not h-oh-2

h02 <- PBS |> filter(ATC2 == 'H02') |> select(Concession, Type, ATC1, Cost)

h02 |>
  autoplot()


h02 |>
  gg_season()

h02 |>
  gg_subseries()

h02 |> filter(Concession =='Concessional', Type == 'Co-payments', ATC1 == 'H') |>
  gg_lag()

h02 |> filter(Concession =='Concessional', Type == 'Co-payments', ATC1 == 'H') |>
  ACF(Cost) |>
  autoplot()


# 5

us_gasoline |> autoplot()

us_gasoline |>
  gg_season()

us_gasoline |>
  gg_subseries()


us_gasoline |>
  gg_lag()

us_gasoline |>
  ACF(Barrels) |>
  autoplot()


# Ex. 10


pigs <- aus_livestock |>
  filter(grepl('pig', Animal, ignore.case=TRUE), 
         State=='Victoria', 
         between(Month, as.Date('1990-01-01'), as.Date('1995-01-01')) )

pigs |> autoplot()

pigs |> ACF(Count) |> autoplot()

pigs1 <- aus_livestock |>
  filter(grepl('pig', Animal, ignore.case=TRUE), 
         State=='Victoria', 
         between(Month, as.Date('1980-01-01'), as.Date('1995-01-01')) )

pigs1 |> autoplot()


pigs1 |> ACF(Count) |> autoplot()


# Ex.11

dgoog <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) >= 2018) |>
  mutate(trading_day = row_number()) |>
  update_tsibble(index = trading_day, regular = TRUE) |>
  mutate(diff = difference(Close))

dgoog |> select(diff) |> autoplot()

dgoog |>  ACF(diff) |> autoplot()
