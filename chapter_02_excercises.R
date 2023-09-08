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
