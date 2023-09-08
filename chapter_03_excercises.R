library(ggplot2)
library(dplyr)
library(tsibble)
library(lubridate)
library(fabletools)
library(feasts)
library(seasonal)
library(fpp3)



# Exercise 1 --------------------------------------------------------------


df <- global_economy |> mutate(GDP_Per_Capita  = GDP/Population)

ggplot(df, aes(Year, GDP_Per_Capita, colour = Country)) +
  geom_line() +
  theme(legend.position = "none")

top_countries <- df |> as.data.frame() |> group_by(Year) |> arrange(desc(GDP_Per_Capita), .by_group = TRUE) |> top_n(n=1)

View(top_countries)



# Excersice 2 -------------------------------------------------------------


# United States GDP

us_gdp_per_capita <- global_economy |> 
  filter(Country == 'United States') |> 
  mutate(GDP_Per_Capita  = GDP/Population) |>
  select(Year, GDP_Per_Capita)


# Slaughter of Victorian "Bulls, bullocks and steers"

us_gdp_per_capita |> autoplot() + labs(title = 'US GDP Per Capita', x= 'Year', y = 'GDP Per Captia')

                               

sl_per_day <- aus_livestock |> filter(Animal == 'Bulls, bullocks and steers', State == 'Victoria') |>
  mutate(Count_Per_Day = Count / days_in_month(Month))

sl_per_day |> autoplot()


# Victorian Electricity Demand

vic_elec |> autoplot()

# Gas production

gas_production <- aus_production |> select(Quarter, Gas) 

gas_production |> autoplot()



# Exercise 4 --------------------------------------------------------------


# Total Private Employed

tpe <- us_employment |> filter(Title == 'Total Private') |> 
  mutate(Employed_Per_Day = Employed / days_in_month(Month)) |>
  select(Month, Employed_Per_Day)

tpe |> autoplot(box_cox(Employed_Per_Day, 0)) +
  labs(x = 'Month', y = 'Employed per day')


# Bricks from aus_production

bricks_production <- aus_production |> select(Quarter, Bricks) 

bricks_production |> autoplot(box_cox(Bricks, 0.05))


# Hare from pelt

hare <- pelt |> select(Year, Hare)

hare |> autoplot()
hare |> autoplot(box_cox(Hare, 0.2))


# 'H02' Cost from PBS

h02_cost <- PBS |> filter(ATC2 == 'H02') |> select(Month, Cost)

h02_cost |> autoplot()

h02_cost <- PBS |> filter(ATC2 == 'H02', Concession == 'Concessional', Type == 'Co-payments') |> select(Month, Cost)

h02_cost |> autoplot(box_cox(Cost, 0.3))

# Barrles form us_gasoline

us_gasoline |> autoplot()



# Exercise 5 --------------------------------------------------------------

# Tobacco Production

tobacco_production <- aus_production |> select(Quarter, Tobacco) 

tobacco_production |> autoplot()

lambda <- tobacco_production |>
  features(Tobacco, features = guerrero) |>
  pull(lambda_guerrero)


tobacco_production |>
  autoplot(box_cox(Tobacco, lambda))

# Economy class passengers between Melbourne and Sydney

passengers <- ansett |> filter(Airports == 'MEL-SYD', Class == 'Economy')

passengers |> autoplot()

lambda <- passengers |>
  features(Passengers, features = guerrero) |>
  pull(lambda_guerrero)

passengers |> 
  autoplot(box_cox(Passengers, lambda))


# Pedestrian counts at Southern Cross Stations

pedestrian_counts <- pedestrian |> filter(Sensor == 'Southern Cross Station') |>
  as.data.frame() |>
  select(Date, Count) |>
  group_by(Date) |>
  summarise(Count = sum(Count, na.rm = TRUE)) |>
  as_tsibble()

pedestrian_counts |> autoplot()

labmda <- pedestrian_counts |>
  features(Count, features = guerrero) |>
  pull(lambda_guerrero)

pedestrian_counts |>
  autoplot(box_cox(Count, lambda))


# Excercise 7 -------------------------------------------------------------

gas <- tail(aus_production, 5*4) |> select(Gas)

gas |> autoplot()

gas |> 
  model(
    classical_decomposition(Gas, type = "multiplicative")
  ) |>
  components() |>
  autoplot() |>
  labs(title = "Classical decomposition of Gas production data")



gas |> 
  model(
    classical_decomposition(Gas, type = "multiplicative")
  ) |>
  components() |>
  as_tsibble() |>
  autoplot(season_adjust)

# e

gas_outlier <- gas
gas_outlier[12, 1] <- gas_outlier[12,1] + 300
gas_outlier |> autoplot()


gas_outlier |> 
  model(
    classical_decomposition(Gas, type = "multiplicative")
  ) |>
  components() |>
  as_tsibble() |>
  autoplot(season_adjust)

# f

gas_outlier1 <- gas
gas_outlier1[4, 1] <- gas_outlier[4,1] + 300
gas_outlier1 |> autoplot()


gas_outlier1 |> 
  model(
    classical_decomposition(Gas, type = "multiplicative")
  ) |>
  components() |>
  as_tsibble() |>
  autoplot(season_adjust)



# Exercise 8 --------------------------------------------------------------


# Total Private Employed


# Why do we neee year(Month) >= 1990
# Strange error when interval is larger than 55 years
tpe <- us_employment |> filter(year(Month) >= 1960, year(Month) <= 2013, Title == 'Total Private') |>
  select(-Series_ID)
  
tpe |> autoplot() +
  labs(x = 'Month', y = 'Employed')


x11_tpe <- tpe |>
  model(x11 = X_13ARIMA_SEATS(Employed ~ x11())) |>
  components()


autoplot(x11_tpe)  +
  labs(title = "Decomposition of US Total Private Employment per day")

# Bricks from aus_production

bricks_production <- aus_production |> select(Quarter, Bricks) 
bricks_production |> autoplot() +
  labs(x = 'Quarter', y = 'Bricks')

x11_bricks <- bricks_production |>
  model(x11 = X_13ARIMA_SEATS(Bricks ~ x11())) |>
  components()

x11_bricks |> autoplot() +
  labs(x = 'Quarter', y = 'Brics')

# 'H02' Cost from PBS

h02_cost <- PBS |> filter(ATC2 == 'H02') |> select(Month, Cost)
h02_cost |> autoplot()

h02_cost <- PBS |> filter(ATC2 == 'H02', Concession == 'Concessional', Type == 'Co-payments') |> select(Month, Cost)
h02_cost |> autoplot()

x11_h02 <- h02_cost |>
  model(x11 = X_13ARIMA_SEATS(Cost ~ x11())) |>
  components()

x11_h02 |> autoplot()


# Exercise 10 -------------------------------------------------------------


View(canadian_gas)


canadian_gas |> autoplot()

canadian_gas |> gg_season()

canadian_gas |> gg_subseries() 

canadian_gas |>
  model(
    STL(Volume ~ trend(window = 7) +
          season(window = 7),
        robust = TRUE)) |>
  components() |>
  autoplot()

canadian_gas |>
  model(
    STL(Volume ~ trend(window = 7) +
          season(window = 7),
        robust = TRUE)) |>
  components() |>
  autoplot(season_adjust)

stl_canadian_gas <- canadian_gas |>
  model(
    STL(Volume ~ trend(window = 7) +
          season(window = 7),
        robust = TRUE)) |>
  components() 


x11_canadian_gas <- canadian_gas |>
  model(x11 = X_13ARIMA_SEATS(Volume ~ x11())) |>
  components() 

seats_canadian_gas <- canadian_gas |>
  model(x11 = X_13ARIMA_SEATS(Volume ~ seats())) |>
  components() 
  
g <- ggplot() +
  geom_line(data = x11_canadian_gas, aes(x = Month, y = trend)) + 
  geom_line(data = stl_canadian_gas, aes(x = Month, y = trend), color = "red") +
  geom_line(data = seats_canadian_gas, aes(x = Month, y = trend), color = "darkgreen") 


g1 <- ggplot() +
  geom_line(data = x11_canadian_gas, aes(x = Month, y = seasonal)) + 
  geom_line(data = stl_canadian_gas, aes(x = Month, y = season_year), color = "red") +
  geom_line(data = seats_canadian_gas, aes(x = Month, y = seasonal), color = "darkgreen") 
