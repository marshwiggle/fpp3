library(tsibble)
library(dplyr)
library(fpp3)
library(fable)


# Exercise 1 --------------------------------------------------------------


# Australian Population

aus_pop <- global_economy |> filter(Country == 'Australia') |> select(Population)

aus_pop |> autoplot()

fit <- aus_pop |> model(pop = RW(Population ~ drift()))

fc <- fit |> forecast(h = 5)

fc |> autoplot(aus_pop)

# Bricks

bricks <- aus_production |> 
  filter_index("1970 Q1" ~ "2004 Q4") |>
  select(Bricks)

bricks |> autoplot()

fit <- bricks |> model(MEAN(Bricks))

fc <- fit |> forecast(h = 5)

fc |> autoplot(bricks)


# NSW Lambds

nsw_lambs <- aus_livestock |>
  filter(State == "New South Wales", Animal == "Lambs")


nsw_lambs |> autoplot()

fit <- nsw_lambs |> model(SNAIVE(Count))

fc <- fit |> forecast(h = 12)

fc |> autoplot(nsw_lambs)


# Household wealth

hh_wealth <- hh_budget |> filter(Country == 'Australia')

hh_wealth |> autoplot()

fit <- hh_wealth |> model(RW(Wealth ~ drift()))

fc <- fit |> forecast(h = 5)

fc |> autoplot(hh_wealth)


# Australian takeaway food turnover (aus_retail)

aus_takeaway <- aus_retail |> filter(Industry == 'Takeaway food services') |>
  summarise(TotalTurnover = sum(Turnover))

aus_takeaway |> autoplot()

fit <- aus_takeaway |> model(SNAIVE(TotalTurnover))

fc <- fit |> forecast(h = 12)

fc |> autoplot(aus_takeaway)



# Excercise 2 -------------------------------------------------------------


unique(gafa_stock$Symbol)

fb_stock <- gafa_stock |> filter(Symbol == 'FB', year(Date) >= 2015) |>
  mutate(day = row_number()) |>
  update_tsibble(index = day, regular = TRUE)


fb_2015 <- fb_stock |> filter(year(Date) == 2015)
  
fb_2015|> autoplot(Close)


fb_fit <- fb_2015 |> 
  model(
    Mean  = MEAN(Close),
    Naive = NAIVE(Close),
    Drift = RW(Close ~ drift())
  )

fb_stock_jan_2016 <- fb_stock |>
  filter(yearmonth(Date) == yearmonth("2016 Jan"))

fb_fc <- fb_fit |>
  forecast(new_data = fb_stock_jan_2016) 

fc_line <- fb_2015[c(1, dim(fb_2015)[1]), c('day', 'Close')]

fb_fc |>
  autoplot(fb_2015, level = NULL) +
  autolayer(fb_stock_jan_2016, Close, colour = "black") +
  geom_line(data = fc_line, aes(x = day, y = Close), colour = "#99DD11") +
  labs(y = "$US",
       title = "Facebook daily closing stock prices",
       subtitle = "(Jan 2015 - Jan 2016)") +
  guides(colour = guide_legend(title = "Forecast")) 


# Exercise 3 --------------------------------------------------------------

# Extract data of interest

recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)

# Define and estimate a model

fit <- recent_production |> model(SNAIVE(Beer))

# Look at the residuals

fit |> gg_tsresiduals()

# Look a some forecasts

fit |> forecast() |> autoplot(recent_production)


# Exercise 4 --------------------------------------------------------------


# Australian Exports ------------------------------------------------------

aus_economy <- global_economy |>
  filter(Country == 'Australia')

aus_economy_2010 <- aus_economy |> filter(Year <= 2010)

aus_economy_2011 <- aus_economy |> filter(Year > 2010)

aus_economy |> autoplot(Exports)

fit <- aus_economy_2010 |> 
  model(
    Naive = NAIVE(Exports)
  )

fit |> gg_tsresiduals()

fit |> forecast(h = 5) |> 
  autoplot(aus_economy_2010) + 
  autolayer(aus_economy_2011, Exports)


# Bricks  -------------------------------------------------------

bricks <- aus_production |> select(Quarter, Bricks)

bricks |> autoplot()

bricks_2000 <- bricks |> filter(Quarter < yearquarter('2001-01'))

bricks_2001 <- bricks |> filter(Quarter >= yearquarter('2001-01'))

fit <- bricks_2000 |>
  model(
    Snaive = SNAIVE(Bricks)
  )

fit |> gg_tsresiduals()

fit |> forecast(h=20) |> 
  autoplot(bricks_2000)  + 
  autolayer(bricks_2001, Bricks)



# Exercise 5 -------------------------------------------------------------

vic_livestock <- aus_livestock |>
  filter(State == 'Victoria')

fit <- vic_livestock |>
  model(
    Snaive = SNAIVE(Count)
  )

fc <- fit |> forecast(h = 10) 

fc |> hilo() |>
  ggplot(aes(x = Month, y = .mean)) +
  geom_line(colour = 'red') +
  geom_line(data = vic_livestock, mapping = aes(x = Month, y = Count), colour = 'blue') +
  facet_grid(vars(Animal), scales = 'free_y')



# # Exercise 7 ------------------------------------------------------------

set.seed(12345678)

myseries <- aus_retail |>
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))

myseries_train <- myseries |>
  filter(year(Month) < 2011)

autoplot(myseries, Turnover) +  
  autolayer(myseries_train, Turnover, colour = "red")

fit <- myseries_train |>
  model(SNAIVE(Turnover))


fit |> gg_tsresiduals()

fc <- fit |>
  forecast(new_data = anti_join(myseries, myseries_train))

fit |> accuracy()

fc |> accuracy(myseries)


# Exercise 8 --------------------------------------------------------------


pigs <- aus_livestock |>
  filter(State == 'New South Wales', Animal == 'Pigs')

pigs |> autoplot()

pigs_2012 <- pigs |> filter(Month < yearmonth('2013-01'))

pigs_2013 <- pigs |> filter(Month >= yearmonth('2013-01'))


fit <- pigs_2012 |>
  model(
    Naive = NAIVE(Count),
    SNaive = SNAIVE(Count),
    Drift = SNAIVE(Count ~ drift())
  )

fc <- fit |> forecast(h = 72)

fc |> autoplot(pigs_2012)

fc |> accuracy(pigs_2013)

fit |> select(Drift) |> gg_tsresiduals()

fit |> select(Naive) |> gg_tsresiduals()

fit |> select(SNaive) |> gg_tsresiduals()


# Exercise 9 --------------------------------------------------------------

unique(hh_budget$Country)

unique(hh_budget$Year)

aus_hh_budget <- hh_budget |> filter(Country == 'Australia')

aus_hh_budget |> autoplot()

aus_hh_budget_2012 <- aus_hh_budget |> filter(Year < 2013)

aus_hh_budget_2013 <- aus_hh_budget |> filter(Year > 2013)

fit <- aus_hh_budget_2012 |>
  model(
    Rw = RW(Wealth ~ drift())
  )

fc <- fit |> forecast(h = 4)

fc |> autoplot(aus_hh_budget)

fit |> gg_tsresiduals()

fc |> accuracy(aus_hh_budget_2013)

