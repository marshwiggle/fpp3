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

fb_jan_2016 <- fb_stock |>
  filter(yearmonth(Date) == yearmonth("2016 Jan"))

fb_fc <- fb_fit |>
  forecast(new_data = fb_stock_jan_2006) 

fc_line <- fb_2015[c(1, dim(fb_2015)[1]), c('day', 'Close')]

fb_fc |>
  autoplot(fb_2015, level = NULL) +
  autolayer(fb_jan_2016, Close, colour = "black") +
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
