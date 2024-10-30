library(tidyverse)
library(here)

my_data <- here("datasets",
                "test.csv") |>
  read_csv() |>
  mutate(building = factor(building, 
                           levels = c("Single-family",
                                      "Duplex",
                                      "Three-plus")))

## price and distance

ggplot(my_data,
       aes(x = city_dist,
           y = `sales-price`)) +
  geom_point(alpha = 0.3,
             size = 0.5,
             color = "orange") +
  scale_x_continuous(name = "Distance to city (miles)") +
  scale_y_continuous(name = "Home sale price",
                     transform = "log10",
                     breaks = c(10000,
                                100000,
                                1000000),
                     labels = c("$10,000",
                                "$100,000",
                                "$1,000,000"))  +
  stat_smooth(method = "lm",
              color = "sienna",
              fill = "sienna") +
  theme_minimal()

lm(`sales-price` ~ city_dist, data = my_data) |>
  summary()

## price and school quality

ggplot(my_data,
       aes(x = sch_quality,
           y = `sales-price`)) +
  geom_jitter(alpha = 0.3,
             size = 0.5,
             color = "orange") +
  scale_x_continuous(name = "School quality rating (stars)") +
  scale_y_continuous(name = "Home sale price",
                     transform = "log10",
                     breaks = c(10000,
                                100000,
                                1000000),
                     labels = c("$10,000",
                                "$100,000",
                                "$1,000,000"))  +
  stat_smooth(method = "lm",
              color = "sienna",
              fill = "sienna") +
  theme_minimal()

lm(`sales-price` ~ sch_quality, data = my_data) |>
  summary()

## price and number of rooms

ggplot(my_data,
       aes(x = rooms,
           y = `sales-price`)) +
  geom_jitter(alpha = 0.3,
              size = 0.5,
              color = "orange") +
  scale_x_continuous(name = "Number of rooms") +
  scale_y_continuous(name = "Home sale price",
                     transform = "log10",
                     breaks = c(10000,
                                100000,
                                1000000),
                     labels = c("$10,000",
                                "$100,000",
                                "$1,000,000"))  +
  stat_smooth(method = "lm",
              color = "sienna",
              fill = "sienna") +
  theme_minimal()

lm(`sales-price` ~ rooms, data = my_data) |>
  summary()

## price and building type

ggplot(my_data,
       aes(x = building,
           y = `sales-price`)) +
  geom_jitter(alpha = 0.3,
              size = 0.5,
              color = "orange") +
  geom_violin(fill = "sienna",
              alpha = 0.2,
              color = "sienna",
              draw_quantiles = c(0.5)) +
  scale_y_continuous(name = "Sales price",
                     transform = "log10",
                     breaks = c(10000,
                                100000,
                                1000000),
                     labels = c("$10,000",
                                "$100,000",
                                "$1,000,000")) +
  scale_x_discrete(name = "Building type") +
  theme_minimal()

lm(`sales-price` ~ building, data = my_data) |>
  summary()

## All 

lm(`sales-price` ~ building + rooms + sch_quality + city_dist, data = my_data) |>
  summary()
