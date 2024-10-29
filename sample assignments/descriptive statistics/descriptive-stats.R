library(tidyverse)
library(here)

here("sample assignments",
     "descriptive statistics",
     "helpers.R") |>
  source()

my_data <- here("datasets",
                "test.csv") |>
  read_csv()

rooms_plot <- descriptive_histogram(my_data$rooms,
                                    label_x = "Number of rooms in home",
                                    label_y = "Number of home sales",
                                    decimal_places = 2)

rooms_plot

dist_plot <- descriptive_histogram(my_data$city_dist,
                                    label_x = "Distance from city (miles)",
                                    label_y = "Number of home sales",
                                    decimal_places = 2)

dist_plot

school_plot <- descriptive_histogram(my_data$sch_quality,
                                   label_x = "Quality rating of closest school to home (stars)",
                                   label_y = "Number of home sales",
                                   decimal_places = 2)

school_plot

price_plot <- descriptive_histogram(my_data$`sales-price`,
                                     label_x = "Sales price (US Dollars)",
                                     label_y = "Number of home sales",
                                     decimal_places = 0,
                                    is_log = TRUE)

price_plot


summary(my_data$`sales-price`)

type_plot <- descriptive_cat_bars(my_data$building,
                                  x_label = "Type of home",
                                  y_label = "Proportion of home sales")

type_plot
