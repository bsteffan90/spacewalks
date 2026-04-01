# BreanneSteffan
# 2026-03-31


# https://data.nasa.gov/resource/eva.json (with modifications)
input_file = 'eva-data.json'
output_file = 'eva-data.csv'
graph_file = 'cumulative_eva_graph.png'


library(tidyverse) #tidyverse "contains" ggplot2
library(jsonlite)
library(lubridate)


input_file  <- "./eva-data.json"
output_file <- "./eva-data.csv"
graph_file  <- "./cumulative_eva_graph.png"

#creates a table in the environment of the data so that a human can read it
eva_tbl <- jsonlite::fromJSON(input_file) |>
  as_tibble()

#takes the table and identifies data of interest
eva_tbl <- eva_tbl |>
  mutate(
    eva  = as.numeric(eva),
    date = ymd_hms(date, quiet = TRUE) ) |>
  filter(!is.na(duration), duration != "", !is.na(date))


readr::write_csv(eva_tbl, output_file)


eva_tbl <- eva_tbl |>
  arrange(date)

#identifies/renames parameters in the table
eva_tbl <- eva_tbl |>
  mutate(
    duration_hours = {
      parts <- str_split(duration, ":", n = 2, simplify = TRUE)
      as.numeric(parts[, 1]) + as.numeric(parts[, 2]) / 60
    },
    cumulative_time = cumsum(duration_hours) #defines cumulative time
  )

#creates plot based on cumulative time vs date
cumulative_spacetime_plot <- ggplot(eva_tbl, aes(x = date, y = cumulative_time)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Year",
    y = "Total time spent in space to date (hours)"
  ) +
  theme_minimal()
#defines graph parameters (i.e. height/width/resolution)
ggsave(graph_file, plot = cumulative_spacetime_plot, width = 9, height = 5, dpi = 300)
print(cumulative_spacetime_plot)
