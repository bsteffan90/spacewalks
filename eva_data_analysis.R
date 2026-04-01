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

#read JSON file into a table (tibble)
read_json_to_dataframe <- function(input_file) {jsonlite::fromJSON(input_file)|>
    tibble::as.tibble()
  }

#convert + write to CSV
write_dataframe_to_csv <-function(df, output_file) {df <- df |>
  dplyr::mutate(
    eva = as.numeric(eva),
    date = lubridate::ymd_hms (date, quiet =TRUE)
  )|>
  dplyr::filter(!is.na(duration), duration !="", !is.na(date))
readr::write_csv(df, output_file)
df
}

# Plot cumulative time in space and save the figure
plot_cumulative_time_in_space <- function (df, graph_file) {
  df <- df |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      duration_hours = {
        parts <- stringr::str_split(duration, ":", n = 2, simplify = TRUE)
        as.numeric(parts[,1]) + as.numeric(parts [,2])/60
      },
      cumulative_time = cumsum(duration_hours)
    )
  p <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = cumulative_time)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = "Year",
      y = "Total time spent in space to date (hours)"
    ) +
    ggplot2::theme_minimal()
  
  ggplot2::ggsave(graph_file, plot = p, width = 9, height = 5, dpi = 300)
  print(p)
  
  invisible(p)
  # Return p silently: print(p) above already rendered the plot,
  # so invisible() prevents a second auto-print at the top level
  # while still allowing callers to capture the plot object if needed
}

#---Pipeline---
eva_tbl <-read_json_to_dataframe (input_file) |>
  write_dataframe_to_csv(output_file = output_file)

plot_cumulative_time_in_space(eva_tbl,graph_file)

