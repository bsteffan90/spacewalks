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


#' Read a JSON file into a tibble
#'
#' This function reads a JSON file from disk and converts it into a tibble.
#' It uses \code{jsonlite::fromJSON()} to parse the JSON and
#' \code{tibble::as.tibble()} to return a tidy data frame.
#'
#' @param input_file A character string giving the path to a JSON file.
#'
#' @return A tibble containing the data parsed from the JSON file.
#'
#' @examples
#' \dontrun{
#' df <- read_json_to_dataframe("data/example.json")
#' }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as.tibble
#' @export
read_json_to_dataframe <- function(input_file) {jsonlite::fromJSON(input_file)|>
    tibble::as.tibble()
  }

#' Write a cleaned data frame to a CSV file
#'
#' This function mutates and filters a data frame before writing it to a CSV
#' file. Specifically, it coerces the \code{eva} column to numeric, parses the
#' \code{date} column as a date-time using \code{lubridate::ymd_hms()}, removes
#' rows with missing or empty \code{duration} values, and drops rows with missing
#' dates. The cleaned data frame is written to disk and also returned.
#'
#' @param df A data frame or tibble containing at least the columns
#'   \code{eva}, \code{date}, and \code{duration}.
#' @param output_file A character string giving the path to the output CSV file.
#'
#' @return The cleaned data frame that was written to the CSV file.
#'
#' @examples
#' \dontrun{
#' cleaned_df <- write_dataframe_to_csv(my_df, "outputs/cleaned_data.csv")
#' }
#'
#' @importFrom dplyr mutate filter
#' @importFrom lubridate ymd_hms
#' @importFrom readr write_csv
#' @export
write_dataframe_to_csv <- function(df, output_file) {
  df <- df |>
    dplyr::mutate(
      eva = as.numeric(eva),
      date = lubridate::ymd_hms(date, quiet = TRUE)
    ) |>
    dplyr::filter(!is.na(duration), duration != "", !is.na(date))
  
  readr::write_csv(df, output_file)
  df
}



#' Plot cumulative time spent in space over time
#'
#' This function calculates cumulative time spent in space from a data frame
#' containing observation dates and durations, then generates and saves a line
#' plot. Durations are expected to be stored as character strings in
#' \code{HH:MM} format and are converted to hours before computing the cumulative
#' sum. The resulting plot is saved to disk and also returned invisibly.
#'
#' @param df A data frame or tibble containing at least the columns
#'   \code{date} (date-time) and \code{duration} (character, formatted as
#'   \code{"HH:MM"}).
#' @param graph_file A character string giving the file path where the plot
#'   image should be saved (e.g., \code{".png"}).
#'
#' @return A ggplot object showing cumulative time spent in space over time.
#'   The plot is returned invisibly.
#'
#' @examples
#' \dontrun{
#' plot_cumulative_time_in_space(my_df, "figures/cumulative_time.png")
#' }
#'
#' @importFrom dplyr arrange mutate
#' @importFrom stringr str_split
#' @importFrom ggplot2 ggplot aes geom_point geom_line labs theme_minimal ggsave
#' @export
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

