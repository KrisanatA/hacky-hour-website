library(ggtilecal)
library(dplyr)
library(tibble)
library(lubridate)
library(yaml)
library(ggplot2)
library(ggiraph)

## for YAML collections
yaml_date_nodes_to_tbl <- function(list) {
    list |>
        lapply(as.data.frame) |>
        dplyr::bind_rows() |>
        tibble::as_tibble()
}

## for data frames
tidy_date_tbl <- function(df) {
    df |>
        dplyr::mutate(across(ends_with("Date"), ~ lubridate::ymd(.x))) |>
        dplyr::mutate(endDate = dplyr::coalesce(endDate, startDate)) |>
        dplyr::arrange(startDate)
}

verify_date_df <- function(df) {
    assertr::verify(df, startDate <= endDate)
}

tidy_date_df_export <- function(df) {
    df |>
        dplyr::mutate(
            startDate = format(startDate, "%Y-%m-%d"),
            endDate = format(endDate, "%Y-%m-%d")
        )
}

write_yaml_col_maj_FALSE <- function(x, file) {
    write_yaml(x, file, column.major = FALSE)
}
