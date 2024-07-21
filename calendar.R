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

plot_calendar <- function(tbl) {
    tbl |>
        gg_facet_wrap_months(unit_date) +
        geom_text(aes(label = emoji), nudge_y = -0.25, nudge_x = -0.1, size = 3.5, na.rm = TRUE) +
        geom_tile_interactive(
            aes(
                tooltip = paste(
                    details |> tidyr::replace_na(""),
                    ifelse(is.na(details), "", "@"),
                    room |> tidyr::replace_na("")),
                data_id = id
            ),
            alpha = 0.2,
            fill = "transparent",
            colour = "grey80"
        )
}

render_calendar <- function(yaml_file) {
    read_yaml(yaml_file) |>
        yaml_date_nodes_to_tbl() |>
        tidy_date_tbl() |>
        verify_date_df() |>
        dplyr::mutate(id = row_number()) |>
        dplyr::relocate(id, .before = details) |>
        reframe_events(startDate, endDate) |>
        plot_calendar()
}


