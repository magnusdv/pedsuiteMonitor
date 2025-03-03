
style_versions = function(tb) {

  tb |> gt() |>
    opt_stylize(6, color = "cyan") |>
    tab_options(
      table.width = "100%",
      table.align = "left",
      data_row.padding = px(1),
      table.border.top.style = "hidden",
      table.border.bottom.width = px(0),
      container.padding.y = px(0),
      container.overflow.x = FALSE
    ) |>
    tab_style(
      style = list(
        cell_text(size = pct(80), weight = "bold"),
        "padding: 1px 0px 1px 5px; vertical-align: middle;"
      ),
      locations = cells_column_labels()) |>
    cols_align(align = "left",
               columns = c(CRAN, GITHUB, DEV)) |>
    cols_align(align = "center",
               columns = starts_with("date")) |>
    tab_style(style = cell_borders(sides = c("left", "right"), color = "gray"),
              locations = list(cells_body(), cells_column_labels())) |>
    tab_style(
      style = list(
        cell_text(size = "small"),
        cell_borders(sides = "left", style = NULL),
        "padding-left: 0px; padding-right: 1px"
      ),
      locations = list(
        cells_body(columns = starts_with("date")),
        cells_column_labels(columns = starts_with("date")))
      ) |>
    tab_style(style = "padding-right: 0px",
              locations = cells_body(columns = c(CRAN, GITHUB, DEV))) |>
    tab_style(style = cell_text(whitespace = "nowrap"),
              locations = cells_body()) |>
    cols_label(starts_with("date") ~ "") |>
    tab_style(style = cell_text(color = "brown", weight = "bolder"),
              locations = cells_body(columns = GITHUB,
                                     rows = sub("v", "", GITHUB) != CRAN))
}


style_commits = function(tb) {
  tb |> gt() |>
    tab_options(
      table.width = "100%",
      table.align = "left",
      heading.align = "left",
      table.font.size = "small",
      data_row.padding = px(1),
      column_labels.hidden = TRUE,
      table.border.top.style = "hidden",
      container.padding.y = px(0)
    ) |>
    opt_row_striping() |>
    cols_align(align = "left",
               columns = everything()) |>
    tab_style(style = cell_text(whitespace = "nowrap"),
              locations = cells_body()) |>
    tab_style(style = cell_text(size = "small", weight = "bolder"),
              locations = cells_title())
}

style_issues = function(tb) {
  tb |> gt() |>
    tab_options(
      table.width = "100%",
      table.align = "left",
      heading.align = "left",
      table.font.size = "small",
      data_row.padding = px(1),
      column_labels.hidden = TRUE,
      table.border.top.style = "hidden",
      container.padding.y = px(0)
    ) |>
    opt_row_striping() |>
    cols_align(align = "left",
               columns = everything()) |>
    tab_style(style = cell_text(whitespace = "nowrap"),
              locations = cells_body()) |>
    tab_style(style = cell_text(size = "small", weight = "bolder"),
              locations = cells_title()) |>
    tab_style(style = cell_text(style = "italic"),
              locations = cells_body(columns = Title))
}
