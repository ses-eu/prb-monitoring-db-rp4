if (exists("country") == FALSE) {
  country <- "Belgium"
}

# import data  ----
if (!exists("data_assets")) {
  source("R/get_investment_data.R")
}

# process data  ----
data_pre_prep <- data_assets |>
  filter(
    type_of_investment %in%
      c(
        "New major investment",
        "Other new investments",
        "Additional new major investment",
        "Additional other new investment",
        "Additional other new investments"
      ) &
      ansp_type == "Main"
  ) |>
  group_by(member_state) |>
  summarise(
    value = sum(value_of_the_assets, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ungroup() |>
  mutate(
    type = "ANSP"
  )

total_uw <- data_pre_prep |>
  summarise(
    value = sum(value, na.rm = TRUE)
  ) |>
  pull()

data_prep <- data_pre_prep |>
  filter(member_state == .env$country) |>
  mutate(
    mymetric = value / total_uw,
    type = paste0(
      main_ansp,
      " asset value of new investments compared to Union-wide (%)"
    )
  ) |>
  select(type, mymetric)


# table ----
table1 <- mygtable(data_prep, myfont) |>
  fmt_percent(
    columns = 2, # replace with your actual column name
    decimals = 0
  ) |>
  tab_options(column_labels.hidden = TRUE) |>
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "#F2F2F2")
    ),
    locations = cells_body(
      rows = c(1)
    )
  )

# tab_options(
#   column_labels.background.color = "#F2F2F2",
#   column_labels.font.weight = 'bold',
#   container.padding.y = 0
# ) %>%
# cols_align(columns = 1, align = "left") %>%
# cols_label(
#   type = html(paste0(
#     "Total value of the asset for new investments (M€<sub>",
#     cef_ref_year,
#     "</sub>)"
#   )),
#   value = total_value
# )

#
# fmt_number(
#   columns = 2, # replace with your actual column name
#   decimals = 2,
#   use_seps = TRUE,
#   sep_mark = ",",
#   dec_mark = "."
# ) %>%
# fmt_percent(
#   columns = 3, # replace with your actual column name
#   decimals = 0
# ) %>%
# tab_style(
#   style = list(
#     cell_text(weight = "bold")
#   ),
#   locations = cells_body(
#     rows = c(1, 4)
#   )
# ) %>%
# tab_style(
#   style = cell_text(indent = px(20)),
#   locations = cells_body(
#     columns = c(type),
#     rows = type == "Additional" | type == "Included in the performance plan"
#   )
# )

# create latex table
if (knitr::is_latex_output()) {
  table_level2_cef_cost_infl <- mylatex(table1)
} else {
  table1
}
