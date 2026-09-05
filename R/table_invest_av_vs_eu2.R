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
        "New major investments",
        "Other new investment",
        "Other new investments",
        "Additional new major investment",
        "Additional new major investments",
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

eu_share <- data_prep |> pull(mymetric) * 100

inv_decimals <- ceiling(abs(min(1, log10(eu_share))))

# table ---  -
table1 <- mygtable(data_prep, myfont) |>
  fmt_percent(
    columns = 2, # replace with your actual column name
    decimals = inv_decimals
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
  ) |>
  cols_width(
    c(2) ~ pct(15)
  )

# create latex table
if (knitr::is_latex_output()) {
  table_level2_cef_cost_infl <- mylatex(table1)
} else {
  table1
}
