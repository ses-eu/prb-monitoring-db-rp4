if (exists("country") == FALSE) {
  country <- "Belgium"
  source("R/params_country.R")
}

# import data  ----
if (!exists("data_assets")) {
  source("R/get_investment_data.R")
}

# process data  ----
data_prep <- data_assets |>
  filter(
    member_state == .env$country,
    ansp_type == "Main",
    type_of_investment == "New major investment"
  ) |>
  mutate(
    `SES mandated` = if_else(ses_mandated == 1, "X", ""),
    Partnership = if_else(partnership == 1, "X", "")
  ) |>
  select(
    name_of_investment,
    `SES mandated`,
    Partnership
  ) |>
  arrange(name_of_investment)

# render table ----
table1 <- mygtable(data_prep, myfont) %>%
  tab_options(
    column_labels.background.color = "#F2F2F2",
    column_labels.font.weight = 'bold',
    container.padding.y = 0
  ) %>%
  cols_align(columns = 1, align = "left") %>%
  cols_label(
    name_of_investment = "New major investments"
  ) |>
  cols_align(
    align = "center",
    columns = 2:ncol(data_prep) # center all except first column
  ) |>
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = 2:ncol(data_prep))
  ) |>
  tab_style(
    style = list(
      # cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 1
    )
  )


# create latex table
if (knitr::is_latex_output()) {
  table_invest_type <- mylatex(table1)
} else {
  table1
}
