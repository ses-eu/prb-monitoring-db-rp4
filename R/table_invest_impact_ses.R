if (exists("country") == FALSE) {country <- "Belgium"
source("R/params_country.R")}

# import data  ----
if (!exists("data_new_major")) {
  source("R/get_investment_data.R")
}

# process data  ----
data_prep <- data_benefit_ses %>% 
  filter(field %in% c("NETWORK benefits", "LOCAL benefits", "Non-performance benefits")) %>% 
  filter(status == "A") %>% 
  mutate(value = value / 10^6) %>% 
  select(-state, -status) %>% 
  pivot_wider(names_from = "year", values_from = "value") %>% 
  mutate(
    !!rp_short := rowSums(across(all_of(as.character(rp_years))), na.rm = TRUE)
  )


# render table ----
table1 <- mygtable(data_prep, myfont) %>% 
  tab_options(column_labels.background.color = "#F2F2F2",
              column_labels.font.weight = 'bold',
              container.padding.y = 0) %>% 
  cols_align(columns = 1, align = "left") %>%
  cols_label(
    field = html(paste0("Actual costs - expected impact (M€<sub>",cef_ref_year,"</sub>)"))
  ) %>% 
  tab_style(
    style = list(
      # cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 1
    )) %>% 
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#E5E5E5",
      weight = px(2)
    ),
    locations = cells_body(
      columns = rp_short
    )
  ) %>% 
  fmt_number(
    columns = where(is.numeric),
    decimals = 0
  ) %>% 
  cols_width(
    1 ~ pct(50),        # first column: 50%
    everything() ~ NULL # other columns: automatic
  )


# create latex table
if (knitr::is_latex_output()) {
  table_inv_impact_ses <- mylatex(table1)
  
} else {
  table1
}
