if (exists("country") == FALSE) {country <- "Belgium"
source("R/params_country.R")}

if (!exists("cost_type")) {cost_type <- "en route"}

# import data  ----
if (!exists("data_new_major")) {
  source("R/get_investment_data.R")
}

# process data  ----
if (cost_type == "en route") {
    c_prefixes1 <- c("d_enr", "a_enr")
    
} else {
    c_prefixes1 <- c("d_ter", "a_ter")
    
  }
  
cols1 <- c(
  as.vector(outer(c_prefixes1, rp_years, paste, sep = "_"))
)
new_names <- cols1 %>% stringr::str_remove_all(., "ter_") %>% 
  stringr::str_remove_all(., "enr_") %>%
  sub("^([da])_(\\d{4})$", "\\2\\U\\1", ., perl = TRUE)
  
rename_map <- setNames(cols1, new_names)  

data_filtered <- data_cost_inv_rt %>% 
  select(member_state, cost_type, all_of(cols1)) %>%
  rename(!!!rename_map)

data_filtered <- data_filtered %>% 
    filter(member_state == .env$country)

data_calc <- data_filtered %>% 
  select(-member_state) %>% 
  pivot_longer(
    cols = -cost_type,  
    names_to = c("year", "type"),  # Create "type" and "year" columns
    names_pattern = "(\\d{4})(.+?)",  # Regex: Extract "type" + 4-digit year
    values_to = "value"  # Store values in "value" column
  ) %>% 
  mutate(
    value = value/10^3,
    type = if_else(type == "D", "Determined", "Actual")
  ) %>% 
  group_by(cost_type, type, year) %>% 
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>% 
  mutate(
    value = if_else(type == "Actual" & as.numeric(year) > year_report, NA, value)
    ) %>% 
  rename(category = cost_type) %>% 
  pivot_wider(id_cols = c(category, year), names_from ="type", values_from = "value") %>% 
  mutate (
    Difference = Actual - Determined
    ) %>% 
  pivot_longer(-c(category, year), names_to = "type", values_to = "value") 

data_calc_summary <- data_calc %>%
  filter(as.numeric(year) <= year_report) %>%
  group_by(category, type) %>% 
  summarise(value = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(year = rp_short) %>% 
  select(category, year, type, value) 

data_prep <- rbind(data_calc, data_calc_summary) %>% 
  arrange(year) %>% 
  pivot_wider(id_cols = c(category, type), names_from = "year", values_from = "value") %>% 
  mutate(category = factor(category, levels = c("Depreciation",
                                                "Cost of capital",
                                                "Cost of leasing"))) %>% 
  arrange(category)

data_prep1 <- data_prep %>% filter(type == "Determined") %>% 
      summarise(across(-c(category, type), ~sum(.x, na.rm = FALSE))) %>%
      mutate(category = paste0("Total costs of new and existing investments (M€<sub>",cef_ref_year,"</sub>)")) %>%
      select(colnames(select(data_prep, -type))) %>%  
  bind_rows(
    data_prep %>% filter(type == "Determined") 
  ) %>% 
  select(-type) %>% 
  mutate(category = purrr::map(category, gt::html))

data_prep2 <- data_prep %>% filter(type == "Actual") %>% 
  summarise(across(-c(category, type), ~sum(.x, na.rm = FALSE))) %>%
  mutate(category = paste0("Total costs of new and existing investments (M€<sub>",cef_ref_year,"</sub>)")) %>%
  select(colnames(data_prep1)) %>%  
  bind_rows(
    data_prep %>% filter(type == "Actual") 
  ) %>% 
  select(-type) %>% 
  mutate(category = purrr::map(category, gt::html))

data_prep3 <- data_prep %>% filter(type == "Difference") %>% 
  summarise(across(-c(category, type), ~sum(.x, na.rm = FALSE))) %>%
  mutate(category = paste0("Total difference (M€<sub>",cef_ref_year,"</sub>)")) %>%
  select(colnames(data_prep1)) %>%  
  bind_rows(
    data_prep %>% filter(type == "Difference") 
  ) %>% 
  mutate(across(2:7, ~ if_else(
    type == "Difference_perc" & !is.na(type),
    paste0(if_else(.x >0, "+", ""), round(.x, 0), "%"),
    format_parens(.x)
  ))) %>%
  ungroup() %>%   
  select(-type) %>% 
  mutate(across(2:7, ~ if_else(str_detect(.x, "NA%"), NA, .x))) %>% 
  mutate(category = purrr::map(category, gt::html))


# render tables ----
first_column_width <- 40
  
## table1 -----
table1 <- mygtable(data_prep1, myfont) %>% 
  tab_options(column_labels.background.color = "#F2F2F2",
              column_labels.font.weight = 'bold',
              container.padding.y = 0) %>% 
  cols_align(columns = 1, align = "left") %>%
  cols_label(
    category = "Determined costs"
  ) %>%
  fmt_number(
    columns = 2:7,   
    decimals = if_else(country == rp_full, 1, 2),
    use_seps = TRUE,
    sep_mark = ",",
    dec_mark = "."
  ) %>% 
  # fmt_percent(
  #   columns = 3,   # replace with your actual column name
  #   decimals = 0
  # ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(rows = 1)
    ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = 7)
  ) %>% 
  tab_style(
    style = cell_text(indent = px(20)),
    locations = cells_body(
      columns = 1,
      rows = 2:nrow(data_prep1)
    )
  ) %>%
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
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#E5E5E5",
      weight = px(2)
    ),
    locations = cells_column_labels(
      columns = rp_short
    )
  )%>% 
  cols_width(
    category ~ pct(first_column_width),
    c(2:7) ~ pct((100-first_column_width) / 6)
  )

  
## table2 -----
table2 <- mygtable(data_prep2, myfont) %>% 
  tab_options(column_labels.background.color = "#F2F2F2",
              column_labels.font.weight = 'bold',
              container.padding.y = 0) %>% 
  cols_align(columns = 1, align = "left") %>%
  cols_label(
    category = "Actual costs"
   ) %>%
  fmt_number(
    columns = 2:7,   
    decimals = if_else(country == rp_full, 1, 2),
    use_seps = TRUE,
    sep_mark = ",",
    dec_mark = "."
  ) %>% 
  # fmt_percent(
  #   columns = 3,   # replace with your actual column name
  #   decimals = 0
  # ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(rows = 1)
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = 7)
  ) %>% 
  tab_style(
    style = cell_text(indent = px(20)),
    locations = cells_body(
      columns = 1,
      rows = 2:nrow(data_prep2)
    )
  ) %>%
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
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#E5E5E5",
      weight = px(2)
    ),
    locations = cells_column_labels(
      columns = rp_short
    )
  )%>% 
  cols_width(
    category ~ pct(first_column_width),
    c(2:7) ~ pct((100-first_column_width) / 6)
  )


## table3 -----
table3 <- mygtable(data_prep3, myfont) %>% 
  tab_options(column_labels.background.color = "#F2F2F2",
              column_labels.font.weight = 'bold',
              container.padding.y = 0) %>% 
  cols_align(columns = 1, align = "left") %>%
  cols_label(
    category = "Difference (A-D)"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(rows = 1)
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = 7)
  ) %>% 
  tab_style(
    style = cell_text(indent = px(20)),
    locations = cells_body(
      columns = 1,
      rows = 2:nrow(data_prep3)
    )
  ) %>%
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
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#E5E5E5",
      weight = px(2)
    ),
    locations = cells_column_labels(
      columns = rp_short
    )
  )%>% 
  cols_width(
    category ~ pct(first_column_width),
    c(2:7) ~ pct((100-first_column_width) / 6)
  )



# create latex table
if (knitr::is_latex_output()) {
  table_level2_cef_cost_infl <- mylatex(table1)
  
} else {
  # table1
  # table2
  # table3
}

