if (exists("country") == FALSE) {country <- "Belgium"
source("R/params_country.R")}

# import data  ----
if (!exists("data_new_major")) {
  source("R/get_investment_data.R")
}

# process data  ----
c_prefixes <- c("nm", "on", "e")
c_suffixes <- c("d", "a")

cols <- c(
  as.vector(outer(paste0(c_prefixes, "_d"), rp_years, paste, sep = "_")),
  as.vector(outer(paste0(c_prefixes, "_a"), rp_years, paste, sep = "_"))
)

data_calc <- data_cost_inv %>% 
  filter(member_state == .env$country) %>% 
  select(all_of(cols)
  ) %>% 
  pivot_longer(
    cols = everything(),  # Pivot all columns
    names_to = c("category", "year"),  # Create "type" and "year" columns
    names_pattern = "(.+?)_(\\d{4})",  # Regex: Extract "type" + 4-digit year
    values_to = "value"  # Store values in "value" column
  ) %>% 
  mutate(
    type = if_else(str_detect(category, "_d"), "Determined", "Actual"),
    value = if_else(type == "Actual" & as.numeric(year) > year_report, NA, value/10^6),
    category = case_when(
      str_detect(category, "nm_") ~ "New major investments",
      str_detect(category, "on_") ~ "Other new investments",
      str_detect(category, "e_") ~ "Existing investments",
    )
  ) %>% 
  pivot_wider(id_cols = c(category, year), names_from ="type", values_from = "value") %>% 
 mutate (
    Difference = Actual - Determined,
    Difference_perc = if_else(Actual ==0, 0, Actual/Determined-1) * 100
    ) %>% 
  pivot_longer(-c(category, year), names_to = "type", values_to = "value") 

data_calc_summary <- data_calc %>%
  filter(as.numeric(year) <= year_report) %>%
  group_by(category, type) %>% 
  summarise(value = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(year = paste0("RP", rp)) %>% 
  select(category, year, type, value) 

data_prep <- rbind(data_calc, data_calc_summary) %>% 
  arrange(year) %>% 
  pivot_wider(id_cols = c(category, type), names_from = "year", values_from = "value")

data_prep1 <- data_prep %>% filter(type == "Determined") %>% 
      summarise(across(-c(category, type), ~sum(.x, na.rm = FALSE))) %>%
      mutate(category = paste0("Total costs of new and existing investments (M€<sub>",cef_ref_year,"</sub>)")) %>%
      select(colnames(select(data_prep, -type))) %>%
  bind_rows(
    data_prep %>% filter(type == "Determined") 
  ) %>% 
  select(-type) %>% 
  rename_with(
    ~ paste0(.x, "D"),
    .cols = all_of(as.character(rp_years))
  ) %>% 
  mutate(category = purrr::map(category, gt::html)) %>% 
  relocate(category, .before = everything()) 


data_prep2 <- data_prep %>% filter(type == "Actual") %>% 
  summarise(across(-c(category, type), ~sum(.x, na.rm = FALSE))) %>%
  mutate(category = paste0("Total costs of new and existing investments (M€<sub>",cef_ref_year,"</sub>)")) %>%
  bind_rows(
    data_prep %>% filter(type == "Actual") 
  ) %>% 
  select(-type) %>% 
  rename_with(
    ~ paste0(.x, "A"),
    .cols = all_of(as.character(rp_years))
  ) %>% 
  mutate(category = purrr::map(category, gt::html)) %>% 
  relocate(category, .before = everything()) 

data_prep3 <- data_prep %>% filter(type == "Difference") %>% 
  summarise(across(-c(category, type), ~sum(.x, na.rm = FALSE))) %>%
  mutate(category = paste0("Total difference (M€<sub>",cef_ref_year,"</sub>)")) %>%
  bind_rows(
    data_prep %>% filter(str_detect(type,"Difference")) 
  ) %>% 
  mutate(
    category = if_else(type == "Difference_perc" & !is.na(type), 
                       "% change of actual with respect to determined", category)
  ) %>% 
  rowwise() %>%
  mutate(across(1:6, ~ if_else(
    type == "Difference_perc" & !is.na(type),
    paste0(if_else(.x >0, "+", ""), round(.x, 0), "%"),
    format_parens(.x)
  ))) %>%
  ungroup() %>% 
  select(-type) %>% 
  mutate(category = purrr::map(category, gt::html)) %>% 
  relocate(category, .before = everything()) %>% 
  mutate(across(2:7, ~ if_else(str_detect(.x, "NA%"), NA, .x)))  


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
    decimals = 2,
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
      columns = paste0("RP", rp)
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#E5E5E5",
      weight = px(2)
    ),
    locations = cells_column_labels(
      columns = paste0("RP", rp)
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
    decimals = 2,
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
      columns = paste0("RP", rp)
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#E5E5E5",
      weight = px(2)
    ),
    locations = cells_column_labels(
      columns = paste0("RP", rp)
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
    style = cell_text(style = "italic"),
    locations = cells_body(rows = c(3, 5, 7))
  )%>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#E5E5E5",
      weight = px(2)
    ),
    locations = cells_body(
      columns = paste0("RP", rp)
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#E5E5E5",
      weight = px(2)
    ),
    locations = cells_column_labels(
      columns = paste0("RP", rp)
    )
  ) %>% 
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

