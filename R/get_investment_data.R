# import data  ----
data_assets <- readxl::read_xlsx(
  here(data_folder, investments_data_file),
  sheet = "Output_Assets_MR",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() |>
  mutate(across(-member_state, .fns = ~ if_else(. == 'n/a', NA, .))) |>
  rename(
    value_of_the_assets = value_of_the_assets_allocated_to_ans_in_the_scope_of_the_pp_in_national_currency
  ) |>
  mutate(
    value_of_the_assets = as.numeric(value_of_the_assets)
  )


## State case ----
data_new_major <- readxl::read_xlsx(
  here(data_folder, investments_data_file_old),
  sheet = "New Major Inv pivot",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() %>%
  right_join(as_tibble(state_list), by = c("member_state" = "value")) %>%
  mutate(across(-member_state, .fns = ~ if_else(is.na(.), 0, .)))

data_new_major_detail <- readxl::read_xlsx(
  here(data_folder, investments_data_file),
  sheet = "New Major Investments",
  range = cell_limits(c(4, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()


data_capex <- readxl::read_xlsx(
  here(data_folder, investments_data_file),
  sheet = "CAPEX per Main ANSP",
  range = cell_limits(c(2, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

data_category <- readxl::read_xlsx(
  here(data_folder, investments_data_file),
  sheet = "Benefits | Investment category",
  range = cell_limits(c(2, NA), c(180, NA))
) %>%
  as_tibble() %>%
  clean_names()

data_union_wide <- readxl::read_xlsx(
  here(data_folder, investments_data_file),
  sheet = "Union-wide median",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

data_investments_type_state <- readxl::read_xlsx(
  here(data_folder, investments_data_file),
  sheet = "Union-wide chart",
  range = cell_limits(c(1, 30), c(NA, 32))
) %>%
  as_tibble() %>%
  clean_names()

data_cost_inv <- readxl::read_xlsx(
  here(data_folder, investments_data_file),
  sheet = "Costs of inv main ANSP (MR)",
  range = cell_limits(c(3, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

data_cost_inv_rt <- readxl::read_xlsx(
  here(data_folder, investments_data_file),
  sheet = "Costs of inv. (RT)-ANSP",
  range = cell_limits(c(3, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

data_impact <- readxl::read_xlsx(
  here(data_folder, investments_data_file),
  sheet = "IMPACT ANSP",
  range = cell_limits(c(2, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

data_funding_enr <- readxl::read_xlsx(
  here(data_folder, investments_data_file),
  sheet = "Funding (2)",
  range = cell_limits(c(3, 1), c(NA, 7))
) %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(type = "enroute")

data_funding_ter <- readxl::read_xlsx(
  here(data_folder, investments_data_file),
  sheet = "Funding (2)",
  range = cell_limits(c(3, 9), c(NA, 15))
) %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(type = "terminal")

data_funding_self <- rbind(data_funding_ter, data_funding_enr) %>%
  pivot_longer(
    cols = -c(member_state, type), # Pivot all columns
    names_to = c("year"), # Create "type" and "year" columns
    values_to = "value" # Store values in "value" column
  ) %>%
  mutate(year = str_replace_all(year, "x", "")) %>%
  group_by(member_state, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = 'drop') %>%
  ungroup() %>%
  mutate(type = "Total self-declared funding")


data_funding_sdm <- readxl::read_xlsx(
  here(data_folder, investments_data_file),
  sheet = "Funding (2)",
  range = cell_limits(c(3, 17), c(NA, 23))
) %>%
  as_tibble() %>%
  clean_names() %>%
  pivot_longer(
    cols = -c(member_state), # Pivot all columns
    names_to = c("year"), # Create "type" and "year" columns
    values_to = "value" # Store values in "value" column
  ) %>%
  mutate(year = str_replace_all(year, "x", ""), type = "SDM data")

data_funding <- rbind(data_funding_self, data_funding_sdm) %>%
  mutate(
    year = if_else(year == paste0("total_rp", rp, "_to_date"), rp_short, year)
  )

## SES case ----
data_cost_ses <- readxl::read_xlsx(
  here(data_folder, investments_data_file),
  sheet = "Union-wide chart",
  range = cell_limits(c(1, 30), c(NA, 40))
) %>%
  as_tibble() %>%
  clean_names()

data_benefit_ses <- readxl::read_xlsx(
  here(data_folder, investments_data_file),
  sheet = "Union-wide chart",
  range = cell_limits(c(1, 19), c(NA, 23))
) %>%
  as_tibble() %>%
  clean_names()

data_benefit_ses_forchart <- readxl::read_xlsx(
  here(data_folder, investments_data_file),
  sheet = "Union-wide median",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

data_inv_loaded <- TRUE
print(data_inv_loaded)
