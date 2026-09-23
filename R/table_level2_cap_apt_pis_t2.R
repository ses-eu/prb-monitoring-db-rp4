## import data  ----
if (!exists("data_loaded")) {
  source("R/get_data.R")
}

data_raw <- cap_apt_pis_actual |>
  select(-terminal_delay, slot_adherence)

big_airports <- airports_table |>
  filter(
    country_name == .env$country,
    big == 1
  ) |>
  select(apt_code) |>
  pull()

## prepare data ----
data_prep <- data_raw %>%
  filter(
    state == .env$country,
    airport_icao %in% big_airports
  ) %>%
  mutate_at(
    vars(-one_of(c('year', 'airport_icao'))),
    ~ ifelse(year > year_report, NA, .)
  ) %>%
  filter(airport_icao %in% airports_table$apt_code) %>%
  left_join(airports_table, by = c("airport_icao" = "apt_code")) %>%
  arrange(apt_name) %>%
  rename("Airport name" = apt_name) %>%
  mutate(
    year = factor(year, levels = rp_min_year:rp_max_year),
    "ATC pre departure\ndelay (PI#2)" = format(
      janitor::round_half_up(atc_predep_dly, 2),
      decimals = 2
    ),
    "All causes pre departure\ndelay (PI#3)" = format(
      janitor::round_half_up(all_cause_predep_dly, 2),
      decimals = 2
    ),
  ) %>%
  select(
    year,
    "Airport name",
    "ATC pre departure\ndelay (PI#2)",
    "All causes pre departure\ndelay (PI#3)"
  ) %>%
  pivot_wider(
    names_from = "year",
    values_from = c(
      "ATC pre departure\ndelay (PI#2)",
      "All causes pre departure\ndelay (PI#3)"
    )
  ) %>%
  mutate(across(everything(), ~ str_replace_all(.x, fixed("NA%"), "NA")))
## order columns alphabetically
# select(order(colnames(.))) %>%
# select("Airport Name", everything())

## plot table

table1 <- mygtable(data_prep, myfont) %>%
  tab_spanner_delim(
    delim = "_"
  )


table1
