
## import data  ----
if (!exists("data_loaded")) {
  source("R/get_data.R")
}

data_raw  <-  cdo_cco_actual

## prepare data ----
airports_country <- airports_table %>% 
  filter(country_name == .env$country) %>% 
  select(apt_code, apt_name)

cross_table <- crossing(airports_country, rp_years)

data_filtered <- data_raw %>% 
  filter(
    year <= .env$year_report
    # apt_icao %in% airports_country$apt_code
  ) %>% 
  right_join(
    cross_table, by = c("apt_icao" = "apt_code", "year" = "rp_years")
  )


### We take the top 15 airports for France
if (country == "France") {
  data_filtered <- data_filtered |> 
    filter(is.na(nbr_flights_descent) == FALSE) |> 
    arrange(desc(nbr_flights_descent)) |> 
    slice_head(n = 15)
}

data_prep <- data_filtered %>% 
  select(
    year, 
    Airport = apt_name, 
    `Avg. duration in descent` = avg_seconds_per_descent,
    `Avg. duration in climb` = avg_seconds_per_climb
  ) %>%
  pivot_wider(names_from = "year", values_from = c("Avg. duration in descent",
                                                   "Avg. duration in climb")
              # , names_glue = "{year}_{.value}" #suffix to prefix
  ) %>%
  mutate(
    across(c(-1), ~format(round(.x, 1), nsmall =1))
  ) 

data_prep_pdf <- data_prep 


# plot table ----

table1 <- mygtable(data_prep, myfont*0.9) %>% 
  tab_spanner_delim(
    delim = "_"
  ) |> 
  tab_header(
    title = md("**Airport level**")
  )

table1
