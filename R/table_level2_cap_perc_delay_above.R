## import data  ----
if (!exists("data_loaded")) {
  source("R/params_country.R")
  source("R/get_data.R")
}

data_raw  <-  cap_perc_delay_tp_above

## prepare data ----
cross_table <- crossing(year = rp_years, acc_full_name=acc_list$acc_full_name)

fmt_int <- function(x) format(x, big.mark = ",", scientific = FALSE)
fmt_pct <- function(x) paste0(format(round(x * 100, 1), nsmall = 1, scientific = FALSE), "%")

spec <- list(
  Flights = list(src = "tot_tfc", fn = fmt_int),
  `Total en route ATFM delay (min)` = list(src = "tot_atfm_dly", fn = fmt_int),
  `% en route ATFM delay\n(traffic > forecast) (PI#4)` = list(src = "perc_delay_tp_above", fn = fmt_pct),
  `Days with traffic > forecast\nAND en-route ATFM delay` = list(src = "days_where_tfc_guid_tfc", fn = fmt_int)
)

out_cols <- c("ACC name", names(spec))

data_prep <- data_raw %>%
  filter(acc_full_name %in% acc_list$acc_full_name) %>%
  filter(year >= rp_min_year, year <= year_report) %>%
  right_join(cross_table, by = c("year", "acc_full_name")) %>%
  arrange(acc_full_name, year) %>%
  rename(`ACC name` = acc_full_name) %>%
  mutate(
    ### chatgpt
    year = factor(year, levels = rp_min_year:rp_max_year),
    !!!setNames(
      lapply(spec, \(s) rlang::call2(s$fn, rlang::sym(s$src))),
      names(spec)
    )
  ) %>%
  select(year, all_of(out_cols)) %>%
  pivot_wider(names_from = year, values_from = all_of(names(spec))) %>% 
  mutate(across(everything(), ~ str_replace_all(.x, fixed("NA%"), "NA")))


## plot table

table1 <- mygtable(data_prep, myfont*0.9) %>% 
  tab_spanner_delim(
    delim = "_"
  )
# |> 
#   tab_header(
#     title = md("**Airport level**")
#   )


table1
