# main state parameters  ----
country_lower <- country %>% str_to_lower() %>% str_replace_all(., " ", "-")

if (country_lower == 'home') {
  main_ansp <- ""
  nat_curr <- ""
  state_type <- ""
  pp_version <- ""
} else {
  state_parameters <- params_table %>% filter(state == .env$country)
  main_ansp <- state_parameters %>% select(main_ansp) %>% pull()
  # due to new quarto version not handling NAs
  if (is.na(main_ansp)) {
    main_ansp <- ""
  }
  nat_curr <- currency_table %>%
    filter(state == .env$country, year == .env$year_report) %>%
    select(currency) %>%
    pull()
  if (is.na(nat_curr)) {
    nat_curr <- ""
  }
  state_type <- state_parameters %>% select(dashboard_case) %>% pull()
  if (is.na(state_type)) {
    state_type <- ""
  }
  pp_version <- state_parameters %>% select(pp_adoption_full) %>% pull()
  if (is.na(pp_version)) {
    pp_version <- ""
  }
}
## aua entity for capacity  ----
saf_ansps <- saf_ansp_table %>%
  filter(country_name == .env$country) %>%
  filter(year >= year_report) %>%
  filter(year == min(year)) %>%
  select(ansp_name, main)

no_saf_ansps <- nrow(saf_ansps)
main_safety_ansp <- saf_ansps %>%
  filter(main == 1) %>%
  select(ansp_name) %>%
  pull()
saf_ansps <- saf_ansps %>% select(ansp_name) %>% pull()

## icao code for VEF  ----
state_icao_vef_code <- icao_codes_vef_table %>%
  filter(state == .env$country) %>%
  pull()

## aua entity for capacity  ----
aua_entities <- aua_entities_table %>% filter(state == .env$country)
main_ansp_aua <- aua_entities %>%
  filter(year == .env$year_report) %>%
  select(ansp_name) %>%
  pull()

## acc list for context section  ----
acc_list <- acc_list_table %>% filter(state == .env$country)
acc_no <- nrow(acc_list)
acc1 <- if_else(acc_no < 1, '', acc_list$acc_full_name[1])
acc2 <- if_else(acc_no < 2, '', acc_list$acc_full_name[2])
acc3 <- if_else(acc_no < 3, '', acc_list$acc_full_name[3])
acc4 <- if_else(acc_no < 4, '', acc_list$acc_full_name[4])
acc5 <- if_else(acc_no < 5, '', acc_list$acc_full_name[5])

## ecz list and forecast ----
ecz_list <- ecz_list_table %>%
  filter(state == .env$country) %>%
  mutate(
    across(where(is.character), ~ replace_na(.x, "")),
    across(where(is.numeric), ~ replace_na(.x, 0))
  )
no_ecz <- nrow(ecz_list)

### for spain we present only one  traffic zone
if (country == "Spain") {
  statfor_zone <- ecz_list %>%
    filter(statfor_ecz_name == "Spain") %>%
    select(statfor_ecz_name) %>%
    pull()
  ecz_list <- ecz_list %>% filter(ecz_name != "Spain all")
  no_ecz <- nrow(ecz_list)
} else {
  statfor_zone <- ecz_list %>%
    filter(state == country) %>%
    select(statfor_ecz_name) %>%
    pull()
}

forecast <- ecz_list %>% select(forecast) %>% pull() %>% unique()
forecast_id <- ecz_list %>% select(forecast_id) %>% pull() %>% unique()

## tcz list ----
tcz_list <- tcz_list_table %>%
  filter(state == .env$country) %>%
  mutate(
    across(where(is.character), ~ replace_na(.x, "")),
    across(where(is.numeric), ~ replace_na(.x, 0))
  )

no_tcz <- nrow(tcz_list)

# to avoid annoing if_else errors
if (no_tcz == 0) {
  tcz_list <- tcz_list_table %>%
    filter(state == "Spain") %>%
    mutate(
      across(where(is.character), ~""),
      across(where(is.numeric), ~0)
    )
}

## context data ----
context_data <- context_data_table %>%
  filter(state == .env$country, year_report == .env$year_report)

if (year_folder == "rp4") {
  context_data_rp <- context_data_table %>%
    mutate(
      ts_us = as.numeric(stringr::str_replace_all(ts_us, "-", "0")),
      ert_costs = as.numeric(stringr::str_replace_all(ert_costs, "-", "0")),
      trm_costs = as.numeric(stringr::str_replace_all(trm_costs, "-", "0")),
    ) %>%
    group_by(state) %>%
    summarise(
      ts_us = sum(ts_us, na.rm = TRUE),
      ert_costs = sum(ert_costs, na.rm = TRUE),
      trm_costs = sum(trm_costs, na.rm = TRUE)
    )

  tsu_rp_country <- context_data_rp %>%
    filter(state == .env$country) %>%
    select(ts_us) %>%
    pull()

  ert_costs_rp_country <- context_data_rp %>%
    filter(state == .env$country) %>%
    select(ert_costs) %>%
    pull()

  trm_costs_rp_country <- context_data_rp %>%
    filter(state == .env$country) %>%
    select(trm_costs) %>%
    pull()

  tsu_rp_ses <- context_data_rp %>%
    filter(state == rp_full) %>%
    select(ts_us) %>%
    pull()

  ert_costs_rp_ses <- context_data_rp %>%
    filter(state == rp_full) %>%
    select(ert_costs) %>%
    pull()

  trm_costs_rp_ses <- context_data_rp %>%
    filter(state == rp_full) %>%
    select(trm_costs) %>%
    pull()

  tsu_share <- paste0(
    format(
      janitor::round_half_up(tsu_rp_country / tsu_rp_ses * 100, 1),
      nsmall = 1
    ),
    '%'
  )

  ert_cost_share <- paste0(
    format(
      janitor::round_half_up(ert_costs_rp_country / ert_costs_rp_ses * 100, 1),
      nsmall = 1
    ),
    '%'
  )
  trm_cost_share <- paste0(
    format(
      janitor::round_half_up(trm_costs_rp_country / trm_costs_rp_ses * 100, 1),
      nsmall = 1
    ),
    '%'
  )

  ert_trm_share <- paste0(
    format(
      janitor::round_half_up(
        ert_costs_rp_country /
          (ert_costs_rp_country + trm_costs_rp_country) *
          100,
        0
      ),
      nsmall = 0
    ),
    "% / ",
    format(
      janitor::round_half_up(
        trm_costs_rp_country /
          (ert_costs_rp_country + trm_costs_rp_country) *
          100,
        0
      ),
      nsmall = 0
    ),
    "%"
  )

  # =IFERROR(TEXT([@[ERT_costs]]/([@[TRM_Costs]]+[@[ERT_costs]]),"0%") & " / " & TEXT([@[TRM_Costs]]/([@[TRM_Costs]]+[@[ERT_costs]]),"0%"),"-")
} else {
  tsu_share <- paste0(
    format(
      janitor::round_half_up(as.numeric(context_data$tsu_share) * 100, 1),
      nsmall = 1
    ),
    '%'
  )
  ert_cost_share <- paste0(
    format(
      janitor::round_half_up(as.numeric(context_data$ert_cost_share) * 100, 1),
      nsmall = 1
    ),
    '%'
  )
  ert_trm_share <- context_data$ert_trm_share
}

xrate_ref <- context_data[[paste0("xrate", cef_ref_year)]]

no_apt_big <- context_data$no_apts_big
no_apt_small <- context_data$no_apts_small

other_orgs <- other_orgs_table %>% filter(state == .env$country)
other_ansps <- other_orgs %>%
  filter(type == "Other ANSPs") %>%
  select(ansp) %>%
  filter(ansp != '-')
other_met <- other_orgs %>%
  filter(type == "MET Providers") %>%
  select(ansp) %>%
  filter(ansp != '-')

### create strings for context section
other_ansps_str <- '--'
if (nrow(other_ansps) > 0) {
  for (i in 1:nrow(other_ansps)) {
    other_ansps_str <- paste0(
      if_else(i == 1, '', other_ansps_str),
      '• ',
      other_ansps[i, ],
      '<br/>'
    )
  }
}

other_met_str <- '--'
if (nrow(other_met) > 0) {
  for (i in 1:nrow(other_met)) {
    other_met_str <- paste0(
      if_else(i == 1, '', other_met_str),
      '• ',
      other_met[i, ],
      '<br/>'
    )
  }
}

fac_val_date <- fact_val_table |>
  filter(state == country) |>
  pull(max_date) |>
  format(format = "%Y-%m-%d")

fac_val_file <- fact_val_table |> filter(state == country) |> pull(file_version)


# # get level 2 data files (not needed for SES or NM) ----
cap_file <- ''
cap_trm_file <- ''
ceff_file <- ''
ceff_file_canarias <- ''
env_kea_file <- ''
env_apt_file <- ''
env_mil_file <- ''
saf_eosm_file <- ''
