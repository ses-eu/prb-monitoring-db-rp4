library(janitor)
library(tidyr)
library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)
library(here)
library(lubridate)

# main state params ----
params_table <- read_mytable(lists_data_file, "lists", "Table_States") %>%
  clean_names()

state_list <- params_table %>% select(state) %>% unlist()

icao_codes_vef_table <- read_mytable(
  lists_data_file,
  "lists",
  "Table_ICAO_Codes"
) %>%
  clean_names()

aua_entities_table <- read_mytable(lists_data_file, "lists", "Table_AUA") %>%
  clean_names()

acc_list_table <- read_mytable(lists_data_file, "lists", "Table_ACCs") %>%
  clean_names()

ecz_list_table <- read_mytable(lists_data_file, "lists", "Table_ECZ") %>%
  left_join(
    read_mytable(lists_data_file, "lists", "Table_forecast"),
    by = "forecast_id"
  ) %>%
  clean_names()

tcz_list_table <- read_mytable(lists_data_file, "lists", "Table_TCZ") %>%
  clean_names()

context_data_table <- read_mytable(
  context_data_file,
  "context",
  "Table_context"
) %>%
  clean_names()

other_orgs_table <- read_mytable(
  lists_data_file,
  "lists",
  "Table_PP_other_ANSPs"
) %>%
  clean_names()

saf_ansp_table <- read_mytable(lists_data_file, "lists", "Table_SAF_ANSP") %>%
  clean_names()

airports_table <- read_mytable(lists_data_file, "lists", "Table_tcz_apt") %>%
  clean_names()


# traffic ----
## forecast & actuals ----
statfor_mvt <- read_xlsx(
  here(data_folder, statfor_mvt_data_file),
  sheet = "data",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() %>%
  filter(daio == "T" & year >= rp_min_year - 1 & year <= rp_max_year)

statfor_tsu <- read_xlsx(
  here(data_folder, statfor_tsu_data_file),
  sheet = "data",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() %>%
  filter(year >= rp_min_year - 1 & year <= rp_max_year)

## targets ----
traffic_target <- read_xlsx(
  here(data_folder, targets_data_file),
  sheet = "IFR_MVTS",
  range = cell_limits(c(3, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() %>%
  select(state, year, x121_ecz_name, x121_ecz_ifr_mvt, x121_ecz_su)

# SAF ----
## State ----
### EoSM ----
saf_maturity <- read_xlsx(
  here(data_folder, saf_data_file),
  sheet = "A>P",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

saf_eosm <- read_xlsx(
  here(data_folder, saf_data_file),
  sheet = "EoSM",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

### RI & SMI ----
saf_ri_actual <- read_xlsx(
  here(data_folder, saf_data_file),
  sheet = "RI - occurrences",
  range = cell_limits(c(1, 1), c(NA, 10))
) %>%
  as_tibble() %>%
  clean_names()

saf_smi_actual <- read_xlsx(
  here(data_folder, saf_data_file),
  sheet = "SMI - occurrences",
  range = cell_limits(c(1, 1), c(NA, 10))
) %>%
  as_tibble() %>%
  clean_names()

saf_ri_actual_apt <- read_xlsx(
  here(data_folder, saf_data_file),
  sheet = "SPI1c-RI_Airport",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

saf_smi_actual_apt <- read_xlsx(
  here(data_folder, saf_data_file),
  sheet = "SPI1d-SMI_ANSPs",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

### AUTO TOOLS ----
saf_auto_tools_actual <- read_xlsx(
  here(data_folder, saf_data_file),
  sheet = "Automated tools",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

## SES ----
### EoSM ----
saf_eosm_ses <- read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "SES_EoSM",
  range = cell_limits(c(1, 1), c(NA, 6))
) %>%
  as_tibble() %>%
  clean_names()

saf_eosm_ansp_ses <- read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "EoSM target #ANSPs",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

saf_eosm_interdep_ses <- read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "EoSM interdipendency #ANSPs",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

### RI & SMI ----
saf_ri_actual_ses <- read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "RI - occurrences",
  range = cell_limits(c(1, 1), c(NA, 5))
) %>%
  as_tibble() %>%
  clean_names() |>
  mutate(state = rp_full)

saf_smi_actual_ses <- read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "SMI - occurrences",
  range = cell_limits(c(1, 1), c(NA, 5))
) %>%
  as_tibble() %>%
  clean_names() |>
  mutate(state = rp_full)

saf_ri_var_ses <- read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "RI SES variation",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(
    field = str_remove_all(field, " RI")
  )

saf_smi_var_ses <- read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "SMI SES variation",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(
    field = str_remove_all(field, " SMI")
  )


## NM ----
### EoSM ----
saf_eosm_nm <- read_xlsx(
  here(data_folder, nm_data_file),
  sheet = "EoSM",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

### OVERDELIVERIES ----
saf_overd_nm <- read_xlsx(
  here(data_folder, nm_data_file),
  sheet = "PI_overdeliveries",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

# ENV ----
## targets ----
### State ----
#### KEA ----
kea_target <- read_xlsx(
  here(data_folder, targets_data_file),
  sheet = "KEA",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() %>%
  rbind(
    read_xlsx(
      here(data_folder, targets_data_file),
      sheet = "KEA_FABEC",
      range = cell_limits(c(1, 1), c(NA, NA))
    ) %>%
      as_tibble() %>%
      clean_names()
  ) %>%
  select(year, state, kea_target = x321_kea_target)

### SES ----
#### KEA ----
kea_target_ses <- read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "Table_KEA Targets",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() |>
  mutate(entity_name = rp_full)

## actuals ----
### State ----
#### KEA ----
kea_actual <- read_xlsx(
  here(data_folder, env_data_file),
  sheet = "kea",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

kea_actual_mm <- read_xlsx(
  here(data_folder, env_data_file),
  sheet = "kea_mm",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(month = as.Date(month))

#### KEP ----
kep_actual <- read_xlsx(
  here(data_folder, env_data_file),
  sheet = "kep",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

kep_actual_mm <- read_xlsx(
  here(data_folder, env_data_file),
  sheet = "kep_mm",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(month = as.Date(month))

#### SCR ----
scr_actual <- read_xlsx(
  here(data_folder, env_data_file),
  sheet = "scr",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()


scr_actual_mm <- read_xlsx(
  here(data_folder, env_data_file),
  sheet = "scr_mm",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(month = as.Date(month))

#### VFE_ERT ----
vfe_ert_actual <- read_xlsx(
  here(data_folder, env_data_file),
  sheet = "VFE_ERT",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() %>%
  group_by(year, area_id, area_name) %>%
  summarise(
    dist_km_enr_vfe = sum(dist_km_enr_vfe, na.rm = TRUE),
    dist_km_vfe_above_within = sum(dist_km_vfe_above_within, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    vfe_ert = dist_km_vfe_above_within / dist_km_enr_vfe
  )

#### AXOT ----
axot_actual_ms <- read_xlsx(
  here(data_folder, env_data_file),
  sheet = "axot_ms",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

axot_actual_apt <- read_xlsx(
  here(data_folder, env_data_file),
  sheet = "axot_apt",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

#### AXIT ----
axit_actual_ms <- read_xlsx(
  here(data_folder, env_data_file),
  sheet = "axin_ms",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

axit_actual_apt <- read_xlsx(
  here(data_folder, env_data_file),
  sheet = "axin_apt",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()


#### ASMA ----
asma_actual_ms <- read_xlsx(
  here(data_folder, env_data_file),
  sheet = "asma_ms",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

asma_actual_apt <- read_xlsx(
  here(data_folder, env_data_file),
  sheet = "asma_apt",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

#### CDO ----
cdo_cco_actual <- read_xlsx(
  here(data_folder, env_data_file),
  sheet = "CDO_APT_MM",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() %>%
  group_by(year, apt_icao) %>%
  summarise(
    nbr_flights_descent = sum(nbr_flights_descent, na.rm = TRUE),
    tot_time_level_seconds_descent = sum(
      tot_time_level_seconds_descent,
      na.rm = TRUE
    ),
    nbr_cdo_flights = sum(nbr_cdo_flights, na.rm = TRUE),
    nbr_flights_climb = sum(nbr_flights_climb, na.rm = TRUE),
    tot_time_level_seconds_climb = sum(
      tot_time_level_seconds_climb,
      na.rm = TRUE
    ),
    nbr_cco_flights = sum(nbr_cco_flights, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    avg_seconds_per_descent = tot_time_level_seconds_descent /
      nbr_flights_descent,
    avg_seconds_per_climb = tot_time_level_seconds_climb / nbr_flights_climb
  )

#### MIL ----
env_mil_actual <- read_xlsx(
  here(data_folder, env_data_file),
  sheet = "mil_pis",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

### SES ----
#### KEA ----
kea_actual_ses <- read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "Table_HFE",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() |>
  mutate(entity_name = rp_full)

kea_actual_mm_ses <- read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "Table_HFE MM",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

#### KEP ----
kep_actual_ses <- read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "Table_KEP",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() |>
  mutate(entity_name = rp_full)

kep_actual_mm_ses <- read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "Table_KEP MM",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

#### SCR ----
scr_actual_ses <- read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "Table_SCR",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() |>
  mutate(entity_name = rp_full)

scr_actual_mm_ses <- read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "Table_SCR MM",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

### NM ----
kep_nm <- read_xlsx(
  paste0(data_folder, nm_data_file),
  sheet = "Environment",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

# CAP ----
## NM ----

## Targets ----
cap_ert_nm <- read_xlsx(
  here(data_folder, nm_data_file),
  sheet = "Capacity_En route",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

cap_trm_nm <- read_xlsx(
  here(data_folder, nm_data_file),
  sheet = "Capacity_Terminal",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()


cap_pis_nm <- read_xlsx(
  paste0(data_folder, nm_data_file),
  sheet = "Capacity_PIs",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

### State ----
#### ATFM DELAY ----
cap_ert_target <- read_xlsx(
  here(data_folder, targets_data_file),
  sheet = "ER_CAP",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() %>%
  select(year, state, delay_target = x331_ert_delay_target) %>%
  rbind(
    read_xlsx(
      here(data_folder, targets_data_file),
      sheet = "ER_CAP_FABEC",
      range = cell_limits(c(1, 1), c(NA, NA))
    ) %>%
      as_tibble() %>%
      clean_names() %>%
      select(
        year,
        state = x331_entity_name,
        delay_target = x331_ert_delay_target
      )
  ) %>%
  mutate(
    state = case_when(
      state == "skeyes" ~ 'Belgium',
      state == "DSNA" ~ 'France',
      state == "DFS" ~ 'Germany',
      state == "LVNL" ~ 'Netherlands',
      state == "Skyguide" ~ 'Switzerland',
      .default = state
    )
  )

cap_trm_target <- read_xlsx(
  here(data_folder, targets_data_file),
  sheet = "TRM_CAP",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() %>%
  select(year, state, delay_target = x332_state_arr_delay_target)

#### ATCOs IN OPS ----
cap_atco_acc_planned <- read_xlsx(
  here(data_folder, targets_data_file),
  sheet = "ATCO_PLANNING",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() %>%
  select(state, acc, year, planned_atco_number)

### SES ----
#### ATFM DELAY ----
cap_ert_target_ses <- read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "en route delay targets",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

#### ATCOs IN OPS ----
cap_atco_planned_ses <- cap_atco_acc_planned %>%
  group_by(year) %>%
  summarise(
    planned_atco_number = sum(planned_atco_number, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    state = rp_full,
    acc = "ZZZZ"
  )


## Actuals ----
### State ----
#### ATFM DELAY ----
cap_ert_atfm_actual_mm <- read_xlsx(
  here(data_folder, cap_data_file),
  sheet = "enroute_atfm_delay_mm",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

cap_ert_atfm_actual <- cap_ert_atfm_actual_mm %>%
  group_by(state, year) %>%
  summarise(
    ifr_movements = sum(ifr_movements, na.rm = TRUE),
    atc_capacity = sum(atc_capacity, na.rm = TRUE),
    atc_disruptions = sum(atc_disruptions, na.rm = TRUE),
    atc_staffing = sum(atc_staffing, na.rm = TRUE),
    other_non_atc = sum(other_non_atc, na.rm = TRUE),
    weather = sum(weather, na.rm = TRUE),
    total_delay = sum(total_delay, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    average_delay_per_flight = total_delay / ifr_movements
  )

cap_trm_atfm_actual_mm <- read_xlsx(
  here(data_folder, cap_data_file),
  sheet = "terminal_atfm_delay_mm",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() %>%
  rename(
    ifr_movements = arrivals,
    total_delay = total_arrival_delay
  )

cap_trm_atfm_actual <- cap_trm_atfm_actual_mm %>%
  group_by(state, year) %>%
  summarise(
    ifr_movements = sum(ifr_movements, na.rm = TRUE),
    atc_capacity = sum(atc_capacity, na.rm = TRUE),
    atc_disruptions = sum(atc_disruptions, na.rm = TRUE),
    atc_staffing = sum(atc_staffing, na.rm = TRUE),
    other_non_atc = sum(other_non_atc, na.rm = TRUE),
    weather = sum(weather, na.rm = TRUE),
    total_delay = sum(total_delay, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    average_delay_per_flight = total_delay / ifr_movements
  )

#### DELAY TIME BIN ----
cap_delay_bin_actual <- read_xlsx(
  here(data_folder, cap_data_file),
  sheet = "delay_time_bin",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

#### ALL-CAUSE PREDEP DELAY ----
cap_all_c_predep_delay_actual <- read_xlsx(
  here(data_folder, cap_data_file),
  sheet = "all_cause_predep_delay",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

#### ATCOs IN OPS ----
cap_atco_acc_actual <- read_xlsx(
  here(data_folder, cap_data_file),
  sheet = "atcos",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() %>%
  select(state, acc, year, actual_atco_number)

#### SECTOR HOURS ----
cap_sector_hours_actual <- read_xlsx(
  here(data_folder, cap_data_file),
  sheet = "sector_hour",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

#### OTHER APT PIs ----
cap_apt_pis_actual <- read_xlsx(
  here(data_folder, cap_data_file),
  sheet = "airport_pis",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

#### AVG PEAK THROUPUT ----
cap_avg_peak_tp_actual <- read_xlsx(
  here(data_folder, cap_data_file),
  sheet = "AVG_DAY_PEAK_THROUPUT",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(
    acc_id = substr(stat_aua_code, 1, 4)
  ) %>%
  right_join(acc_list_table, by = "acc_id")


#### SHARE_DELAY_WHEN_TP_ABOVE ----
cap_perc_delay_tp_above <- read_xlsx(
  here(data_folder, cap_data_file),
  sheet = "SHARE_DELAY_WHEN_TP_ABOVE",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(
    acc_id = substr(asp_id, 1, 4)
  ) %>%
  right_join(acc_list_table, by = "acc_id") %>%
  group_by(year, acc_full_name, acc_ctry) %>%
  summarise(
    tot_tfc = sum(tot_tfc, na.rm = TRUE),
    tot_atfm_dly = sum(tot_atfm_dly, na.rm = TRUE),
    tot_dly_where_tfc_guid_tfc = sum(tot_dly_where_tfc_guid_tfc, na.rm = TRUE),
    days_where_tfc_guid_tfc = sum(days_where_tfc_guid_tfc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    perc_delay_tp_above = tot_dly_where_tfc_guid_tfc / tot_atfm_dly
  )


### SES ----
#### ATFM DELAY ----
cap_ert_atfm_actual_mm_ses <- read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "en route monthly delay",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

cap_ert_atfm_actual_ses <- cap_ert_atfm_actual_mm_ses %>%
  group_by(state, year) %>%
  summarise(
    ifr_movements = sum(ifr_movements, na.rm = TRUE),
    atc_capacity = sum(atc_capacity, na.rm = TRUE),
    atc_disruptions = sum(atc_disruptions, na.rm = TRUE),
    atc_staffing = sum(atc_staffing, na.rm = TRUE),
    other_non_atc = sum(other_non_atc, na.rm = TRUE),
    weather = sum(weather, na.rm = TRUE),
    total_delay = sum(total_delay, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    average_delay_per_flight = total_delay / ifr_movements
  )

cap_trm_atfm_actual_ses <- cap_trm_atfm_actual %>%
  group_by(year) %>%
  summarise(
    ifr_movements = sum(ifr_movements, na.rm = TRUE),
    atc_capacity = sum(atc_capacity, na.rm = TRUE),
    atc_disruptions = sum(atc_disruptions, na.rm = TRUE),
    atc_staffing = sum(atc_staffing, na.rm = TRUE),
    other_non_atc = sum(other_non_atc, na.rm = TRUE),
    weather = sum(weather, na.rm = TRUE),
    total_delay = sum(total_delay, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    state = rp_full,
    average_delay_per_flight = total_delay / ifr_movements
  )

cap_trm_atfm_actual_mm_ses <- cap_trm_atfm_actual_mm %>%
  group_by(year, month) %>%
  summarise(
    ifr_movements = sum(ifr_movements, na.rm = TRUE),
    atc_capacity = sum(atc_capacity, na.rm = TRUE),
    atc_disruptions = sum(atc_disruptions, na.rm = TRUE),
    atc_staffing = sum(atc_staffing, na.rm = TRUE),
    other_non_atc = sum(other_non_atc, na.rm = TRUE),
    weather = sum(weather, na.rm = TRUE),
    total_delay = sum(total_delay, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    state = rp_full,
    average_delay_per_flight = total_delay / ifr_movements
  )

#### ALL-CAUSE PREDEP DELAY ----
cap_all_c_predep_delay_actual_ses <- cap_all_c_predep_delay_actual %>%
  group_by(year) %>%
  summarise(
    fl_total = sum(fl_total, na.rm = TRUE),
    dly_all_min_total = sum(dly_all_min_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    all_cause_predep_dly = dly_all_min_total / fl_total,
    state = rp_full
  )

#### DELAY TIME BIN ----
cap_delay_bin_actual_ses <- cap_delay_bin_actual %>%
  group_by(year) %>%
  summarise(
    total = sum(total, na.rm = TRUE),
    x0_5mins = sum(x0_5mins, na.rm = TRUE),
    x5_15_mins = sum(x5_15_mins, na.rm = TRUE),
    x15_30_mins = sum(x15_30_mins, na.rm = TRUE),
    x30_60_mins = sum(x30_60_mins, na.rm = TRUE),
    x60_mins = sum(x60_mins, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    state = rp_full,
    ansp = rp_full
  )

#### ATCOs IN OPS ----
cap_atco_actual_ses <- cap_atco_acc_actual %>%
  group_by(year) %>%
  summarise(
    actual_atco_number = sum(actual_atco_number, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    state = rp_full,
    acc = "ZZZZ"
  )

#### SECTOR HOURS ----
cap_sector_hours_actual_ses <- cap_sector_hours_actual %>%
  group_by(year) %>%
  summarise(
    total_soh = sum(total_soh, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    state = rp_full,
    ansp = rp_full
  )

# CEFF ----
## En-route ----
ceff_t1_ert <- read_xlsx(
  here(data_folder, ceff_data_file),
  sheet = "Enroute_T1",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

ceff_t2_ert <- read_xlsx(
  here(data_folder, ceff_data_file),
  sheet = "Enroute_T2",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

ceff_t3_ert <- read_xlsx(
  here(data_folder, ceff_data_file),
  sheet = "Enroute_T3",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

### SES ----
ceff_ses_duc_ert <- read_xlsx(
  here(data_folder, ceff_data_file),
  sheet = "ses_duc_ert",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

ceff_ses_duc_trm <- read_xlsx(
  here(data_folder, ceff_data_file),
  sheet = "ses_duc_trm",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

### NM ----
ceff_nm_ert <- read_xlsx(
  here(data_folder, nm_data_file),
  sheet = "Cost-efficiency",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()


## Terminal ----
ceff_t1_trm <- read_xlsx(
  here(data_folder, ceff_data_file),
  sheet = "Terminal_T1",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

ceff_t2_trm <- read_xlsx(
  here(data_folder, ceff_data_file),
  sheet = "Terminal_T2",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

ceff_t3_trm <- read_xlsx(
  here(data_folder, ceff_data_file),
  sheet = "Terminal_T3",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()

## Yearly xrates ----
xrate_ert <- read_xlsx(
  here(data_folder, ceff_data_file),
  sheet = "xrate_ert",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() %>%
  filter(status == "D") %>%
  mutate(cztype = "enroute", cz_code = paste0(country_zone_code, "_ECZ")) %>%
  select(
    cz_code,
    cztype,
    year,
    currency,
    xrate = exchange_rate,
    xrate_ref = exchange_rate_ref2022
  )

tcz_list_table_xrate <- tcz_list_table %>%
  distinct(country_zone_code = icao_code, cz_code = tcz_id)


xrate_trm <- read_xlsx(
  here(data_folder, ceff_data_file),
  sheet = "xrate_trm",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names() %>%
  filter(status == "D") %>%
  right_join(
    tcz_list_table_xrate,
    by = c("country_zone_code"),
    relationship = "many-to-many"
  ) %>%
  mutate(cztype = "terminal") %>%
  select(
    cz_code,
    cztype,
    year,
    currency,
    xrate = exchange_rate,
    xrate_ref = exchange_rate_ref2022
  )


xrate_year_states <- xrate_ert %>% rbind(xrate_trm)

xrate_ses <- xrate_year_states %>%
  distinct(year, cztype) %>%
  mutate(
    cz_code = "SES",
    currency = "EUR",
    xrate = 1,
    xrate_ref = 1
  ) %>%
  select(cz_code, cztype, year, currency, xrate, xrate_ref)


xrate_year <- xrate_year_states %>% rbind(xrate_ses)

## Forecast SU for Temp ur ----
cef_temp_su_t2 <- read_xlsx(
  here(data_folder, ceff_data_file),
  sheet = "forecast_su_ert",
  range = cell_limits(c(1, 1), c(NA, NA))
) %>%
  as_tibble() %>%
  clean_names()


data_loaded <- TRUE
