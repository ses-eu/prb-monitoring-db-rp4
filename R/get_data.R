library(janitor)
library(tidyr)
library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)
library(here)
library(lubridate)

# main state params ----
params_table <- read_mytable(lists_data_file, "lists", "Table_States") %>% clean_names()

state_list <- params_table %>% select(state) %>% unlist()

aua_entities_table <- read_mytable(lists_data_file, "lists", "Table_AUA") %>% clean_names()

acc_list_table <- read_mytable(lists_data_file, "lists", "Table_ACCs") %>% clean_names()

ecz_list_table <- read_mytable(lists_data_file, "lists", "Table_ECZ") %>%
  left_join(
    read_mytable(lists_data_file, "lists", "Table_forecast"),
    by = "forecast_id"
    ) %>% clean_names()

tcz_list_table <- read_mytable(lists_data_file, "lists", "Table_TCZ") %>% clean_names()

context_data_table <- read_mytable(context_data_file, "context", "Table_context") %>%  clean_names()

other_orgs_table <- read_mytable(lists_data_file, "lists", "Table_PP_other_ANSPs") %>%  clean_names()

saf_ansp_table <- read_mytable(lists_data_file, "lists", "Table_SAF_ANSP") %>% clean_names()

airports_table <- read_mytable(lists_data_file, "lists", "Table_tcz_apt") %>%  clean_names()

# traffic ----
## forecast & actuals ----
statfor_mvt  <-  read_xlsx(
  here(data_folder, statfor_mvt_data_file),
  sheet = "data",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>%
  clean_names() %>% 
  filter (daio == "T" & year >= rp_min_year-1 & year <= rp_max_year)

statfor_tsu  <-  read_xlsx(
  here(data_folder, statfor_tsu_data_file),
  sheet = "data",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>%
  clean_names() %>% 
  filter (year >= rp_min_year-1 & year <= rp_max_year)

## targets ----
traffic_target  <-  read_xlsx(
  here(data_folder, targets_data_file),
  sheet = "IFR_MVTS",
  range = cell_limits(c(3, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() %>% 
  select(state, year, x121_ecz_name,x121_ecz_ifr_mvt, x121_ecz_su)
  
# SAF ----
## State ----
### EoSM ----
saf_maturity  <-  read_xlsx(
  here(data_folder, saf_data_file),
  sheet = "A>P",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

saf_eosm  <-  read_xlsx(
  here(data_folder, saf_data_file),
  sheet = "EoSM",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

### RI & SMI ----
saf_ri_actual  <-  read_xlsx(
  here(data_folder, saf_data_file),
  sheet = "RI - occurrences",
  range = cell_limits(c(1, 1), c(NA, 10))) %>%
  as_tibble() %>% 
  clean_names() 

saf_smi_actual  <-  read_xlsx(
  here(data_folder, saf_data_file),
  sheet = "SMI - occurrences",
  range = cell_limits(c(1, 1), c(NA, 10))) %>%
  as_tibble() %>% 
  clean_names() 

saf_ri_actual_apt  <-  read_xlsx(
  here(data_folder, saf_data_file),
  sheet = "SPI1c-RI_Airport",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

saf_smi_actual_apt  <-  read_xlsx(
  here(data_folder, saf_data_file),
  sheet = "SPI1d-SMI_ANSPs",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

### AUTO TOOLS ----
saf_auto_tools_actual <- read_xlsx(
  here(data_folder, saf_data_file),
  sheet = "Automated tools",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

## SES ----
### EoSM ----
saf_eosm_ses  <-  read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "SES_EoSM",
  range = cell_limits(c(1, 1), c(NA, 6))) %>%
  as_tibble() %>% 
  clean_names() 

saf_eosm_ansp_ses  <-  read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "EoSM target #ANSPs",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

saf_eosm_interdep_ses  <-  read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "EoSM interdipendency #ANSPs",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

### RI & SMI ----
saf_ri_actual_ses <- read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "RI - occurrences",
  range = cell_limits(c(1, 1), c(NA, 5))) %>%
  as_tibble() %>% 
  clean_names() |> 
  mutate(state = rp_full)

saf_smi_actual_ses <- read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "SMI - occurrences",
  range = cell_limits(c(1, 1), c(NA, 5))) %>%
  as_tibble() %>% 
  clean_names() |> 
  mutate(state = rp_full)

saf_ri_var_ses <- read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "RI SES variation",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() %>% 
  mutate(
    field = str_remove_all(field, " RI")
  )

saf_smi_var_ses <- read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "SMI SES variation",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
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
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names()

### OVERDELIVERIES ----
saf_overd_nm  <-  read_xlsx(
  here(data_folder, nm_data_file),
  sheet = "PI_overdeliveries",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# ENV ----
## targets ----
### State ----
#### KEA ----
kea_target  <-  read_xlsx(
  here(data_folder, targets_data_file),
  sheet = "KEA",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() %>% 
  rbind (
    read_xlsx(
      here(data_folder, targets_data_file),
      sheet = "KEA_FABEC",
      range = cell_limits(c(1, 1), c(NA, NA))) %>%
      as_tibble() %>% 
      clean_names()
  ) %>% 
  select(year, state, 
         kea_target = x321_kea_target)

### SES ----
#### KEA ----
kea_target_ses  <-  read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "Table_KEA Targets",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() |> 
  mutate(entity_name = rp_full)

## actuals ----
### State ----
#### KEA ----
kea_actual  <-  read_xlsx(
  here(data_folder, env_data_file),
  sheet = "kea",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

kea_actual_mm  <-  read_xlsx(
  here(data_folder, env_data_file),
  sheet = "kea_mm",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() %>% 
  mutate(month = as.Date(month))

#### KEP ----
kep_actual <-  read_xlsx(
  here(data_folder, env_data_file),
  sheet = "kep",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

kep_actual_mm  <-  read_xlsx(
  here(data_folder, env_data_file),
  sheet = "kep_mm",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() %>% 
  mutate(month = as.Date(month))

#### SCR ----
scr_actual <-  read_xlsx(
  here(data_folder, env_data_file),
  sheet = "scr",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 


scr_actual_mm  <-  read_xlsx(
  here(data_folder, env_data_file),
  sheet = "scr_mm",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() %>% 
  mutate(month = as.Date(month))

#### AXOT ----
axot_actual_ms  <-  read_xlsx(
  here(data_folder, env_data_file),
  sheet = "axot_ms",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

axot_actual_apt  <-  read_xlsx(
  here(data_folder, env_data_file),
  sheet = "axot_apt",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

#### ASMA ----
asma_actual_ms  <-  read_xlsx(
  here(data_folder, env_data_file),
  sheet = "asma_ms",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

asma_actual_apt  <-  read_xlsx(
  here(data_folder, env_data_file),
  sheet = "asma_apt",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

#### CDO ----
cdo_actual_ms  <-  read_xlsx(
  here(data_folder, env_data_file),
  sheet = "cdo_ms",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

cdo_actual_apt  <-  read_xlsx(
  here(data_folder, env_data_file),
  sheet = "cdo_apt",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

#### MIL ----
env_mil_actual  <-  read_xlsx(
  here(data_folder, env_data_file),
  sheet = "mil_pis",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

### SES ----
#### KEA ----
kea_actual_ses  <-  read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "Table_HFE",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() |> 
  mutate(entity_name = rp_full)

kea_actual_mm_ses  <-  read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "Table_HFE MM",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names()

#### KEP ----
kep_actual_ses  <-  read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "Table_KEP",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>%
  clean_names() |>
  mutate(entity_name = rp_full)

kep_actual_mm_ses  <-  read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "Table_KEP MM",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names()

#### SCR ----
scr_actual_ses  <-  read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "Table_SCR",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>%
  clean_names() |>
  mutate(entity_name = rp_full)

scr_actual_mm_ses  <-  read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "Table_SCR MM",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names()

### NM ----
kep_nm <- read_xlsx(
  paste0(data_folder, nm_data_file),
  sheet = "Environment",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names()

# CAP ----
## NM ----

## Targets ----
cap_ert_nm  <-  read_xlsx(
  here(data_folder, nm_data_file),
  sheet = "Capacity_En route",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

cap_trm_nm <- read_xlsx(
  here(data_folder, nm_data_file),
  sheet = "Capacity_Terminal",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 


cap_pis_nm  <- read_xlsx(
  paste0(data_folder, nm_data_file),
  sheet = "Capacity_PIs",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names()

### State ----
#### ATFM DELAY ----
cap_ert_target  <-  read_xlsx(
  here(data_folder, targets_data_file),
  sheet = "ER_CAP",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() %>% 
  select(year, state, delay_target = x331_ert_delay_target) %>% 
  rbind (
    read_xlsx(
      here(data_folder, targets_data_file),
      sheet = "ER_CAP_FABEC",
      range = cell_limits(c(1, 1), c(NA, NA))) %>%
      as_tibble() %>% 
      clean_names() %>% 
      select(year, state = x331_entity_name, delay_target = x331_ert_delay_target)
  ) %>% 
  mutate( state = case_when(
      state == "skeyes" ~ 'Belgium',
      state == "DSNA" ~ 'France',
      state == "DFS" ~ 'Germany',
      state == "LVNL" ~ 'Netherlands',
      state == "Skyguide" ~ 'Switzerland',
      .default = state
    )
  )

cap_trm_target  <-  read_xlsx(
  here(data_folder, targets_data_file),
  sheet = "TRM_CAP",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() %>% 
  select(year, state, delay_target = x332_state_arr_delay_target)

#### ATCOs IN OPS ----
cap_atco_acc_planned  <- read_xlsx(
  here(data_folder, targets_data_file),
  sheet = "ATCO_PLANNING",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() %>% 
  select(state, acc, year, planned_atco_number)

### SES ----
#### ATFM DELAY ----
cap_ert_target_ses  <-  read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "en route delay targets",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 


## Actuals ----
### State ----
#### ATFM DELAY ----
cap_ert_atfm_actual  <-  read_xlsx(
  here(data_folder, cap_data_file),
  sheet = "enroute_atfm_delay",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

cap_ert_atfm_actual_mm  <-  read_xlsx(
  here(data_folder, cap_data_file),
  sheet = "enroute_atfm_delay_mm",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

cap_trm_atfm_actual  <-  read_xlsx(
  here(data_folder, cap_data_file),
  sheet = "terminal_atfm_delay",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

cap_trm_atfm_actual_mm  <-  read_xlsx(
  here(data_folder, cap_data_file),
  sheet = "terminal_atfm_delay_mm",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

#### DELAY TIME BIN ----
cap_delay_bin_actual  <-  read_xlsx(
  here(data_folder, cap_data_file),
  sheet = "delay_time_bin",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

#### ALL-CAUSE PREDEP DELAY ----
cap_all_c_predep_delay_actual  <-  read_xlsx(
  here(data_folder, cap_data_file),
  sheet = "all_cause_predep_delay",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

#### ATCOs IN OPS ----
cap_atco_acc_actual  <- read_xlsx(
  here(data_folder, cap_data_file),
  sheet = "atcos",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() %>% 
  select(state, acc, year, actual_atco_number)

#### SECTOR HOURS ----
cap_sector_hours_actual  <-  read_xlsx(
  here(data_folder, cap_data_file),
  sheet = "sector_hour",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

#### OTHER APT PIs ----
cap_apt_pis_actual  <-  read_xlsx(
  here(data_folder, cap_data_file),
  sheet = "airport_pis",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

### SES ----
#### ATFM DELAY ----
cap_ert_atfm_actual_ses  <-  read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "Avg en-route ATFM delay",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

cap_ert_atfm_actual_mm_ses  <-  read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "en route monthly delay",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

cap_trm_atfm_actual_ses  <-  read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "Avg terminal ATFM delay",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

cap_trm_atfm_actual_mm_ses  <-  read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "terminal monthly delay",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

#### ALL-CAUSE PREDEP DELAY ----
cap_all_c_predep_delay_actual_ses  <-  read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "ACausePreDep",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() |> 
  mutate(state = rp_full)

#### DELAY TIME BIN ----
cap_delay_bin_actual_ses  <-  read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "DTB",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names()

#### ATCOs IN OPS ----
cap_atco_acc_ses  <- read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "ATCOs",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names()

#### SECTOR HOURS ----
cap_sector_hours_actual_ses  <-  read_xlsx(
  here(data_folder, ses_data_file),
  sheet = "Sectorhour",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# CEFF ----
## En-route ----
ceff_t1_ert  <-  read_xlsx(
  here(data_folder, ceff_data_file),
  sheet = "Enroute_T1",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>%
  clean_names()

ceff_t2_ert  <-  read_xlsx(
  here(data_folder, ceff_data_file),
  sheet = "Enroute_T2",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>%
  clean_names()

ceff_t3_ert  <-  read_xlsx(
  here(data_folder, ceff_data_file),
  sheet = "Enroute_T3",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>%
  clean_names()

### SES ----
ceff_ses_duc_ert <- read_xlsx(
  here(data_folder, ceff_data_file),
  sheet = "ses_duc",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>%
  clean_names()

### NM ----
ceff_nm_ert <- read_xlsx(
  here(data_folder, nm_data_file),
  sheet = "Cost-efficiency",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>%
  clean_names()


## Terminal ----
ceff_t1_trm  <-  read_xlsx(
  here(data_folder, ceff_data_file),
  sheet = "Terminal_T1",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>%
  clean_names()

ceff_t2_trm  <-  read_xlsx(
  here(data_folder, ceff_data_file),
  sheet = "Terminal_T2",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>%
  clean_names()

ceff_t3_trm  <-  read_xlsx(
  here(data_folder, ceff_data_file),
  sheet = "Terminal_T3",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>%
  clean_names()

## Yearly xrates ----
xrate_ert  <-  read_xlsx(
  here(data_folder, ceff_data_file),
  sheet =  "xrate_ert",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>%
  clean_names() %>% 
  filter(status == "D") %>% 
  mutate(cztype = "enroute",
         cz_code = paste0(country_zone_code, "_ECZ")) %>% 
  select(cz_code, cztype, year, currency,
         xrate = exchange_rate,
         xrate_ref = exchange_rate_ref2022)

xrate_trm  <-  read_xlsx(
  here(data_folder, ceff_data_file),
  sheet =  "xrate_trm",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>%
  clean_names() %>% 
  filter(status == "D") %>% 
  mutate(cztype = "terminal",
         cz_code = paste0(country_zone_code, "_TCZ")) %>% 
  select(cz_code, cztype, year, currency,
         xrate = exchange_rate,
         xrate_ref = exchange_rate_ref2022)


xrate_year_states <- xrate_ert %>% rbind(xrate_trm)

xrate_ses <- xrate_year_states %>%
  distinct(year, cztype) %>%            
  mutate(
    cz_code   = "SES",
    currency  = "EUR",
    xrate     = 1,
    xrate_ref = 1
  ) %>%  select (cz_code, cztype, year, currency, xrate, xrate_ref)


xrate_year <- xrate_year_states %>% rbind(xrate_ses)

## Forecast SU for Temp ur ----
cef_temp_su_t2  <-  read_xlsx(
  here(data_folder, ceff_data_file),
  sheet = "forecast_su_ert",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>%
  clean_names()


data_loaded <- TRUE
