if (!exists("country") | is.na(country)) {country = "Denmark"
source("R/params_country.R")
}

if (!data_loaded) {
  source("R/get_data.R")
} 

if (exists("cz") == FALSE) {cz = c("1", "enroute")}

# define cz ----
ez <- as.numeric(cz[[1]])
cztype <- cz[[2]]
# cztype <- "terminal"
mycz <- if_else(cztype == "terminal",
                tcz_list$tcz_id[ez],
                ecz_list$ecz_id[ez])
mycz_name <- if_else(cztype == "terminal",
                     tcz_list$tcz_name[ez],
                     ecz_list$ecz_name[ez])


# import data  ----
if(cztype == "terminal") {
  data_raw <- ceff_t2_trm
} else {
  data_raw <- ceff_t2_ert
}
  
# prepare data ----
# table 2
data_prep_t2 <- data_raw %>% 
  filter(status == "A",
         year == year_report,
         (entity_type == "ECZ" | entity_type == "TCZ")) %>% 
  select(
    entity_code,
    year,
    x4_7_total_su,
    x3_1_investment,
    x3_3_cost_authority_qes,
    x3_4_ectl_cost_eur,
    x3_5_pension_cost,
    x3_6_interest_loan,
    x3_7_change_in_law,
    x3_8_diff_det_cost_actual_cost
  ) %>%
  pivot_longer(cols = -c(entity_code, year, x4_7_total_su),
               names_to = 'type',
               values_to = 'mymetric') %>% 
  mutate(
    xlabel = case_when(
      type == "x3_1_investment" ~ "New and existing investments",
      type == "x3_3_cost_authority_qes" ~ "Competent authorities and qualified entities costs",
      type == "x3_4_ectl_cost_eur" ~ "Eurocontrol costs",
      type == "x3_5_pension_cost" ~ "Pension costs",
      type == "x3_6_interest_loan" ~ "Interest on loans",
      type == "x3_7_change_in_law" ~ "Changes in law",
      type == "x3_8_diff_det_cost_actual_cost" ~ "Total cost exempt from cost risk sharing")
  ) %>% 
  mutate(
    mymetric = mymetric/1000
  )
  
# t exchange rates
data_prep_xrates <- xrate_year %>% 
  filter(cztype == cztype) %>% 
  select(cz_code, year, xrate)

data_prep_eur <- data_prep_t2 %>% 
  left_join(xrate_year_cz, by = c('year', "entity_code"  = "cz_code")) %>% 
  mutate(mymetric = mymetric/xrate) %>% 
  select(
    entity_code, year, xlabel, mymetric, x4_7_total_su
  )
  
data_prep_eur_ses <- data_prep_eur %>% 
  group_by(year, xlabel) %>% 
  summarise(
    mymetric = sum(mymetric, na.rm = TRUE), 
    ################### TB Verified ----
    x4_7_total_su = sum(x4_7_total_su, na.rm = TRUE)/n_distinct(entity_code) , 
    .groups = "drop") %>% 
  mutate(entity_code = "SES") %>% 
  select(
    entity_code, year, xlabel, mymetric, x4_7_total_su
  )
  

data_prep <- data_prep_eur %>% 
  rbind(data_prep_eur_ses) %>% 
  filter(
    entity_code == mycz,
  ) %>% 
  mutate(
    mymetric = case_when(
      xlabel > year_report ~ NA,
      .default = round(mymetric/1000,2),
    ),
    myothermetric = case_when(
      xlabel > year_report ~ NA,
      .default = mymetric*1000/x4_7_total_su
    )
    ) %>% 
  select(xlabel, mymetric, myothermetric)
      

local_decimals <- if_else(country == rp_full, 0, 1)

# pdf table ----
data_prep_pdf <- data_prep %>% 
  mutate(
    across(2, ~format(round(.x,local_decimals), nsmall =local_decimals, big.mark = ",")),
    across(3, ~format(round(.x,2), nsmall =2, big.mark = ","))
  )



# plot table ----
table1 <- mygtable(data_prep_pdf, myfont) %>% 
  cols_label(xlabel = paste0("Cost exempt from cost sharing by item - ", 
                             if_else(year_report == 2021 | year_report == 2020, "2020-2021", as.character(year_report))),
             mymetric = "€'000",
             myothermetric = "€/SU") %>% 
  tab_options(column_labels.background.color = "#F2F2F2",
              column_labels.font.weight = 'bold',
              container.padding.y = 0) |> 
  # cols_align(columns = 1, align = "left") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = 7
    ))


if (!knitr::is_latex_output()) {
  table1
}


