if (!exists("country") | is.na(country)) {country <- rp_full
source("R/params_country.R")
}

if (!exists("data_loaded")) {
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
  data_raw <- ceff_t1_trm
} else {
  data_raw <- ceff_t1_ert
}

## SES  ----
if (country == rp_full) {
  data_raw  <-  data_raw %>% 
    group_by(status, year, entity_type_id, entity_type) %>% 
    summarise(
      staff_costs_eur_ref = sum(staff_costs_eur_ref, na.rm = TRUE),
      other_operating_costs_eur_ref = sum(other_operating_costs_eur_ref, na.rm = TRUE),
      depreciation_costs_eur_nom = sum(depreciation_costs_eur_nom, na.rm = TRUE),
      cost_of_capital_eur_nom = sum(cost_of_capital_eur_nom, na.rm = TRUE),
      exceptional_items_costs_eur_ref = sum(exceptional_items_costs_eur_ref, na.rm = TRUE),
      vfr_exempted_costs_eur_ref = sum(vfr_exempted_costs_eur_ref, na.rm = TRUE),
      .groups = "drop") %>% 
    mutate(charging_zone_code = "SES")
} 

# prepare data ----
data_pre_prep <- data_raw %>% 
  filter(
    charging_zone_code == mycz,
    entity_type_id == "ANSP1"
  ) %>% 
  select(
    year,
    status,
    staff_costs_eur_ref,
    other_operating_costs_eur_ref,
    depreciation_costs_eur_nom,
    cost_of_capital_eur_nom,
    exceptional_items_costs_eur_ref,
    vfr_exempted_costs_eur_ref
  ) 

data_prep_pivot <- data_pre_prep |> 
  pivot_longer(cols = -c(year, status),
    names_to = 'type', 
    values_to = 'value') |> 
  mutate(xlabel = year,
         type = case_when(
           type == "vfr_exempted_costs_eur_ref" ~ "VFR exempted", 
           type == "exceptional_items_costs_eur_ref" ~ "Exceptional items",
           type == "cost_of_capital_eur_nom" ~ "Cost of capital",
           type == "depreciation_costs_eur_nom" ~ "Depreciation costs",
           type == "other_operating_costs_eur_ref" ~ "Other operating costs",
           type == "staff_costs_eur_ref" ~ "Staff costs",
           .default = type
         ) 
           )

data_prep <- data_prep_pivot |>
  filter(xlabel == year_report) |> 
  pivot_wider(names_from = 'status', values_from = 'value') %>% 
  arrange(desc(type)) %>% 
  mutate(mymetric = (A-D)/10^6,
         mylabel = if_else(D == 0, '-', 
                           paste0(
                             if_else(mymetric > 0, '+', ''),
                             round((A/D-1) *100, 1), 
                             '%')),
         ylabel = as.factor(c("VFR exempted", 
                        "Exceptional items",
                        "Cost of capital",
                        "Depreciation costs",
                        "Other operating costs",
                        "Staff costs"))) |> 
  mutate(mymetric = case_when(
    ylabel == "VFR exempted" ~ -mymetric,
    .default = mymetric
  ))

# check if all values are negative or very small to fix formatting issue
all_negative_or_zero <- all(data_prep$mymetric <= 0.05)

# set x axis range to avoid labels being clipped
myroundup <- max(floor((log10(abs(max(data_prep$mymetric, na.rm = TRUE))))), floor((log10(abs(min(data_prep$mymetric, na.rm = TRUE))))))
range_min <- floor(min(data_prep$mymetric, na.rm = TRUE)/10^myroundup) * 10^myroundup - 10^myroundup/1.2
range_min <- if_else(range_min >0, 0, range_min)
range_max <- ceiling(max(data_prep$mymetric, na.rm = TRUE)/10^myroundup) * 10^myroundup + 10^myroundup/2

# chart parameters ----
c_title_text <- paste0(if_else(country == rp_full, 
                         "Costs by nature for main ANSPs - ",
                         paste0("Costs by nature - ", main_ansp," ")
                         ),
                        year_report
                        )

c_xaxis_title <- paste0("Costs (M€<sub>",cef_ref_year,"</sub>)")
c_barcolor_pos <- '#A5A5A5'
c_barcolor_neg <- '#A5A5A5'

c_hovertemplate <- paste0('%{y} (A-D): %{x:,.1f}<extra></extra>')
# c_xaxis_tickformat <- "0,.1f"
c_xaxis_tickformat <- if_else(all_negative_or_zero, "0,.1f", "+0,")
c_decimals <- 3

###set up order of traces
c_factor <- c("VFR exempted", 
              "Exceptional items",
              "Cost of capital",
              "Depreciation costs",
              "Other operating costs",
              "Staff costs")

# plot chart  ----
myhbarc2(data_prep,
          decimals = c_decimals,
          suffix = c_suffix,
          local_factor = c_factor,
          
          mybarcolor_pos = c_barcolor_pos,
          mybarcolor_neg = c_barcolor_neg,
          
          hovertemplate = c_hovertemplate,
          
          title_text = c_title_text,
          title_y = 0.5,
          title_x = 0.99,

          xaxis_title = c_xaxis_title,
          xaxis_tickformat = c_xaxis_tickformat
          
) %>% 
  layout(xaxis = list(range = c(range_min, range_max)))

