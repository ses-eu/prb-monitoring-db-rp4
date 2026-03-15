if (!exists("doclevel")) {doclevel = "level1"}
if (!exists("data_loaded")) {
  source("R/get_data.R")
}
 
if (country == "Network Manager") {
  # NM case ----
  ## import data  ----
  data_raw  <-  kep_nm 
  
  ## prepare data ----
  data_raw_target <- data_raw %>% 
    mutate(
      xlabel = year,
      state = "Network Manager",
      # for NM it's kep but we name it kea so it has the same structure as others
      kea_target = nm_target,
      value = actual
    ) 
  
  data_raw_actual <- data_raw_target

} else if (country == rp_full) {
  # SES case ----
  data_raw_target  <-  kea_target_ses %>% 
    mutate(kea_target = kea_reference_value_percent/100,
           state = rp_full)
  
  data_raw_actual  <-  kea_actual_ses %>% 
    mutate(value = hfe_kpi_percent/100,
           state = rp_full)
  
} else  {
  # State case ----
  data_raw_target <- kea_target
  data_raw_actual <- kea_actual
}
  
## prepare data ----
data_prep_target <- data_raw_target %>% 
  filter(
    state == .env$country
  ) %>% 
  mutate(
    xlabel = year,
    myothermetric = round(kea_target *100, 2),
    type = "Target"
  ) %>% 
  select(
    xlabel,
    myothermetric,
    type
  )

data_prep_actual <- data_raw_actual %>% 
  filter(
    state == country,
    year <= year_report) %>% 
  mutate (actual = value) %>% 
  select(
    year,
    actual
  ) %>% 
  mutate(
    xlabel = year,
    mymetric = round(actual * 100,2),
    type = "Actual"
  ) %>% 
  select (
    xlabel,
    mymetric,
    type
  )

## chart parameters ----
c_suffix <- "%"
c_decimals <- 2

c_colors <- PRBActualColor
c_factor <- c("Actual")

### hover
c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

#### title
if (knitr::is_latex_output()) {
  c_level1_title <- "Average horizontal flight efficiency\nof the actual trajectory (KEA)"
  c_title_y <- 0.95
  c_margin <- list(t = 50)
  
} else {
  c_level1_title <- "Average horizontal flight efficiency of the actual trajectory (KEA)"
  c_title_y <- 0.99
  c_margin <- mymargin
  
}

c_title_text <- paste0(if_else(country == "Network Manager", 
                               "KEP", 
                               if_else(doclevel == "level1",
                                       c_level1_title,
                                       "KEA")))

#### yaxis
c_yaxis_title <- paste0(if_else(country == "Network Manager", "KEP", "KEA"), " (%)")
c_yaxis_tickformat <- paste0(".",c_decimals, "f")

#### text
c_textangle <- mytextangle
c_textposition <- "inside"
c_insidetextanchor <- 'middle'

# plot chart ----
myplot <- mybarchart2(data_prep_actual, 
                      height = myheight+30,
                      colors = c_colors,
                      local_factor = c_factor,
                      suffix = c_suffix,
                      decimals = c_decimals,
                      
                      hovertemplate = c_hovertemplate,
                      
                      textangle = c_textangle,
                      textposition = c_textposition,
                      insidetextanchor = c_insidetextanchor,
                      
                      title_text = c_title_text,
                      
                      yaxis_title = c_yaxis_title,
                      yaxis_ticksuffix = c_suffix,
                      yaxis_tickformat = c_yaxis_tickformat
)


myplot %>% 
  add_line_trace2(., data_prep_target,
                  textdecimals = c_decimals,
                  textsuffix = c_suffix
  )
