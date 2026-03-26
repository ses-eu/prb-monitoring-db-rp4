## import data  ----
if (!exists("data_loaded")) {
  source("R/params_country.R")
  source("R/get_data.R")
}

data_raw <- vfe_ert_actual

## prepare data ----
data_prep <- data_raw %>% 
  filter(
    area_id == .env$state_icao_vef_code,
    year == .env$year_report
  ) %>%
  mutate(
    mymetric = round(vfe_ert*100 , 1),
    type = "En route VFE"
  ) %>% 
  select(
    xlabel = year,
    type,
    mymetric
  ) 

## chart parameters ----
c_suffix <- "%"
c_decimals <- 1

### trace parameters
c_colors <- c(PRBActualColor)

###set up order of traces
c_factor <- data_prep %>% distinct(type) %>% pull(type)

c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textangle <- 0
c_textposition <- "inside"
c_insidetextanchor <- "middle"

#### title
c_title_text <- paste0("Vertical en-route flight efficiency")

#### yaxis
c_yaxis_title <- "Vertical en-route flight efficiency (%)"
c_yaxis_tickformat <- paste0(".",0, "f")

## plot chart  ----
myplot <- mybarchart2(data_prep, 
                      height = myheight,
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

myplot %>% layout(xaxis=list(range = c(rp_min_year-0.5,rp_max_year+0.5)))

