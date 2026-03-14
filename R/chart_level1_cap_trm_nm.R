if (!data_loaded) {
  source("R/get_data.R")
}
# import data  ----
data_raw  <-  cap_trm_nm
  
# prepare data ----
data_prep <- data_raw %>% 
  mutate(
    xlabel = year,
    Target = round(target * 100, 0),
    Actual = case_when(
      year > year_report  ~ NA,
      .default = round(actual * 100, 1))
  ) %>% 
  select(xlabel, Target, Actual) %>% 
  pivot_longer(-xlabel, names_to = "type", values_to = "mymetric") %>% 
  mutate(myothermetric = mymetric)

# chart parameters ----
c_suffix <- "%"
c_decimals <- 1

### trace parameters
c_colors = PRBActualColor
###set up order of traces
c_factor <- "Actual"
c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

mytextposition <- "inside"
myinsidetextanchor <- 'middle'

### layout parameters
c_barmode <- 'group'

#### title
c_title_text <- "Percentage of arrival ATFM delay savings"

#### yaxis
c_yaxis_title <- "Arrival ATFM delay savings (%)"
c_yaxis_tickformat <- ".1f"

# plot chart ----
myplot <- mybarchart2(filter(data_prep, type == "Actual"), 
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


myplot %>% 
  add_line_trace2(., filter(data_prep, type == "Target"),
                  textdecimals = c_decimals,
                  textsuffix = c_suffix
  )

