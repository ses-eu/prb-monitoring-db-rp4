if (!exists("data_loaded")) {
  source("R/get_data.R")
}

# import data  ----
data_raw  <-  cap_pis_nm 

# prepare data ----
data_prep <- data_raw %>% 
  filter(
    year_report == .env$year_report) %>% 
  mutate(
    xlabel = year,
    mymetric = case_when(
      year > year_report  ~ NA,
      .default = round(percent_above_15_min * 100, 1)),
    type = "Actual"
      ) %>% 
  select(
    xlabel,
    type,
    mymetric
  )

# chart parameters ----
c_suffix <- "%"
c_decimals <- 1

### trace parameters
c_colors = c( '#ED7D31')
###set up order of traces
c_factor <- "Actual"

c_textposition <- "inside"
c_insidetextanchor <- 'middle'
c_textfont_color <- 'white'
c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

### layout parameters
c_barmode <- 'group'

#### title
c_title_text <- "IFR flights with ATFM delay above 15 min."

#### yaxis
c_yaxis_title <- "IFR flights with ATFM delay > 15 min (%)"
c_yaxis_tickformat <- ".1f"


# plot chart ----
myplot <- mybarchart2(data_prep, 
                      height = myheight+10,
                      colors = c_colors,
                      local_factor = c_factor,
                      suffix = c_suffix,
                      decimals = c_decimals,
                      
                      hovertemplate = c_hovertemplate,
                      
                      textposition = c_textposition,
                      insidetextanchor = c_insidetextanchor,
                      textfont_color = c_textfont_color,
                      
                      title_text = c_title_text,
                      
                      yaxis_title = c_yaxis_title,
                      yaxis_ticksuffix = c_suffix,
                      yaxis_tickformat = c_yaxis_tickformat
)

myplot %>% add_empty_trace(., data_prep)

