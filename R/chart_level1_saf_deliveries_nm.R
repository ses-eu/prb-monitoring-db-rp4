if (!exists("data_loaded")) {
  source("R/get_data.R")
}

# import data  ----
data_raw <- saf_overd_nm

# prepare data ----
data_prep <- data_raw %>% 
  mutate(
    xlabel = year,
    mymetric =  case_when(
      year > year_report  ~ NA,
      .default = round(percentage_overdeliveries * 100, 1)),
    type = "Actual"
      )

# chart parameters ----
c_suffix <- "%"
c_decimals <- 1

### trace parameters
c_colors = PRBActualColor

###set up order of traces
c_factor <- "Actual"
c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textposition <- "inside"
c_insidetextanchor <- 'middle'

### layout parameters
c_barmode <- 'group'

#### title
c_title_text <- "Percentage of overdeliveries"

#### yaxis
c_yaxis_title <- "Precentage of overdeliveries (%)"
c_yaxis_tickformat <- ".1f"


# plot chart ----
myplot <- mybarchart2(data_prep, 
                      height = myheight,
                      colors = c_colors,
                      local_factor = c_factor,
                      suffix = c_suffix,
                      decimals = c_decimals,
                      
                      hovertemplate = c_hovertemplate,
                      
                      textposition = c_textposition,
                      insidetextanchor = c_insidetextanchor,

                      title_text = c_title_text,
                      
                      yaxis_title = c_yaxis_title,
                      yaxis_ticksuffix = c_suffix,
                      yaxis_tickformat = c_yaxis_tickformat
)

myplot %>% add_empty_trace(., data_prep)

