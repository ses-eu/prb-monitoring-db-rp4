if (!exists("data_loaded")) {
  source("R/get_data.R")
} 

# import data  ----
data_raw  <-  ceff_nm_ert

# prepare data ----
data_prep_wide <- data_raw %>% 
  select(-c(entity_name)) 

data_prep <- data_prep_wide %>%
  mutate(xlabel= year) %>% 
  select(xlabel, target, actual) %>% 
  pivot_longer(-xlabel, names_to = "type", values_to = "mymetric") %>% 
  mutate(mymetric = if_else(type == "actual" & xlabel > year_report, NA, mymetric),
    type = if_else(type == "actual", "Actual CSU", "Determined CSU"),
         )

data_prep_costs <- data_prep_wide %>%
  mutate(xlabel= year) %>% 
  select(xlabel, planned_costs, actual_costs) %>% 
  pivot_longer(-xlabel, names_to = "type", values_to = "myothermetric") %>% 
  mutate(myothermetric = if_else(type == "actual_costs" & xlabel > year_report, 
                                 NA, 
                                 round(myothermetric/1000, 2)),
    type = if_else(type == "actual_costs", "Actual costs", "Planned costs"),
  )

# chart parameters ----
c_suffix <- ""
c_decimals <- 2

### trace parameters
c_colors = c(PRBPlannedColor, PRBActualColor, PRBPlannedColor, PRBActualColor)
###set up order of traces
c_factor <- c("Determined CSU", "Actual CSU")
c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textangle <- -90
c_textposition <- "inside"
c_insidetextanchor <- "middle"
c_textfont_color <- 'black'

### layout parameters
c_barmode <- 'group'
c_minsize <- myfont*0.95

#### title
c_title_text <- paste0("Costs per service unit")

#### yaxis
c_yaxis_title <- paste0("Costs per service unit (€",cef_ref_year,")")
c_yaxis_tickformat <- ".0f"

#### margin
c_margin = list(t = 60, b = 0, l = 40, r = 50)


# plot chart  ----

p1 <- mybarchart2(data_prep, 
                  height = myheight,
                  colors = c_colors,
                  local_factor = c_factor,
                  suffix = c_suffix,
                  decimals = c_decimals,
                  
                  barmode = c_barmode,
                  
                  hovertemplate = c_hovertemplate,
                  
                  textangle = c_textangle,
                  textposition = c_textposition, 
                  insidetextanchor = c_insidetextanchor,
                  textfont_color = c_textfont_color,
                  
                  title_text = c_title_text,
                  
                  yaxis_title = c_yaxis_title,
                  yaxis_ticksuffix = c_suffix,
                  yaxis_tickformat = c_yaxis_tickformat,
                  
                  minsize = c_minsize,
                  margin = c_margin
                  
) 

p1 %>% 
  add_line_trace2(., filter(data_prep_costs, type == "Planned costs"),
                  name = "Planned costs",
                  textfontcolor = "rgba(0,0,0,0)",
                  linecolor = PRBPlannedColor,
                  markercolor = PRBPlannedColor,
                  showlegend = TRUE,
                  yaxis = "y2"
  ) %>% 
  add_line_trace2(., filter(data_prep_costs, type == "Actual costs"),
                  name = "Actual costs",
                  textfontcolor = "rgba(0,0,0,0)",
                  linecolor = PRBActualColor,
                  markercolor = PRBActualColor,
                  showlegend = TRUE,
                  yaxis = "y2"
  ) %>% 
  layout(yaxis2 = list(title = paste0("Total costs ('000 €",cef_ref_year,")"),
                       overlaying = "y",
                       side = "right",
                       showgrid = FALSE,
                       showline = FALSE,
                       tickformat = ",",
                       rangemode = "tozero",
                       zeroline = TRUE,
                       zerolinecolor = 'rgb(255,255,255)',
                       titlefont = list(size = myfont), tickfont = list(size = myfont)
    ))
  
