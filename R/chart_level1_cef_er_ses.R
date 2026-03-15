if (!exists("data_loaded")) {
  source("R/get_data.R")
} 

# import data  ----
data_raw  <-  ceff_ses_duc_ert %>% 
  filter(entity_type == "ECZ") %>% 
  select(year, cz_code = charging_zone_code, status, total_cost_eur_ref, total_su) %>% 
  left_join(xrate_ert, by = c("cz_code", "year")) %>% 
  group_by(year, status) %>% 
  summarise(
    total_cost_eur_ref = sum(total_cost_eur_ref, na.rm = TRUE),
    total_su = sum(total_su, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    duc_value = total_cost_eur_ref/total_su
  )

# prepare data ----
data_prep <- data_raw %>% 
  filter(year >= rp_min_year) %>% 
  select(
    year,
    status,
    duc_value
  ) %>% 
  mutate(xlabel = year,
         type = str_replace(status, "A", "Actual unit cost"),
         type = str_replace(type, "D", "Determined unit cost"),
         mymetric = if_else(status == "A" & year > year_report, NA, round(duc_value,2))
  ) %>% 
  arrange(xlabel)

data_actual_trend <- data_raw %>%
  filter(status == "A") %>% 
  arrange(year) %>% 
  mutate(
    duc_py = lag(duc_value,1),
    mymetric = (duc_value/duc_py-1) * 100
    ) %>% 
  select(xlabel = year, mymetric, myothermetric = duc_value) %>%
  mutate(type = 'Actual') %>% 
  filter(xlabel >= rp_min_year) 
  

data_target_trend <- data_prep %>%
  filter(status == "D") %>% 
  arrange(year) %>% 
  mutate(
    myothermetric = mymetric,
    mymetric = -1.2
  ) %>% 
  select(xlabel, type, mymetric, myothermetric) %>%
  mutate(type = 'Target')

# chart parameters ----
c_suffix <- ""
c_decimals <- 2

### trace parameters
c_colors = c(PRBPlannedColor, PRBActualColor)
###set up order of traces
c_factor <- data_prep %>% select(type) %>% unique() 
invisible(as.list(c_factor$type))
c_factor <- sort(c_factor$type, decreasing = TRUE)
c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textangle <- -90
c_textposition <- "inside"
c_insidetextanchor <- "middle"
c_textfont_color <- "black"

### layout parameters
c_barmode <- 'group'

#### title
c_title_text <- paste0("En route unit costs - ", rp_full)
c_title_y <- 0.99

#### yaxis
c_yaxis_title <- paste0("En route unit costs (€",cef_ref_year,")")
c_yaxis_tickformat <- ".0f"

#### legend
if (knitr::is_latex_output()) {
  c_legend_x <- -0.1
  c_legend_xanchor <- 'left'
  
} else {
  c_legend_x <- 0.5
  c_legend_xanchor <- 'center'
  
}

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
                  
                  legend_x = c_legend_x,
                  legend_xanchor = c_legend_xanchor
                  
) 

p1 %>% 
add_trace(
  data = data_target_trend,
  x = ~ xlabel,
  y = ~ myothermetric,
  yaxis = 'y1',
  mode = 'line+markers',
  type = 'scatter',
  name = "Target trend",
  text = ~ paste0("<b>", format(mymetric,  big.mark  = ",", nsmall = 1),
                  "%",
                  "</b>"),
  textangle = 0,
  textposition = 'top',
  textfont = list(color = PRBTargetColor, size = myfont),
  line = list(color = PRBTargetColor, width = mylinewidth),
  marker = list(size = mylinewidth * 3,
                color = PRBTargetColor,
                symbol = NA),
  hovertemplate = "Target trend: %{text}<extra></extra>",
  showlegend = T
) %>% 
  add_trace(
    data = data_actual_trend,
    x = ~ xlabel,
    y = ~ myothermetric,
    yaxis = 'y1',
    mode = 'line+markers',
    type = 'scatter',
    name = "Actual trend",
    text = ~ paste0("<b>", format(round(mymetric,1),  big.mark  = ",", nsmall = 1),
                    "%",
                    "</b>"),
    textangle = 0,
    textposition = 'bottom',
    textfont = list(color = 'black', size = myfont),
    line = list(color = '#ED7D31', width = mylinewidth),
    marker = list(size = mylinewidth * 3,
                  color = '#ED7D31',
                  symbol = NA),
    hovertemplate = "Actual trend: %{text}<extra></extra>",
    showlegend = T
  )
