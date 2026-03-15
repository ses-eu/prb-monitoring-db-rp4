if (!exists("data_loaded")) {
  source("R/get_data.R")
} 

if (exists("cz") == FALSE) {cz = c("1", "terminal")}

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

# get data ----
# regulatory result
data_prep_reg_all <- regulatory_result(cztype, mycz)

data_prep_reg <- data_prep_reg_all %>% 
  group_by(year, x5_4_total_su) %>% 
  summarise(reg_res = sum(regulatory_result), .groups = "drop") %>% 
  mutate(reg_res_per_su = reg_res * 1000 / x5_4_total_su) %>% 
  select(year, reg_res, reg_res_per_su) 

# aucu
data_prep_aucu_all <- aucu(cztype, mycz) 
data_prep_aucu <- data_prep_aucu_all %>% 
  select(year, aucu_excluding_or) 

# join tables
data_prep <- data_prep_reg %>% 
  left_join(data_prep_aucu, by = 'year') %>% 
  select(-reg_res) %>% 
  mutate(rr_as_perc_aucu = reg_res_per_su / aucu_excluding_or * 100) %>% 
  pivot_longer(-c(year, rr_as_perc_aucu), names_to = 'type', values_to = 'mymetric') %>% 
  mutate(type = case_when(
    type == 'reg_res_per_su'  ~ 'Regulatory result per SU',
    type == 'aucu_excluding_or'  ~ 'AUCU (before other revenues)'
    ),
    xlabel = year,
    myothermetric = round(rr_as_perc_aucu, 2)
  ) %>% 
  mutate(across(c(mymetric, myothermetric), ~if_else(xlabel > year_report, NA, .x)))


# chart parameters ----
c_suffix <- ""
C_decimals <- 1

### trace parameters
c_colors = c( '#9DC3E6', PRBActualColor)
###set up order of traces
c_factor <- c("AUCU (before other revenues)",
              "Regulatory result per SU")
c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textangle <- -90
c_textposition <- "inside"
c_insidetextanchor <- "middle"
c_textfont_color <- 'transparent'

### layout parameters
c_bargap <- 0.25
c_barmode <- 'group'

#### title
c_title_text <- paste0("Share of RR in AUCU")

#### yaxis
c_yaxis_title <- "AUCU & RR (€/SU)"
c_yaxis_tickformat <- ".0f"


#### margin
c_margin = list(t = 60, b = 0, l = 60, r = 60)

# setup ranges to ensure zero line at same height
# https://stackoverflow.com/questions/76289470/plotly-barplot-with-two-y-axis-aligned-at-zero
# === Step 1: y1 setup ===
y1_max <- max(max(data_prep$mymetric, na.rm = TRUE), 0)
y1_min <- min(min(data_prep$mymetric, na.rm = TRUE), 0)
y1_padding <- (y1_max - y1_min) / 16
y1_range <- c(y1_min - y1_padding, y1_max + y1_padding)

# === Step 2: relative zero ===
y1_relative_zero <- (0 - y1_range[1]) / (y1_range[2] - y1_range[1])

# === Step 3: y2 data range ===
y2_data_min <- min(data_prep$myothermetric, na.rm = TRUE)
y2_data_max <- max(data_prep$myothermetric, na.rm = TRUE)
y2_data_range <- y2_data_max - y2_data_min

# === Step 4: Compute y2 total range to match y1 zero alignment ===
y2_total_range <- y2_data_range / (1 - 2 * y1_relative_zero)

# === Step 5: Apply padding symmetrically around 0 ===
y2_padding_lower <- y1_relative_zero * y2_total_range
y2_padding_upper <- (1 - y1_relative_zero) * y2_total_range
y2_range_candidate <- c(0 - y2_padding_lower, 0 + y2_padding_upper)

# === Step 6: Ensure all y2 data is visible ===
# (Adjust range only outward to include all points)
y2_range <- c(
  min(y2_range_candidate[1], y2_data_min),
  max(y2_range_candidate[2], y2_data_max)
)

# === Step 7: RECOMPUTE y2 range to force aligned zero ===
# In case we had to expand for visibility
y2_total_range <- y2_range[2] - y2_range[1]

y2_range <- c(
  floor(y2_range[1] / 5) * 5,
  ceiling(y2_range[2] / 5) * 5
)

# Recompute new y2 total range and zero position
y2_total_range <- y2_range[2] - y2_range[1]
y2_relative_zero <- (0 - y2_range[1]) / y2_total_range

# Realign y1 range to this updated relative zero
y1_total_range <- y1_range[2] - y1_range[1]
y1_range <- c(
  0 - y2_relative_zero * y1_total_range,
  0 + (1 - y2_relative_zero) * y1_total_range
)

# plot chart  ----
p1 <- mybarchart2(data_prep, 
                  height = myheight+30,
                  colors = c_colors,
                  local_factor = c_factor,
                  suffix = c_suffix,
                  decimals = c_decimals,
                  
                  barmode = c_barmode,
                  bargap = c_bargap,
                  
                  hovertemplate = c_hovertemplate,
                  
                  textangle = c_textangle,
                  textposition = c_textposition, 
                  textfont_color = c_textfont_color,
                  insidetextanchor = c_insidetextanchor,
                  
                  title_text = c_title_text,
                  
                  yaxis_title = c_yaxis_title,
                  yaxis_ticksuffix = c_suffix,
                  yaxis_tickformat = c_yaxis_tickformat,
                  
                  margin = c_margin
) 

p1 %>% 
  add_line_trace2(., filter(data_prep, type == 'Regulatory result per SU'),
                  name = "Share of RR in AUCU (%)",
                  textfontcolor = "rgba(0,0,0,0)",
                  linecolor = "rgba(0,0,0,0)",
                  showlegend = TRUE,
                  yaxis = "y2"
  ) %>% 
  layout(
    xaxis=(list(range = c(rp_min_year-0.5, rp_max_year+0.5))),
    yaxis = list(
      range = y1_range
    ),   # to force the zeros to coincide
    yaxis2 = list(
      title = 'RR as % of AUCU',
      zerolinecolor = '#E8E8E8',
      range = y2_range,
      # rangemode = "nonnegative",
      overlaying = "y",
      ticksuffix = '%',
      side = 'right',
      showgrid = FALSE
    )) 

