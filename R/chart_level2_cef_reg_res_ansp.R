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

# import data ----
  data_reg_res <- regulatory_result(cztype, mycz)
    
# pre-prep data ----
data_for_chart_wide <- data_reg_res %>% 
  filter(type == 'Main ANSP') %>% 
  mutate(
    share_rr_act_rev_expost = regulatory_result/actual_revenues * 100,
    share_rr_act_rev_exante = ex_ante_roe/x4_2_cost_excl_vfr_d * 100
    ) %>% 
  select(year,regulatory_result, ex_ante_roe, share_rr_act_rev_expost, share_rr_act_rev_exante) %>% 
  mutate(across(-year, ~if_else(year>year_report, NA, .x)))

# prep data ----

## separate in two tables for pivoting 
data_for_chart_value <- data_for_chart_wide %>% 
  select(-c(share_rr_act_rev_expost, share_rr_act_rev_exante)) %>% 
  pivot_longer(-year, names_to = "xlabel", values_to = 'mymetric') %>% 
  mutate(xlabel = case_when(
    xlabel == 'regulatory_result'  ~ 'Ex-post',
    xlabel == 'ex_ante_roe'  ~ 'Ex-ante')
    , mymetric = if_else(xlabel == "Ex-post" & year > year_report, NA, mymetric/1000)
  )

data_for_chart_share <- data_for_chart_wide %>% 
  select(-c(regulatory_result, ex_ante_roe)) %>% 
  pivot_longer(-year, names_to = "xlabel", values_to = 'share') %>% 
  mutate(xlabel = case_when(
    xlabel == 'share_rr_act_rev_expost'  ~ 'Ex-post',
    xlabel == 'share_rr_act_rev_exante'  ~ 'Ex-ante'),
    share = if_else(xlabel == "Ex-post" & year > year_report, NA, share)
  )

data_prep <- data_for_chart_value %>% 
  left_join(data_for_chart_share, by = c('year', 'xlabel')) %>% 
  mutate(type = xlabel,
         xlabel = year
  )
  
# chart parameters ----
c_suffix <- ""
c_decimals <- 1

### trace parameters
c_colors = c( '#CEE0EA', '#4B8DB1')
###set up order of traces
c_factor <- c("Ex-ante",
              "Ex-post")
c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textangle <- -90
c_textposition <- "inside"
c_insidetextanchor <- "middle"
c_textfont_color <- 'transparent'

c_trace_showlegend <- F

### layout parameters
c_barmode <- 'group'

#### title
c_title_text <- if_else(country == rp_full, 
                        "RR - Main ANSPs",
                        paste0("RR - ", main_ansp)
                        )

#### yaxis
c_yaxis_title <- "RR"
c_yaxis_tickformat <- ".1f"

#### legend
c_legend_y <- -0.24

#### margin
c_margin = list(t = 60, b = 80, l = 40, r = 60)

# setup ranges to ensure zero line at same height
# https://stackoverflow.com/questions/76289470/plotly-barplot-with-two-y-axis-aligned-at-zero
# 1. Ensure y1 includes zero
y1_max <- max(max(data_prep$mymetric, na.rm = TRUE), 0)
y1_min <- min(min(data_prep$mymetric, na.rm = TRUE), 0)

# 2. Ensure y2 includes zero
y2_max <- max(max(data_prep$share, na.rm = TRUE), 0)
y2_min <- min(min(data_prep$share, na.rm = TRUE), 0)

# 3. Compute y1 range with standard plotly padding
y1_padding <- (y1_max - y1_min) / 16
y1_range <- c(y1_min - y1_padding, y1_max + y1_padding)

# 4. Find the relative position of 0 in y1 range
y1_relative_zero <- (0 - y1_range[1]) / (y1_range[2] - y1_range[1])

# 5. Calculate the total range for y2 to mirror zero position
y2_total_range <- (y2_max - y2_min) / (1 - 1.5 * y1_relative_zero)

# 6. Calculate padding needed on both ends
y2_padding_lower <- y1_relative_zero * y2_total_range
y2_padding_upper <- (1 - y1_relative_zero) * y2_total_range

# 7. Final y2 range
y2_range <- c(0 - y2_padding_lower, 0 + y2_padding_upper)

# plot chart  ----
mybarchart2(data_prep, 
                  height = myheight+30,
                  colors = c_colors,
                  local_factor = c_factor,
                  suffix = c_suffix,
                  decimals = c_decimals,
                  barmode = c_barmode,
                  
                  hovertemplate = c_hovertemplate,
                  
                  textangle = c_textangle,
                  textposition = c_textposition, 
                  textfont_color = c_textfont_color,
                  insidetextanchor = c_insidetextanchor,
                  
                  title_text = c_title_text,
                  
                  yaxis_title = c_yaxis_title,
                  yaxis_ticksuffix = c_suffix,
                  yaxis_tickformat = c_yaxis_tickformat,
                  
                  legend_y = c_legend_y,
                  
                  margin = c_margin
) %>% add_trace(
  inherit = FALSE,
  data = data_prep,
  x = ~xlabel ,
  y = ~share,
  name = "RR in percent of en-route revenues",
  yaxis = "y2",
  # mode = "markers",
  type = 'box',
  color = ~ factor(type, levels = c_factor),
  line = list(color = '#E46C0A', width = mylinewidth), fillcolor = '#E46C0A',
  # marker = list(size = mylinewidth * 3, color = '#E46C0A'),
  hoverinfo = 'none',
  showlegend = F
) %>% 
  add_trace(
    data = filter(data_prep, type == "Ex-ante"),
    x = ~ xlabel,
    y = ~ paste0(share, '%'),
    yaxis = "y2",
    mode = "markers", 
    type = 'scatter',
    name = "RR (ex-ante) in % of revenues",
    text = "" ,
    marker = list(size = 1, 
                  color = "transparent"),
    hovertemplate <- paste0('%{y:,.2f}%'),
    showlegend = F
  )  %>% 
  add_trace(
    data = filter(data_prep, type == "Ex-post"),
    x = ~ xlabel,
    y = ~ share,
    yaxis = "y2",
    mode = "markers", 
    type = 'scatter',
    name = "RR (ex-post) in % of revenues",
    text = "" ,
    marker = list(size = 1, 
                  color = "transparent"),
    hovertemplate <- paste0('%{x:,.2f}%'),
    showlegend = F
  )  %>%     
  layout(
    xaxis=(list(range = c(rp_min_year-0.5, rp_max_year+0.5))),
    yaxis = list(
      # rangemode = "tozero",
      range = y1_range,
      secondary_y= TRUE
    ),
    yaxis2 = list(
      title = 'RR as % of revenues',
      overlaying = "y",
      zerolinecolor = '#E8E8E8',
      # rangemode = "tozero",
      range = y2_range,
      secondary_y= FALSE,
      ticksuffix = '%',
      tickformat = if_else(max(data_prep$share, na.rm = TRUE) >0.1, 
                            ".1f",
                            ".0f"),
      side = 'right',
      showgrid = FALSE
    ),
  boxmode = "group", bargroupgap = 0.1, boxgroupgap = 0.4, 
  boxgap = mybargap,
  # fake legend
  annotations = list(
      list (
      xanchor = "left",
      x = 0.0,
      y = c_legend_y,
      text = '■',
      font = list(size = myfont * 1.2, color = c_colors[[1]]),
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      ax = 0,
      ay = 0
      ),
      list (
        xanchor = "left",
        x = 0.07,
        y = c_legend_y,
        text = 'Ex-ante RR (in value)',
        font = list(size = myfont*0.9, color = "black"),
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        ax = 0,
        ay = 0
      ),
      list (
        xanchor = "left",
        x = 0.6,
        y = c_legend_y,
        text = '■',
        font = list(size = myfont * 1.2, color = c_colors[[2]]),
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        ax = 0,
        ay = 0
      ),
      list (
        xanchor = "left",
        x = 0.67,
        y = c_legend_y,
        text = 'Ex-post RR (in value)',
        font = list(size = myfont*0.9, color = "black"),
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        ax = 0,
        ay = 0
      ),
      list (
        xanchor = "left",
        x = 0.0,
        y = c_legend_y-0.1,
        text = '<b>―</b>',
        font = list(size = myfont * 1.2, color = '#E46C0A'),
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        ax = 0,
        ay = 0
      ),
      list (
        xanchor = "left",
        x = 0.07,
        y = c_legend_y-0.1,
        text = 'RR in percent of en route revenues',
        font = list(size = myfont*0.9, color = "black"),
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        ax = 0,
        ay = 0
      ) 
      )
  ) 

