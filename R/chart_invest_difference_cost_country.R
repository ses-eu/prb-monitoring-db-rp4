if (!exists("country")) {country <- "Bulgaria"}
if (!exists("cost_type")) {cost_type <- "terminal"}

# import data  ----
if (!exists("data_costs_rt")) {
  source("R/get_investment_data.R")
}

# process data  ----
  data_filtered <- data_costs_rt |> 
    filter(tolower(en_route_terminal) == cost_type) |> 
    select(
      member_state,
      contains('20')) |> 
    group_by(member_state) |> 
    summarise(
      across(where(is.numeric), ~ sum(.x, na.rm = TRUE) / 10^3),
      .groups = "drop"
    ) |> 
    pivot_longer(
      cols = -member_state,
      names_to = c("year", "type"),
      names_pattern = "^x(\\d{4})([da])$",
      values_to = "value"
    ) |> 
    filter(
      year <= year_report
    ) |> 
    pivot_wider(
      names_from = "type",
      values_from = "value"
    ) |> 
    group_by(member_state) |> 
    summarise(
      d = sum(d, na.rm = TRUE),
      a = sum(a, na.rm = TRUE)
    ) |> 
    mutate(
      mymetric = d-a,
      myothermetric = (d/a -1)*100
    ) |> 
    select(
      xlabel = member_state,
      mymetric,
      myothermetric
    ) |> 
    filter(
      !is.na(mymetric) & !is.na(myothermetric)
    ) |> 
    arrange(desc(mymetric))

sort_country <- data_filtered |> select( xlabel) |> pull()
  
data_prep <- data_filtered |> 
select (xlabel, mymetric)  |> 
  mutate(
    xlabel = factor(xlabel, levels = sort_country),
    type = "Difference (magnigude)"
    )

data_prep2 <- data_filtered %>% select (xlabel, myothermetric) %>%
  mutate(
    xlabel = factor(xlabel, levels = sort_country),
    type = "Difference %")

# chart ----
## chart parameters ----
local_suffix <- ""
local_decimals <- 1

local_hovertemplate <- "%{y}"

#### legend
if (knitr::is_latex_output()) {
  local_legend_y <- mylegend_y
  local_legend_x <- -0.18
  local_legend_xanchor <- 'left'
  local_legend_fontsize <- myfont-1
  
} else {
  local_legend_y <- 1.5
  local_legend_x <- 0.5
  local_legend_xanchor <- 'center'
  local_legend_fontsize <- myfont
  
}

mylocalcolors <- c('#044598')

# plot chart ----
myplot <- mybarchart2(data_prep, 
                      height = myheight+20,
                      colors = mylocalcolors,
                      local_factor = c("Difference (magnigude)"),

                      suffix = local_suffix,
                      decimals = local_decimals,
                      
                      # text = ~textlabel,
                      hovertemplate = paste0('%{y:,.', local_decimals, 'f}', local_suffix),
                      hovermode = "x unified",
                      
                      textangle = 0,
                      textposition = "none",
                      textfont_color = 'black',
                      insidetextanchor = 'middle',
                      
                      bargap = 0.25,
                      barmode = 'stack',
                      
                      title_text = "",
                      title_y = 0.99,
                      
                      xaxis_tickangle = -90,
                      
                      yaxis_title = paste0("Difference between ", 
                                           cost_type,
                                           " actual and\ndetermined investment costs (M€<sub>",cef_ref_year,"</sub>)"),
                      yaxis_titlefont_size = myfont-1,
                      yaxis_ticksuffix = local_suffix,
                      yaxis_tickformat = ".0f",
                      yaxis_tickfont_size = myfont-1,
                      
                      legend_y = local_legend_y, 
                      legend_x = local_legend_x,
                      legend_xanchor = local_legend_xanchor,
                      legend_fontsize = local_legend_fontsize,
                      # trace_showlegend = FALSE
                      margin = list(t = 0, r= 50)
                      
                      ) %>% 
  layout(
      yaxis = list(
        zeroline = TRUE,
        zerolinecolor = "#808080",   # darker line at 0
        zerolinewidth = 1
    )
  ) %>% 
  add_trace(
    data = data_prep2,
    x = ~xlabel,
    y = ~myothermetric,
    name = "Difference (%)",
    mode = "markers",
    type = "scatter",
    yaxis = "y2",
    marker = list(
      size = 5,
      color = PRBActualColor
    ),
    inherit = FALSE
  ) %>% 
  layout(
    yaxis2 = list(title = list(text = "Difference (%)", standoff = -50),
                 showgrid = FALSE,
                 ticksuffix = "%", 
                 tickformat = ".0f",
                 zeroline = FALSE,
                 titlefont = list(size = myfont-1), 
                 tickfont = list(size = myfont-1),
                 overlaying = "y",
                 side = "right"
    )
  )


myplot
