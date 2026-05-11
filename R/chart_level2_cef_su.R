if (!exists("data_loaded")) {
  source("R/get_data.R")
}
if (exists("cz") == FALSE) {
  cz = c("1", "enroute")
}

# define cz ----
ez <- as.numeric(cz[[1]])
cztype <- cz[[2]]
# cztype <- "terminal"
mycz <- if_else(cztype == "terminal", tcz_list$tcz_id[ez], ecz_list$ecz_id[ez])
mycz_name <- if_else(
  cztype == "terminal",
  tcz_list$tcz_name[ez],
  ecz_list$ecz_name[ez]
)

# import data  ----
## State  ----
if (cztype == "terminal") {
  data_raw <- ceff_t1_trm
} else {
  data_raw <- ceff_t1_ert
}

## SES  ----
if (country == rp_full) {
  data_raw <- data_raw %>%
    filter(entity_type == "ECZ" | entity_type == "TCZ") %>%
    group_by(status, year) %>%
    summarise(total_su = sum(total_su, na.rm = TRUE), .groups = "drop") %>%
    mutate(entity_code = "SES")
}


# prepare data ----
data_prep <- data_raw |>
  filter(entity_code == mycz) %>%
  mutate(
    mymetric = round(total_su / 1000, 0),
    status = str_replace(status, "A", "Actual SUs"),
    status = str_replace(status, "D", "Planned SUs")
  ) |>
  arrange(year)


## replace 0 by NAs so they are not plotted
data_prep[data_prep == 0] <- NA

## add a column to determine the minimum y axis value
data_prep <- data_prep %>% mutate(min_y_axis = min(mymetric, na.rm = T) / 1.5)

## create dfs for series
data_prep_planned <- data_prep %>%
  filter(status == "Planned SUs")

data_prep_actual <- data_prep %>%
  filter(status == "Actual SUs")

# set parameters for chart ----
c_axis_title <- paste0(
  if_else(cztype == "terminal", "Terminal", "En route"),
  " service units ('000)"
)
c_chart_title <- paste0(
  if_else(cztype == "terminal", "Terminal", "En route"),
  " service units"
)

c_margin = list(t = 40, b = 80)

c_legend_y_position <- -0.37

# define chart function ----
myc <- function(
  width = mywidth,
  height = myheight,
  font = myfont,
  linewidth = mylinewidth,
  margin = mymargin
) {
  plot_ly(
    width = width,
    height = height
  ) %>%
    add_trace(
      data = data_prep_planned,
      x = ~year,
      y = ~mymetric,
      text = ~ format(round(mymetric * .02, 0), nsmall = 0, big.mark = ','),
      yaxis = "y1",
      type = 'scatter',
      mode = 'lines',
      line = list(width = 0, dash = 'solid', color = 'transparent'),
      error_y = list(type = "data", array = ~ mymetric * 0.02, color = 'grey'),
      opacity = 1,
      hoverinfo = 'none',
      # hovertemplate = paste('±2%: ±%{text}<extra></extra>'),
      showlegend = F
    ) %>%
    add_trace(
      data = data_prep_planned,
      x = ~year,
      y = ~mymetric,
      text = ~ format(round(mymetric * .1, 0), nsmall = 0, big.mark = ','),
      yaxis = "y1",
      type = 'scatter',
      mode = 'lines',
      line = list(width = 0, dash = 'solid', color = 'transparent'),
      error_y = list(type = "data", array = ~ mymetric * 0.1, color = 'black'),
      opacity = 1,
      hoverinfo = 'none',
      # hovertemplate = paste('±10%: ±%{text}<extra></extra>'),
      showlegend = F
    ) %>%
    add_trace(
      data = data_prep_planned,
      x = ~year,
      y = ~mymetric,
      name = ~status,
      text = ~status,
      yaxis = "y1",
      type = 'scatter',
      mode = 'lines+markers',
      line = list(width = linewidth, dash = 'solid', color = PRBPlannedColor),
      marker = list(size = linewidth * 3, color = PRBPlannedColor),
      # error_y = list(type = "data", array = ~mymetric*0.1, color= 'black'),
      opacity = 1,
      hovertemplate = paste('%{text}: %{y:,.0f}<extra></extra>'),
      showlegend = T
      # hoverinfo = 'none'
    ) %>%
    add_trace(
      #we add them again for the tooltip order
      data = data_prep_planned,
      x = ~year,
      y = ~mymetric,
      text = ~ format(round(mymetric * .02, 0), nsmall = 0, big.mark = ','),
      yaxis = "y1",
      type = 'scatter',
      mode = 'lines',
      line = list(width = 0, dash = 'solid', color = 'transparent'),
      opacity = 1,
      # hoverinfo = 'none',
      hovertemplate = paste('±2%: ±%{text}<extra></extra>'),
      showlegend = F
    ) %>%
    add_trace(
      data = data_prep_planned,
      x = ~year,
      y = ~mymetric,
      text = ~ format(round(mymetric * .1, 0), nsmall = 0, big.mark = ','),
      yaxis = "y1",
      type = 'scatter',
      mode = 'lines',
      line = list(width = 0, dash = 'solid', color = 'transparent'),
      opacity = 1,
      # hoverinfo = 'none',
      hovertemplate = paste('±10%: ±%{text}<extra></extra>'),
      showlegend = F
    ) %>%
    add_trace(
      inherit = FALSE,
      data = data_prep_actual,
      x = ~year,
      y = ~mymetric,
      name = ~status,
      customdata = ~status,
      yaxis = "y1",
      cliponaxis = FALSE,
      yaxis = "y1",
      type = 'scatter',
      mode = 'lines+markers',
      line = list(width = linewidth, dash = 'solid', color = PRBActualColor),
      marker = list(size = linewidth * 3, color = PRBActualColor),
      opacity = 1,
      hovertemplate = paste('%{customdata}: %{y:,.0f}<extra></extra>'),
      # hoverinfo = "none",
      showlegend = T
    ) %>%
    add_trace(
      ## to push the y axis down
      x = ~year,
      y = ~min_y_axis,
      yaxis = "y1",
      cliponaxis = FALSE,
      yaxis = "y1",
      type = 'scatter',
      mode = 'lines',
      line = list(width = 0, dash = 'solid', color = 'transparent'),
      opacity = 1,
      hoverinfo = 'none',
      showlegend = F
    ) %>%
    config(responsive = TRUE, displaylogo = FALSE, displayModeBar = F) %>%
    layout(
      font = list(family = "Roboto"),
      title = list(
        text = c_chart_title,
        y = mytitle_y,
        x = mytitle_x,
        xanchor = mytitle_xanchor,
        yanchor = mytitle_yanchor,
        font = list(size = mytitle_font_size)
      ),
      dragmode = FALSE,
      hovermode = "x unified",
      hoverlabel = list(bgcolor = "rgba(255,255,255,0.88)"),
      xaxis = list(
        title = "",
        gridcolor = 'rgb(255,255,255)',
        showgrid = FALSE,
        showline = FALSE,
        showticklabels = TRUE,
        dtick = 1,
        # tickcolor = 'rgb(127,127,127)',
        # ticks = 'outside',
        zeroline = TRUE,
        tickfont = list(size = font)
      ),
      yaxis = list(
        title = c_axis_title,
        # gridcolor = 'rgb(255,255,255)',
        showgrid = TRUE,
        showline = FALSE,
        tickformat = ",",
        tickprefix = " ",
        # rangemode = "tozero",
        # autorange = 'max',
        # showticklabels = TRUE,
        # tickcolor = 'rgb(127,127,127)',
        # ticks = 'outside',
        zeroline = TRUE,
        zerolinecolor = 'rgb(240,240,240)',
        titlefont = list(size = font),
        tickfont = list(size = font)
      ),
      # showlegend = FALSE
      legend = list(
        orientation = 'h',
        xanchor = "center",
        x = 0.5,
        y = -0.1,
        font = list(size = font)
      ),
      margin = margin
    )
}


# plot chart ----
myc(mywidth, myheight + 20, myfont, mylinewidth, c_margin) %>%
  layout(
    annotations = list(
      list(
        xanchor = "right",
        x = 0.49,
        y = c_legend_y_position,
        text = '<span style="color:grey;font-family:Arial"><b>Ɪ</b></span>  ±2% dead-band',
        font = list(size = myfont),
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        # arrowhead = 7,
        ax = 0,
        ay = 0
      ),
      list(
        xanchor = "left",
        x = 0.51,
        y = c_legend_y_position,
        text = '<span style="color:black;font-family:Arial"><b>Ɪ</b></span>  ±10% threshold',
        font = list(size = myfont),
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        # arrowhead = 7,
        ax = 0,
        ay = 0
      )
    )
  )
