if (!exists("country") | is.na(country)) {
  country = rp_full
}
if (!exists("data_loaded")) {
  source("R/get_data.R")
}

# import data  ----
if (country == rp_full) {
  ## SES case ----
  data_raw_kep <- kep_actual_mm_ses %>%
    mutate(value = kep_value_percent / 100, state = rp_full)

  data_raw_scr <- scr_actual_mm_ses %>%
    mutate(value = scr_value, state = rp_full)

  data_raw_kea <- kea_actual_mm_ses %>%
    mutate(value = hfe_kpi_percent / 100, state = rp_full)
} else {
  ## State case ----
  data_raw_kea <- kea_actual_mm
  data_raw_kep <- kep_actual_mm
  data_raw_scr <- scr_actual_mm
}

# prepare data ----
data_raw_kep_p <- data_raw_kep %>%
  rename(type = indicator_type, mymetric = value, xlabel = month) %>%
  mutate(mymetric = round(mymetric * 100, 2), type = "Flight plan") %>%
  filter(
    state == country,
    lubridate::year(xlabel) == year_report
  ) %>%
  select(xlabel, type, mymetric)

data_raw_scr_p <- data_raw_scr %>%
  rename(type = indicator_type, mymetric = value, xlabel = month) %>%
  mutate(mymetric = round(mymetric * 100, 2), type = "Shortest constrained") %>%
  filter(
    state == country,
    lubridate::year(xlabel) == year_report
  ) %>%
  select(xlabel, type, mymetric)

data_prep <- data_raw_kep_p %>%
  rbind(data_raw_scr_p) %>%
  mutate(
    xlabel = lubridate::floor_date(xlabel, unit = 'month')
  ) %>%
  as_tibble()

data_prep_kea <- data_raw_kea %>%
  filter(
    state == country,
    lubridate::year(month) == year_report
  ) %>%
  mutate(
    myothermetric = round(value * 100, 2),
    type = indicator_type,
    xlabel = lubridate::floor_date(month, unit = 'month')
  ) %>%
  select(
    xlabel,
    type,
    myothermetric
  )

c_suffix <- "%"
c_decimals <- 2

### trace parameters
c_colors = c(PRBActualColor, PRBPlannedColor)

###set up order of traces
c_factor <- c("Flight plan", "Shortest constrained")

c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textangle <- -90
c_textposition <- "inside"
c_insidetextanchor <- 'middle'

#### title
c_title_text <- paste0("Horizontal flight efficiency")

#### xaxis
c_xaxis_dtick <- 'M1'
c_xaxis_tickformat <- "%b"

#### yaxis
c_yaxis_title <- "Horizontal flight efficiency (%)"
c_yaxis_tickformat <- paste0(".", c_decimals, "f")

#### legend
if (knitr::is_latex_output()) {
  c_textfont_size <- myfont * 0.8
  c_legend_y <- -0.18
  c_bargap <- 0.05
  c_xaxis_tickangle <- -90
} else {
  c_legend_y <- mylegend_y
  c_textfont_size <- myfont * 0.9
  c_bargap <- mybargap
  c_xaxis_tickangle <- 0
}

# plot chart ----
myplot <- mybarchart2(
  data_prep,
  height = myheight + 30,
  colors = c_colors,
  local_factor = c_factor,
  suffix = c_suffix,
  decimals = c_decimals,

  hovertemplate = c_hovertemplate,

  textangle = c_textangle,
  textposition = c_textposition,
  insidetextanchor = c_insidetextanchor,

  title_text = c_title_text,

  xaxis_dtick = c_xaxis_dtick,
  xaxis_tickformat = c_xaxis_tickformat,
  xaxis_tickangle = c_xaxis_tickangle,

  yaxis_title = c_yaxis_title,
  yaxis_ticksuffix = c_suffix,
  yaxis_tickformat = c_yaxis_tickformat,

  legend_y = c_legend_y
)

myplot %>%
  add_line_trace2(
    .,
    data_prep_kea,
    name = "Actual trajectory",
    textfontcolor = "rgba(0,0,0,0)",
    linecolor = "rgba(0,0,0,0)",
  )
