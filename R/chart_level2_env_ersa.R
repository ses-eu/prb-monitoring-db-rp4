## import data  ----
if (!exists("data_loaded")) {
  source("R/get_data.R")
}

data_raw <- env_mil_actual

## prepare data ----
data_prep <- data_raw %>%
  filter(
    state == .env$country,
    year <= .env$year_report
  ) %>%
  mutate(
    mymetric = round(ersa * 100, 0),
    type = "ERSA"
  ) %>%
  select(
    xlabel = year,
    mymetric,
    type
  )

## chart parameters ----
c_suffix <- "%"
c_decimals <- 0

### trace parameters
c_colors = c(PRBActualColor)

###set up order of traces
c_factor <- c("ERSA")

c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textangle <- 0
c_textposition <- "outside"
c_insidetextanchor <- NA

#### title
if (knitr::is_latex_output()) {
  c_title_text <- paste0(
    "Effective use of reserved or segregated\nairspace (ERSA)(PI#9)"
  )
  c_title_y <- 0.95
  c_margin = list(t = 60)
} else {
  c_title_text <- paste0(
    "Effective use of reserved or segregated airspace (ERSA)(PI#9)"
  )
  c_title_y <- mytitle_y
  c_margin = mymargin
}

#### yaxis
c_yaxis_title <- "ERSA (%)"
c_yaxis_tickformat <- paste0(".", 0, "f")

# plot chart ----
myplot <- mybarchart2(
  data_prep,
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
  title_y = c_title_y,

  yaxis_title = c_yaxis_title,
  yaxis_ticksuffix = c_suffix,
  yaxis_tickformat = c_yaxis_tickformat
)

rp_years_df_value <- as_tibble(rp_years) %>%
  select(xlabel = value) %>%
  mutate(mymetric = 0, type = 'none')

myplot %>%
  add_empty_trace(rp_years_df_value) %>%
  layout(yaxis = list(range = c(0, 100)))
