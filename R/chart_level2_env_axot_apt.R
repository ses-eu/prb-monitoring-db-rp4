## import data  ----
if (!exists("data_loaded")) {
  source("R/params_country.R")
  source("R/get_data.R")
}

data_raw <- rbind(axot_actual_apt, axit_actual_apt, asma_actual_apt)

## prepare data ----
data_prep <- data_raw %>%
  filter(
    state == .env$country,
    year == .env$year_report,
    airport_code %in% airports_table$apt_code
  ) %>%
  left_join(airports_table, by = c("airport_code" = "apt_code")) %>%
  mutate(
    xlabel = apt_name,
    type = case_when(
      indicator_type == "ARP_TAXI" ~ "AXOT",
      indicator_type == "ARP_TAXI_IN" ~ "AXIT",
      indicator_type == "ARP_ASMA" ~ "ASMA"
    ),
    mymetric = value
  ) %>%
  select(
    xlabel,
    type,
    mymetric
  ) %>%
  arrange(xlabel, type) %>%
  drop_na(mymetric)

## chart parameters ----
c_suffix <- ""
c_decimals <- 2

### trace parameters
c_colors <- c(PRBSecondBlue, PRBActualColor, "#BFBFBF")

###set up order of traces
c_factor <- c("AXOT", "ASMA", "AXIT")

c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textposition <- "outside"
c_insidetextanchor <- NA
c_textfont_color <- 'black'

#### title
c_title_text <- paste0("AXOT, ASMA & AXIT main airport(s) - ", year_report)

#### yaxis
c_yaxis_title <- "AXOT, ASMA & AXIT (min/flight)"
c_yaxis_tickformat <- paste0(".", c_decimals, "f")

### legend
c_legend_x = -0.1
c_legend_y = 1.3
c_legend_xanchor = "left"

### margin
c_margin = list(t = 70)

## plot chart  ----
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

  yaxis_title = c_yaxis_title,
  yaxis_ticksuffix = c_suffix,
  yaxis_tickformat = c_yaxis_tickformat,

  legend_x = c_legend_x,
  legend_y = c_legend_y,
  legend_xanchor = c_legend_xanchor,

  margin = c_margin
)

myplot
