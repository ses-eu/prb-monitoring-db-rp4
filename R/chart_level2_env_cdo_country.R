## import data  ----
if (!exists("data_loaded")) {
  source("R/get_data.R")
}

data_raw <- cdo_cco_actual

airports_country <- airports_table %>%
  filter(country_name == .env$country)

data_filtered <- data_raw %>%
  filter(
    year == .env$year_report
  ) %>%
  right_join(
    airports_country,
    by = c("apt_icao" = "apt_code")
  )

## prepare data ----
data_prep <- data_filtered %>%
  group_by(year) %>%
  summarise(
    nbr_flights_descent = sum(nbr_flights_descent, na.rm = TRUE),
    tot_time_level_seconds_descent = sum(
      tot_time_level_seconds_descent,
      na.rm = TRUE
    ),
    nbr_flights_climb = sum(nbr_flights_climb, na.rm = TRUE),
    tot_time_level_seconds_climb = sum(
      tot_time_level_seconds_climb,
      na.rm = TRUE
    )
  ) %>%
  mutate(
    CDO = tot_time_level_seconds_descent /
      nbr_flights_descent,
    CCO = tot_time_level_seconds_climb / nbr_flights_climb
  ) %>%
  select(
    year,
    CDO,
    CCO
  ) %>%
  pivot_longer(
    c(CDO, CCO),
    names_to = "type",
    values_to = "mymetric"
  ) %>%
  mutate(
    xlabel = year,
    textposition = 'top',
    linedash = 'solid'
  ) %>%
  select(
    xlabel,
    type,
    mymetric,
    textposition,
    linedash
  )

## chart parameters ----
c_suffix <- ""
c_decimals <- 1


### trace parameters
c_colors = c(PRBSecondBlue, PRBActualColor)

###set up order of traces
c_factor <- data_prep %>% select(type) %>% unique() %>% pull()

c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

#### title
c_title_text <- paste0("CDOs & CCOs")

#### yaxis
c_yaxis_title <- "CDOs & CCOs (average sec/flight)"
c_yaxis_tickformat <- paste0(".", "0f")
c_yaxis_rangemode <- NA
# c_yaxis_range <- c((floor(min(data_prep$mymetric, na.rm = TRUE)/5)*5)-5, (ceiling(max(data_prep$mymetric, na.rm = TRUE)/5)*5)+5)

## define chart function ----
myplot <- mylinechart2(
  data_prep,
  colors = c_colors,
  local_factor = c_factor,
  suffix = c_suffix,
  decimals = c_decimals,

  hovertemplate = c_hovertemplate,

  textangle = c_textangle,

  title_text = c_title_text,

  yaxis_title = c_yaxis_title,
  yaxis_ticksuffix = c_suffix,
  yaxis_tickformat = c_yaxis_tickformat
)

myplot %>%
  layout(
    yaxis = list(
      rangemode = "tozero"
    ),
    xaxis = list(
      range = c(rp_min_year - 0.5, rp_max_year + 0.5)
    )
  )
