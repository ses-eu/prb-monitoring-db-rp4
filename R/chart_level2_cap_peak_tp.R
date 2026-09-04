if (!exists("data_loaded")) {
  source("R/params_country.R")
  source("R/get_data.R")
}

# import data  ----
data_raw <- cap_avg_peak_tp_actual

# prepare data ----
data_prep <- data_raw %>%
  filter(acc_id %in% acc_list$acc_id) %>%
  mutate(month = month(entry_day)) %>%
  arrange(acc_id, month, entry_day) %>%
  group_by(acc_id, month) %>%
  mutate(
    change_yoy = weighted_td - lag(weighted_td, 1),
    myothermetric = paste0(
      if_else(change_yoy >= 0, "+", ""),
      format(janitor::round_half_up(change_yoy, 1), nsmall = 1)
    ),
    mytooltip = paste0(
      year,
      ": ",
      format(janitor::round_half_up(weighted_year, 1), nsmall = 1),
      " (",
      myothermetric,
      " vs ",
      year - 1,
      ")"
    )
  ) %>%
  ungroup() %>%
  group_by(year) %>%
  filter(
    entry_day == max(entry_day)
  ) %>%
  ungroup() %>%
  filter(year >= rp_min_year, year <= year_report) %>%
  mutate(
    textposition = 'top',
    linedash = 'solid'
  ) %>%
  select(
    xlabel = year,
    type = acc_full_name,
    mymetric = weighted_year,
    textposition,
    linedash
  )

## adjustments due to comment by France/Spain during FV2025
data_prep <- data_prep |>
  mutate(
    textposition = if_else(
      type %in% c("Brest ACC", "Palma ACC") & xlabel == rp_min_year,
      "bottom",
      textposition
    )
  )

if (country == 'Spain') {
  data_prep <- data_prep |>
    mutate(
      type = factor(
        type,
        levels = c(
          'Madrid ACC',
          'Barcelona ACC',
          'Sevilla ACC',
          'Canarias ACC',
          'Palma ACC'
        )
      )
    )
}


# chart parameters ----
c_suffix <- ""
c_decimals <- 1

### trace parameters
max_colors <- c(
  PRBSecondBlue,
  PRBActualColor,
  '#BFBFBF',
  PRBPlannedColor,
  '#E97132'
)
c_colors = head(max_colors, acc_no)

###set up order of traces
c_factor <- data_prep %>% select(type) %>% unique()
invisible(as.list(c_factor$type))
c_factor <- sort(c_factor$type, decreasing = FALSE)
c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)
# c_hovertemplate <- "%{meta}<extra></extra>"

#### title
c_title_text <- paste0(
  "Average daily peak throughput",
  if_else(year_report == rp_min_year, paste0(" - ", year_report), "")
)

#### yaxis
c_yaxis_title <- paste0("Average daily peak (flights per hour)")
c_yaxis_tickformat <- ",.0f"

# plot chart  ----
p1 <- mylinechart2(
  data_prep,
  height = myheight + 30,
  colors = c_colors,
  local_factor = c_factor,
  suffix = c_suffix,
  decimals = c_decimals,
  # barmode = c_barmode,

  hovertemplate = c_hovertemplate,

  # textangle = c_textangle,
  # textposition = c_textposition,
  # insidetextanchor = c_insidetextanchor,
  textfont_color = "black",
  textfontsize = myfont - 1,
  #
  title_text = c_title_text,

  yaxis_title = c_yaxis_title,
  yaxis_ticksuffix = c_suffix,
  yaxis_tickformat = c_yaxis_tickformat
)


p1 %>%
  layout(
    yaxis = list(
      rangemode = if_else(country %in% c('France', 'Spain'), "nomral", "tozero")
    ),
    xaxis = list(
      range = c(rp_min_year - 0.5, rp_max_year + 0.5)
    )
  )
