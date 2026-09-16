if (exists("country") == FALSE) {
  country <- "Bulgaria"
}

# import data  ----
if (!exists("data_total_costs_rt")) {
  source("R/get_investment_data.R")
}


# process data  ----
## total costs
data_raw <- data_total_costs_rt |>
  select(
    member_state,
    contains('20')
  ) |>
  pivot_longer(
    cols = -member_state,
    names_to = c("year", "type"),
    names_pattern = "^x(\\d{4})([da])$",
    values_to = "value"
  ) |>
  filter(
    year == year_report,
    !is.na(member_state)
  )


data_total_cost_all <- data_raw |>
  mutate(
    member_state = if_else(
      member_state == "Belgium-Luxembourg",
      "Belgium",
      member_state,
    )
  ) |>
  select(member_state, type, value) |>
  group_by(member_state, type) |>
  summarise(
    total_costs = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

## investment costs
data_inv_costs <- data_costs_rt_muac_not_split |>
  mutate(
    member_state = if_else(
      member_state == "Belgium-Luxembourg",
      "Belgium",
      member_state,
    )
  ) |>
  select(
    member_state,
    contains('20')
  ) |>
  pivot_longer(
    cols = -member_state,
    names_to = c("year", "type"),
    names_pattern = "^x(\\d{4})([da])$",
    values_to = "value"
  ) |>
  filter(year == year_report) |>
  group_by(member_state, type) |>
  summarise(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

data_prep <- data_inv_costs |>
  left_join(data_total_cost_all, by = c("member_state", "type")) |>
  mutate(
    type = if_else(type == 'd', "Determined", "Actual"),
    mymetric = value / total_costs * 100
  ) |>
  select(
    xlabel = member_state,
    type,
    mymetric
  )

data_sort <- data_prep |>
  filter(type == "Determined") |>
  arrange(desc(mymetric))

states_factor <- unique(data_sort$xlabel)

data_prep <- data_prep %>%
  mutate(xlabel = factor(xlabel, levels = states_factor))

# chart ----
## chart parameters ----
local_suffix <- "%"
local_decimals <- 1

###set up order of traces
local_hovertemplate <- paste0('%{y:.', local_decimals, 'f}', local_suffix)

#### legend
if (knitr::is_latex_output()) {
  local_legend_y <- mylegend_y
  local_legend_x <- -0.18
  local_legend_xanchor <- 'left'
  local_legend_fontsize <- myfont - 1
} else {
  local_legend_y <- 1
  local_legend_x <- 1
  local_legend_xanchor <- 'right'
  local_legend_fontsize <- myfont - 2
}

# plot chart ----
myplot <- mybarchart2(
  data_prep,
  height = myheight + 40,
  colors = c(PRBPlannedColor, PRBActualColor),
  local_factor = c("Determined", "Actual", NULL),
  # shape = c("/", "", "/", "", "/", "", "/", "", "/", ""),

  suffix = local_suffix,
  decimals = local_decimals,

  hovertemplate = local_hovertemplate,
  hovermode = "x unified",

  textangle = 0,
  textposition = "none",
  textfont_color = 'black',
  insidetextanchor = 'middle',

  bargap = 0.25,
  barmode = 'group',

  title_text = "",
  title_y = 0.99,

  xaxis_tickangle = -90,

  yaxis_title = "Share of costs of investments\nin total costs (%)",
  yaxis_standoff = 5,
  yaxis_ticksuffix = local_suffix,
  yaxis_tickformat = ",.0f",
  yaxis_tickfont_size = myfont,

  legend_orientation = "v",
  legend_y = local_legend_y,
  legend_x = local_legend_x,
  legend_xanchor = local_legend_xanchor,
  legend_fontsize = local_legend_fontsize
)

myplot
