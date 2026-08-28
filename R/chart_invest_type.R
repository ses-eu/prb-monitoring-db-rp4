if (exists("country") == FALSE) {
  country <- "Bulgaria"
}

# import data  ----
if (!exists("data_assets")) {
  source("R/get_investment_data.R")
}


# process data  ----
data_inv_type <- data_assets |>
  filter(
    member_state == .env$country,
    ansp_type == "Main",
    type_of_investment == "New major investment"
  ) |>
  select(
    name_of_investment,
    ses_mandated,
    partnership
  ) |>
  arrange(name_of_investment)

data_inv_cost <- data_costs |>
  filter(
    member_state == .env$country,
    ansp_type == "Main",
    type_of_investment == "New major investment"
  ) |>
  select(
    member_state,
    name_of_investment,
    contains("20"),
    -contains("wacc")
  ) |>
  group_by(member_state, name_of_investment) |>
  summarise(
    across(
      where(is.numeric),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = -c(member_state, name_of_investment),
    names_to = c("year", "type"),
    names_pattern = "^x(\\d{4})([da])$",
    values_to = "value"
  ) |>
  group_by(name_of_investment, type) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

data_prep <- data_inv_cost |>
  left_join(data_inv_type, by = c("name_of_investment")) |>
  mutate(
    `SES mandated` = as.numeric(ses_mandated) * value / 10^6,
    Partnership = as.numeric(partnership) * value / 10^6
  ) |>
  group_by(type) |>
  summarise(
    `SES mandated` = sum(`SES mandated`, na.rm = TRUE),
    Partnership = sum(Partnership, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = -type,
    names_to = "xlabel",
    values_to = "mymetric"
  ) |>
  mutate(
    type = if_else(type == "d", "Determined", "Actual"),
    xlabel = factor(xlabel, levels = c('SES mandated', 'Partnership'))
  )


# chart ----
## chart parameters ----
local_suffix <- ""
local_decimals <- 1

###set up order of traces
local_hovertemplate <- paste0('%{y:,.', local_decimals, 'f}', local_suffix)

#### legend
if (knitr::is_latex_output()) {
  local_legend_y <- mylegend_y
  local_legend_x <- -0.18
  local_legend_xanchor <- 'left'
  local_legend_fontsize <- myfont - 1
} else {
  local_legend_y <- -0.12
  local_legend_x <- 0.5
  local_legend_xanchor <- 'center'
  local_legend_fontsize <- myfont
}

# plot chart ----
myplot <- mybarchart2(
  data_prep,
  height = myheight,
  colors = c(PRBPlannedColor, PRBActualColor),
  local_factor = c("Determined", "Actual", NULL),
  # shape = c("/", "/", "/", "/", "", "", "", ""),

  suffix = local_suffix,
  decimals = local_decimals,

  hovertemplate = local_hovertemplate,
  hovermode = "x unified",

  textangle = 0,
  textposition = "outside",
  textfont_color = 'black',
  insidetextanchor = 'middle',

  bargap = 0.25,
  barmode = 'group',

  title_text = "Costs by type of investments - actual and determined",
  title_y = 0.99,

  yaxis_title = paste0(
    rp_short,
    " determined versus actual\ncost of investments (M€<sub>",
    cef_ref_year,
    "</sub>)"
  ),
  yaxis_titlefont_size = myfont,
  yaxis_ticksuffix = local_suffix,
  yaxis_tickformat = ".0f",
  yaxis_standoff = 10,

  legend_y = local_legend_y,
  legend_x = local_legend_x,
  legend_xanchor = local_legend_xanchor,
  legend_fontsize = local_legend_fontsize
)

myplot
