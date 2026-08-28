if (exists("country") == FALSE) {
  country <- "Belgium"
}

# import data  ----
if (!exists("data_funding_rt")) {
  source("R/get_investment_data.R")
}

# process data  ----
data_prep_all <- data_funding_rt |>
  group_by(member_state) |>
  summarise(
    across(
      where(is.numeric),
      ~ sum(.x, na.rm = TRUE) / 10^3
    ),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = -member_state,
    names_to = c("year"),
    names_pattern = "^x(\\d{4})",
    values_to = "value"
  )

total_funding_state <- data_prep_all |>
  filter(member_state == .env$country) |>
  summarise(value = sum(value, na.rm = TRUE))

total_funding_uw <- data_prep_all |>
  summarise(value = sum(value, na.rm = TRUE)) |>
  pull()

share_funding_uw <- (1 - pull(total_funding_state) / total_funding_uw) * 100

data_prep <- total_funding_state |>
  mutate(
    mymetric = value / total_funding_uw * 100,
    type = "ANSP"
  ) |>
  select(-value) |>
  add_row(mymetric = share_funding_uw, type = "Union-wide") |>
  mutate(
    textposition = if_else(mymetric == 0 | mymetric > 2, "inside", "outside"),
    textlabel = if_else(
      mymetric == 0,
      " ",
      paste0(format(janitor::round_half_up(mymetric, 0), nsmall = 0), "%")
    )
  ) |>
  select(type, mymetric, textlabel, textposition)


# chart ----
## legend
if (knitr::is_latex_output()) {
  local_legend_x <- 1
  local_legend_y <- 0.5
} else {
  local_legend_x <- 0.5
  local_legend_y <- -0.05
  local_legend_xanchor <- 'center'
}


# plot chart ----
mydonutchart(
  data_prep,
  colors = c('#22A0E7', '#58595B'),
  shape = c("/", ""), # not supported by plotly on donut charts
  hovertemplate = "%{label}: %{value:.0f}%<extra></extra>",
  title_text = paste0(
    "Share of declared funding in RP",
    rp,
    " (M€<sub>",
    cef_ref_year,
    "</sub>)"
  ),
  minsize = 14,
  legend_x = local_legend_x,
  legend_y = local_legend_y,
  legend_xanchor = local_legend_xanchor,
  legend_orientation = "h"
)
