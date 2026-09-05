if (exists("country") == FALSE) {
  country <- "Belgium"
}

# import data  ----
if (!exists("data_assets")) {
  source("R/get_investment_data.R")
}

# process data  ----
data_pre_prep <- data_assets |>
  filter(
    type_of_investment %in%
      c(
        "New major investment",
        "New major investments",
        "Other new investments",
        "Other new investment",
        "Additional new major investment",
        "Additional new major investments",
        "Additional other new investment",
        "Additional other new investments"
      ) &
      ansp_type == "Main"
  ) |>
  group_by(member_state) |>
  summarise(
    value = sum(value_of_the_assets, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ungroup() |>
  mutate(
    type = "ANSP"
  )

total_uw <- data_pre_prep |>
  summarise(
    value = sum(value, na.rm = TRUE)
  ) |>
  pull()

data_prep <- data_pre_prep |>
  filter(member_state == .env$country) |>
  mutate(
    mymetric = value / total_uw * 100
  ) |>
  select(type, mymetric) |>
  rbind(tibble(type = "Union-wide", mymetric = NA)) |>
  mutate(
    mymetric = case_when(
      type == "Union-wide" ~ 100 - lag(mymetric, 1),
      .default = mymetric
    ),
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
  colors = c('#FFF000', '#22A0DD'),
  shape = c("/", ""), # not supported by plotly on donut charts
  hovertemplate = "%{label}: %{value:.0f}%<extra></extra>",
  title_text = paste0("Asset value of new investments RP", rp, " (%)"),
  minsize = 14,
  legend_x = local_legend_x,
  legend_y = local_legend_y,
  legend_xanchor = local_legend_xanchor,
  legend_orientation = "h"
)
