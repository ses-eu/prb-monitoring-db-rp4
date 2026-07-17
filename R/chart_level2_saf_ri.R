if (!exists("country") | is.na(country)) {
  country = rp_full
  source("R/params_country.R")
}

if (!exists("data_loaded")) {
  source("R/get_data.R")
}

# import data  ----
if (country == rp_full) {
  ## SES case ----
  data_raw <- saf_ri_actual_ses |>
    #so they have the same structure
    mutate(
      rate_per_100_000 = NA,
      rate_per_100_000_with_ans_contribution = NA
    )
} else {
  ## State case ----
  data_raw <- saf_ri_actual
}

# prepare data ----

data_prep <- data_raw %>%
  filter(
    state == country,
    year <= year_report
  ) |>
  select(
    year,
    rate_per_100_000,
    rate_per_100_000_with_ans_contribution,
    eu_wide_average,
    eu_wide_average_ans_contribution
  ) |>
  pivot_longer(-c(year), names_to = "type", values_to = "mymetric") |>
  mutate(
    xlabel = year,
    linedash = case_when(
      type == "rate_per_100_000" ~ "solid",
      type == "rate_per_100_000_with_ans_contribution" ~ "solid",
      type == "eu_wide_average" ~ "dot",
      type == "eu_wide_average_ans_contribution" ~ "dot"
    ),
    type = case_when(
      type == "rate_per_100_000" ~ "RI",
      type ==
        "rate_per_100_000_with_ans_contribution" ~ "RI with ANS contribution",
      type == "eu_wide_average" ~ "EU Wide Average - RI",
      type ==
        "eu_wide_average_ans_contribution" ~ "EU Wide Average - RI with ANS contribution"
    ),
    textposition = "top center",
  ) |>
  select(xlabel, type, mymetric, textposition, linedash) |>
  #otherwise the lindash column does not work
  arrange(desc(linedash))


# chart parameters ----
c_suffix <- ""
c_decimals <- 2
c_markersize <- 4


### trace parameters
local_color1 <- "#22a0dd"
local_color2 <- "#196AB4"
c_colors = c(local_color1, local_color2, local_color2, local_color1)

###set up order of traces
c_factor <- c(
  "RI with ANS contribution",
  "RI",
  "EU Wide Average - RI with ANS contribution",
  "EU Wide Average - RI"
)

c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

#### title
c_title_text <- paste0("RIs per 100,000 movements")

#### yaxis
c_yaxis_title <- "RIs per 100,000 movements"
c_yaxis_tickformat <- ".1f"


# plot chart ----
##I had to do it this way because when there are NA values the dash doesn't work
data_prep_s1 <- data_prep |>
  filter(
    type %in%
      c("EU Wide Average - RI", "EU Wide Average - RI with ANS contribution")
  )
data_prep_s2 <- data_prep |>
  filter(type %in% c("RI", "RI with ANS contribution")) |>
  mutate(myothermetric = mymetric)

p1 <- mylinechart2(
  data_prep_s1,
  height = myheight,
  colors = c_colors,
  local_factor = c_factor,
  suffix = c_suffix,
  decimals = c_decimals,

  hovertemplate = c_hovertemplate,

  title_text = c_title_text,

  yaxis_title = c_yaxis_title,
  yaxis_ticksuffix = c_suffix,
  yaxis_tickformat = c_yaxis_tickformat
) %>%
  layout(
    yaxis = list(rangemode = "tozero"),
    xaxis = list(range = c(rp_min_year - 0.5, rp_max_year + 0.5))
  )

p2 <- p1 %>%
  add_line_trace2(
    .,
    filter(data_prep_s2, type == "RI with ANS contribution"),
    name = "RI with ANS contribution",
    textfontcolor = "black",
    textdecimals = c_decimals,
    textfontsize = myfont - 1,
    markercolor = local_color2,
    markersize = 6,
    linecolor = local_color2
  )

p2 %>%
  add_line_trace2(
    .,
    filter(data_prep_s2, type == "RI"),
    name = "RI",
    textfontsize = myfont - 1,
    textfontcolor = "black",
    textdecimals = c_decimals,
    markercolor = local_color1,
    markersize = 6,
    linecolor = local_color1
  ) %>%
  layout(
    legend = list(
      traceorder = "reversed",
      font = list(size = myfont - 1)
    )
  )
