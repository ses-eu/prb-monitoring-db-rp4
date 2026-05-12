if (!exists("data_loaded")) {
  source("R/get_data.R")
}

if (exists("cz") == FALSE) {
  cz = c("1", "terminal")
}

# define cz ----
ez <- as.numeric(cz[[1]])
cztype <- cz[[2]]
# cztype <- "terminal"
mycz <- if_else(cztype == "terminal", tcz_list$tcz_id[ez], ecz_list$ecz_id[ez])
mycz_name <- if_else(
  cztype == "terminal",
  tcz_list$tcz_name[ez],
  ecz_list$ecz_name[ez]
)

# get data ----
# regulatory result
data_prep_reg_all <- regulatory_result(cztype, mycz)

data_prep_reg <- data_prep_reg_all %>%
  group_by(year, x5_4_total_su) %>%
  summarise(reg_res = sum(regulatory_result), .groups = "drop") %>%
  mutate(reg_res_per_su = reg_res * 1000 / x5_4_total_su) %>%
  select(year, reg_res, reg_res_per_su)

# aucu
data_prep_aucu_all <- aucu(cztype, mycz)
data_prep_aucu <- data_prep_aucu_all %>%
  select(year, aucu_excluding_or)

# join tables
data_prep <- data_prep_reg %>%
  left_join(data_prep_aucu, by = 'year') %>%
  select(-reg_res) %>%
  mutate(rr_as_perc_aucu = reg_res_per_su / aucu_excluding_or * 100) %>%
  pivot_longer(
    -c(year, rr_as_perc_aucu),
    names_to = 'type',
    values_to = 'mymetric'
  ) %>%
  mutate(
    type = case_when(
      type == 'reg_res_per_su' ~ 'Regulatory result per SU',
      type == 'aucu_excluding_or' ~ 'AUCU (before other revenues)'
    ),
    xlabel = year,
    myothermetric = round(rr_as_perc_aucu, 2)
  ) %>%
  mutate(across(
    c(mymetric, myothermetric),
    ~ if_else(xlabel > year_report, NA, .x)
  ))


# chart parameters ----
c_suffix <- ""
C_decimals <- 1

### trace parameters
c_colors = c('#9DC3E6', PRBActualColor)
###set up order of traces
c_factor <- c("AUCU (before other revenues)", "Regulatory result per SU")
c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textangle <- -90
c_textposition <- "inside"
c_insidetextanchor <- "middle"
c_textfont_color <- 'transparent'

### layout parameters
c_bargap <- 0.25
c_barmode <- 'group'

#### title
c_title_text <- paste0("Share of RR in AUCU")

#### yaxis
c_yaxis_title <- "AUCU & RR (€/SU)"
c_yaxis_tickformat <- ".0f"


#### margin
c_margin = list(t = 60, b = 0, l = 60, r = 60)

# setup ranges to ensure zero line at same height

# === Step 1: y1 setup ===
y1_max <- max(max(data_prep$mymetric, na.rm = TRUE), 0)
y1_min <- min(min(data_prep$mymetric, na.rm = TRUE), 0)

y1_span <- y1_max - y1_min
if (!is.finite(y1_span) || y1_span == 0) {
  y1_span <- max(abs(c(y1_min, y1_max, 1)))
}

y1_padding <- y1_span / 16
y1_range <- c(y1_min - y1_padding, y1_max + y1_padding)

# === Step 2: relative zero on y1 ===
y1_relative_zero <- (0 - y1_range[1]) / (y1_range[2] - y1_range[1])

# === Step 3: y2 data range ===
y2_vals <- data_prep$myothermetric[is.finite(data_prep$myothermetric)]
y2_data_min <- min(y2_vals, na.rm = TRUE)
y2_data_max <- max(y2_vals, na.rm = TRUE)
y2_data_range <- y2_data_max - y2_data_min

# === Step 4: special-case flat y2 series ===
if (length(y2_vals) == 0) {
  y2_range <- c(-5, 5)
} else if (y2_data_range == 0) {
  # only one distinct y2 value; create a real span around zero
  y2_extent <- max(abs(y2_vals), 1)
  y2_range <- c(-y2_extent, y2_extent)
} else {
  y2_total_range <- y2_data_range / (1 - 2 * y1_relative_zero)

  y2_padding_lower <- y1_relative_zero * y2_total_range
  y2_padding_upper <- (1 - y1_relative_zero) * y2_total_range
  y2_range_candidate <- c(0 - y2_padding_lower, 0 + y2_padding_upper)

  y2_range <- c(
    min(y2_range_candidate[1], y2_data_min),
    max(y2_range_candidate[2], y2_data_max)
  )
}

# === Step 5: round y2 range ===
y2_range <- c(
  floor(y2_range[1] / 5) * 5,
  ceiling(y2_range[2] / 5) * 5
)

# === Step 6: recompute zero position from final y2 range ===
y2_total_range <- y2_range[2] - y2_range[1]
y2_relative_zero <- (0 - y2_range[1]) / y2_total_range

# === Step 7: realign y1 to same zero position, but keep all y1 data visible ===
y1_total_from_min <- if (y2_relative_zero > 0) {
  abs(y1_min) / y2_relative_zero
} else {
  0
}
y1_total_from_max <- if (y2_relative_zero < 1) {
  abs(y1_max) / (1 - y2_relative_zero)
} else {
  0
}

y1_total_range <- max(y1_total_from_min, y1_total_from_max)

# keep at least the original padded span
y1_original_total_range <- y1_range[2] - y1_range[1]
y1_total_range <- max(y1_total_range, y1_original_total_range)

y1_range <- c(
  -y2_relative_zero * y1_total_range,
  (1 - y2_relative_zero) * y1_total_range
)


# plot chart  ----
p1 <- mybarchart2(
  data_prep,
  height = myheight + 30,
  colors = c_colors,
  local_factor = c_factor,
  suffix = c_suffix,
  decimals = c_decimals,

  barmode = c_barmode,
  bargap = c_bargap,

  hovertemplate = c_hovertemplate,

  textangle = c_textangle,
  textposition = c_textposition,
  textfont_color = c_textfont_color,
  insidetextanchor = c_insidetextanchor,

  title_text = c_title_text,

  yaxis_title = c_yaxis_title,
  yaxis_ticksuffix = c_suffix,
  yaxis_tickformat = c_yaxis_tickformat,

  margin = c_margin
)

p1 %>%
  add_line_trace2(
    .,
    filter(data_prep, type == 'Regulatory result per SU'),
    name = "Share of RR in AUCU (%)",
    textfontcolor = "rgba(0,0,0,0)",
    linecolor = "rgba(0,0,0,0)",
    showlegend = TRUE,
    yaxis = "y2"
  ) %>%
  layout(
    xaxis = (list(range = c(rp_min_year - 0.5, rp_max_year + 0.5))),
    yaxis = list(
      range = y1_range
    ), # to force the zeros to coincide
    yaxis2 = list(
      title = 'RR as % of AUCU',
      zerolinecolor = '#E8E8E8',
      range = y2_range,
      # rangemode = "nonnegative",
      overlaying = "y",
      ticksuffix = '%',
      side = 'right',
      showgrid = FALSE
    )
  )
