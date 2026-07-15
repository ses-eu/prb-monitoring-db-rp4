if (!exists("data_loaded")) {
  source("R/get_data.R")
}

if (!exists("cz")) {
  cz <- c("1", "enroute")
}

# define cz ----
ez <- as.numeric(cz[[1]])
cztype <- cz[[2]]

mycz <- if_else(
  cztype == "terminal",
  tcz_list$tcz_id[ez],
  ecz_list$ecz_id[ez]
)

mycz_name <- if_else(
  cztype == "terminal",
  tcz_list$tcz_name[ez],
  ecz_list$ecz_name[ez]
)

# import data ----
data_reg_res <- regulatory_result(cztype, mycz)

# pre-prep data ----
data_for_chart_wide <- data_reg_res %>%
  filter(type == "Main ANSP") %>%
  mutate(
    share_rr_act_rev_expost = regulatory_result / actual_revenues * 100,
    share_rr_act_rev_exante = ex_ante_roe / x4_2_cost_excl_vfr_d * 100
  ) %>%
  select(
    year,
    regulatory_result,
    ex_ante_roe,
    share_rr_act_rev_expost,
    share_rr_act_rev_exante
  ) %>%
  mutate(across(-year, ~ if_else(year > year_report, NA_real_, .x)))

# prep data ----
data_for_chart_value <- data_for_chart_wide %>%
  select(-c(share_rr_act_rev_expost, share_rr_act_rev_exante)) %>%
  pivot_longer(-year, names_to = "xlabel", values_to = "mymetric") %>%
  mutate(
    xlabel = case_when(
      xlabel == "regulatory_result" ~ "Ex-post",
      xlabel == "ex_ante_roe" ~ "Ex-ante"
    ),
    mymetric = if_else(
      xlabel == "Ex-post" & year > year_report,
      NA_real_,
      mymetric / 1000
    )
  )

data_for_chart_share <- data_for_chart_wide %>%
  select(-c(regulatory_result, ex_ante_roe)) %>%
  pivot_longer(-year, names_to = "xlabel", values_to = "share") %>%
  mutate(
    xlabel = case_when(
      xlabel == "share_rr_act_rev_expost" ~ "Ex-post",
      xlabel == "share_rr_act_rev_exante" ~ "Ex-ante"
    ),
    share = if_else(xlabel == "Ex-post" & year > year_report, NA_real_, share)
  )

data_prep <- data_for_chart_value %>%
  left_join(data_for_chart_share, by = c("year", "xlabel")) %>%
  mutate(
    type = xlabel,
    xlabel = year
  )

# chart parameters ----
c_suffix <- ""
c_decimals <- 1

c_colors <- c("#CEE0EA", "#5A9ABD")
c_factor <- c("Ex-ante", "Ex-post")
c_hovertemplate <- paste0("%{y:,.", c_decimals, "f}", c_suffix)

c_textangle <- 0
c_textposition <- "outside"
c_insidetextanchor <- NULL
c_textfont_color <- "black"
c_trace_showlegend <- FALSE

c_barmode <- "group"

if (country == 'Luxembourg' & cztype == 'enroute') {
  main_ansp_graph <- 'skeyes'
} else {
  main_ansp_graph <- main_ansp
}

c_title_text <- if_else(
  country == rp_full,
  "RR - Main ANSPs",
  paste0("RR - ", main_ansp_graph)
)

c_yaxis_title <- "RR (M€)"
c_yaxis_tickformat <- ".1f"
c_legend_y <- -0.24
c_margin <- list(t = 40, b = 80, l = 40, r = 60)

#### xaxis
negative_check <- ((min(data_prep$mymetric, na.rm = TRUE) < 0) &
  (abs(min(data_prep$mymetric, na.rm = TRUE)) >
    abs(max(data_prep$mymetric, na.rm = TRUE)))) |
  ((min(data_prep$mymetric, na.rm = TRUE) >= 0) &
    (min(data_prep$share, na.rm = TRUE) < 0.1))
if (negative_check) {
  c_ticklen <- 15
} else {
  c_ticklen <- 0
}


# setup ranges to ensure zero line at same height ----

# 1) y1 values
y1_vals <- data_prep$mymetric[is.finite(data_prep$mymetric)]
if (length(y1_vals) == 0) {
  y1_vals <- c(0)
}

y1_min <- min(c(y1_vals, 0), na.rm = TRUE)
y1_max <- max(c(y1_vals, 0), na.rm = TRUE)

y1_span <- y1_max - y1_min
if (!is.finite(y1_span) || y1_span == 0) {
  y1_span <- max(abs(c(y1_min, y1_max, 1)), na.rm = TRUE)
}

y1_padding <- y1_span / 16
y1_range <- c(y1_min - y1_padding, y1_max + y1_padding)

# 2) zero position on y1
y1_relative_zero <- (0 - y1_range[1]) / (y1_range[2] - y1_range[1])

# 3) y2 values
y2_vals <- data_prep$share[is.finite(data_prep$share)]
if (length(y2_vals) == 0) {
  y2_vals <- c(0)
}

y2_min <- min(c(y2_vals, 0), na.rm = TRUE)
y2_max <- max(c(y2_vals, 0), na.rm = TRUE)
y2_span <- y2_max - y2_min

# 4) build y2 range so zero lands at the same relative height as y1
if (!is.finite(y2_span) || y2_span == 0) {
  y2_extent <- max(abs(c(y2_min, y2_max, 1)), na.rm = TRUE)
  y2_range <- c(-y2_extent, y2_extent)
} else {
  y2_total_from_min <- if (y1_relative_zero > 0) {
    abs(y2_min) / y1_relative_zero
  } else {
    0
  }

  y2_total_from_max <- if (y1_relative_zero < 1) {
    abs(y2_max) / (1 - y1_relative_zero)
  } else {
    0
  }

  y2_total_range <- max(y2_total_from_min, y2_total_from_max)

  if (!is.finite(y2_total_range) || y2_total_range <= 0) {
    y2_total_range <- max(abs(c(y2_min, y2_max, 1)), na.rm = TRUE) * 2
  }

  y2_range <- c(
    -y1_relative_zero * y2_total_range,
    (1 - y1_relative_zero) * y2_total_range
  )
}

# 5) optional rounding for nicer ticks
y2_range <- c(
  floor(y2_range[1] / 5) * 5,
  ceiling(y2_range[2] / 5) * 5
)

# 6) recompute final y2 zero position after rounding
y2_total_range <- y2_range[2] - y2_range[1]
y2_relative_zero <- (0 - y2_range[1]) / y2_total_range

# 7) realign y1 to that exact zero position too, without cutting data
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

y1_original_total_range <- y1_range[2] - y1_range[1]

y1_total_range <- max(
  y1_total_from_min,
  y1_total_from_max,
  y1_original_total_range
)

y1_range <- c(
  -y2_relative_zero * y1_total_range,
  (1 - y2_relative_zero) * y1_total_range
)

# check if all values are positive or negative

y1_vals <- data_prep$mymetric[is.finite(data_prep$mymetric)]
y2_vals <- data_prep$share[is.finite(data_prep$share)]

if (length(y1_vals) == 0) {
  y1_vals <- 0
}
if (length(y2_vals) == 0) {
  y2_vals <- 0
}

y1_all_positive <- all(y1_vals >= 0)
y1_all_negative <- all(y1_vals <= 0)
y2_all_positive <- all(y2_vals >= 0)
y2_all_negative <- all(y2_vals <= 0)

if (y1_all_positive & y2_all_positive) {
  y1_range <- c(0, max(y1_range))
  y2_range <- c(0, max(y2_range))
} else if (y1_all_negative & y2_all_negative) {
  y1_range <- c(min(y1_range), 0)
  y2_range <- c(min(y2_range), 0)
}

# percent label annotations aligned to grouped bars ----
annotation_xshift <- 12

ann_ex_ante <- data_prep %>%
  filter(type == "Ex-ante", !is.na(share)) %>%
  transmute(
    x = xlabel,
    y = share,
    text = paste0(format(janitor::round_half_up(share, 0), nsmall = 0), "%"),
    xshift = -annotation_xshift
  )

ann_ex_post <- data_prep %>%
  filter(type == "Ex-post", !is.na(share)) %>%
  transmute(
    x = xlabel,
    y = share,
    text = paste0(format(janitor::round_half_up(share, 0), nsmall = 0), "%"),
    xshift = annotation_xshift
  )

percent_annotations <- c(
  lapply(seq_len(nrow(ann_ex_ante)), function(i) {
    list(
      x = ann_ex_ante$x[i],
      y = ann_ex_ante$y[i],
      xref = "x",
      yref = "y2",
      text = ann_ex_ante$text[i],
      showarrow = FALSE,
      textangle = 0,
      xshift = ann_ex_ante$xshift[i],
      yshift = 0,
      font = list(color = "#F58A2E", size = myfont - 1),
      xanchor = "center",
      yanchor = "top"
    )
  }),
  lapply(seq_len(nrow(ann_ex_post)), function(i) {
    list(
      x = ann_ex_post$x[i],
      y = ann_ex_post$y[i],
      xref = "x",
      yref = "y2",
      text = ann_ex_post$text[i],
      showarrow = FALSE,
      textangle = 0,
      xshift = ann_ex_post$xshift[i],
      yshift = 0,
      font = list(color = "#F58A2E", size = myfont - 1),
      xanchor = "center",
      yanchor = "top"
    )
  })
)

legend_annotations <- list(
  list(
    xanchor = "left",
    x = 0.0,
    y = c_legend_y - 0.04 * negative_check,
    text = "■",
    font = list(size = myfont * 1.2, color = c_colors[[1]]),
    xref = "paper",
    yref = "paper",
    showarrow = FALSE,
    ax = 0,
    ay = 0
  ),
  list(
    xanchor = "left",
    x = 0.07,
    y = c_legend_y - 0.04 * negative_check,
    text = "Ex-ante RR (in value)",
    font = list(size = myfont * 0.9, color = "black"),
    xref = "paper",
    yref = "paper",
    showarrow = FALSE,
    ax = 0,
    ay = 0
  ),
  list(
    xanchor = "left",
    x = 0.6,
    y = c_legend_y - 0.04 * negative_check,
    text = "■",
    font = list(size = myfont * 1.2, color = c_colors[[2]]),
    xref = "paper",
    yref = "paper",
    showarrow = FALSE,
    ax = 0,
    ay = 0
  ),
  list(
    xanchor = "left",
    x = 0.67,
    y = c_legend_y - 0.04 * negative_check,
    text = "Ex-post RR (in value)",
    font = list(size = myfont * 0.9, color = "black"),
    xref = "paper",
    yref = "paper",
    showarrow = FALSE,
    ax = 0,
    ay = 0
  ),
  list(
    xanchor = "left",
    x = 0.0,
    y = c_legend_y - 0.1 - 0.04 * negative_check,
    text = "<b>―</b>",
    font = list(size = myfont * 1.2, color = "#E46C0A"),
    xref = "paper",
    yref = "paper",
    showarrow = FALSE,
    ax = 0,
    ay = 0
  ),
  list(
    xanchor = "left",
    x = 0.07,
    y = c_legend_y - 0.1 - 0.04 * negative_check,
    text = paste0(
      "RR as percentage of ",
      if_else(cztype == "enroute", "en route", "terminal"),
      " revenues"
    ),
    font = list(size = myfont * 0.9, color = "black"),
    xref = "paper",
    yref = "paper",
    showarrow = FALSE,
    ax = 0,
    ay = 0
  )
)

all_annotations <- c(legend_annotations, percent_annotations)

# plot chart ----
mybarchart2(
  data_prep,
  height = myheight + 40,
  colors = c_colors,
  local_factor = c_factor,
  suffix = c_suffix,
  decimals = c_decimals,
  barmode = c_barmode,
  hovertemplate = c_hovertemplate,
  textangle = c_textangle,
  textposition = c_textposition,
  textfont_color = c_textfont_color,
  insidetextanchor = c_insidetextanchor,
  trace_showlegend = c_trace_showlegend,
  title_text = c_title_text,
  yaxis_title = c_yaxis_title,
  yaxis_ticksuffix = c_suffix,
  yaxis_tickformat = c_yaxis_tickformat,
  legend_y = c_legend_y,
  margin = c_margin
) %>%
  add_trace(
    inherit = FALSE,
    data = data_prep,
    x = ~xlabel,
    y = ~share,
    name = paste0(
      "RR as percentage of ",
      if_else(cztype == "enroute", "en route", "terminal"),
      " revenues"
    ),
    yaxis = "y2",
    # mode = "markers",
    text = "",
    type = 'box',
    color = ~ factor(type, levels = c_factor),
    line = list(color = '#E46C0A', width = mylinewidth),
    fillcolor = '#E46C0A',
    # marker = list(size = mylinewidth * 3, color = '#E46C0A'),
    hoverinfo = 'none',
    showlegend = F
  ) %>%
  add_trace(
    data = filter(data_prep, type == "Ex-ante"),
    x = ~xlabel,
    y = ~ paste0(share, '%'),
    yaxis = "y2",
    mode = "markers",
    type = 'scatter',
    text = "",
    name = paste0(
      "RR (ex-ante) as % of ",
      if_else(cztype == "enroute", "en route", "terminal"),
      " revenues"
    ),
    marker = list(size = 1, color = "transparent"),
    hovertemplate = paste0('%{y:,.1f}%'),
    showlegend = F
  ) %>%
  add_trace(
    data = filter(data_prep, type == "Ex-post"),
    x = ~xlabel,
    y = ~share,
    yaxis = "y2",
    mode = "markers",
    type = 'scatter',
    text = "",
    name = paste0(
      "RR (ex-post) as % of ",
      if_else(cztype == "enroute", "en route", "terminal"),
      " revenues"
    ),
    marker = list(size = 1, color = "transparent"),
    hovertemplate = paste0('%{y:,.1f}%'),
    showlegend = F
  ) %>%
  layout(
    xaxis = list(
      range = c(rp_min_year - 0.5, rp_max_year + 0.5),
      tickcolor = "white",
      ticklen = c_ticklen
    ),
    yaxis = list(
      range = y1_range,
      secondary_y = TRUE
    ),
    yaxis2 = list(
      title = paste0(
        "RR as % of ",
        if_else(cztype == "enroute", "en route", "terminal"),
        " revenues"
      ),
      overlaying = "y",
      zerolinecolor = "#E8E8E8",
      range = y2_range,
      secondary_y = FALSE,
      ticksuffix = "%",
      tickformat = if_else(
        max(abs(data_prep$share), na.rm = TRUE) < 0.1,
        ".1f",
        ".0f"
      ),
      side = "right",
      showgrid = FALSE
    ),
    boxmode = "group",
    bargroupgap = 0.1,
    boxgroupgap = 0.4,
    boxgap = mybargap,
    annotations = all_annotations
  )
