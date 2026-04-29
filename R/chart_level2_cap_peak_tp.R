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
      format(round(change_yoy, 1), nsmall = 1)
    ),
    mytooltip = paste0(
      year,
      ": ",
      format(round(weighted_year, 1), nsmall = 1),
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
  textfont_color = "transparent",
  #
  title_text = c_title_text,

  yaxis_title = c_yaxis_title,
  yaxis_ticksuffix = c_suffix,
  yaxis_tickformat = c_yaxis_tickformat
)


p1 %>%
  layout(
    yaxis = list(
      rangemode = "tozero"
    ),
    xaxis = list(
      range = c(rp_min_year - 0.5, rp_max_year + 0.5)
    )
  )

# `%||%` <- function(x, y) if (is.null(x)) y else x
#
# add_bar_top_annotations <- function(
#     p,
#     data,
#     x_col = "xlabel",
#     group_col = "type",
#     y_col = "mymetric",
#     label_col = "myothermetric",
#     group_levels = NULL,     # pass c_factor here if you want explicit ordering
#     xshift_step = 50,        # NOTE: -25 for left bar in 2-group case => step=50
#     y_pad_frac = 0.02,
#     font_size = myfont
# ) {
#   stopifnot(all(c(x_col, group_col, y_col, label_col) %in% names(data)))
#
#   df <- data %>%
#     transmute(
#       .x = as.character(.data[[x_col]]),
#       .g = as.character(.data[[group_col]]),
#       .y = as.numeric(.data[[y_col]]),
#       .lbl = as.character(.data[[label_col]])
#     )
#
#   if (is.null(group_levels)) {
#     group_levels <- sort(unique(df$.g))
#   } else {
#     group_levels <- as.character(group_levels)
#   }
#
#   n_g <- length(group_levels)
#   if (n_g == 0L) return(p)
#
#   g_idx <- match(df$.g, group_levels) - 1L
#   xshift <- (g_idx - (n_g - 1) / 2) * xshift_step
#
#   y_rng <- range(df$.y, na.rm = TRUE)
#   y_pad <- (y_rng[2] - y_rng[1]) * y_pad_frac
#   if (!is.finite(y_pad) || y_pad == 0) y_pad <- 0
#
#   ann <- lapply(seq_len(nrow(df)), function(i) {
#     lbl <- df$.lbl[i]
#     if (is.na(lbl) || lbl == "" || lbl == "NA") return(NULL)
#
#     list(
#       x = df$.x[i], xref = "x",
#       y = df$.y[i] + y_pad, yref = "y",
#       text = lbl,
#       showarrow = FALSE,
#       xanchor = "center",
#       yanchor = "bottom",
#       xshift = xshift[i],
#       font = list(size = font_size)
#     )
#   })
#   ann <- Filter(Negate(is.null), ann)
#
#   p %>%
#     layout(
#       annotations = (p$x$layout$annotations %||% list()) |> c(ann),
#       cliponaxis = FALSE
#     )
# }
#
# p1 <- add_bar_top_annotations(
#   p1,
#   data = data_prep,
#   group_levels = c_factor,  # keeps year order identical to your bars
#   xshift_step = 48          # keep this if -25 worked for the left bar
# )

# p1
