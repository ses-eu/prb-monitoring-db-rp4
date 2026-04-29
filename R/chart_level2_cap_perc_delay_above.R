if (!exists("data_loaded")) {
  source("R/params_country.R")
  source("R/get_data.R")
}

# import data  ----
data_raw <- cap_perc_delay_tp_above

# prepare data ----
data_prep <- data_raw %>%
  filter(acc_full_name %in% acc_list$acc_full_name) %>%
  filter(year >= rp_min_year, year <= year_report) %>%
  mutate(
    mymetric = round(perc_delay_tp_above * 100, 1)
  ) %>%
  select(
    type = year,
    xlabel = acc_full_name,
    mymetric
  )

# chart parameters ----
c_suffix <- "%"
c_decimals <- 1

### trace parameters
max_colors <- c(
  PRBSecondBlue,
  PRBActualColor,
  '#BFBFBF',
  PRBPlannedColor,
  '#E97132'
)
c_colors <- head(max_colors, year_report - rp_min_year + 1)

###set up order of traces
c_factor <- data_prep %>% select(type) %>% unique()
invisible(as.list(c_factor$type))
c_factor <- sort(c_factor$type, decreasing = FALSE)
c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)
# c_hovertemplate <-"%{meta}<extra></extra>"

c_textangle <- -90
c_textposition <- "inside"
c_insidetextanchor <- "middle"
c_textfont_color <- "transparent"

### layout parameters
c_barmode <- 'group'

#### title
c_title_text <- paste0(
  "% of en-route ATFM delay occurring when daily\nthroughput was above expected daily traffic",
  if_else(year_report == rp_min_year, paste0(" - ", year_report), "")
)
c_title_y <- 0.95

#### yaxis
c_yaxis_title <- paste0("% of en route ATFM Delay minutes")
c_yaxis_tickformat <- ",.0f"

c_margin <- list(t = 60)

# plot chart  ----
p1 <- mybarchart2(
  data_prep,
  height = myheight + 30,
  colors = c_colors,
  local_factor = c_factor,
  suffix = c_suffix,
  decimals = c_decimals,
  barmode = c_barmode,

  hovertemplate = c_hovertemplate,

  textangle = c_textangle,
  textposition = c_textposition,
  insidetextanchor = c_insidetextanchor,
  textfont_color = c_textfont_color,

  title_text = c_title_text,
  title_y = c_title_y,

  yaxis_title = c_yaxis_title,
  yaxis_ticksuffix = c_suffix,
  yaxis_tickformat = c_yaxis_tickformat,

  margin = c_margin
)


p1
