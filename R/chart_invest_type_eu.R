if (exists("country") == FALSE) {
  country <- "Belgium"
}

# import data  ----
if (!exists("data_assets")) {
  source("R/get_investment_data.R")
}


# process data  ----
if (country == rp_full) {
  data_prep <- data_benefit_ses_forchart %>%
    filter(union_wide_median == 'Union-wide ave') %>%
    filter(
      variable %in% c('SES mandated', 'Partnership', 'CP/MP investment')
    ) %>%
    mutate(
      type = "Union-wide average",
      xlabel = factor(
        variable,
        levels = c('SES mandated', 'Partnership', 'CP/MP investment')
      ),
      mymetric = percent * 100
    )
} else {
  data_inv_type <- data_assets |>
    filter(
      # member_state == .env$country,
      ansp_type == "Main",
      type_of_investment == "New major investment"
    ) |>
    select(
      member_state,
      name_of_investment,
      ses_mandated,
      partnership
    ) |>
    arrange(name_of_investment)

  data_inv_cost <- data_costs |>
    filter(
      # member_state == .env$country,
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
    filter(type == 'a') |>
    group_by(member_state, name_of_investment) |>
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  data_prep_all <- data_inv_type |>
    left_join(data_inv_cost, by = c("member_state", "name_of_investment")) |>
    mutate(
      `SES mandated` = as.numeric(ses_mandated) * value,
      Partnership = as.numeric(partnership) * value
    ) |>
    group_by(member_state) |>
    summarise(
      `SES mandated` = sum(`SES mandated`, na.rm = TRUE),
      Partnership = sum(Partnership, na.rm = TRUE),
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      `SES mandated` = `SES mandated` / value * 100,
      Partnership = Partnership / value * 100
    ) |>
    select(-value) |>
    pivot_longer(
      cols = -member_state,
      names_to = 'xlabel',
      values_to = 'mymetric'
    )

  data_prep_uw <- data_prep_all |>
    group_by(xlabel) |>
    summarise(
      mymetric = median(mymetric, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      type = "Union-wide median"
    )

  data_prep_ansp <- data_prep_all |>
    filter(
      member_state == .env$country,
    ) |>
    select(-member_state) |>
    mutate(
      type = "ANSP"
    )

  data_prep <- rbind(data_prep_ansp, data_prep_uw) |>
    mutate(
      xlabel = factor(xlabel, levels = c("SES mandated", "Partnership"))
    ) |>
    arrange(type, desc(xlabel))
}


# chart ----
## chart parameters ----
local_suffix <- "%"
local_decimals <- 0

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

mylocalfactor <- if (country == rp_full) {
  c("Union-wide average")
} else {
  c("ANSP", "Union-wide median", NULL)
}
mylocalcolors <- if (country == rp_full) {
  c('#58595B')
} else {
  c(PRBActualColor, '#58595B')
}


# plot chart ----
myplot <- mybarchart2(
  data_prep,
  height = myheight,
  colors = mylocalcolors,
  local_factor = mylocalfactor,
  shape = c("", "", "/", "/"),
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

  title_text = "Costs by type of investments - impact",
  title_y = 0.99,

  yaxis_title = paste0(
    "% of RP",
    rp,
    " actual costs of new\ninvestments by type of investment"
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
