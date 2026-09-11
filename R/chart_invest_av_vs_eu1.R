if (exists("country") == FALSE) {
  country <- "France"
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
      )
  ) |>
  mutate(
    type_of_investment = case_when(
      type_of_investment == "New major investment" ~ "New major investments",
      type_of_investment ==
        "Additional new major investment" ~ "New major investments",
      type_of_investment ==
        "Additional new major investments" ~ "New major investments",
      type_of_investment == "Other new investment" ~ "Other new investments",
      type_of_investment ==
        "Additional other new investment" ~ "Other new investments",
      type_of_investment ==
        "Additional other new investments" ~ "Other new investments",
      .default = type_of_investment
    )
  ) |>
  group_by(member_state, type_of_investment, ansp_type) |>
  summarise(
    value_of_the_assets = sum(value_of_the_assets, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  rename(type = type_of_investment)


data_prep_uw <- data_pre_prep |>
  group_by(member_state, type) |>
  summarise(
    value_of_the_assets = sum(value_of_the_assets, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  group_by(member_state) |>
  mutate(mymetric = value_of_the_assets / sum(value_of_the_assets) * 100) |>
  group_by(type) |>
  summarise(
    mymetric = median(mymetric),
    .groups = "drop"
  ) |>
  mutate(
    xlabel = "Union-wide median"
  ) |>
  select(xlabel, type, mymetric)


data_prep_ansp <- data_pre_prep |>
  filter(member_state == .env$country &
           ansp_type == "Main") |>
  mutate(mymetric = value_of_the_assets / sum(value_of_the_assets) * 100) |>
  mutate(xlabel = "ANSP") |>
  select(xlabel, type, mymetric)

data_prep <- rbind(data_prep_ansp, data_prep_uw) |>
  mutate(xlabel = factor(xlabel, levels = c("ANSP", "Union-wide median"))) 


# chart ----
## chart parameters ----
local_suffix <- "%"
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
  height = myheight + 20,
  colors = c('#FFF000', '#22A0E7'),
  local_factor = c("Other new investments", "New major investments"),
  shape = c("", "/", "", "/"),

  suffix = local_suffix,
  decimals = local_decimals,

  hovertemplate = local_hovertemplate,
  hovermode = "x unified",

  textangle = 0,
  textposition = "inside",
  textfont_color = NULL,
  insidetextanchor = 'middle',

  bargap = 0.25,
  barmode = 'stack',

  title_text = "",
  title_y = 0.99,

  yaxis_title = paste0("Asset value for new investment for RP", rp),
  yaxis_ticksuffix = "%",
  yaxis_tickformat = ".0f",

  legend_y = local_legend_y,
  legend_x = local_legend_x,
  legend_xanchor = local_legend_xanchor,
  legend_fontsize = local_legend_fontsize
)

myplot
