if (exists("country") == FALSE) {country <- "Bulgaria"}

# import data  ----
if (!exists("data_cost_inv")) {
  source("R/get_investment_data.R")
}


# process data  ----
rp_years <- as.integer(rp_years)

c_prefixes <- c("nm", "on", "e")

cols <- c(
  as.vector(outer(paste0(c_prefixes, "_d"), rp_years, paste, sep = "_")),
  as.vector(outer(paste0(c_prefixes, "_a"), rp_years, paste, sep = "_"))
)


data_prep <- data_cost_inv %>% 
  filter(member_state == .env$country) %>% 
  select(all_of(cols)) %>% 
  pivot_longer(
    cols = everything(),  # Pivot all columns
    names_to = c("type", "year"),  # Create "type" and "year" columns
    names_pattern = "(.+?)_(\\d{4})",  # Regex: Extract "type" + 4-digit year
    values_to = "value"  # Store values in "value" column
  ) %>% 
  mutate(type = if_else(str_detect(type,"_d"), "Determined", "Actual")) %>% 
  group_by(type, year) %>% 
  summarise(total = sum(value, na.rm = TRUE)/10^6) %>% 
  ungroup() %>% 
  mutate(total = if_else(total == 0, NA, total)) %>% 
  select(
    xlabel = year,
    type,
    mymetric = total)   

# chart ----
## chart parameters ----
local_suffix <- ""
local_decimals <- 0

###set up order of traces
local_hovertemplate <- paste0('%{y:,.', local_decimals, 'f}', local_suffix)

#### legend
if (knitr::is_latex_output()) {
  local_legend_y <- mylegend_y
  local_legend_x <- -0.18
  local_legend_xanchor <- 'left'
  local_legend_fontsize <- myfont-1
  
} else {
  local_legend_y <- -0.12
  local_legend_x <- 0.5
  local_legend_xanchor <- 'center'
  local_legend_fontsize <- myfont
  
}

# plot chart ----
myplot <- mybarchart2(data_prep, 
                      height = myheight,
                      colors = c(PRBPlannedColor, PRBActualColor),
                      local_factor = c("Determined",
                                       "Actual",
                                        NULL),
                      # shape = c("/", "", "/", "", "/", "", "/", "", "/", ""),
                      
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
                      
                      title_text = "Total costs of investments",
                      title_y = 0.99,
                      
                      yaxis_title = paste0("Total costs of investments (M€<sub>",cef_ref_year,"</sub>)"),
                      yaxis_ticksuffix = local_suffix,
                      yaxis_tickformat = ",.0f",
                      
                      legend_y = local_legend_y, 
                      legend_x = local_legend_x,
                      legend_xanchor = local_legend_xanchor,
                      legend_fontsize = local_legend_fontsize)

myplot




