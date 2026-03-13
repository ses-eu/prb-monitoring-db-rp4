if (exists("country") == FALSE) {country <- "Bulgaria"}

# import data  ----
if (!exists("data_cost_inv")) {
  source("R/get_investment_data.R")
}


# process data  ----
cols <- paste0(
  c("ses", "par", "cpmp", "sesdet", "pardet", "cpmpdet"),
  "_rp",
  rp
)

data_prep <- data_impact %>% 
  filter(state == .env$country) %>%
  select(all_of(cols)) %>%
  pivot_longer(everything(), names_to = "xlabel", values_to = "mymetric") %>% 
  mutate(
    mymetric = mymetric / 10^6,
    type = if_else(str_detect(xlabel, "det"), "Determined", "Actual"),
    xlabel = str_replace_all(xlabel, "det", ""),
    xlabel = case_when(
      xlabel == paste0("ses_rp", rp) ~ "SES mandated",
      xlabel == paste0("par_rp", rp) ~ "Partnership",
      xlabel == paste0("cpmp_rp", rp) ~ "CP/MP investments"
    ),
    xlabel = factor(xlabel, levels = c("SES mandated", "Partnership", "CP/MP investments"))
  )


# chart ----
## chart parameters ----
local_suffix <- ""
local_decimals <- 1

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
                      # shape = c("/", "/", "/", "/", "", "", "", ""),
                      
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
                      
                      title_text = "Costs by type of investments - actual and determined",
                      title_y = 0.99,
                      
                      yaxis_title = paste0("RP",rp," determined versus actual\ncost of investments (M€<sub>",cef_ref_year,"</sub>)"),
                      yaxis_titlefont_size = myfont,
                      yaxis_ticksuffix = local_suffix,
                      yaxis_tickformat = ".0f",
                      yaxis_standoff = 10,
                      
                      legend_y = local_legend_y, 
                      legend_x = local_legend_x,
                      legend_xanchor = local_legend_xanchor,
                      legend_fontsize = local_legend_fontsize)

myplot




