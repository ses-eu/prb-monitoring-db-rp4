if (exists("country") == FALSE) {country <- "Bulgaria"}

# import data  ----
if (!exists("data_cost_inv")) {
  source("R/get_investment_data.R")
}


# process data  ----
if (country == rp_full) {
  data_prep <- data_benefit_ses_forchart %>% 
    filter(union_wide_median == 'Union-wide ave') %>% 
    filter(variable %in% c('Network', 'Local', 'Non-performance')) %>% 
    mutate(
      type = "Union-wide average",
      xlabel = factor(variable, levels = c('Network', 'Local', 'Non-performance')),
      mymetric = percent * 100
    )
  
} else {
  data_prep_uw <- data_union_wide %>% 
    filter(variable == "Network" | variable == "Local" |
             variable == "Non-performance") %>% 
    mutate(mymetric =percent *100) %>% 
    select(type = union_wide_median,
           xlabel = variable,
           mymetric)%>% 
    filter(type == "Union-wide median")
      
  denom <- paste0("nmajor_rp", rp)
  
  data_prep_ansp <- data_impact %>% 
    filter(state == .env$country) %>% 
    filter(state != rp_full) %>% 
    mutate(
      type = "ANSP",
      Network = if_else(
        .data[[denom]] == 0, 0,
        .data[[paste0("nw_rp", rp)]] / .data[[denom]]
      ) * 100,
      Local = if_else(
        .data[[denom]] == 0, 0,
        .data[[paste0("local_rp", rp)]] / .data[[denom]]
      ) * 100,
      `Non-performance` = if_else(
        .data[[denom]] == 0, 0,
        .data[[paste0("np_rp", rp)]] / .data[[denom]]
      ) * 100
    ) %>%
    select(type, Network, Local, "Non-performance") %>% 
    pivot_longer(-c(type), names_to = "xlabel", values_to = "mymetric")
  
  data_prep <- rbind(data_prep_uw, data_prep_ansp) %>% 
    mutate(xlabel = factor(xlabel, levels = c("Network", "Local", "Non-performance")))
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
  local_legend_fontsize <- myfont-1
  
} else {
  local_legend_y <- -0.12
  local_legend_x <- 0.5
  local_legend_xanchor <- 'center'
  local_legend_fontsize <- myfont
  
}

mylocalfactor <- if (country == rp_full) c("Union-wide average") else c("ANSP", "Union-wide median", NULL)
mylocalcolors <- if (country == rp_full) c('#58595B')else c('#FFC000', '#58595B')


# plot chart ----
myplot <- mybarchart2(data_prep, 
                      height = myheight,
                      colors = mylocalcolors,
                      local_factor = mylocalfactor,
                      shape = c( "", "", "", "/", "/", "/"),
                      
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
                      
                      title_text = paste0("Expected benefits of investments - RP", rp),
                      title_y = 0.99,
                      
                      yaxis_title = paste0("% of RP",rp," actual costs of investments\nwith expected impact"),
                      yaxis_titlefont_size = myfont,
                      yaxis_ticksuffix = local_suffix,
                      yaxis_tickformat = ".0f",
                      yaxis_standoff = 10,
                      
                      legend_y = local_legend_y, 
                      legend_x = local_legend_x,
                      legend_xanchor = local_legend_xanchor,
                      legend_fontsize = local_legend_fontsize)

myplot




