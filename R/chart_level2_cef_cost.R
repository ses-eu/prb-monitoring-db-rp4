if (!exists("data_loaded")) {
  source("R/get_data.R")
} 

if (exists("cz") == FALSE) {cz = c("1", "enroute")}

# define cz ----
if (country != "MUAC") {
  ez <- as.numeric(cz[[1]])
  cztype <- cz[[2]]
  # cztype <- "terminal"
  mycz <- if_else(cztype == "terminal",
                  tcz_list$tcz_id[ez],
                  ecz_list$ecz_id[ez])
  mycz_name <- if_else(cztype == "terminal",
                       tcz_list$tcz_name[ez],
                       ecz_list$ecz_name[ez])
}

# import data  ----
## State  ----
if(cztype == "terminal") {
  data_raw <- ceff_t1_trm
} else {
  data_raw <- ceff_t1_ert
}

# prepare data ----
if(country == rp_full){
  data_pre_prep  <-  data_raw %>% 
    filter(entity_type == "ECZ" | entity_type == "TCZ") %>% 
    group_by(status, year) %>% 
    summarise(total_cost_eur_ref = sum(total_cost_eur_ref, na.rm = TRUE), .groups = "drop") %>% 
    mutate(entity_code = "SES")
  
} else if(country == "MUAC") {
  data_pre_prep <- data_raw |> 
    filter(grepl("MUAC", entity_code)) |> 
    mutate(entity_code = NA) |> 
    group_by(year, status, entity_code) |> 
    summarise(total_cost_eur_ref = sum(total_cost_eur_ref, na.rm = TRUE)) |> 
    ungroup() 
  
} else {
  
  data_pre_prep <- data_raw %>% 
    filter(entity_code == mycz) 
}

data_prep<- data_pre_prep %>% 
  mutate(
    mymetric = round(total_cost_eur_ref/10^6, 2),
    status = str_replace(status, "A", "Actual costs"),
    status = str_replace(status, "D", "Determined costs")
  ) %>%  
  arrange(year) %>% 
  select(
    year,
    type = status,
    mymetric,
    xlabel = year
  ) 

### replace 0 by NAs so they are not plotted
data_prep[data_prep == 0] <- NA

# chart parameters ----
c_suffix <- ""
c_decimals <- if_else(country == rp_full | country == "MUAC", 0, 1)

### trace parameters
c_colors = c(PRBPlannedColor, PRBActualColor)

###set up order of traces
c_factor <- data_prep %>% select(type) %>% unique() 
invisible(as.list(c_factor$type))
c_factor <- sort(c_factor$type, decreasing = TRUE)
c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textangle <- -90
c_textposition <- "inside"
c_insidetextanchor <- "middle"

### layout parameters
c_barmode <- 'group'

#### title
c_title_text <- paste0("Total costs")

#### yaxis
c_yaxis_title <- paste0(if_else(cztype == "terminal", "Terminal", "En route"),
                        " costs (M€<sub>",cef_ref_year,"</sub>)")
c_yaxis_tickformat <- ",.0f"

# plot chart  ----
p1 <- mybarchart2(data_prep, 
                  height = myheight,
                  colors = c_colors,
                  local_factor = c_factor,
                  suffix = c_suffix,
                  decimals = c_decimals,
                  barmode = c_barmode,
                  
                  hovertemplate = c_hovertemplate,
                  
                  textangle = c_textangle,
                  textposition = c_textposition, 
                  insidetextanchor = c_insidetextanchor,
                  
                  title_text = c_title_text,

                  yaxis_title = c_yaxis_title,
                  yaxis_ticksuffix = c_suffix,
                  yaxis_tickformat = c_yaxis_tickformat
) 
p1
