if (!exists("data_loaded")) {
  source("R/get_data.R")
} 

if (exists("cz") == FALSE) {cz = c("1", "enroute")}

# define cz ----
ez <- as.numeric(cz[[1]])
cztype <- cz[[2]]
# cztype <- "terminal"
mycz <- if_else(cztype == "terminal",
                tcz_list$tcz_id[ez],
                ecz_list$ecz_id[ez])
mycz_name <- if_else(cztype == "terminal",
                     tcz_list$tcz_name[ez],
                     ecz_list$ecz_name[ez])

# import data & prep ----
if(cztype == "terminal") {
  data_raw <- ceff_t1_trm
} else {
  data_raw <- ceff_t1_ert
}

## SES  ----
if(country == rp_full){
  data_raw  <-  data_raw %>% 
    group_by(status, year, entity_type, entity_type_id) %>% 
    summarise(total_cost_eur_ref = sum(total_cost_eur_ref, na.rm = TRUE), .groups = "drop") %>% 
    mutate(charging_zone_code = "SES")
}

# prepare data ----
data_prep <- data_raw %>% 
  filter(
    charging_zone_code == mycz,
    entity_type != if_else(cztype == "terminal", "TCZ", "ECZ"),
    year == year_report
  ) %>% 
  mutate(
    entity_group = case_when(
      entity_type_id == "ANSP1" ~ "Main ATSP",
      str_detect(entity_type_id,"ANSP") & entity_type_id != "ANSP1" ~ "Other ATSP",
      entity_type == "MUAC" ~ "Other ATSP",
      entity_type == "MET" ~ "METSP",
      .default = "NSA (including\nEUROCONTROL)"
    ),
    mymetric = round(total_cost_eur_ref/10^6,2),
    status = str_replace(status, "A", "Actual costs"),
    status = str_replace(status, "D", "Determined costs")
  ) %>% 
  rename(type = status) %>% 
  select(
    year,
    type,
    entity_group,
    mymetric
  ) %>% 
  group_by(entity_group, type, year) %>% 
  summarise(mymetric = sum(mymetric), .groups = "drop") %>%
  mutate(xlabel = factor(entity_group, levels = c("Main ATSP",
                                                  "Other ATSP",
                                                  "METSP",
                                                  "NSA (including\nEUROCONTROL)"
                                                  )
                         )
         )



### replace 0 by NAs so they are not plotted
data_prep[data_prep == 0] <- NA

# chart parameters ----
c_suffix <- ""
c_decimals <- if_else(country == rp_full, 0, 1)

### trace parameters
c_colors = c(PRBPlannedColor, PRBActualColor)

###set up order of traces
c_factor <- data_prep %>% select(type) %>% unique() 
invisible(as.list(c_factor$type))
c_factor <- sort(c_factor$type, decreasing = TRUE)
c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textangle <- 0
c_textposition <- "outside"
c_insidetextanchor <- NULL

### layout parameters
c_barmode <- 'group'

#### title
c_title_text <- paste0("Total costs per entity group - ",
                               as.character(year_report))

#### yaxis
c_yaxis_title <- paste0(if_else(cztype == "terminal", "Terminal", "En route"),
                        " costs (M€<sub>",cef_ref_year,"</sub>)")
c_yaxis_tickformat <- ",.0f"

#### legend
c_legend_y <- -0.17

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
                  yaxis_tickformat = c_yaxis_tickformat,
                  
                  legend_y = c_legend_y
) 
p1

