if (!exists("data_loaded")) {
  source("R/get_data.R")
} 

if (exists("cz") == FALSE) {cz = c("1", "terminal", "level1")}

# define cz & level----
ez <- as.numeric(cz[[1]])
cztype <- cz[[2]]
# cztype <- "terminal"
mycz <- if_else(cztype == "terminal",
                tcz_list$tcz_id[ez],
                ecz_list$ecz_id[ez])
mycz_name <- if_else(cztype == "terminal",
                     tcz_list$tcz_name[ez],
                     ecz_list$ecz_name[ez])

doclevel <- cz[[3]]

# import data  ----
## State  ----
if(cztype == "terminal") {
  data_raw <- ceff_t1_trm
} else {
  data_raw <- ceff_t1_ert
}

## SES  ----
if(country == rp_full){
  data_raw  <-  data_raw %>% 
    filter(entity_type == "ECZ" | entity_type == "TCZ") %>% 
    group_by(status, year) %>% 
    summarise(duc_eur_ref = sum(total_cost_eur_ref, na.rm = TRUE)/sum(total_su, na.rm = TRUE), .groups = "drop") %>% 
    mutate(entity_code = "SES")
}

# prepare data ----
data_prep <- data_raw %>% 
  filter(
    entity_code == mycz) %>% 
  mutate(
    mymetric = round(duc_eur_ref, 2)
  ) %>%  
  select(
    year,
    status,
    mymetric
  ) %>%  
  filter(year >= rp_min_year) %>% 
  mutate(xlabel = as.character(year),
         status = str_replace(status, "A", "Actual unit cost"),
         status = str_replace(status, "D", "Determined unit cost")
  ) %>% 
  arrange(xlabel) %>% 
  rename(type = status)
  
### replace 0 by NAs so they are not plotted
data_prep[data_prep == 0] <- NA

# chart parameters ----
c_suffix <- ""
c_decimals <- 2

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
c_minsize <- myfont*0.95

#### title
if (knitr::is_latex_output()) {
  if ((cztype == "terminal" & no_tcz > 1) | (cztype == "enroute" & no_ecz > 1))  {
    c_level1_title <- paste0(" determined/actual unit\ncosts (DUC/AUC) - ",
                             mycz_name)
  } else{
    c_level1_title <- paste0(" determined/actual unit\ncosts (DUC/AUC)")

  }
  c_title_y <- 0.95
  c_margin <- list(t = 50)
  
} else {
  if ((cztype == "terminal" & no_tcz > 1) | (cztype == "enroute" & no_ecz > 1))  {
    c_level1_title <- paste0(" determined/actual unit costs (DUC/AUC)\n",
                             mycz_name)
    c_title_y <- 0.95
  } else{
    c_level1_title <- paste0(" determined/actual unit costs (DUC/AUC)")
    c_title_y <- mytitle_y
    
  }
  c_margin <- mymargin
  
}

c_title_text <- if_else(doclevel == "level1",
                        paste0(if_else(cztype == "terminal", "Terminal", "En route"),
                               c_level1_title),
                        "DUC/AUC")

#### yaxis
c_yaxis_title <- paste0(if_else(cztype == "terminal", "Terminal ", "En route "), 
                        " unit costs (€<sub>",cef_ref_year,"</sub>)")
c_yaxis_tickformat <- ".0f"


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
                   title_y = c_title_y,
                   
                   yaxis_title = c_yaxis_title,
                   yaxis_ticksuffix = c_suffix,
                   yaxis_tickformat = c_yaxis_tickformat,
                   
                   margin = c_margin,
                   minsize = c_minsize
) 
p1
