if (!exists("data_loaded")) {
  source("R/get_data.R")
} 

# fix ez if script not executed from qmd file ----
if (exists("cz") == FALSE) {cz = c("1", "terminal")}
# ez=1

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

# import data  ----
if(cztype == "terminal") {
  data_raw <- ceff_t2_trm
} else {
  data_raw <- ceff_t2_ert
}

# prepare data ----
# table 2
data_prep_t2 <- data_raw %>% 
  filter(status == "A" & (entity_type == "ECZ" | entity_type == "TCZ")) %>% 
  select(entity_code, year, x3_8_diff_det_cost_actual_cost) 

# exchange rates
xrate_year_cz <- xrate_year %>% 
  # mutate (xrate = if_else(cz_code == "EBEL_ECZ", 2, xrate)) %>% 
  filter(cztype == cztype)  

data_prep_eur <- data_prep_t2 %>% 
  left_join(xrate_year_cz, by = c('year', "entity_code"  = "cz_code")) %>% 
  mutate(mymetric = x3_8_diff_det_cost_actual_cost/xrate,
         xlabel = year,
         type = 'Cost exempt') %>% 
  arrange(xlabel) %>% 
  select(
    entity_code, xlabel, mymetric
  )
  
data_prep_eur_ses <- data_prep_eur %>% 
  group_by(xlabel) %>% 
  summarise(mymetric = sum(mymetric, na.rm = TRUE), .groups = "drop") %>% 
  mutate(entity_code = "SES") %>% 
  select(
    entity_code, xlabel, mymetric
  )

data_prep <- data_prep_eur %>% 
  rbind(data_prep_eur_ses) %>% 
  filter(
   entity_code == mycz,
  ) %>% 
  mutate(
    mymetric = case_when(
      xlabel > year_report ~ NA,
      .default = round(mymetric/1000,2),
    ),
    type = "A"
  )



# chart parameters ----
c_suffix <- ""
c_decimals <- if_else(country == rp_full, 0, 1)

### trace parameters
c_colors = c( '#8497B0')
###set up order of traces
c_factor <- data_prep %>% select(type) %>% unique() 
invisible(as.list(c_factor$type))
c_factor <- sort(c_factor$type, decreasing = TRUE)
c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textposition <- "auto"
c_insidetextanchor <- NULL

#### title
c_title_text <- paste0("Cost exempt from cost sharing")

#### yaxis
c_yaxis_title <- "Cost exempt from cost sharing\n(€'000)"
c_yaxis_tickformat <- ",.0f"

# plot chart  ----
p1 <- mybarchart2(data_prep, 
                  height = myheight + 20,
                  colors = c_colors,
                  local_factor = c_factor,
                  suffix = c_suffix,
                  decimals = c_decimals,
                  
                  hovertemplate = c_hovertemplate,
                  
                  textposition = c_textposition, 
                  insidetextanchor = c_insidetextanchor,
                  
                  title_text = c_title_text,

                  yaxis_title = c_yaxis_title,
                  yaxis_ticksuffix = c_suffix,
                  yaxis_tickformat = c_yaxis_tickformat
) 

p1 %>% 
  add_empty_trace(., data_prep) 
