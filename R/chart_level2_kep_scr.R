############### state level adapted to RP4, not NM or SES

# import data  ----
if (country == "SES RP4"){
  ## SES case ----
  data_raw_kep  <-  read_xlsx(
    paste0(data_folder, "SES file.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Table_KEP",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() |> 
    #so it has the same structure as the state case
    mutate(entity_name = "SES RP3") 

  data_raw_scr  <-  read_xlsx(
    paste0(data_folder, "SES file.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Table_SCR",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names()  |> 
    #so it has the same structure as the state case
    mutate(entity_name = "SES RP3") 
    
} else  {
  ## State case ----
  if (!exists("kep_actual")) {
    source("R/get_data.R")
  }
  
  data_raw_kep <- kep_actual
  data_raw_scr <- scr_actual
}

# prepare data ----
## create sequence of years to ensure full series
rp_years_df <- data.frame(rp_years) %>% rename(xlabel = rp_years)

data_prep_kep <- data_raw_kep %>% 
  mutate(type = indicator_type,
         mymetric = round(value * 100,2),
         xlabel = year) %>% 
  filter(
    state == country,
    year <= year_report
  ) %>% 
  select(xlabel, type, mymetric)

data_prep_kep_full <- data_prep_kep %>% 
  right_join(rp_years_df, by = 'xlabel') 

data_prep_scr <- data_raw_scr %>% 
  mutate(type = indicator_type,
         mymetric = round(value*100,2),
         xlabel = year) %>% 
  filter(
    state == country,
    year <= year_report
  ) %>% 
  select(xlabel, type, mymetric)

data_prep_scr_full <- data_prep_scr %>% 
  right_join(rp_years_df, by = 'xlabel') 


data_prep <- data_prep_kep_full %>% 
  rbind(data_prep_scr_full)


# chart parameters ----
c_suffix <- "%"
c_decimals <- 2

### trace parameters
c_colors = c(PRBActualColor, PRBPlannedColor )

###set up order of traces
c_factor <- c("KEP", "SCR")

c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textangle <- -90
c_textposition <- "inside"
c_insidetextanchor <- 'middle'

#### title
c_title_text <-  paste0("KEP & SCR")

#### yaxis
c_yaxis_title <- "KEP & SCR (%)"
c_yaxis_tickformat <- paste0(".",c_decimals, "f")

# plot chart ----
myplot <- mybarchart2(data_prep, 
                      height = myheight+30,
                      colors = c_colors,
                      local_factor = c_factor,
                      
                      textangle = c_textangle,
                      textposition = c_textposition,
                      insidetextanchor = c_insidetextanchor,
                      
                      title_text = c_title_text,

                      yaxis_title = c_yaxis_title,
                      yaxis_ticksuffix = c_suffix,
                      yaxis_tickformat = c_yaxis_tickformat
)

myplot %>% add_empty_trace(data_prep)
