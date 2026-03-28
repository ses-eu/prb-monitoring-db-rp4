## import data  ----
if (!exists("data_loaded")) {
  source("R/get_data.R")
}

data_raw <- cdo_cco_actual

## prepare data ----
airports_country <- airports_table %>% 
  filter(country_name == .env$country) 

data_filtered <- data_raw %>% 
  filter(
    year == .env$year_report
    ) %>% 
  right_join(
    airports_country, by = c("apt_icao" = "apt_code")
  )

### We take the top 15 airports for France
if (country == "France") {
  data_filtered <- data_filtered |> 
    filter(is.na(nbr_flights_descent) == FALSE) |> 
    arrange(desc(nbr_flights_descent)) |> 
    slice_head(n = 15)
}

data_prep <- data_filtered %>% 
  select(
    year, apt_name, avg_seconds_per_descent, avg_seconds_per_climb
  ) %>% 
  pivot_longer(
    cols = c(-year, -apt_name),
    names_to = "type",
    values_to = "mymetric"
  ) %>% 
  mutate(
    xlabel = apt_name,
    type = case_when(
      type == "avg_seconds_per_descent" ~ "Avg. duration in descent",
      type == "avg_seconds_per_climb" ~ "Avg. duration in climb"
    ),
    mymetric = round(mymetric, 1)
  ) %>%  
  select(
    xlabel,
    type,
    mymetric)

## chart parameters ----
c_suffix <- ""
c_decimals <- 1

### trace parameters
c_colors = c(PRBSecondBlue, PRBPlannedColor)

###set up order of traces
c_factor <- data_prep %>% distinct(type) %>% pull(type)

c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textposition <- "outside"
c_insidetextanchor <- NA
c_textfont_color <- 'black'

#### title
c_title_text <- paste0("Average duration climb/descent,\nmain airport(s) - ", year_report)
c_title_y <- 0.95

#### yaxis
c_yaxis_title <- "Average duration (seconds)"
c_yaxis_tickformat <- paste0(".",c_decimals, "f")

### legend
c_legend_x = -0.1
c_legend_y = 1.3
c_legend_xanchor = "left"

### margin 
c_margin = list(t = 70)


## plot chart  ----
myplot <- mybarchart2(data_prep, 
                      height = myheight + 40,
                      colors = c_colors,
                      local_factor = c_factor,
                      suffix = c_suffix,
                      decimals = c_decimals,
                      hovertemplate = c_hovertemplate,
                      
                      textangle = c_textangle,
                      textposition = c_textposition,
                      insidetextanchor = c_insidetextanchor,
                      
                      title_text = c_title_text,
                      title_y = c_title_y,
                      
                      yaxis_title = c_yaxis_title,
                      yaxis_ticksuffix = c_suffix,
                      yaxis_tickformat = c_yaxis_tickformat,
                      
                      ### legend
                      legend_x = c_legend_x,
                      legend_y = c_legend_y,
                      legend_xanchor = c_legend_xanchor,
                      
                      
                      ### margin 
                      margin = c_margin
                      
)

myplot 

