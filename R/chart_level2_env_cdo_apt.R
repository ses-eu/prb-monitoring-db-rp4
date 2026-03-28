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
c_textangle <- -90
c_textfont_size <- mytextfont_size -1.5

#### title
c_title_text <- paste0("Average duration climb/descent, main airport(s) - ", year_report)
c_title_y <- mytitle_y
c_title_font_size <- mytitle_font_size

#### xaxis
c_xaxis_tickfont_size <- myxaxis_tickfont_size

#### yaxis
c_yaxis_title <- "Average duration (seconds)"
c_yaxis_tickformat <- paste0(".",c_decimals, "f")
c_yaxis_tickfont_size <- myyaxis_tickfont_size
c_yaxis_titlefont_size <- myyaxis_titlefont_size

### legend
c_legend_x = -0.1
c_legend_y = 1.38
c_legend_xanchor = "left"
c_legend_font_size <- mylegend_font_size

### margin 
c_margin = list(t = 70)

if (knitr::is_latex_output()) {
  pdf_ratio <- 5/6
  c_title_font_size <- c_title_font_size * pdf_ratio
  c_textfont_size <- c_textfont_size * pdf_ratio
  
  c_xaxis_tickfont_size <- c_xaxis_tickfont_size * pdf_ratio
  
  c_yaxis_tickfont_size <- c_yaxis_tickfont_size * pdf_ratio
  c_yaxis_titlefont_size <- c_yaxis_titlefont_size * pdf_ratio
  
  c_legend_font_size <- c_legend_font_size * pdf_ratio
  }


## plot chart  ----
myplot <- mybarchart2(data_prep, 
                      height = myheight + 50,
                      colors = c_colors,
                      local_factor = c_factor,
                      suffix = c_suffix,
                      decimals = c_decimals,
                      hovertemplate = c_hovertemplate,
                      
                      textangle = c_textangle,
                      textposition = c_textposition,
                      insidetextanchor = c_insidetextanchor,
                      textfont_size = c_textfont_size,
                      
                      title_text = c_title_text,
                      title_y = c_title_y,
                      title_font_size = c_title_font_size,
                      
                      xaxis_tickangle = -90,
                      xaxis_tickfont_size = c_xaxis_tickfont_size,
                      
                      yaxis_title = c_yaxis_title,
                      yaxis_ticksuffix = c_suffix,
                      yaxis_tickformat = c_yaxis_tickformat,
                      yaxis_tickfont_size = c_yaxis_tickfont_size,
                      yaxis_titlefont_size = c_yaxis_titlefont_size,
                      
                      ### legend
                      legend_x = c_legend_x,
                      legend_y = c_legend_y,
                      legend_xanchor = c_legend_xanchor,
                      legend_fontsize = c_legend_font_size,
                      
                      ### margin 
                      margin = c_margin
                      
)

myplot 

