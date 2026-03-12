if (!data_loaded) {
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
  data_prep <- regulatory_result(cztype, mycz)
  data_prep <- data_prep %>% 
    mutate(mymetric = regulatory_result / 1000) %>% 
    mutate(across(.cols = -c(year, type), ~if_else(year>year_report, NA, .x))
    ) |> 
    rename(xlabel = year) 


    
# chart parameters ----
c_suffix <- ""
c_decimals <- 1

### trace parameters
c_colors = c( PRBPlannedColor, PRBActualColor, '#BFBFBF')
###set up order of traces
c_factor <- c("Main ANSP",
              "Other ANSP",
              "MET")
c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textangle <- -90
c_textposition <- "inside"
c_insidetextanchor <- "middle"
c_textfont_color <- 'transparent'

### layout parameters
c_barmode <- 'group'

#### title
c_title_text <- paste0("RR by entity group")

#### yaxis
c_yaxis_title <- "RR (M€)"
c_yaxis_tickformat <- ",.1f"


# plot chart  ----
p1 <- mybarchart2(data_prep, 
                  height = myheight+10,
                  colors = c_colors,
                  local_factor = c_factor,
                  suffix = c_suffix,
                  decimals = c_decimals,
                  barmode = c_barmode,
                  
                  hovertemplate = c_hovertemplate,
                  
                  textangle = c_textangle,
                  textposition = c_textposition, 
                  textfont_color = c_textfont_color,
                  insidetextanchor = c_insidetextanchor,
                  
                  title_text = c_title_text,
                  
                  yaxis_title = c_yaxis_title,
                  yaxis_ticksuffix = c_suffix,
                  yaxis_tickformat = c_yaxis_tickformat
) 

p1 %>% 
  add_empty_trace(., data_prep)  
