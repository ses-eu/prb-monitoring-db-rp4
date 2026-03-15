if (!exists("data_loaded")) {
  source("R/get_data.R")
} 

if (!exists("country") | is.na(country)) {country <- rp_full
source("R/params_country.R")
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

# import data  ----
data_raw  <-  regulatory_result(cztype, mycz)
  
# pre-prepare data ----
data_pre_prep <- data_raw %>% 
  filter(type == "Main ANSP",
         year == year_report
         )  %>% 
  select(year, atsp_gain_loss_cost_sharing, trs, financial_incentive, ex_post_roe) 

# pre-prepare data ----
data_prep <- data_pre_prep |> 
  pivot_longer(-year, names_to = "status", values_to = "mymetric") %>% 
  mutate (
    mymetric = mymetric/1000,
    ylabel = case_when (
      status == 'atsp_gain_loss_cost_sharing' ~ "Cost sharing", 
      status == 'trs' ~ "Traffic risk sharing",
      status == 'financial_incentive' ~ "Incentives",
      status == 'ex_post_roe' ~ "Actual RoE in value"),
    mylabel = format(round(mymetric,1), big.mark = ",", nsmall =1)
  )


# chart parameters ----
if (knitr::is_latex_output()) {
  c_height <- myheight
  
}else{
  c_height <- myheight+30
}

c_title_text <- paste0("Net result from ", 
                        if_else(cztype == 'terminal', 'terminal', 'en route'),
                        " activity - ", 
                        if_else(country == rp_full, "Main ANSPs ", paste0(main_ansp," ")),
                        year_report)
                        
c_xaxis_title <- ""
c_barcolor_pos <- '#9ECF8D'
c_barcolor_neg <- '#F87474'
c_textcolor <- 'black'
c_hovertemplate <- paste0('%{x:,.1f}<extra></extra>')
c_xaxis_tickformat <- "0,.1f"

###set up order of traces
c_factor <- c("Actual RoE in value",
              "Incentives",
              "Traffic risk sharing",
              "Cost sharing")

c_margin <- list (t = 30, b = 70)
c_decimals <- 1

# plot chart  ----

### calculate x range, and annotation and image position
myroundup <- max(floor((log10(abs(max(data_prep$mymetric, na.rm = TRUE))))), floor((log10(abs(min(data_prep$mymetric, na.rm = TRUE))))))
range_min <- floor(min(data_prep$mymetric, na.rm = TRUE)/10^myroundup) * 10^myroundup - 10^myroundup/1.2
range_min <- if_else(range_min >0, 0, range_min)
range_max <- ceiling(max(data_prep$mymetric, na.rm = TRUE)/10^myroundup) * 10^myroundup + 10^myroundup/2

### plot chart and add annotations
myhbarc2(data_prep,
         height = c_height,
         decimals = c_decimals,
         suffix = c_suffix,
         local_factor = c_factor,
         
         mybarcolor_pos = c_barcolor_pos,
         mybarcolor_neg = c_barcolor_neg,
         
         hovertemplate = c_hovertemplate,
         
         title_text = c_title_text,
         title_y = 0.5,
         title_x = 0.99,
         
         textangle = 0,
         textfont_color = "black",
         
         xaxis_title = c_xaxis_title,
         xaxis_tickformat = c_xaxis_tickformat,
         
         margin = c_margin
         
) %>%  
  layout(
    uniformtext=list(minsize = 14, mode='show'),
    xaxis = list(
      title = "",
      range = c(range_min, range_max)
      ),
    images = list(
      list(
        # Add images
        source =  base64enc::dataURI(file = here("images","arrow_right.png")),  
        xref = "paper",  
        yref = "paper",  
        x = if_else(range_min >=0,0, abs(range_min)/(range_max-range_min)),  
        y = -0.20,  
        sizex = 0.18,  
        sizey = 0.18,  
        layer = "above",
        xanchor="left",  
        yanchor="bottom" 
      ),
      list(
        # Add images
        source =  base64enc::dataURI(file = here("images","arrow_left.png")),  
        xref = "paper",  
        yref = "paper",  
        x = if_else(range_min >=0,0, abs(range_min)/(range_max-range_min)),  
        y = -0.20,  
        sizex = 0.18,  
        sizey = 0.18,  
        layer = "above",
        xanchor="right",  
        yanchor="bottom" 
      )
      )
    ,
    annotations = list(
      list(
      xanchor = "left",
      x = if_else(range_min >=0,0, abs(range_min)/(range_max-range_min)) + 0.05,
      y = -0.28,
      text = '<b>ANSP gain</b>',
      font = list(size = myfont*0.85),
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      # arrowhead = 7,
      ax = 0,
      ay = 0
    ),
    list(
      xanchor = "right",
      x = if_else(range_min >=0,0, abs(range_min)/(range_max-range_min)) -0.05,
      y = -0.28,
      text = '<b>ANSP loss</b>',
      font = list(size = myfont*0.85),
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      # arrowhead = 7,
      ax = 0,
      ay = 0
    ),
    list(
      xanchor = "center",
      x = 0.5,
      y = -0.17,
      text = 'M€',
      font = list(size = myfont),
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      # arrowhead = 7,
      ax = 0,
      ay = 0
    )
    )
  )



