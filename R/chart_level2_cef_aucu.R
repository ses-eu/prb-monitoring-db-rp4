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

# import data ----
data_pre_prep <- aucu(cztype, mycz) %>% 
  arrange(year) 

data_prep <- data_pre_prep |> 
  select(
    year,
    new_duc,
    total_adjustments_aucu,
    aucu
  ) |> 
  mutate(across(.cols = -year, .fns = ~ if_else(year > year_report, NA, .x)))

# chart parameters ----
c_chart_title <- paste0("AUCU")
c_axis_title <- "AUCU (€/SU)"
c_barcolor <- c( PRBPlannedColor, 'transparent', '#BFBFBF', '#9DC3E6')
c_textcolor <- 'black'
c_legend_y_position <- -0.28
c_margin = list (t = 60, b = 80)
  
# define chart function ---
mybarc_aucu <-  function(width = mywidth, height = myheight, 
                         font = myfont, margin = mymargin) {
  mychart <- list()
  data_prep_filtered <- list()
  
  ### loop through years ----
  for (i in 1:nrow(data_prep)) {
     # i=2
    ### prepare data for chart ----
    data_prep_filtered[[i]] <- data_prep %>% 
      slice(i:i) %>% 
      pivot_longer(-c(year), names_to = 'type', values_to = 'metric')  %>% 
      mutate(
        mydatalabel = case_when(
          type == 'total_adjustments_aucu' ~ if_else(is.na(metric) == TRUE, 
                                                  NA, 
                                                  paste0(if_else(metric >= 0, '+', ''),
                                                         trimws(format(round(metric, 2), big.mark = ",", nsmall = 2)))
                                                  ),
          .default = if_else(is.na(metric) == TRUE, NA, format(round(metric, 2), big.mark = ",", nsmall = 2))
          ),
        new_duc = case_when(
            type == 'new_duc' ~ metric,
            .default = NA),
        total_adjustments_aucu = case_when(
            type == 'total_adjustments_aucu' ~ abs(metric),
            .default = NA),      
        aucu = case_when(
            type == 'aucu' ~ metric,
            .default = NA),
        fake_series = case_when(
            type == 'total_adjustments_aucu' ~ min(c(aucu, new_duc), na.rm = TRUE),
            .default = NA)
        ) %>% 
      relocate(fake_series, .after = new_duc) %>% 
      select(-metric) %>% 
      pivot_longer(-c(year, type, mydatalabel), names_to ='subtype', values_to = 'metric') %>% 
      mutate(mydatalabel = case_when(
        subtype == 'fake_series' ~ NA,
        .default = mydatalabel
          ))
  
    ### plot indivicual year charts ----
    mychart[[i]] <- data_prep_filtered[[i]] %>% 
    plot_ly(
      width = width,
      height = height,
      y = ~ round(metric, 2),
      x = ~ factor(type, levels = c('new_duc', 'total_adjustments_aucu',
                                    'aucu')),
      yaxis = "y1",
      type = 'bar',
      color = ~ factor(subtype, levels = c('new_duc', 'fake_series',
                                                     'total_adjustments_aucu',
                                                     'aucu')),
      colors = c_barcolor,
      text = ~ mydatalabel,
      textangle = -90,
      textposition = "outside", 
      cliponaxis = FALSE,
      # insidetextanchor =  "middle",
      # name = mymetric,
      textfont = list(color = c_textcolor, size = font * 0.8),
      # hovertemplate = paste0('%{y} (A-D): %{x:+0,}<extra></extra>'),
      hoverinfo = "none",
      showlegend = FALSE
    ) %>% 
    layout(
      showlegend = F,  
      barmode = "stack",
      bargap = '0',
      xaxis = list(title = '',
                   gridcolor = 'rgb(255,255,255)',
                   showgrid = FALSE,
                   showline = TRUE,
                   showticklabels = FALSE,
                   # dtick = 1,
                   # tickcolor = 'rgb(127,127,127)',
                   # ticks = 'outside',
                   # font = list(size = 1, color = 'transparent'),
                   zeroline = TRUE
      ),
      yaxis = list(zerolinecolor = 'rgb(240,240,240)')
    )
    
  }

  ### group year charts ----
  subplot(mychart[[1]], mychart[[2]], mychart[[3]], mychart[[4]], mychart[[5]],
          titleX = TRUE, shareY = T) %>% 
    config( responsive = TRUE,
            displaylogo = FALSE,
            displayModeBar = F
            # modeBarButtons = list(list("toImage")),
    ) %>% 
    layout(
      font = list(family = "Roboto"),
      title = list(text = c_chart_title,
                   y = mytitle_y, 
                   x = mytitle_x, 
                   xanchor = mytitle_xanchor, 
                   yanchor = mytitle_yanchor,
                   font = list(size = mytitle_font_size)
      ),
      dragmode = FALSE,
      hovermode = "x",
      hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
      # uniformtext = list(minsize = myfont, mode='show'),
      yaxis = list(title = c_axis_title,
                   # gridcolor = 'rgb(255,255,255)',
                   showgrid = TRUE,
                   showline = FALSE,
                   # tickprefix = if_else(" ",
                   # ticksuffix = "% ",
                   tickformat = "0, ",
                   tickcolor='white',     # to increase space between tick and plot
                   ticklen = 7,
                   # showticklabels = TRUE,
                   # ticks = 'outside',
                   zeroline = TRUE,
                   zerolinecolor = 'rgb(240,240,240)',
                   titlefont = list(size = font), tickfont = list(size = font)
      ),
      # legend = list(
      #   orientation = 'h', 
      #   xanchor = "left",
      #   x = -0.1, 
      #   y = -0.5,
      #   font = list(size = myfont*0.9)
      #   ),
      # couldn't get the legend working so I had to resort to this
      annotations = list(
        list (
          xanchor = "center",
          x = 0.1,
          y = -0.15,
          text = rp_min_year,
          font = list(size = font),
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          # arrowhead = 7,
          ax = 0,
          ay = 0
        ),
        list (
          xanchor = "center",
          x = 0.1+0.2,
          y = -0.15,
          text = rp_min_year+1,
          font = list(size = font),
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          # arrowhead = 7,
          ax = 0,
          ay = 0
        ), 
        list (
          xanchor = "center",
          x = 0.50,
          y = -0.15,
          text = rp_min_year+2,
          font = list(size = font),
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          # arrowhead = 7,
          ax = 0,
          ay = 0
        ), 
        list (
          xanchor = "center",
          x = 0.92-0.21,
          y = -0.15,
          text = rp_min_year+3,
          font = list(size = font),
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          # arrowhead = 7,
          ax = 0,
          ay = 0
        ), 
        list (
          xanchor = "center",
          x = 0.92,
          y = -0.15,
          text = rp_min_year+4,
          font = list(size = font),
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          # arrowhead = 7,
          ax = 0,
          ay = 0
        ), 
        list (
          xanchor = "left",
          x = 0.22,
          y = c_legend_y_position,
          text = '■',
          font = list(size = font * 1.2, color = PRBPlannedColor),
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          # arrowhead = 7,
          ax = 0,
          ay = 0
        ), 
        list (
          xanchor = "left",
          x = 0.26,
          y = c_legend_y_position,
          text = 'DUC',
          font = list(size = font),
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          # arrowhead = 7,
          ax = 0,
          ay = 0
        ),
        list (
          xanchor = "left",
          x = 0.35,
          y = c_legend_y_position,
          text = '■',
          font = list(size = font * 1.2, color = '#9DC3E6'),
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          # arrowhead = 7,
          ax = 0,
          ay = 0
        ), 
        list (
          xanchor = "left",
          x = 0.39,
          y = c_legend_y_position,
          text = 'AUCU',
          font = list(size = font),
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          # arrowhead = 7,
          ax = 0,
          ay = 0
        ),
        list (
          xanchor = "left",
          x = 0.50,
          y = c_legend_y_position,
          text = '■',
          font = list(size = font * 1.2, color = '#BFBFBF'),
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          # arrowhead = 7,
          ax = 0,
          ay = 0
        ), 
        list (
          xanchor = "left",
          x = 0.54,
          y = c_legend_y_position,
          text = 'Total adjustments',
          font = list(size = font),
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          # arrowhead = 7,
          ax = 0,
          ay = 0
        )
        ),
      
      margin = margin
      
    )
}
  
# plot chart  ----
mybarc_aucu(mywidth, myheight+40, myfont, c_margin)
