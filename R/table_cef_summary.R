if (!data_loaded) {
  source("R/get_data.R")
} 

if (exists("cztype") == FALSE) {cztype = "enroute"}

# import data  ----
## dimension table ----
if(cztype == "terminal"){
  cz_table <- tcz_list_table
  data_raw_cz  <-  ceff_t1_trm %>% filter(entity_type == "TRM")
} else {
  cz_table <- ecz_list_table
  data_raw_cz  <-  ceff_t1_ert %>% filter(entity_type == "ECZ")
}

cz_table <- cz_table %>% 
  select(State = state, 
         cz_id = if_else(cztype == "terminal", "tcz_id", "ecz_id"))

state_table <- state_list |> as_tibble() |> 
  filter(value != "MUAC", 
         value != "Luxembourg", 
         value != rp_full, 
         value != "Network Manager") |> 
  select(State = value) |> 
  left_join(cz_table, by = "State")


# prepare data ----
## SES ----
data_prep_ses <- data_raw_cz %>% 
  group_by(year, status) %>% 
  summarise(
    su = sum(total_su, na.rm = TRUE),
    costs = sum(total_cost_nominal_eur, na.rm = TRUE),
    costs_eur_ref = sum(total_cost_eur_ref, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    entity_name = "Union-wide",
    duc = costs_eur_ref / su
    ) %>% 
  select(entity_name, 
         year, 
         status, 
         su,
         costs,
         costs_eur_ref,
         duc)

## state ----
data_prep_cz <- data_raw_cz |> 
  select(
    entity_name, 
    year,
    status,
    su = total_su,
    costs = total_cost_nominal_eur,
    costs_eur_ref = total_cost_eur_ref,
    duc = duc_eur_ref
  )

data_prep <- data_prep_cz |> 
  rbind(data_prep_ses) |> 
  filter(year == year_report) %>% 
  select(-year)

data_prep_su <- data_prep |> 
  pivot_wider(id_cols = -c(costs, costs_eur_ref, duc), values_from = "su", names_from = "status") |> 
  mutate(
    mymetric = (A/D-1) * 100,
    entity_name = factor(entity_name, levels = entity_name[order(mymetric)]),
    mycolor = if_else(entity_name == "Union-wide", PRBActualColor,  PRBSecondBlue)) |> 
  arrange(entity_name)

data_prep_costs <- data_prep |> 
  pivot_wider(id_cols = -c(su, costs_eur_ref, duc), values_from = "costs", names_from = "status") |> 
  mutate(mymetric = (A/D-1) * 100,
         mycolor = if_else(entity_name == "Union-wide", PRBActualColor,  PRBSecondBlue)) 

data_prep_costs_ref <- data_prep |> 
  pivot_wider(id_cols = -c(su, costs, duc), values_from = "costs_eur_ref", names_from = "status") |> 
  mutate(mymetric = (A/D-1) * 100,
         mycolor = if_else(entity_name == "Union-wide", PRBActualColor,  PRBSecondBlue)) 

data_prep_duc <- data_prep |> 
  pivot_wider(id_cols = -c(su, costs, costs_eur_ref), values_from = "duc", names_from = "status") |> 
  mutate(mymetric = (A/D-1) * 100,
         mycolor = if_else(entity_name == "Union-wide", PRBActualColor,  PRBSecondBlue)) 

# plot function  ----
cef_plot <- function(df, xtitle) {
  plot_ly(
  df,
  x = ~mymetric, 
  y = ~entity_name,
  text = ~paste0(round(mymetric,0), "%"),
  textposition = "outside",
  textfont = list(size = c_minsize),
  type = 'bar',
  cliponaxis = FALSE,
  hoverinfo = "none",
  orientation = 'h', 
  marker = list(color = ~mycolor)
  ) %>%
  layout(
    showlegend = FALSE,
    uniformtext=list(minsize = c_minsize, mode='show'),
    font = list(family = myfont_family),
    dragmode = FALSE,
    xaxis = list(
      title =  list(text = xtitle,
                    font = list(
                      size = c_xtitlefontsize
                      ),
                    standoff = c_standoff # Adjust this value to control spacing
                    ),
      showticklabels = FALSE,
      showgrid = FALSE,
      range = c(floor((min(df$mymetric)-5)/10)*10-5, ceiling((max(df$mymetric)+5)/10)*10+5)
      ), 
    yaxis = list(
      title = '', 
      tickfont = list(size=c_tickfontsize),
      showgrid = FALSE
      )
    ) |> 
  config( responsive = TRUE,
          displaylogo = FALSE,
          displayModeBar = F
          # modeBarButtons = list(list("toImage")),
  )
}

if (knitr::is_latex_output()) {
  c_minsize <- 7
  c_tickfontsize <- c_minsize
  c_xtitlefontsize <- c_minsize +1
  c_standoff <- 0
  c_title1 <- '% difference\nservice units'
  c_title2 <- '% difference total\ncosts nominal €'
  c_title3 <- paste0("% difference total\ncosts €<sub>",cef_ref_year,"</sub>")
  c_title4 <- '% difference\nAUC/DUC'
}else{
  c_minsize <- myminsize
  c_tickfontsize <- myfont*0.93
  c_xtitlefontsize <- myfont
  c_standoff <- NULL
  c_title1<- '% difference service units'
  c_title2 <- '% difference total costs nominal €'
  c_title3 <- paste0("% difference total costs €<sub>",cef_ref_year,"</sub>")
  c_title4 <- '% difference AUC/DUC'
}


p1 <- cef_plot(data_prep_su, c_title1) |> 
  layout(xaxis=list(range = c(floor((min(data_prep_su$mymetric)-5)/10)*10-20, 
                              ceiling((max(data_prep_su$mymetric)+5)/10)*10+10)
))
p2 <- cef_plot(data_prep_costs, c_title2)
p3 <- cef_plot(data_prep_costs_ref, c_title3)
p4 <- cef_plot(data_prep_duc, c_title4)


subplot(p1, p2, p3, p4, nrows = 1, shareY = TRUE, titleX = TRUE) |> 
  layout(
    yaxis = list(tickmode = "linear", dtick = 1)
    )
  
