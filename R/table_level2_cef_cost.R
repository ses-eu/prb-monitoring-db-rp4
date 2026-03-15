if (!exists("country") | is.na(country)) {country = "MUAC"
source("R/params_country.R")
}

if (!exists("data_loaded")) {
  source("R/get_data.R")
} 

if (exists("cztype") == FALSE) {cz = c("1", "enroute")}
ez <- as.numeric(cz[[1]])
cztype <- cz[[2]]

# define cz ----
if (country != "MUAC") {
  # cztype <- "terminal"
  mycz <- if_else(cztype == "terminal",
                  tcz_list$tcz_id[ez],
                  ecz_list$ecz_id[ez])
  mycz_name <- if_else(cztype == "terminal",
                       tcz_list$tcz_name[ez],
                       ecz_list$ecz_name[ez])
}

# import data ----
if(cztype == "terminal") {
  data_raw <- ceff_t1_trm
} else {
  data_raw <- ceff_t1_ert
}

if(country == rp_full){
  data_pre_prep  <-  data_raw %>% 
    filter(entity_type == "ECZ" | entity_type == "TCZ") %>% 
    group_by(status, year) %>% 
    summarise(total_cost_eur_ref = sum(total_cost_eur_ref, na.rm = TRUE), .groups = "drop") %>% 
    mutate(entity_code = "SES")%>% 
    select(year, status, total_cost_eur_ref)
  
} else if(country == "MUAC") {
  data_pre_prep <- data_raw |> 
    filter(grepl("MUAC", entity_code)) |> 
    mutate(entity_code = NA) |> 
    group_by(year, status, entity_code) |> 
    summarise(total_cost_eur_ref = sum(total_cost_eur_ref, na.rm = TRUE)) |> 
    ungroup() %>% 
    select(year, status, total_cost_eur_ref)
  
} else {
  
  data_pre_prep <- data_raw %>% 
    filter(entity_code == mycz) %>% 
    select(year, status, total_cost_eur_ref)
}


data_prep<- data_pre_prep %>% 
  mutate(
    mymetric = case_when (
      status == 'A' & year > max(.env$year_report, 2021) ~ NA,
      .default = round(total_cost_eur_ref/10^6, 2)
    ),
    status = str_replace(status, "A", "Actual costs"),
    status = str_replace(status, "D", "Determined costs")
  ) %>%   
  arrange(year, status) %>% 
  select(year, status, mymetric) %>% 
  pivot_wider(values_from = 'mymetric', names_from = 'status') %>% 
  mutate('Difference costs' = .[[2]] - .[[3]]) %>% 
  mutate_at(c(2:4), ~round(.,0)) %>% 
  pivot_longer(-year, names_to = "name", values_to = 'mymetric') %>% 
  pivot_wider(values_from = 'mymetric', names_from = 'year')




# this is not working at the moment
if (knitr::is_latex_output()) {
  mytablefontsize <- "8pt"
  mytitletablefontsize <- "9pt"
}else{
  mytablefontsize <- NULL
  mytitletablefontsize <- NULL
}

# pdf table
data_prep_pdf <- data_prep %>% 
  mutate(
    across(-c(1), ~format(round(.x,0), nsmall =0, big.mark = ","))
  )
  

# plot table ----
table1 <- mygtable(data_prep_pdf, myfont) %>% 
  cols_label(name = html("Total costs - nominal (M€)")) %>% 
  tab_options(column_labels.background.color = "#F2F2F2",
              column_labels.font.weight = 'bold',
              container.padding.y = 0) %>% 
  cols_align(columns = 1, align = "left") %>%
  tab_style(
    style = cell_text(size = mytablefontsize),  # Set font size
    locations = list(
      cells_body(),                    # Apply to the table content
      cells_column_labels()         # Apply to column labels
    )
  )|> 

  tab_header(
    title = md(paste0("**Actual and determined data**"))
  )|> 
  tab_style(
    style = cell_text(size = mytitletablefontsize),  # Set font size to 8pt
    locations = list(
      cells_title(groups = "title")   # Apply to the title
    )
  ) 


if (!knitr::is_latex_output()) {
  table1
}
