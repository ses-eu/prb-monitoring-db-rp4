# main state params ----
params_table <- read_mytable(lists_data_file, "lists", "Table_States") %>% clean_names()

state_list <- params_table %>% select(state) %>% unlist()

aua_entities_table <- read_mytable(lists_data_file, "lists", "Table_AUA") %>% clean_names()

acc_list_table <- read_mytable(lists_data_file, "lists", "Table_ACCs") %>% clean_names()

ecz_list_table <- read_mytable(lists_data_file, "lists", "Table_ECZ") %>%
  left_join(
    read_mytable(lists_data_file, "lists", "Table_forecast"),
    by = "forecast_id"
    ) %>% clean_names()

tcz_list_table <- read_mytable(lists_data_file, "lists", "Table_TCZ") %>% clean_names()

context_data_table <- read_mytable(context_data_file, "context", "Table_context") %>%  clean_names()

other_orgs_table <- read_mytable(lists_data_file, "lists", "Table_PP_other_ANSPs") %>%  clean_names()

saf_ansp_table <- read_mytable(lists_data_file, "lists", "Table_SAF_ANSP") %>% clean_names()

airports_table <- read_mytable(lists_data_file, "lists", "Table_tcz_apt") %>%  clean_names()

