# import data ----
data_raw <- cap_pis_nm

data_prep <- data_raw %>%
  select(year, pi, value) |>
  pivot_wider(names_from = "year", values_from = "value") |>
  arrange(pi) |>
  mutate(
    pi = case_when(
      pi ==
        'cap_pi1' ~ 'Average daily number of ATM regulatons (producing >200 min) (PI#1)',
      pi == 'cap_pi2' ~ 'Average en route ATFM weekend delay (PI#2)',
      pi ==
        'cap_pi3' ~ 'Annual percentage of all first rotations ATFM delaus with most potential delay reduction (PI#3)',
    )
  )

# this is not working at the moment
if (knitr::is_latex_output()) {
  mytablefontsize <- "8pt"
  mytitletablefontsize <- "9pt"
} else {
  mytablefontsize <- NULL
  mytitletablefontsize <- NULL
}

# pdf table
data_prep_pdf <- data_prep %>%
  mutate(
    across(
      -c(1),
      ~ format(janitor::round_half_up(.x, 1), nsmall = 1, big.mark = ",")
    )
  )


# plot table ----
table1 <- mygtable(data_prep_pdf, myfont) %>%
  cols_label(pi = html("")) %>%
  tab_options(
    column_labels.background.color = "#F2F2F2",
    column_labels.font.weight = 'bold',
    container.padding.y = 0
  ) %>%
  cols_align(columns = 1, align = "left") %>%
  tab_style(
    style = cell_text(size = mytablefontsize), # Set font size
    locations = list(
      cells_body(), # Apply to the table content
      cells_column_labels() # Apply to column labels
    )
  ) |>

  tab_style(
    style = cell_text(size = mytitletablefontsize), # Set font size to 8pt
    locations = list(
      cells_title(groups = "title") # Apply to the title
    )
  )


if (!knitr::is_latex_output()) {
  table1
}
