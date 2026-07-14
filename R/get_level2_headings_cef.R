if (knitr::is_latex_output()) {
  jump_page <- "\n\n\\Needspace{3\\baselineskip}\n\n"
} else {
  jump_page <- ''
}

# CEF ----
# fmt: skip
cef_titles_s1 <- c(
  paste0(jump_page, 
         "**AUC vs. DUC**"),
  paste0(jump_page, 
         "**", cztype_proper, " service units**"),
  paste0(jump_page, 
         "**", cztype_proper, " costs by entity**"),
  paste0(jump_page,
         "**", cztype_proper, " costs for the main ANSP (", if_else(country == 'Luxembourg' & cztype == 'enroute', 'skeyes', main_ansp), ") at charging zone level**"),
  paste0(jump_page,
         "**Assessment of the actual performance in the charging zone reported by the NSA**"),
  paste0(jump_page, 
         "**Explanation of the differences between actual and determined costs at charging zone level reported by the NSA**"),
  paste0(jump_page, 
         "**Recommendations formulated by the NSA to the ANSP (", if_else(country == 'Luxembourg' & cztype == 'enroute', 'skeyes', main_ansp), ") to rectify the situation and actions taken by the ANSP**")
)

# fmt: skip
cef_titles_s2 <- c(
  paste0(jump_page, 
         "**", cztype_proper, " AUCU monitoring at charging zone level**"),
  paste0(jump_page, 
         "**Initiatives implemented or planned that will improve this PI reported by the NSA**")
)

# fmt: skip
cef_titles_s3 <- c(
  paste0(jump_page, 
         "**", if_else(country == 'Luxembourg' & cztype == 'enroute', 'skeyes', main_ansp), " net gain/loss on activity in the ", czname, 
         " ", tolower(cztype_proper), " charging zone in ", year_report, "**"),
  paste0(jump_page, 
         "**", if_else(country == 'Luxembourg' & cztype == 'enroute', 'skeyes', main_ansp), " overall regulatory result (RR) for the ", tolower(cztype_proper), " activity**")
)
