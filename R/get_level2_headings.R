jump_page <- "\n\n\\Needspace{3\\baselineskip}\n\n"

# ENV ----
## MIL ----
# fmt: skip
env_mil_titles_s1 <- c(
  paste0(jump_page, 
         "**Effective use of reserved or segregated local airspace (ERSA)**")
)

# fmt: skip
env_mil_titles_s2 <- c(
  paste0(jump_page, 
         "**Rate of planning via available airspace structures**")
)

# fmt: skip
env_mil_titles_s3 <- c(
  paste0(jump_page, 
         "**Rate of using available airspace structures**")
)

## TRM ----
# fmt: skip
env_trm_titles_s1 <- c(
  paste0(jump_page, 
         "**Additional time in taxi-out phase (AXOT)**")
)

# fmt: skip
env_trm_titles_s2 <- c(
  paste0(jump_page, 
         "**Additional time in taxi-in phase (AXIT)**")
)

# fmt: skip
env_trm_titles_s3 <- c(
  paste0(jump_page, 
         "**Additional time in terminal airspace (ASMA)**")
)

# fmt: skip
env_trm_titles_s4 <- c(
  paste0(jump_page, 
         "**Vertical flight efficiency of the descent**")
)

# fmt: skip
env_trm_titles_s5 <- c(
  paste0(jump_page, 
         "**Vertical flight efficiency of the climb**")
)

# CAP ----
## ERT ----
# fmt: skip
cap_ert_titles_s1 <- c(
  paste0(jump_page, 
         "**En route ATFM delay**")
)

# fmt: skip
cap_ert_titles_s2 <- c(
  paste0(jump_page, 
         "**En route capacity incentive scheme**")
)

# fmt: skip
cap_ert_titles_s3 <- c(
  paste0(jump_page, 
         "**Percentage of total en route ATFM delay that occurred on days when the daily throughput was above the expected daily traffic (ACC)**")
)

# fmt: skip
cap_ert_titles_s4 <- c(
  paste0(jump_page, 
         "**Annual weighted average of the daily peak throughput (ACC)**")
)

# fmt: skip
cap_ert_titles_s5 <- c(
  paste0(jump_page, 
         "**ATCOs in operations**")
)

# fmt: skip
cap_ert_titles_s6 <- c(
  paste0(jump_page, 
         "**Sector opening hours**")
)

## TRM ----
# fmt: skip
cap_trm_titles_s1 <- c(
  paste0(jump_page, 
         "**Terminal and airport ANS ATFM arrival delay per flight**")
)
# fmt: skip
cap_trm_titles_s2 <- c(
  paste0(jump_page, 
         "**Terminal capacity incentive scheme**")
)
# fmt: skip
cap_trm_titles_s3 <- c(
  paste0(jump_page, 
         "**Adherence to ATFM slots**")
)
# fmt: skip
cap_trm_titles_s4 <- c(
  paste0(jump_page, 
         "**Air traffic control pre-departure delay**")
)
# fmt: skip
cap_trm_titles_s5 <- c(
  paste0(jump_page, 
         "**Average departure delay from all causes per flight**")
)

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
         "**", cztype_proper, " costs for the main ANSP (", main_ansp, ") at charging zone level**"),
  paste0(jump_page,
         "**Assessment of the actual performance in the charging zone reported by the NSA**"),
  paste0(jump_page, 
         "**Explanation of the differences between actual and determined costs at charging zone level reported by the NSA**"),
  paste0(jump_page, 
         "**Recommendations formulated by the NSA to the ANSP (", main_ansp, ") to rectify the situations and actions taken by the ANSP**")
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
         "**", main_ansp, " net gain/loss on activity in the ", czname, 
         " ", tolower(cztype_proper), " charging zone in ", year_report, "**"),
  paste0(jump_page, 
         "**", main_ansp, " overall regulatory result (RR) for the ", tolower(cztype_proper), " activity**")
)
