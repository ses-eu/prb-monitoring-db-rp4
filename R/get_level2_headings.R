if (knitr::is_latex_output()) {
  jump_page <- "\n\n\\Needspace{3\\baselineskip}\n\n"
} else {
  jump_page <- ''
}

# ENV ----
## MIL ----
### PRU ----
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

### NSA ----
# fmt: skip
env_mil_nsa_titles_ersa <- c(
  paste0(jump_page, 
         "Regarding initiatives implemented or planned that will improve this PI and how does the NSA intend on monitoring their effectiveness on performance, the NSA reports:")
)

env_mil_nsa_titles_rap <- c(
  paste0(
    jump_page,
    "Regarding initiatives implemented or planned that will improve this PI and how does the NSA intend on monitoring their effectiveness on performance, the NSA reports:"
  )
)

env_mil_nsa_titles_rau <- c(
  paste0(
    jump_page,
    "Regarding initiatives implemented or planned that will improve this PI and how does the NSA intend on monitoring their effectiveness on performance, the NSA reports:"
  )
)


## TRM ----
### PRU ----
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

### NSA ----
# fmt: skip
env_trm_nsa_titles_axot <- c(
  paste0(jump_page, 
         "Regarding initiatives implemented or planned that will improve this PI and how does the NSA intend on monitoring their effectiveness on performance, the NSA reports:"),
  paste0(jump_page, 
         "Regarding the absence of data at airport level, the NSA reports:")
)

env_trm_nsa_titles_axit <- c(
  paste0(
    jump_page,
    "Regarding initiatives implemented or planned that will improve this PI and how does the NSA intend on monitoring their effectiveness on performance, the NSA reports:"
  ),
  paste0(
    jump_page,
    "Regarding the absence of data at airport level, the NSA reports:"
  )
)

env_trm_nsa_titles_asma <- c(
  paste0(
    jump_page,
    "Regarding initiatives implemented or planned that will improve this PI and how does the NSA intend on monitoring their effectiveness on performance, the NSA reports:"
  ),
  paste0(
    jump_page,
    "Regarding the absence of data at airport level, the NSA reports:"
  )
)

env_trm_nsa_titles_vfed <- c(
  paste0(
    jump_page,
    "Regarding initiatives implemented or planned that will improve this PI and how does the NSA intend on monitoring their effectiveness on performance, the NSA reports:"
  )
)

env_trm_nsa_titles_vfec <- c(
  paste0(
    jump_page,
    "Regarding initiatives implemented or planned that will improve this PI and how does the NSA intend on monitoring their effectiveness on performance, the NSA reports:"
  )
)


# CAP ----
## ERT ----
### PRU ----
# fmt: skip
cap_er_titles_s1 <- c(
  paste0(jump_page, 
         "**En route ATFM delay**")
)

# fmt: skip
cap_er_titles_s2 <- c(
  paste0(jump_page, 
         "**En route capacity incentive scheme**")
)

# fmt: skip
cap_er_titles_s3 <- c(
  paste0(jump_page, 
         "**Percentage of total en route ATFM delay that occurred on days when the daily throughput was above the expected daily traffic (ACC)**")
)

# fmt: skip
cap_er_titles_s4 <- c(
  paste0(jump_page, 
         "**Annual weighted average of the daily peak throughput (ACC)**")
)

# fmt: skip
cap_er_titles_s5 <- c(
  paste0(jump_page, 
         "**ATCOs in operations**")
)

# fmt: skip
cap_er_titles_s6 <- c(
  paste0(jump_page, 
         "**Sector opening hours**")
)


### NSA ----
# fmt: skip
cap_er_nsa_titles_atfm <- c(
  paste0(jump_page, 
         "Regarding the reasons and circumstances resulting in the achieved level of actual performance of the en route ATFM delay per flight KPI, the NSA reports:"),
  paste0(jump_page, 
         "Regarding the description of the adjustments to the capacity plan included in adopted performance plan, the NSA reports:"),
  paste0(jump_page, 
         "Regarding the identification and analysis by the NSA of the underlying reasons or circumstances having led to the performance target not being achieved, the NSA reports:"),
  paste0(jump_page, 
         "Regarding recommendations to the ANSP to rectify the situation, the NSA reports:"),
  paste0(jump_page, 
         "Regarding remedial measures that have been / will be taken by the ANSP to rectify the situation, the NSA reports:"),
  paste0(jump_page, 
         "Regarding follow up of the remedial measures indicated in the previous monitoring report(s), the NSA reports:")
  
)

cap_er_nsa_titles_above <- c(
  paste0(
    jump_page,
    "As background information on actual performance, the NSA reports:"
  )
)

cap_er_nsa_titles_tput <- c(
  paste0(
    jump_page,
    "As background information on actual performance, the NSA reports:"
  )
)

cap_er_nsa_titles_atco <- c(
  paste0(
    jump_page,
    "As additional comments to ATCOs in the scope of the performance scheme, the NSA reports:"
  ),
  paste0(
    jump_page,
    "Regarding the training process, including details on the effective failure rate and the process used to allocate newly qualified ATCOs between ACC, APP and TWR positions, the NSA reports:"
  )
)

## TRM ----
### PRU ----
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

### NSA ----
# fmt: skip
cap_trm_nsa_titles_atfm <- c(
  paste0(jump_page, 
         "Regarding the reasons and circumstances resulting in the achieved level of actual performance of the arrival ATFM delay per flight KPI, the NSA reports:"),
  paste0(jump_page, 
         "Regarding the identification and analysis by the NSA of the underlying reasons or circumstances having led to the performance target not being achieved, the NSA reports:"),
  paste0(jump_page, 
         "Regarding recommendations to the ANSP to rectify the situation, the NSA reports:"),
  paste0(jump_page, 
         "Regarding remedial measures that have been / will be taken by the ANSP to rectify the situation, the NSA reports:"),
  paste0(jump_page, 
         "Regarding follow up of the remedial measures indicated in the previous monitoring report(s), the NSA reports:")
  
)


# fmt: skip
cap_trm_nsa_titles_slot <- c(
  paste0(jump_page, 
         "As background information on actual performance, the NSA reports:")
)

# fmt: skip
cap_trm_nsa_titles_pddelay <- c(
  paste0(jump_page, 
         "As background information on actual performance, the NSA reports:"),
  paste0(jump_page, 
         "Regarding the absence of data at airport level, the NSA reports:")
  
)

# fmt: skip
cap_trm_nsa_titles_acdelay <- c(
  paste0(jump_page, 
         "As background information on actual performance, the NSA reports:"),
  paste0(jump_page, 
         "Regarding the absence of data at airport level, the NSA reports:")
  
)
