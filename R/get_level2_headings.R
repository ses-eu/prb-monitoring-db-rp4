if (knitr::is_latex_output()) {
  jump_page <- "\n\n\\Needspace{3\\baselineskip}\n\n"
} else {
  jump_page <- ''
}

# ENV ----
## ERT ----
### NSA ----
env_er_nsa_titles_kea <- c(
  paste0(
    jump_page,
    "Regarding the assessment of the achieved level of actual performance in the environment KPA, the NSA reports:"
  ),
  paste0(
    jump_page,
    "Regarding the implementation any major operational or structural changes during the calendar year impacting performance in this key performance area, the NSA reports:"
  ),
  paste0(
    jump_page,
    "Regarding progress on the implementation of the measures committed to in the ERNIP, the NSA reports:"
  ),
  paste0(
    jump_page,
    "Regarding the identification and analysis by the NSA of the underlying reasons or circumstances having led to the performance target not being achieved, the NSA reports:"
  ),
  paste0(
    jump_page,
    "Regarding recommendations to the ANSP to rectify the situation, the NSA reports:"
  ),
  paste0(
    jump_page,
    "Regarding remedial measures that have been / will be taken by the ANSP to rectify the situation, the NSA reports:"
  ),
  paste0(
    jump_page,
    "Regarding the follow up of the remedial measures indicated in the previous monitoring report(s), the NSA reports:"
  )
)

env_er_nsa_titles_kep <- c(
  paste0(
    jump_page,
    "Regarding initiatives implemented or planned that will improve this PI and how does the NSA intend on monitoring their effectiveness on performance, the NSA reports:"
  )
)

env_er_nsa_titles_kes <- c(
  paste0(
    jump_page,
    "Regarding initiatives implemented or planned that will improve this PI and how does the NSA intend on monitoring their effectiveness on performance, the NSA reports:"
  )
)

env_er_nsa_titles_vfe <- c(
  paste0(
    jump_page,
    "Regarding initiatives implemented or planned that will improve this PI and how does the NSA intend on monitoring their effectiveness on performance, the NSA reports:"
  )
)

## MIL ----
### PRU ----
#not needed

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
#not needed

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

#not needed

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
#not needed

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
