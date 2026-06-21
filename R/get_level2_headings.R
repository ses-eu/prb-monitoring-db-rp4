if (knitr::is_latex_output()) {
  jump_page <- "\n\n\\Needspace{3\\baselineskip}\n\n"
} else {
  jump_page <- ''
}

if (country == "FABEC") {
  head_suffix <- "the NSAs report:"
} else {
  head_suffix <- "the NSA reports:"
}

# ENV ----
## ERT ----
### NSA ----
env_er_nsa_titles_kea <- c(
  paste0(
    jump_page,
    "Regarding the assessment of the achieved level of actual performance in the environment KPA, ",
    head_suffix
  ),
  paste0(
    jump_page,
    "Regarding the implementation any major operational or structural changes during the calendar year impacting performance in this key performance area, ",
    head_suffix
  ),
  paste0(
    jump_page,
    "Regarding progress on the implementation of the measures committed to in the ERNIP, ",
    head_suffix
  ),
  paste0(
    jump_page,
    "Regarding the identification and analysis by the NSA of the underlying reasons or circumstances having led to the performance target not being achieved, ",
    head_suffix
  ),
  paste0(
    jump_page,
    "Regarding recommendations to the ANSP to rectify the situation, ",
    head_suffix
  ),
  paste0(
    jump_page,
    "Regarding remedial measures that have been / will be taken by the ANSP to rectify the situation, ",
    head_suffix
  ),
  paste0(
    jump_page,
    "Regarding the follow up of the remedial measures indicated in the previous monitoring report(s), ",
    head_suffix
  )
)

env_er_nsa_titles_kep <- c(
  paste0(
    jump_page,
    "Regarding initiatives implemented or planned that will improve this PI and how does the NSA intend on monitoring their effectiveness on performance, ",
    head_suffix
  )
)

env_er_nsa_titles_kes <- c(
  paste0(
    jump_page,
    "Regarding initiatives implemented or planned that will improve this PI and how does the NSA intend on monitoring their effectiveness on performance, ",
    head_suffix
  )
)

env_er_nsa_titles_vfe <- c(
  paste0(
    jump_page,
    "Regarding initiatives implemented or planned that will improve this PI and how does the NSA intend on monitoring their effectiveness on performance, ",
    head_suffix
  )
)

## MIL ----
### PRU ----
#not needed

### NSA ----
# fmt: skip
env_mil_nsa_titles_ersa <- c(
  paste0(jump_page, 
         "Regarding initiatives implemented or planned that will improve this PI and how does the NSA intend on monitoring their effectiveness on performance, ",  head_suffix)
)

env_mil_nsa_titles_rap <- c(
  paste0(
    jump_page,
    "Regarding initiatives implemented or planned that will improve this PI and how does the NSA intend on monitoring their effectiveness on performance, ",
    head_suffix
  )
)

env_mil_nsa_titles_rau <- c(
  paste0(
    jump_page,
    "Regarding initiatives implemented or planned that will improve this PI and how does the NSA intend on monitoring their effectiveness on performance, ",
    head_suffix
  )
)


## TRM ----
### PRU ----
#not needed

### NSA ----
# fmt: skip
env_trm_nsa_titles_axot <- c(
  paste0(jump_page, 
         "Regarding initiatives implemented or planned that will improve this PI and how does the NSA intend on monitoring their effectiveness on performance, ",  head_suffix),
  paste0(jump_page, 
         "Regarding the absence of data at airport level, ",  head_suffix)
)

env_trm_nsa_titles_axit <- c(
  paste0(
    jump_page,
    "Regarding initiatives implemented or planned that will improve this PI and how does the NSA intend on monitoring their effectiveness on performance, ",
    head_suffix
  ),
  paste0(
    jump_page,
    "Regarding the absence of data at airport level, ",
    head_suffix
  )
)

env_trm_nsa_titles_asma <- c(
  paste0(
    jump_page,
    "Regarding initiatives implemented or planned that will improve this PI and how does the NSA intend on monitoring their effectiveness on performance, ",
    head_suffix
  ),
  paste0(
    jump_page,
    "Regarding the absence of data at airport level, ",
    head_suffix
  )
)

env_trm_nsa_titles_vfed <- c(
  paste0(
    jump_page,
    "Regarding initiatives implemented or planned that will improve this PI and how does the NSA intend on monitoring their effectiveness on performance, ",
    head_suffix
  )
)

env_trm_nsa_titles_vfec <- c(
  paste0(
    jump_page,
    "Regarding initiatives implemented or planned that will improve this PI and how does the NSA intend on monitoring their effectiveness on performance, ",
    head_suffix
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
         "Regarding the reasons and circumstances resulting in the achieved level of actual performance of the en route ATFM delay per flight KPI, ",  head_suffix),
  paste0(jump_page, 
         "Regarding the description of the adjustments to the capacity plan included in adopted performance plan, ",  head_suffix),
  paste0(jump_page, 
         "Regarding the identification and analysis by the NSA of the underlying reasons or circumstances having led to the performance target not being achieved, ",  head_suffix),
  paste0(jump_page, 
         "Regarding recommendations to the ANSP to rectify the situation, ",  head_suffix),
  paste0(jump_page, 
         "Regarding remedial measures that have been / will be taken by the ANSP to rectify the situation, ",  head_suffix),
  paste0(jump_page, 
         "Regarding follow up of the remedial measures indicated in the previous monitoring report(s), ",  head_suffix)
  
)

cap_er_nsa_titles_above <- c(
  paste0(
    jump_page,
    "As background information on actual performance, ",
    head_suffix
  )
)

cap_er_nsa_titles_tput <- c(
  paste0(
    jump_page,
    "As background information on actual performance, ",
    head_suffix
  )
)

cap_er_nsa_titles_atco <- c(
  paste0(
    jump_page,
    "As additional comments to ATCOs in the scope of the performance scheme, ",
    head_suffix
  ),
  paste0(""),
  paste0(
    jump_page,
    "Regarding the training process, including details on the effective failure rate and the process used to allocate newly qualified ATCOs between ACC, APP and TWR positions, ",
    head_suffix
  )
)

## TRM ----
### PRU ----
#not needed

### NSA ----
# fmt: skip
cap_trm_nsa_titles_atfm <- c(
  paste0(jump_page, 
         "Regarding the reasons and circumstances resulting in the achieved level of actual performance of the arrival ATFM delay per flight KPI, ",  head_suffix),
  paste0(jump_page, 
         "Regarding the identification and analysis by the NSA of the underlying reasons or circumstances having led to the performance target not being achieved, ",  head_suffix),
  paste0(jump_page, 
         "Regarding recommendations to the ANSP to rectify the situation, ",  head_suffix),
  paste0(jump_page, 
         "Regarding remedial measures that have been / will be taken by the ANSP to rectify the situation, ",  head_suffix),
  paste0(jump_page, 
         "Regarding follow up of the remedial measures indicated in the previous monitoring report(s), ",  head_suffix)
  
)


# fmt: skip
cap_trm_nsa_titles_slot <- c(
  paste0(jump_page, 
         "As background information on actual performance, ",  head_suffix)
)

# fmt: skip
cap_trm_nsa_titles_pddelay <- c(
  paste0(jump_page, 
         "As background information on actual performance, ",  head_suffix),
  paste0(jump_page, 
         "Regarding the absence of data at airport level, ",  head_suffix)
  
)

# fmt: skip
cap_trm_nsa_titles_acdelay <- c(
  paste0(jump_page, 
         "As background information on actual performance, ",  head_suffix),
  paste0(jump_page, 
         "Regarding the absence of data at airport level, ",  head_suffix)
  
)
