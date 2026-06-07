if (!exists("substrRight")) {
  source("R/utils.R")
}
if (!exists("rp")) {
  source("R/params_project.R")
}
if (!exists("params_table")) {
  source("R/get_lists.R")
}

year_download <- "2025"
root_input_folder <- "G:/HQ/dgof-pru/Data/SES Monitoring Dashboard/RP4/data_for_web/monitoring_files/"

input_folder_word <- paste0(
  root_input_folder,
  year_download,
  "/pru_analysis/"
)

input_folder_excel <- paste0(
  root_input_folder,
  year_download,
  "/nsa_monitoring_reports"
)


base_folder <- "C:/Users/oaolive/OneDrive - EUROCONTROL/ECTL - AIU - European Commission - Monitoring"

state_list_download <- unname(state_list)[
  !unname(state_list) %in% c("MUAC", "Network Manager", "SES RP4")
]
state_list_download <- c(state_list_download, "FABEC")

# country_download <- "Austria"
year_download <- "2025"

download_nsa_excel_reports <- function(
  country_download
) {
  download_folder <- file.path(
    base_folder,
    year_download,
    "MEMBER STATES MR (I)",
    toupper(country_download),
    "EXCEL SENT BY THE MS"
  )

  excel_files <- list.files(
    download_folder,
    pattern = "\\.xls[xm]?$",
    full.names = TRUE
  )

  file.copy(
    from = excel_files,
    to = file.path(
      input_folder_excel,
      paste0(country_download, "_", year_download, "_AMR.xlsx")
    ),
    overwrite = TRUE
  )
}

purrr::walk(state_list_download, download_nsa_excel_reports)
