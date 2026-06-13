# Setup ----
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
# state_list_download <- c(state_list_download, "FABEC")

# NSA excel files ----
download_nsa_excel_reports <- function(
  country_download
) {
  download_folder <- file.path(
    base_folder,
    year_download,
    "MEMBER STATES MR (I)",
    toupper(country_download),
    if_else(
      country_download == "FABEC",
      "Sent by FABEC",
      "EXCEL SENT BY THE MS"
    )
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

# PRU files ----
pru_kpi <- c(
  "environment-ENR",
  "environment-CIVMIL",
  # "environment-TRM",
  # "capacity-ENR",
  # "capacity-TRM",
  NULL
)

kpi_subfolder_map <- c(
  "environment-ENR" = "env_ert",
  "environment-CIVMIL" = "env_mil",
  "environment-TRM" = "env_trm",
  "capacity-ENR" = "cap_ert",
  "capacity-TRM" = "cap_trm"
)

download_pru_analysis_files <- function(kpi) {
  subfolder <- unname(kpi_subfolder_map[[kpi]])

  download_folder <- file.path(
    base_folder,
    year_download,
    "Word files-input for dashboard",
    kpi
  )

  word_files <- list.files(
    download_folder,
    pattern = "\\.doc[xm]?$",
    full.names = TRUE
  )

  file.copy(
    from = word_files,
    to = file.path(
      input_folder_word,
      subfolder,
      basename(word_files)
    ),
    overwrite = TRUE
  )
}

purrr::walk(pru_kpi, download_pru_analysis_files)
