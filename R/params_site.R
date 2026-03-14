# source data folders ----
# data_folder <- 'G:/HQ/dgof-pru/Data/SES Monitoring Dashboard/RP4/data_for_web/'
data_folder_a2 <- paste0(data_folder, "monitoring_files/", year_report, "/")

# set site parameters ----
site_dir <- here("_site")
root_dir <- if_else(test_check == TRUE, 
                    '//ihx-vdm05/LIVE_var_www_performance$/prb-monitoring/rp4/test/',
                    '//ihx-vdm05/LIVE_var_www_performance$/prb-monitoring/rp4/prod/'
                    )
destination_dir <- paste0(root_dir, year_folder, '/')
home_address <- if_else(test_check == TRUE, 
                        'https://www.eurocontrol.int/performance/prb-monitoring/rp4/test',
                        'https://www.sesperformance.eu'
                        )
external_address <- if_else(test_check == TRUE,
                            str_replace(destination_dir,
                                        fixed('//ihx-vdm05/LIVE_var_www_performance$'),
                                        'https://www.eurocontrol.int/performance'),
                            str_replace(destination_dir,
                                        fixed(root_dir),
                                        paste0(home_address, '/'))
                                )

destination_dir_investments <- paste0(root_dir, "investments/", "rp4/" )
  
