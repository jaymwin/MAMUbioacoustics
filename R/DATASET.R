

# site/station codes from full names --------------------------------------

site_station_codes <-
  readxl::read_excel(here::here('data-raw/site_station_codes.xlsx'))

# this updates the /data folder
usethis::use_data(site_station_codes, overwrite = TRUE)
