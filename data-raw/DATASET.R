

# site/station codes from full names --------------------------------------

site_station_codes <-
  readxl::read_excel(here::here('data-raw/site_station_codes.xlsx'))

site_codes <-
  site_station_codes |>
  dplyr::filter(location_type == 'site') |>
  dplyr::select(site_name = full_name, site_code = code) |>
  tidyr::drop_na()

station_codes <-
  site_station_codes |>
  dplyr::filter(location_type == 'station') |>
  dplyr::select(mamu_station_id = full_name, station_code = code) |>
  tidyr::drop_na()

# this updates the /data folder
usethis::use_data(site_codes, overwrite = TRUE)
usethis::use_data(station_codes, overwrite = TRUE)
