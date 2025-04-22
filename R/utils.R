
# temporary folders to hold wavs on desktop or other local folder
create_temp_folders <- function(wav_file, input_dir, temp_dir) {

  stringr::str_replace(
    stringr::str_remove(stringr::str_replace(wav_file, "\\(-\\d{4}\\)", ""), basename(stringr::str_replace(wav_file, "\\(-\\d{4}\\)", ""))),
    input_dir,
    temp_dir
  ) |>
    fs::dir_create()

}

# create flac folders on an external hard drive
create_flac_folders <- function(date, output_dir, site_name, visit_id, cell_id, station_id) {

  fs::dir_create(
    path = stringr::str_glue('{output_dir}/{site_name}_{visit_id}/{site_name}_{visit_id}_{cell_id}_{station_id}/{site_name}_{visit_id}_{cell_id}_{station_id}_{date}')
  )

}


# function to handle the file conversion process
convert_to_flac <- function(wav_file, input_dir, temp_dir, output_dir, site_name, visit_id, cell_id, station_id, swift_id) {

  temp_path <-
    stringr::str_c(stringr::str_replace(stringr::str_remove(stringr::str_replace(wav_file, "\\(-\\d{4}\\)", ""), basename(stringr::str_replace(wav_file, "\\(-\\d{4}\\)", ""))), input_dir, temp_dir), basename(stringr::str_replace(wav_file, "\\(-\\d{4}\\)", "")))

  fs::file_copy(
    path = wav_file,
    new_path = temp_path,
    overwrite = TRUE
  )

  date <- stringr::str_extract(wav_file, '[0-9]{8}')

  # Define the output flac file path
  output_file <-
    file.path(
      stringr::str_glue(
        '{output_dir}/{site_name}_{visit_id}/{site_name}_{visit_id}_{cell_id}_{station_id}/{site_name}_{visit_id}_{cell_id}_{station_id}_{date}/{site_name}_{visit_id}_{cell_id}_{station_id}{stringr::str_replace(stringr::str_remove(basename(temp_path), swift_id), "wav", "flac")}'
      )
    )

  # Use sox to convert the wav file to flac
  # The '-C 8' option sets compression level for FLAC (range 0-8, where 8 is the highest compression)
  seewave::sox(
    stringr::str_glue('"{temp_path}" "{output_file}"'),
    path2exe = "C:/Program Files (x86)/sox-14-4-2"
  )

  # message('file compressed')

  # then clear
  fs::file_delete(temp_path)

}


# shiny app functions -----------------------------------------------------

get_deployment_info <- function(sd_card_path, deployment_df) {

  # wav count
  n_wavs <- length(fs::dir_ls(sd_card_path, recurse = TRUE, glob = '*.wav'))

  # recording dates
  min_date <-
    fs::dir_ls(sd_card_path, recurse = TRUE, glob = '*.wav') |>
    stringr::str_extract('[0-9]{8}') |>
    lubridate::ymd() |>
    min()

  max_date <-
    fs::dir_ls(sd_card_path, recurse = TRUE, glob = '*.wav') |>
    stringr::str_extract('[0-9]{8}') |>
    lubridate::ymd() |>
    max()

  # deployment_name
  swift <-
    fs::dir_ls(sd_card_path) |>
    stringr::str_subset('S[0-9]{4}') |>
    head(1) |>
    stringr::str_extract('S[0-9]{4}')

  site <-
    deployment_df |>
    dplyr::filter(swift_id == swift) |>
    dplyr::distinct(site_code) |>
    dplyr::pull(site_code)

  cell <-
    deployment_df |>
    dplyr::filter(swift_id == swift) |>
    dplyr::distinct(cell_id) |>
    dplyr::pull(cell_id)

  station <-
    deployment_df |>
    dplyr::filter(swift_id == swift) |>
    dplyr::distinct(station_code) |>
    dplyr::pull(station_code)

  visit <-
    deployment_df |>
    dplyr::filter(swift_id == swift) |>
    dplyr::filter(visit_id == max(visit_id)) |>
    dplyr::pull(visit_id) %>%
    stringr::str_c('V', .)

  tibble::tibble(
    n_wavs = n_wavs,
    min_date = format(min_date, "%B %d"),
    max_date = format(max_date, "%B %d"),
    swift_id = swift,
    site_id = site,
    station_id = station,
    visit_id = visit,
    cell_id = cell
  )

}


create_subfolders <- function(x, site_id, visit_id, station_id, cell_id, hard_drive_path) {

  fs::dir_create(
    stringr::str_glue('{hard_drive_path}/{site_id}_{visit_id}/{site_id}_{visit_id}_{cell_id}_{station_id}/{site_id}_{visit_id}_{cell_id}_{station_id}_{x}')
  )

}


# convert SD wavs to SSD flacs
wav_to_flac <- function(wav_path, site_id, visit_id, station_id, cell_id, desktop_path, hard_drive_path) {

  wav_date_time <- stringr::str_extract(wav_path, '[0-9]{8}_[0-9]{6}')
  wav_desktop_path <- stringr::str_glue('{desktop_path}/{basename(wav_path)}')

  fs::file_copy(
    wav_path,
    wav_desktop_path
  )

  flac_ssd_path <-
    stringr::str_glue('{hard_drive_path}/{site_id}_{visit_id}/{site_id}_{visit_id}_{cell_id}_{station_id}/{site_id}_{visit_id}_{cell_id}_{station_id}_{wav_date_time}Z.flac')

  # wav to flac compression
  seewave::sox(
    stringr::str_glue('"{wav_desktop_path}" "{flac_ssd_path}"'),
    path2exe = "C:/Program Files (x86)/sox-14-4-2"
  )

  # delete temporary wav on desktop
  fs::file_delete(wav_desktop_path)

}
