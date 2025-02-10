
copy_compress_flacs <- function(deployment_df, input_dir, temp_dir, output_dir) {

  # List all .wav files in the input directory
  wav_files <- list.files(input_dir, pattern = "\\.wav$", full.names = TRUE, recursive = TRUE)

  # create pond/swift/date folders if necessary
  # name output files based on wav files
  swift <- stringr::str_extract(wav_files[1], 'S[0-9]+')

  site <-
    deployment_df |>
    dplyr::filter(swift_id == swift) |>
    dplyr::distinct(site_name) |>
    dplyr::pull(site_name)

  cell <-
    deployment_df |>
    dplyr::filter(swift_id == swift) |>
    dplyr::distinct(cell_id) |>
    dplyr::pull(cell_id)

  station <-
    deployment_df |>
    dplyr::filter(swift_id == swift) |>
    dplyr::distinct(station_id) |>
    dplyr::pull(station_id)

  wav_files |>
    purrr::walk(\(x) create_temp_folders(wav_file = x, input_dir = input_dir, temp_dir = temp_dir))

  wav_files |>
    purrr::walk(\(x) create_flac_folders(wav_file = x, output_dir = output_dir, site_name = site, cell_id = cell, station_id = station, swift_id = swift))

  wav_files |>
    furrr::future_walk(\(x) convert_to_flac(wav_file = x, input_dir = input_dir, temp_dir = temp_dir, output_dir = output_dir, site_name = site, cell_id = cell, station_id = station, swift_id = swift))

  message("FLAC compression completed!")

  beepr::beep()

}
