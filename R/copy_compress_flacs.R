
#' Perform WAV to FLAC compression
#'
#' @param deployment_df
#' @param input_dir
#' @param temp_dir
#' @param output_dir
#'
#' @return
#' @export
#'
#' @examples

copy_compress_flacs <- function(deployment_df, input_dir, temp_dir, output_dir) {

  # List all .wav files in the input directory
  wav_files <- list.files(input_dir, pattern = "\\.wav$", full.names = TRUE, recursive = TRUE)

  # create pond/swift/date folders if necessary
  # name output files based on wav files
  swift <- stringr::str_extract(wav_files[1], 'S[0-9]+')

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

  message(stringr::str_glue('site name = {site}, visit ID = {visit}, cell ID = {cell}, MAMU station ID = {station}'))

  wav_files |>
    purrr::walk(\(x) create_temp_folders(wav_file = x, input_dir = input_dir, temp_dir = temp_dir))

  dates <- unique(stringr::str_extract(wav_files, '[0-9]{8}'))

  dates |>
    purrr::walk(\(x) create_flac_folders(date = x, output_dir = output_dir, site_name = site, visit_id = visit, cell_id = cell, station_id = station))

  wav_files |>
    furrr::future_walk(\(x) MAMUbioacoustics:::convert_to_flac(wav_file = x, input_dir = input_dir, temp_dir = temp_dir, output_dir = output_dir, site_name = site, visit_id = visit, cell_id = cell, station_id = station, swift_id = swift))

  flac_files <- list.files(stringr::str_glue('{output_dir}/{site}_{visit}/{site}_{visit}_{cell}_{station}'), pattern = "\\.flac$", full.names = FALSE, recursive = TRUE)

  stopifnot('Number of WAV files does not match number of FLAC files compressed' = length(wav_files) == length(flac_files))

  message(stringr::str_glue("{length(flac_files)} FLACs compressed!"))

  beepr::beep()

}
