
# temporary folders to hold wavs on desktop or other local folder
create_temp_folders <- function(wav_file, input_dir, temp_dir) {

  stringr::str_replace(
    stringr::str_remove(wav_file, basename(wav_file)),
    input_dir,
    stringr::str_c(temp_dir, '/')
  ) |>
    fs::dir_create()

}

# create flac folders on an external hard drive
create_flac_folders <- function(wav_file, output_dir, site_name, visit_id, cell_id, station_id) {

  date <- stringr::str_extract(wav_file, '[0-9]{8}')
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
    overwrite = FALSE
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
