
#' Get EpiCollect entries in tidy format
#'
#' @param project_slug
#' @param token
#'
#' @return
#' @export
#'
#' @examples

read_epicollect <- function(project_slug, token) {

  # create url to access data
  # I removed form.ref from this string to get this working
  url_form <-
    paste(
      "https://five.epicollect.net/api/export/entries/",
      project_slug,
      "?map_index=0&form_ref=",
      # had to add per_page limit because it was only reading 50 records at a time
      "&format=csv&headers=true&per_page=1000",
      sep = ""
    )

  # get URL
  res1 <-
    httr::GET(
      url_form,
      httr::add_headers("Authorization" = paste("Bearer", token))
    )

  # check it worked to access
  message(httr::http_status(res1)$message)

  # read in csv of deployment/retrieval data
  df <-
    readr::read_csv(
      res1$url
    ) |>
    dplyr::select(5:24, 29) |>
    dplyr::rename(
      deploy_or_retrieval = dplyr::matches('Deployment_or'),
      deployer_name = dplyr::matches('Your_name'),
      deployer_org = dplyr::matches('Your_organization'),
      uw_site_name = dplyr::matches('UW_site_name'),
      cell_id = dplyr::matches('UW_cell'),
      uw_mamu_station_id = dplyr::matches('UW_MAMU_station_ID'),
      ncasi_site_name = dplyr::matches('NCASI_site_name'),
      ncasi_mamu_station_id = dplyr::matches('NCASI_MAMU_station'),
      cal_fire_site_name = dplyr::matches('CAL_FIRE_site_name'),
      cal_fire_mamu_station_id = dplyr::matches('CAL_FIRE_MAMU_sta'),
      swift_id = dplyr::matches('Swift_ID'),
      deployed_sd_card_id = dplyr::matches('Deployed_SD_card'),
      retrieved_sd_card_id = dplyr::matches('Retrieved_SD_card'),
      aru_status = dplyr::matches('ARU_light'),
      date = dplyr::matches('Date'),
      time = dplyr::matches('Time'),
      aru_photo_url = dplyr::matches('Picture_of_deploy'),
      lat = dplyr::matches('lat_'),
      long = dplyr::matches('long_'),
      comments = dplyr::matches('Comments')
    ) |>
    dplyr::mutate(
      site_name = dplyr::coalesce(uw_site_name, ncasi_site_name, cal_fire_site_name),
      mamu_station_id = dplyr::coalesce(uw_mamu_station_id, ncasi_mamu_station_id, cal_fire_mamu_station_id),
      date = lubridate::dmy(date),
      mamu_station_id = gsub("[- ]", "_", tolower(mamu_station_id))
    ) |>
    dplyr::group_by(swift_id) |>
    dplyr::mutate(visit_id = dplyr::row_number() - 1) |>
    dplyr::ungroup() |>
    dplyr::select(deploy_or_retrieval:deployer_org, site_name, visit_id, cell_id, mamu_station_id, swift_id:comments)

}
