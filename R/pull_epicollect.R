
#' Get EpiCollect entries in tidy format
#'
#' @param project_slug
#' @param token
#'
#' @return
#' @export
#'
#' @examples

pull_epicollect <- function(project_slug, token) {

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
      res1$url #,
      # col_types = cols(
      #   `2_date` = 'D',
      #   `4_sex` = 'c', # sex F = F, not FALSE
      #   `29_date_baited` = 'D',
      #   `17_photo_C_optional_` = 'c',
      #   `18_photo_D_optional_` = 'c',
      #   `30_comments` = 'c'
      # )
    )

}
