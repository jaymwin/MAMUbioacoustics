
#' Get token to access EpiCollect entries
#'
#' @param client_id
#' @param client_secret
#'
#' @return
#' @export
#'
#' @examples

get_access_token <- function(client_id, client_secret) {

  # enter credentials to server
  res <-
    httr::POST(
      url = "https://five.epicollect.net/api/oauth/token",
      body = list(
        grant_type = "client_credentials",
        client_id = client_id,
        client_secret = client_secret
      )
    )
  message(httr::http_status(res)$message)

  # grab access token
  token <- httr::content(res)$access_token
  return(token)

}
