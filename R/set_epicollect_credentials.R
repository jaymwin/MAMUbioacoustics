
#' Set up EpiCollect credentials
#'
#' @return
#' @export
#'
#' @examples

set_epicollect_credentials <- function() {

  keyring::key_set('project_slug')
  keyring::key_set('token')

}
