
#' Map ARU deployments
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples

map_deployments <- function(df) {

  mapview::mapview(
    df |> sf::st_as_sf(coords = c('long', 'lat'), crs = 4326),
    zcol = 'deployer_org',
    layer.name = 'ARU deployments'
  )

}
