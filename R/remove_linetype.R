#' Remove linetypes
#'
#' Filter gtfs_obj by trips related to certain line types (route_desc), eg all trips, routes etc for "Stadsbuss"
#' @param gtfs_obj GTFS object loaded with tidytransit::read_gtfs()
#' @param remove_linetype String of line types to be removed
#'
#' @import dplyr
#' @import tidytransit
#'
#' @examples \donttest{
#' gtfs_file <- list.files("gtfs_data")
#' dat = tidytransit::read_gtfs(paste0("gtfs_data/", gtfs_file[1]))
#' remove_linetype(dat)
#' }
#'
#' @export


remove_linetype <- function(gtfs_obj, remove_linetype){
  # identify trips belonging to line types (route_desc) to be excluded
  route_trip_include  <- gtfs_obj$routes %>%
    left_join(., gtfs_obj$trips, by = "route_id") %>%
    filter(route_desc %notin% remove_linjetyp) %>%
    select(trip_id) %>%
    distinct() %>%
    pull()

  # remove trips from GTFS
  final <- tidytransit::filter_feed_by_trips(gtfs_obj, route_trip_include)

  return(final)
}
