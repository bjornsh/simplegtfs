#' SF object with stops
#'
#' Create SF object with stop coordinates from GTFS. Multiple stops with same ID are grouped where the position is based on the mean lat/long coordinates
#' @param gtfs_obj GTFS object loaded with tidytransit::read_gtfs()
#' @return SF object with stop coordinates in WGS84
#' @import dplyr
#' @import sf
#'
#' @examples \donttest{
#' library(dplyr)
#' library(sf)
#' gtfs_file <- list.files("gtfs_data")
#' dat = tidytransit::read_gtfs(paste0("gtfs_data/", gtfs_file[1]))
#' stop_sf(dat)
#' }
#'
#' @export

stop_sf <- function(gtfs_obj){

  stops <- gtfs_obj$stops %>%
    # create hpl_id
    mutate(hpl_id = as.integer(substr(stop_id, 8, 13))) %>%
    select(hpl_id, stop_lat, stop_lon) %>%
    # create one point per hpl
    group_by(hpl_id) %>%
    summarise(stop_lat = mean(stop_lat),
              stop_lon = mean(stop_lon)) %>%
    # skapa SF object
    st_as_sf(
      coords = c("stop_lon", "stop_lat"),
      agr = "constant",
      crs = 4326,        # assign WGS84 as CRS
      stringsAsFactors = FALSE,
      remove = TRUE
    )
 return(stops)
}


#' SF object with lines
#'
#' Create SF object with the public transport network from GTFS. Where a line has multiple different paths, the most common path is identified and included. So this is a simplified version of the network.
#' @param gtfs_obj GTFS object loaded with tidytransit::read_gtfs()
#' @return SF object with most common path for each line in WGS84
#' @import dplyr
#' @import sf
#'
#' @examples \donttest{
#' library(dplyr)
#' library(sf)
#' library(mapview)
#' gtfs_file <- list.files("gtfs_data")
#' dat = tidytransit::read_gtfs(paste0("gtfs_data/", gtfs_file[1]))
#' network_sf(dat) %>% mapview()
#' }
#'
#' @export

network_sf <- function(gtfs_obj){

  # identify most common shape, ie route, per line
  mest_frekvent_shape <- left_join(gtfs_obj$trips,
                                   gtfs_obj$routes,
                                   by = "route_id") %>%
    # calculate number of trips per route and line
    group_by(route_short_name, shape_id) %>%
    summarise(n = n()) %>%
    # remove all routes except the most common
    filter(n == max(n)) %>%
    # in case both directions have same number of journeys, remove one
    ungroup() %>%
    group_by(route_short_name) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    # remove variable
    select(-n)

  # create linestring for each line
  lines <- gtfs_obj$shapes %>%
    # remove all routes that are not the most common per line
    filter(shape_id %in% mest_frekvent_shape$shape_id) %>%
    # create SF linestring
    tidytransit::shapes_as_sf(., crs = 4326) %>%
    # add line name
    left_join(., mest_frekvent_shape, by = "shape_id")

  return(lines)
}





