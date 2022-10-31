#' Create dataframe with unique stop name and ID
#'
#' @param gtfs_obj GTFS object loaded with tidytransit::read_gtfs()
#' @return stops Table with unique stop ID and name
#' @import dplyr
#'
#' @export
#' @examples \donttest{
#' library(dplyr)
#' gtfs_file <- list.files("gtfs_data")
#' dat = tidytransit::read_gtfs(paste0("gtfs_data/", gtfs_file[1]))
#' stop_id_name(dat)
#' }

stop_id_name <- function(gtfs_obj){
  stops <- gtfs_obj$stops %>%
    # create hpl_id
    mutate(hpl_id = as.integer(substr(.data$stop_id, 8, 13))) %>%
    select(hpl_id, stop_name) %>%
    distinct()

  return(stops)
}


#' Number of departures
#'
#' Create dataframe with the number of departures per stop, hour and line for todays date
#' @param gtfs_obj GTFS object loaded with tidytransit::read_gtfs()
#' @return dep Table with number of departures per stop, hour and line for single date
#' @import dplyr
#'
#' @examples \donttest{
#' library(dplyr)
#' gtfs_file <- list.files("gtfs_data")
#' dat = tidytransit::read_gtfs(paste0("gtfs_data/", gtfs_file[1]))
#' departure_stop_line_hr(dat)
#' }
#'
#' @export

departure_stop_line_hr = function(gtfs_obj){
  dep <- left_join(gtfs_obj$routes,
                   gtfs_obj$trips, by = "route_id") %>%
    left_join(., gtfs_obj$stop_times, by = "trip_id") %>%
    # create hpl_id
    mutate(hpl_id = as.integer(substr(stop_id, 8, 13)),
           dep_hr = substr(departure_time,1,2)) %>%
    select(hpl_id, route_short_name, dep_hr) %>%
    group_by(hpl_id, route_short_name, dep_hr) %>%
    count()

  return(dep)
}


#' Number of lines per stop
#'
#' Create dataframe with the number of lines per stop for todays date
#' @param gtfs_obj GTFS object loaded with tidytransit::read_gtfs()
#' @return lines Table with number of lines per stop for single date
#' @import dplyr
#'
#' @examples \donttest{
#' library(dplyr)
#' gtfs_file <- list.files("gtfs_data")
#' dat = tidytransit::read_gtfs(paste0("gtfs_data/", gtfs_file[1]))
#' n_lines_stop(dat)
#' }
#'
#' @export

n_lines_stop <- function(gtfs_obj){
  lines <- left_join(gtfs_obj$routes,
                     gtfs_obj$trips,
                     by = "route_id") %>%
    left_join(., gtfs_obj$stop_times, by = "trip_id") %>%
    # create hpl_id
    mutate(hpl_id = as.integer(substr(.data$stop_id, 8, 13))) %>%
    select(hpl_id, route_short_name) %>%
    # remove duplicates
    distinct() %>%
    # lines per hpl
    group_by(hpl_id) %>%
    summarise(antal_linjer = n()) %>%
    ungroup()

  return(lines)
  }


#' Lines per stop
#'
#' Create dataframe with all the lines per stop for todays date
#' @param gtfs_obj GTFS object loaded with tidytransit::read_gtfs()
#' @return lines Table with all lines per stop for single date
#' @import dplyr
#'
#' @examples \donttest{
#' library(dplyr)
#' gtfs_file <- list.files("gtfs_data")
#' dat = tidytransit::read_gtfs(paste0("gtfs_data/", gtfs_file[1]))
#' lines_stop(dat)
#' }
#'
#' @export

lines_stop = function(gtfs_obj){
  lines <- left_join(gtfs_obj$routes, gtfs_obj$trips, by = "route_id") %>%
    left_join(., gtfs_obj$stop_times, by = "trip_id") %>%
    # create hpl_id
    mutate(hpl_id = as.integer(substr(.data$stop_id, 8, 13))) %>%
    select(hpl_id, route_short_name) %>%
    # remove duplicates
    distinct() %>%
    # lines per hpl
    group_by(hpl_id) %>%
    summarise(linjer = paste(route_short_name, collapse = ",")) %>%
    ungroup()

  return(lines)
  }


