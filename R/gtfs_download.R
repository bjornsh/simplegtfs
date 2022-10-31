#' Download GTFS zip file from trafiklab.se
#'
#' Download GTFS data and store zip file in created project folder. Requires key for Trafiklab GTFS Regional API
#' (https://www.trafiklab.se/api/trafiklab-apis/gtfs-regional/)
#' @param rkm Code for the regional public transport authority
#'
#' @importFrom httr GET write_disk
#'
#' @examples \donttest{
#' library(rstudioapi)
#' library(httr)
#' download_gtfs(rkm = "ul")
#' }
#'
#' @export


download_gtfs <- function(rkm){
  # create folder to store GTFS zip file
  dir.create(file.path(getwd(), "gtfs_data"), showWarnings = FALSE)

  # define GTFS area
  rkm <- rkm

  # API key
  trafiklab_key = rstudioapi::askForPassword()

  # define API url
  url <- paste0("https://opendata.samtrafiken.se/gtfs/", rkm, "/", rkm, ".zip?key=", trafiklab_key)

  # download GTFS zip file from API and store in folder "wd/gtfs_data"
  httr::GET(url,
            httr::write_disk(file.path(getwd(), "gtfs_data", paste0("gtfs_", rkm, "_", Sys.Date(), ".zip")),
                             overwrite=TRUE))
  }
