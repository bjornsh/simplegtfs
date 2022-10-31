#' Correct spelling
#'
#' Fix spelling of Swedish special letters in case UTF-8 encoding does not work
#' @param gtfs_obj GTFS object loaded with tidytransit::read_gtfs()
#'
#' @importFrom stringr str_replace_all
#'
#' @examples \donttest{
#' library(stringr)
#' gtfs_file <- list.files("gtfs_data")
#' dat = tidytransit::read_gtfs(paste0("gtfs_data/", gtfs_file[1]))
#' funk_spelling(dat)
#' }
#'
#' @export

spelling <- function(gtfs_obj) {
  for(i in 1:length(gtfs_obj)){
    for(j in 1:length(gtfs_obj[[i]])){

      if (!is.numeric(gtfs_obj[[i]][[j]]) & !is.logical(gtfs_obj[[i]][[j]])){
#      if( is.character(gtfs_obj[[i]][[j]])){
        gtfs_obj[[i]][[j]] =  stringr::str_replace_all(gtfs_obj[[i]][[j]],
                                              c("Ã„" = "Ä",
                                                "Ã¤" = "ä",
                                                "Ã–" = "Ö",
                                                "Ã¶" = "ö",
                                                "Ã…" = "Å",
                                                "Ã¥" = "å",
                                                "Ã©" = "é",
                                                "Ã¼" = "ü"))
      }
    }
    }
  return(gtfs_obj)
  }
