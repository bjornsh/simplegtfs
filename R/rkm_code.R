#' Get codes for Swedish regional GTFS data
#'
#' @return df Table with public transport authority code (rkm) and corresponding regional name and ID. "rkm" serves as input parameter for gtfs_download()
#'
#' @examples \donttest{
#' gtfs_rkm_kod()
#' }
#'
#' @export

gtfs_rkm_kod = function(){
  lan_kod <- c("01", "03", "04", "05", "06", "07", "08", "09", "10",
               "12", "13", "14", "17", "18", "19",
               "20", "21", "22", "23", "24", "25", "")
  region <- c("Stockholm", "Uppsala", "Södermanland", "Östergötland", "Jönköping",
              "Kronoberg", "Kalmar", "Gotland", "Blekinge", "Skåne", "Halland", "Västra Götaland",
              "Värmland", "Örebro", "Västmanland", "Dalarna", "Gävleborg", "Västernorrland",
              "Jämtland", "Västerbotten", "Norrbotten", "SJ")
  rkm <- c("sl", "ul", "sormland", "otraf", "SAKNAS", "krono", "klt", "gotland", "blekinge", "skane",
           "halland", "vt", "varm", "orebro", "vl", "dt", "xt", "dintur", "SAKNAS", "SAKNAS", "SAKNAS", "sj")

  df <- data.frame(lan_kod, region, rkm)

  return(df)
  }
