#' Get metadata from "Estaciones Agroclimaticas de Andalucia"
#'
#' @param provincia numerial code of the target province
#' @param estacion numerical code of the target meteorogical station.
#' @param nombre_estacion name of the meteorological station (optional). Only used as label.
#'
#' @return onw row data.frame with metadata of the station
#' @export
#'
#' @description This function retrives the metadata of the targeted meteorological station.
#'
#' See \url{https://www.juntadeandalucia.es/agriculturaypesca/ifapa/ria/servlet/FrontController?action=Init} for explanation of the
#' data and codes.
#'
#' @author Ignasi Bartomeus \email{nacho.bartomeus@@gmail.com}
#' @examples \dontrun{
#' provincia <- c(41,11)
#' estacion <- c(5,1)
#' nombre_estacion <- c("Aznalcollar", "Basulta")
#' getMetaData(provincia, estacion, nombre_estacion)
#' getOneMetaData(41, 5, "aznalcollar")
#' }
getMetaData <- function(provincia = NA, estacion = NA, nombre_estacion = NA){
  if(length(provincia) < 2){
    dat <- getOneMetaData(provincia, estacion, nombre_estacion)
  } else{
    dat <- getOneMetaData(provincia[1], estacion[1], nombre_estacion[1])
    for(i in 2:length(provincia)){
      temp <- getOneMetaData(provincia[i], estacion[i], nombre_estacion[i])
      dat <- rbind(dat, temp)
    }
  }
  dat
}

getOneMetaData <- function(provincia = NA, estacion = NA, nombre_estacion = NA){
  # require(httr)
  # require(xml2)
  url = "https://www.juntadeandalucia.es/agriculturaypesca/ifapa/ria/servlet/FrontController?action=Static&url=coordenadas.jsp"
  args <- list(c_provincia =  provincia, c_estacion = estacion)
  url_check <- httr::GET(url, query = args)
  doc    <- xml2::read_html(httr::content(url_check, "text", encoding = "latin1"), encoding = "latin1")
  tables <- xml2::xml_find_all(doc, "//table")
  text   <- xml2::xml_text(tables[[1]], trim = TRUE)
  text2 <- gsub("\r\n", replacement = "", text)
  #get starting and end points
  p_prov <- regexpr("Provincia:", text2, fixed = TRUE)
  p_cod <- regexpr("Código de Estación:", text2, fixed = TRUE)
  p_zon <- regexpr("Zona Regable:", text2, fixed = TRUE)
  p_coo <- regexpr("Coordenadas", text2, fixed = TRUE)
  p_x <- regexpr("X:", text2, fixed = TRUE)
  p_y <- regexpr("Y:", text2, fixed = TRUE)
  p_lat <- regexpr("Latitud:", text2, fixed = TRUE)
  p_long <- regexpr("Longitud:", text2, fixed = TRUE)
  p_alt <- regexpr("Altitud:", text2, fixed = TRUE)
  p_mas <- regexpr("Más", text2, fixed = TRUE)
  #create data.frame
  meta <- data.frame(c_provincia = NA ,
                     provincia = NA, estacion = NA, nombre_estacion = NA,
                     zona_regable = NA, x = NA, y = NA, latitud = NA, longitud = NA,
                     altitud = NA)
  #populate data..frame
  meta[, "c_provincia"] <- provincia
  meta[, "provincia"] <- substr(text2, p_prov+attributes(p_prov)$match.length, p_cod-1)
  meta[, "estacion"] <- substr(text2, p_cod+attributes(p_cod)$match.length+1, p_zon-1)
  meta[, "nombre_estacion"] <- nombre_estacion
  temp <- substr(text2, p_zon+attributes(p_zon)$match.length+1, p_coo-1)
  meta[, "zona_regable"] <- gsub(" ", "", temp)
  meta[, "x"] <- substr(text2, p_x+attributes(p_x)$match.length+1, p_y-1)
  meta[, "y"] <- substr(text2, p_y+attributes(p_y)$match.length+1, p_lat-1)
  meta[, "latitud"] <- substr(text2, p_lat+attributes(p_lat)$match.length+1, p_long-1)
  meta[, "longitud"] <- substr(text2, p_long+attributes(p_long)$match.length+1, p_alt-1)
  temp <- substr(text2, p_alt+attributes(p_alt)$match.length, p_mas-1)
  meta[, "altitud"] <- gsub(" ", "", temp)
  meta
}







