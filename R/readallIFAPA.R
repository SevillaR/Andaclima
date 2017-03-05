#' Climatic data from "Instituto de Investigacion y Formacion Agraria y Pesquera" ("Institute of Agricultural Research and Training and Fisheries") of Andalusia
#'
#' @description Function to download and read data from all stations
#'
#' @param file the name of the file for the download
#' @param c_province string with the code of the province
#' @param c_station string with the code of the station
#' @param start start date for download
#' @param end end date for download data
#' @return A data.frame with the data download after some transformations
#' @author Manuel Munoz-Marquez \email{manuel.munoz.marquez@@gmail.com}
#' @example
#' readallIFAPA(start = "01-01-2010", end = '31-01-2010')
#' @export

readallIFAPA <- function(start = '31-01-2000', end = '31-12-2020') {
    ## Read province and code station
    stations <- getAndalusia_ACS()
    ## Loop in all stations
    cat(1, '/', nrow(stations), ': Downloading data from station ', stations$station.code[1], ' at province ', stations$province.code[1], '\n', sep ='')
    data <- readIFAPA(stations$province.code[1], stations$station.code[1], start = start, end = end)
    for (i in 2:nrow(stations)) {
      cat(i, '/', nrow(stations), ': Downloading data from station ', stations$station.code[i], ' at province ', stations$province.code[i], '\n', sep ='')
      data <- rbind(data, readIFAPA(stations$province.code[i], stations$station.code[i], start = start, end = end))
    }
    ## Return data
    data
}



