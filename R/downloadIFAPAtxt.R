#' Climatic data from "Instituto de Investigacion y Formacion Agraria y Pesquera" ("Institute of Agricultural Research and Training and Fisheries") of Andalusia
#'
#' @description Function to download raw txt file with data from one station
#'
#' @param file the name of the file for the download
#' @param c_province string with the code of the province
#' @param c_station string with the code of the station
#' @param start start date for download
#' @param end end date for download data
#' @return A data file as it is from unzip the download file from server
#' @author Manuel Munoz-Marquez \email{manuel.munoz.marquez@@gmail.com}
#' @examples
#'  \dontrun{
#'  downloadIFAPAtxt(file = 'IFAPA.txt', c_province = '11', c_station = '5',
#'                   start = "01-01-2010", end = '31-01-2010')
#'  }
#' @export

downloadIFAPAtxt <- function(file, c_province, c_station, start = '31-01-2000', end = '31-12-2020') {
    ## Build the name for tmp dir
    dirname <- paste0('/tmp/ifapa.', sample.int(1000000, 1))
    ## Create dir to store zip file
    dir.create(dirname)
    ## Build the name for zip file
    filenamezip <- paste0(dirname, '/', sample.int(1000000, 1), '.zip')
    ## Download zip file
    downloadIFAPAzip(filenamezip, c_province = c_province, c_station = c_station, start = start, end = end)
    ## Get filename for file in zip file and unzip
    filename <-  unzip(filenamezip, list = TRUE)$Name
    unzip(filenamezip, exdir = dirname)
    ## Move file
    status <- file.copy(paste0(dirname, '/', filename), file)
}
