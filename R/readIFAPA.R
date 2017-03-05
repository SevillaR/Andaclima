#' Climatic data from "Instituto de Investigacion y Formacion Agraria y Pesquera" ("Institute of Agricultural Research and Training and Fisheries") of Andalusia
#'
#' Function to download and read data from one station
#' 
#' @import httr
#' @param file the name of the file for the download
#' @param c_province string with the code of the province
#' @param c_station string with the code of the station
#' @param start start date for download
#' @param end end date for download data
#' @example
#' readIFAPA(c_province = '11', c_station = '5', start = "01-01-2010", end = '31-01-2010')
#' @export

readIFAPA <- function(c_province, c_station, start = '31-01-2000', end = '31-12-2020') {
    ## Build the name for raw file
    filename <- paste0('/tmp/ifapa.', sample.int(1000000, 1), '.txt')
    ## Download raw data file
    downloadIFAPAtxt(filename, c_province = c_province, c_station = c_station, start = start, end = end)
    ## Read data line by line to be able to handle format
    data <- readLines(filename)
    ## Split first line to get names
    names <- strsplit(data[1], '[ ]+', perl = TRUE)[[1]]
    ## Remove extranger characters in name
    names <- gsub(pattern = 'Cá05', replacement = '', names)
    ## Let n be the number of datas in each row
    n <- length(names)
    ## Split each line of data but 1 and 2
    data <- strsplit(data[-2:-1], '[ ]+', perl = TRUE)
    ## Drop lines with non different number of data
    data <- data[sapply(data, function(e) { length(e) == n })]
    ## Turn data into matrix
    data <- matrix(unlist(data), nrow = n)
    ## Turn into data.frame now
    data <- data.frame(t(data))
    names(data) <- names
    data   
}
    
    

