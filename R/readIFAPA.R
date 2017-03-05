#' Climatic data from "Instituto de Investigacion y Formacion Agraria y Pesquera" ("Institute of Agricultural Research and Training and Fisheries") of Andalusia
#'
#' @description Function to download and read data from one station
#'
#' @param file the name of the file for the download
#' @param c_province string with the code of the province
#' @param c_station string with the code of the station
#' @param start start date for download
#' @param end end date for download data
#' @return A data.frame with the data download after some transformations
#' @author Manuel Munoz-Marquez \email{manuel.munoz.marquez@@gmail.com}
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
    ## Let n be the number of datas in each row
    n <- length(names)
    ## Remove characters depending on station in name
    l <- nchar(names[3]) - 3
    names[3:n] <- sapply(names[3:n], FUN = function(e) substr(e, l, 1000))
    ## Split each line of data but 1 and 2
    data <- strsplit(data[-2:-1], '[ ]+', perl = TRUE)
    ## Drop lines with non different number of data
    data <- data[sapply(data, function(e) { length(e) == n })]
    ## Turn data into matrix
    data <- matrix(unlist(data), nrow = n)
    ## Turn into data.frame now
    data <- data.frame(t(data))
    names(data) <- names
    ## Convert some columns into correct type
    data$FECHA <- as.Date(data$FECHA, format = '%d-%m-%y')
    for (i in c(2:3, 5, 7:n)) data[,i] <- as.numeric(as.character(data[,i]))
    ## Add two columns with c_province and c_station
    data$c_province <- as.factor(c_province)
    data$c_station <- as.factor(c_station)
    ## Reorder columns to put c_province and c_station at the beggining
    data <- data[, c(n + 1, n + 2, 1:n)]
    ## Return data
    data
}



