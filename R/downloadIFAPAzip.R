#' Climatic data from "Instituto de Investigacion y Formacion Agraria y Pesquera" ("Institute of Agricultural Research and Training and Fisheries") of Andalusia
#'
#' @description Function to download raw zip file with data from one station
#'
#' @import httr
#' @param file the name of the file for the download
#' @param c_province string with the code of the province
#' @param c_station string with the code of the station
#' @param start start date for download
#' @param end end date for download data
#' @return A zip file as it is download from server
#' @author Manuel Munoz-Marquez \email{manuel.munoz.marquez@@gmail.com}
#' @example
#' downloadIFAPAzip(file = 'IFAPA.zip', c_province = '11', c_station = '5', start = "01-01-2010", end = '31-01-2010')
#' @export
downloadIFAPAzip <- function(file, c_province, c_station, start = '31-01-2000', end = '31-12-2020') {
    ## Setup url for request
    url = 'https://www.juntadeandalucia.es/agriculturaypesca/ifapa/ria/servlet/FrontController?action=Download&url=descargarDatosEstaciones.jsp'
    ## Send post request to get all variables
    post <- POST(url = url,
                    body = list(c_provincia = c_province,
                                c_estacion = c_station,
                                zInicio = start,
                                zFin = end,
                                TempMax='S',
                                TempMin='S',
                                TempMedia='S',
                                HorMinTempMax='S',
                                HorMinTempMin='S',
                                HumedadMax='S',
                                HumedadMin='S',
                                HumedadMedia='S',
                                Radiacion='S',
                                VelViento='S',
                                DirViento='S',
                                Precipitacion='S',
                                ETo='S',
                                Todas='N'
                                ),
                 encode = 'form')
    ## Save the result as binary file
    writeBin(post$content, file)
}
