#' Function to list all agro-climatic stations (ACS) of Andalusia's network
#'
#' The function \code{getAndalusia_ACS} gets the list of meteorological stations of the agro-climatic network of
#' the agriculture council of the  Andalusian Administration
#'
#' @param url Page off Agroclimatic Information Network of Andalusia (RIA).
#'
#' @param encoding Encoding used by data.frame  return  (defaut: UTF-8)
#'

#'
#' @return  a data.frame with name of the station, code of the province and code of the station and URL of de age with the data of the station
#'
#' @export
#'
#' @author  Francisco.Viciana
#'
#' @seealso \code{getMetaData}
#'
#' @examples  \dontrun{
#'     lista.estaciones <- getAndalusia_ACS()
#'     head(lista.estaciones[,-4],15)
#' }
getAndalusia_ACS   <- function (url = 'http://www.juntadeandalucia.es/agriculturaypesca/ifapa/ria/servlet/FrontController?action=Static&url=estaciones2.html',
                                encoding='UTF-8')
{
   # require(RCurl)
   # require(XML)

   oo <- XML::htmlParse(url)
   nn <- XML::getNodeSet(oo,'//div[@id="menuIzq"]/div[@ class="seccion"]/a')

   provincias <-   data.frame ( provincia =  gsub("[[:space:]]",'',
                                      sapply ( nn[2:9] , XML::xmlValue, encoding="UTF-8")),
                                url       = sapply ( nn[2:9] , XML::xmlAttrs ),
                                stringsAsFactors = F )

   weather.station  <- data.frame()
   for (i in 1:length(provincias$provincia)) {
      provincia <-  provincias$provincia[i]
      url      <-   provincias$url[i]
      XML::htmlParse(url) -> oo
      XML::getNodeSet(oo,'//table[@id="concabecera"]//a') -> nn
      url.list <- sapply ( nn , XML::xmlAttrs )
      data.frame ( station.name        =  gsub("^[[:space:]]",'',sapply ( nn, XML::xmlValue, encoding="UTF-8")),
                   province.code =  as.integer(gsub('^.+c_provincia=([0123456789]+)&.+$','\\1',url.list)),
                   station.code   =  as.integer(gsub('^.+c_estacion=([0123456789]+)*$','\\1',url.list)),
                   url            =  url.list ,
                   stringsAsFactors = F)  -> df.job
      df.job$station.name <- paste0(df.job$station.code,'. ',df.job$station.name)
      weather.station <- rbind(weather.station,df.job)
   }

   return( weather.station )
}


