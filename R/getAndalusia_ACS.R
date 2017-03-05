#' Function to list all agro-climatic stations of Andalusia's network
#' 
#'  \code{getAndalusia_ACS} gets the list of meteorological stations of the agro-climatic network of 
#'  the agriculture council of the Junta de Andalucia
#'  
#'   @param url  Page off Agroclimatic Information Network of Andalusia (RIA)
#'   http://www.juntadeandalucia.es/agriculturaypesca/ifapa/ria/servlet/FrontController?action=Static&url=estaciones2.html
#'  
#'   @return  a data.frame with name of the station, code of the province and code of the station and URL of de 
#'            Page with the data of the station
#'            
#'   @author  Francisco.Viciana        
#'   
#'   @seealso \code{getMetaData}
#'   
#'   @example 
#'     lista.estaciones <- getAndalusia_ACS()
#'     head(lista.estaciones[,-4],15)

getAndalusia_ACS   <- function (url = 'http://www.juntadeandalucia.es/agriculturaypesca/ifapa/ria/servlet/FrontController?action=Static&url=estaciones2.html' ) 
  {
   require(RCurl)
   require(XML)
  
   oo <- htmlParse(url) 
   nn <- getNodeSet(oo,'//div[@id="menuIzq"]/div[@ class="seccion"]/a')
   
   provincias <-   data.frame ( provincia =  gsub("[[:space:]]",'',
                                      sapply ( nn[2:9] , xmlValue, encoding="UTF-8")), 
                                url       = sapply ( nn[2:9] , xmlAttrs ),  
                                stringsAsFactors = F )  


   weather.station  <- data.frame()
   for (i in 1:length(provincias$provincia)) {
      provincia <-  provincias$provincia[i]
      url      <-   provincias$url[i]
      htmlParse(url) -> oo
      getNodeSet(oo,'//table[@id="concabecera"]//a') -> nn
      url.list <- sapply ( nn , xmlAttrs )
      data.frame ( station.name        =  gsub("^[[:space:]]",'',sapply ( nn, xmlValue, encoding="UTF-8")), 
                   province.code =  as.integer(gsub('^.+c_provincia=([0123456789]+)&.+$','\\1',url.list)),
                   station.code   =  as.integer(gsub('^.+c_estacion=([0123456789]+)*$','\\1',url.list)),
                   url            =  url.list , 
                   stringsAsFactors = F)  -> df.job
      df.job$station.name <- paste0(df.job$station.code,'. ',df.job$station.name)
      weather.station <- rbind(weather.station,df.job)
   }

   return( weather.station )
}   


