
stations <- getAndalusia_ACS()

metainfo <- getMetaData(provincia       = stations$province.code,
                         estacion        = stations$station.code ,
                         nombre_estacion = stations$station.name )

data.clima   <-  readallIFAPA()

