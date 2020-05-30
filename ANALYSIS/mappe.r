

##########MAPPE########################################################
regione<-readOGR(dsn="shp", layer = "Regione_2018")
regione<-spTransform(regione, CRS("+proj=longlat +datum=WGS84"))
regione<-rmapshaper::ms_simplify(regione)

province<-readOGR(dsn="shp", layer="Province_2018")
province<-spTransform(province, CRS("+proj=longlat +datum=WGS84"))
province<-rmapshaper::ms_simplify(province)

comuni<-readOGR(dsn="shp", layer = "Comuni_2018_poligonali")
comuni<-spTransform(comuni, CRS("+proj=longlat +datum=WGS84"))
comuni<-rmapshaper::ms_simplify(comuni)

pr<-c("BERGAMO","LECCO", "SONDRIO", "VARESE", "PAVIA", "COMO", "BRESCIA")
prov<-subset(province, province@data$NOME %in% pr)
##########################################################################