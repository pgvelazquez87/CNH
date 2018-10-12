###########################################################
# Análisis de información de demanda de gasolinas         #
# Dirección General de Estadística y Evaluación Económica #
###########################################################


#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("reshape")
#install.packages("ggplot2")
#install.packages("qdapRegex")
#install.packages("ggmap")
#install.packages("stringr")
#install.packages("maps", dependencies=TRUE)
#install.packages("mapdata", dependencies=TRUE)
#install.packages("stringi", type="win.binary")
#install.packages("lubridate")

library(plyr)
library(dplyr)
library(tidyr)
library(reshape)
library(ggplot2)
library(readxl)
library(qdapRegex)
library(devtools)
library(rjson)
library(scales)
library(ggmap)
library(lattice)
library(stringr)
library(maps)
library(ggthemes)
library(mapdata)
library(data.table)
library(gganimate)
library(tibble)
library(lubridate)

setwd("G:/Proyectos/Prospectiva 2018-2032/Prospectiva/Gasolina")

## deshabilitar notación científica
options(scipen = 999)


####################################
# Cargar la informacion de oferta #
####################################

##funcion
agregado <- function(data)
{
  c(pct=with(data, sum(value)))
}


w_aceite <- read.csv("proceso_crudo.csv")
names(w_aceite)[1] <- "Refineria"
p_aceite <- melt(w_aceite, id=(c("Refineria", "Crudo")))
p_aceite$Crudo <- trimws(p_aceite$Crudo, which = c("left"))
p_aceite$Crudo <- gsub("Ã", "i", p_aceite$Crudo)
p_aceite$Crudo <- gsub("Otras corrientes a", "Otro", p_aceite$Crudo)
p_aceite$Crudo <- gsub("Reconstituido", "Pesado", p_aceite$Crudo)
p_aceite$Crudo <- as.factor(p_aceite$Crudo)

#¿Qué porcentaje de tipo de crudo surte a cada refinería?
tipo.crudo <- ddply(p_aceite, .(Refineria, Crudo), .fun=agregado)
total.crudo <- ddply(p_aceite, .(Refineria), .fun=agregado)
total1 <- merge(tipo.crudo, total.crudo, by=c("Refineria"))
total1$pct <- total1$pct.x / total1$pct.y
total1 <- subset(total1, select = -c(pct.x, pct.y))


# ¿Qué petrolíferos son los que se producen en cada refinería?
w_producto <- read.csv("produccion_petrolifero.csv")
names(w_producto)[1] <- "Refineria"
p_producto <- melt(w_producto, id=(c("Refineria", "Producto")))
p_producto$Producto <- gsub("Ã³", "o", p_producto$Producto)
p_producto$Producto <- gsub("Asfaltos", "Otro", p_producto$Producto)
p_producto$Producto <- gsub("Coque", "Otro", p_producto$Producto)
p_producto$Producto <- gsub("Gasoleo industrial d", "Otro", p_producto$Producto)
p_producto$Producto <- gsub("Nova/Base c", "Otro", p_producto$Producto)
p_producto$Producto <- gsub("Lubricantes", "Otro", p_producto$Producto)
p_producto$Producto <- gsub("Otros e", "Otro", p_producto$Producto)
p_producto$Producto <- gsub("Pemex Premium", "Gasolinas", p_producto$Producto)
p_producto$Producto <- gsub("            Extra/Pemex Magna b", "Gasolinas", p_producto$Producto)
p_producto$Producto <- gsub("Gas Seco a", "Gas seco", p_producto$Producto)
p_producto$Producto <- trimws(p_producto$Producto, which = c("left"))
p_producto$Producto <- as.factor(p_producto$Producto)
p_producto$variable <- ymd(sprintf(paste0(str_replace(string=p_producto$variable, pattern = "\\w(\\d)", replacement = "\\1"),"-01-01"), p_producto$variable))
p_producto<- p_producto %>% filter(Producto %in% c('Gasolinas', 'Turbosina', 'Diesel'))  %>% filter(variable<'2018-01-01') %>% group_by(Refineria, Producto, variable) %>% summarize(value = sum(value))



# ¿Cuánto produce de cada producto cada Refínería?
tipo.producto <- ddply(p_producto, .(Refineria, Producto), .fun=agregado)
total.producto <- ddply(p_producto, .(Refineria), .fun=agregado)
total2 <- merge(tipo.producto, total.producto, by=c("Refineria"))
total2$pct <- total2$pct.x / total2$pct.y
total2 <- subset(total2, select = -c(pct.x, pct.y))

#¿Qué porcentaje de crudo en qué refinería produce qué petrolífero?
tot <- merge(total1, total2, by=c("Refineria"))
tot$value <- (tot$pct.x * tot$pct.y)*100
tot <- ddply(tot, .(Refineria, Crudo, Producto), .fun=agregado)


cap <- c(275,177,285,220,330,315)
lon <- c(-99.942398, -97.81048, -94.460094, -101.178782, -95.171192, -99.269776)
lat <- c(25.586819, 22.265767, 17.999697, 20.581475, 16.209, 20.049787)
Refineria <- c("Cadereyta", "Madero", "Minatitlan", "Salamanca", "Salina Cruz", "Tula")
capacidad <- data.frame(Refineria, cap, lon, lat)
p_producto <- merge(p_producto, capacidad, by='Refineria')

#¿Cuál es la capacidad máxima de producción de gasolinas por refinería y tipo de crudo?
tot <- merge(tot, capacidad, by=c("Refineria"))
tot$value <- (tot$pct * tot$cap)/100
ddply(tot, .(Crudo, Producto), .fun=agregado)



####################################
# Cargar la informacion de demanda #
####################################

demanda_gas <- read_excel('gasolina_entidad.xlsx', sheet='Base')
demanda_gas$estado <- rm_between(demanda_gas$Superintendencia, "(", ")", extract=TRUE)
demanda_gas$Petrolifero <- as.factor(demanda_gas$Petrolifero)
demanda_gas$estado <- as.factor(unlist(demanda_gas$estado, use.names=FALSE))
demanda_gas$ciudad <- str_replace(string=demanda_gas$Superintendencia, pattern=".*(Ventas|foranea|terrestre|embarcador|satelite|Lubs.|lubs.|maritima|Sur,|Oriente,)\\s(\\w*\\s?\\w*?\\s?\\w*?)\\s\\(.*", replacement = "\\2")
demanda_gas$ciudad <- gsub("Moclova", "Monclova", demanda_gas$ciudad)

# cargar la información de coordenadas
demanda_gas$locacion <- paste(demanda_gas$ciudad, demanda_gas$estado)
demanda_gas[,3:28][is.na(demanda_gas[,3:28])] <-0
demanda_gas2 <- aggregate(cbind(`1993`, `1994`, `1995`, `1996`, `1997`, `1998`, `1999`, `2000`, `2001`,`2002`, `2003`, `2004`, `2005`, `2006`,`2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`,`2015`, `2016`, `2017`, `2018`) ~ locacion + Petrolifero, demanda_gas, sum)
demanda_gas3 <- data.table(demanda_gas2)
setkey(demanda_gas3, "Petrolifero")
demanda_gas3 <- demanda_gas3[c("Gasolinas", "Diesel", "Turbosina"),]
geocodes <- geocode(as.character(demanda_gas3$locacion), source="dsk")
write.csv(geocodes, file="geocodes.csv", row.names=FALSE)
demanda_gas3 <- data.frame(demanda_gas3[ , ], geocodes)

# gráficar la información
mex <- map_data("worldHires", "Mexico")
gg1 <- ggplot() + geom_polygon(data= mex, aes(x=long, y=lat, group= group), fill = NA, color="black") + coord_fixed(1.3)
gg1 + geom_point(data= demanda_gas3, aes(x=lon, y= lat, color=demanda_gas3$Petrolifero,  size=ifelse(demanda_gas3$X2017==0, NA, demanda_gas3$X2017), na.rm=TRUE))


geocodes_aux <- geocode(as.character(paste0(demanda_gas3$locacion[demanda_gas3$lat>33], ", Mexico")), source="dsk")
geocodes_aux$location <- demanda_gas3$locacion[demanda_gas3$lat>33]
colnames(geocodes_aux)[colnames(geocodes_aux)=="location"] <- "locacion"
# dado que existen varias ciudades que queremos reemplazar, tenemos hacer un loop 
for (id in 1:nrow(geocodes_aux)){
  demanda_gas3$lat[demanda_gas3$locacion %in% geocodes_aux$locacion[id]] <- geocodes_aux$lat[id]
  demanda_gas3$lon[demanda_gas3$locacion %in% geocodes_aux$locacion[id]] <- geocodes_aux$lon[id]
}



#### Base de datos de demanda de gasolinas en el país por centro
write.csv(demanda_gas3, file="demanda_gasolina_centro.csv", row.names=FALSE)

### Importar información ya trabajada anteriormente
demanda_gas3 <- read.csv(file="demanda_gasolina_centro.csv")
demanda_gas3 <- melt(demanda_gas3, id.vars=c("locacion", "Petrolifero", "lon", "lat"), variable.name="ano", value.name="mbd")
demanda_gas3$ano <- as.numeric(str_sub(demanda_gas3$ano, start=2, end=5))

#Gráfica demanda de 1993 a 2017 de manera descriptiva (scatterplot)
gg <- ggplot(demanda_gas3, aes(x=ano, y=mbd))
gg + geom_point(aes(color=Petrolifero)) + facet_wrap(~locacion) + scale_x_continuous(breaks=seq(from=1993, to=2017, by=3)) + theme(axis.text.x = element_text(angle=90))

#Para mapear la información de demanda
mex <- map_data("worldHires", "Mexico")
gg1 <- ggplot() + geom_polygon(data= mex, aes(x=long, y=lat, group= group), fill = NA, color="black") + coord_fixed(1.3) 
map <- gg1 + geom_point(aes(x = lon, y=lat, size = mbd, color=Petrolifero), data=demanda_gas3, alpha = 0.5) + scale_size_continuous(range = c(1,10), breaks = c(1, 5, 10, 30, 50, 80, 100)) + labs(size = 'mbd')
map <- map + geom_point(aes(x=lon, y=lat, size = value, color=Producto), data=p_producto, alpha = 0.5) + labs(size = 'mbd')

ini <- tibble(created_at = as.Date('1992-01-01'), mbd=0, lon=-110, lat=25)
fin <- tibble(created_at = seq(as.Date('2018-01-01'), as.Date('2019-01-01'), by = 'years'), mbd=0, lon=-110, lat=25)

demanda_gas3$ano <- ymd(sprintf("%d-01-01", demanda_gas3$ano))

#animación del mapa
mapa <- gg1 + geom_point(aes(x = lon, y=lat, size = mbd, color=Petrolifero, frame = ano, cumulative=TRUE), 
                        data=demanda_gas3, alpha = 0.5) + 
  geom_point(aes(x=lon, y=lat, size= value, color=Producto, frame = variable, cumulative = TRUE), 
             data=p_producto, alpha=0.5) +  
  geom_point(aes(x=lon, y=lat, size=mbd, frame = created_at, cumulative=TRUE),
             data = ini, alpha = 0) +
  geom_point(aes(x=lon, y=lat, size=mbd, frame = created_at, cumulative=TRUE),
             data = fin, alpha = 0) +
  scale_size_continuous(range = c(1,10), breaks = c(1, 5, 10, 30, 50, 80,100)) + labs(size = 'Demanda/Oferta (mbd)')


library(magick)
##devtools::install_github("nteetor/gganimate") 
library(animation)
library(gganimate)
ani.options(interval = 0.5, ani.width = 1000, ani.height = 1000, ani.res = 1000)
mapa <- gganimate::gg_animate(mapa, cumulative = TRUE, filename = "output.gif")
gg_animate_save(mapa, "oferta_demanda_gasolina.gif")


#######################################################################
# Estimar las distancias entre los puntos de demanda y las refinerías #
#######################################################################

for (i in nrow(capacidad))
{
x <- paste0("dist", gsub(" ", "", capacidad[i,1]))
demanda_gas3[,x] <- distHaversine(cbind(demanda_gas3$lon, demanda_gas3$lat), cbind(capacidad$lon[i], capacidad$lat[i]))
}



########################################
# Cargar la informacion de prospectiva #
########################################

prospectiva.wide <- read.csv("G:/Proyectos/Prospectiva 2018-2032/Prospectiva/Para SENER/Base prospectiva 2018-2032.csv")
prospectiva.long <- melt(prospectiva.wide, id=c("tipo", "ronda", "actividad", "activo", "provincia", "ubic", "p_prin_tipo_h", "hidroc_principal", "clasificacion", "licitacion", "bloque", "empresa", "escenario", "concepto") )
colnames(prospectiva.long)[colnames(prospectiva.long)=="value"] <- "montodiario"
colnames(prospectiva.long)[colnames(prospectiva.long)=="variable"] <- "periodo"

# filtramos para quedarnos s?lo con aceite y escenario medio
prospectiva.long %>% filter(concepto=='aceite_mbd' & escenario=='MEDIO')


## visualizar la informaci?n
ggplot(tot, aes(x=Crudo, y=producto_mbd, fill=Crudo)) + geom_point(aes(colour=factor(Producto))) + facet_wrap(~Refineria)

