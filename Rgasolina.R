#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("reshape")
#install.packages("ggplot2")
#install.packages("qdapRegex")

library(plyr)
library(dplyr)
library(tidyr)
library(reshape)
library(ggplot2)
library(readxl)
library(qdapRegex)

setwd("G:/Proyectos/Prospectiva 2018-2032/Prospectiva/Gasolina")

## deshabilitar notaci?n cient?fica
options(scipen = 999)

##funcion
agregado <- function(data)
{
  c(pct=with(data, sum(value)))
}


w_aceite <- read.csv("proceso_crudo.csv")
names(w_aceite)[1] <- "Refineria"
p_aceite <- melt(w_aceite, id=(c("Refineria", "Crudo")))
p_aceite$Crudo <- trimws(p_aceite$Crudo, which = c("left"))
p_aceite$Crudo <- gsub("?", "i", p_aceite$Crudo)
p_aceite$Crudo <- gsub("Otras corrientes a", "Otro", p_aceite$Crudo)
p_aceite$Crudo <- gsub("Reconstituido", "Pesado", p_aceite$Crudo)
p_aceite$Crudo <- as.factor(p_aceite$Crudo)

tipo.crudo <- ddply(p_aceite, .(Refineria, Crudo), .fun=agregado)
total.crudo <- ddply(p_aceite, .(Refineria), .fun=agregado)
total1 <- merge(tipo.crudo, total.crudo, by=c("Refineria"))
total1$pct <- total1$pct.x / total1$pct.y
total1 <- subset(total1, select = -c(pct.x, pct.y))


w_producto <- read.csv("producto_petrolifero.csv")
names(w_producto)[1] <- "Refineria"
p_producto <- melt(w_producto, id=(c("Refineria", "Producto")))
p_producto$Producto <- gsub("Ã³", "o", p_producto$Producto)
p_producto$Producto <- gsub("Asfaltos", "Otro", p_producto$Producto)
p_producto$Producto <- gsub("Coque", "Otro", p_producto$Producto)
p_producto$Producto <- gsub("Gasoleo industrial d", "Otro", p_producto$Producto)
p_producto$Producto <- gsub("Nova/Base c", "Otro", p_producto$Producto)
p_producto$Producto <- gsub("Lubricantes", "Otro", p_producto$Producto)
p_producto$Producto <- gsub("Otros e", "Otro", p_producto$Producto)
p_producto$Producto <- gsub("            Extra/Pemex Magna b", "Magna", p_producto$Producto)
p_producto$Producto <- gsub("Gas Seco a", "Gas seco", p_producto$Producto)
p_producto$Producto <- trimws(p_producto$Producto, which = c("left"))
p_producto$Producto <- as.factor(p_producto$Producto)


tipo.producto <- ddply(p_producto, .(Refineria, Producto), .fun=agregado)
total.producto <- ddply(p_producto, .(Refineria), .fun=agregado)
total2 <- merge(tipo.producto, total.producto, by=c("Refineria"))
total2$pct <- total2$pct.x / total2$pct.y
total2 <- subset(total2, select = -c(pct.x, pct.y))


tot <- merge(total1, total2, by=c("Refineria"))
tot$value <- (tot$pct.x * tot$pct.y)*100
tot <- ddply(tot, .(Refineria, Crudo, Producto), .fun=agregado)

cap <- c(275,177,285,220,330,315)
Refineria <- c("Cadereyta", "Madero", "Minatitlan", "Salamanca", "Salina Cruz", "Tula")
capacidad <- data.frame(Refineria, cap)

tot <- merge(tot, capacidad, by=c("Refineria"))
tot$value <- (tot$pct * tot$cap)/100

ddply(tot, .(Crudo, Producto), .fun=agregado)
write.csv(tot, file="tot.csv")


#install.packages("ggmap")
#install.packages("stringr")
#install.packages("maps", dependencies=TRUE)
#install.packages("mapdata", dependencies=TRUE)
library(devtools)
library(rjson)
library(ggmap)
library(lattice)
library(stringr)
library(maps)
library(mapdata)
library(data.table)


## Cargar la informaci?n de demanda
demanda_gas <- read_excel('gasolina_entidad.xlsx', sheet='Base')
demanda_gas$estado <- rm_between(demanda_gas$Superintendencia, "(", ")", extract=TRUE)
demanda_gas$Petrolifero <- as.factor(demanda_gas$Petrolifero)
demanda_gas$estado <- as.factor(unlist(demanda_gas$estado, use.names=FALSE))

demanda_gas$ciudad <- rm_between(demanda_gas$Superintendencia, "Ventas ", "(", extract=TRUE)
demanda_gas$ciudad <- gsub("Moclova", "Monclova", demanda_gas$ciudad)
demanda_gas$ciudad <- ifelse(is.na(demanda_gas$ciudad), rm_between(demanda_gas$Superintendencia, "foranea ", "(", extract=TRUE), demanda_gas$ciudad)
demanda_gas$ciudad <- ifelse(is.na(demanda_gas$ciudad), rm_between(demanda_gas$Superintendencia, "terrestre ", "(", extract=TRUE), demanda_gas$ciudad)
demanda_gas$ciudad <- ifelse(is.na(demanda_gas$ciudad), rm_between(demanda_gas$Superintendencia, "embarcador ", "(", extract=TRUE), demanda_gas$ciudad)
demanda_gas$ciudad <- ifelse(is.na(demanda_gas$ciudad), rm_between(demanda_gas$Superintendencia, "maritima ", "(", extract=TRUE), demanda_gas$ciudad)
demanda_gas$ciudad <- ifelse(is.na(demanda_gas$ciudad), rm_between(demanda_gas$Superintendencia, "satelite ", "(", extract=TRUE), demanda_gas$ciudad)
demanda_gas$ciudad <- ifelse(is.na(demanda_gas$ciudad), rm_between(demanda_gas$Superintendencia, "Lubs. ", "(", extract=TRUE), demanda_gas$ciudad)
demanda_gas$ciudad <- ifelse(is.na(demanda_gas$ciudad), rm_between(demanda_gas$Superintendencia, "lubs. ", "(", extract=TRUE), demanda_gas$ciudad)




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
gg1 <- ggplot() + geom_polygon(data= mex, aes(x=long, y=lat, group=group), fill = NA, color="black") + coord_fixed(1.3)
gg1 + geom_point(data= demanda_gas3, aes(x=lon, y= lat, color=demanda_gas3$Petrolifero,  size=ifelse(demanda_gas3$X2017==0, NA, demanda_gas3$X2017), na.rm=TRUE))


geocodes_aux <- geocode(as.character(paste0(demanda_gas3$locacion[demanda_gas3$lat>33], ", Mexico")), source="dsk")
geocodes_aux$location <- demanda_gas3$locacion[demanda_gas3$lat>33]
colnames(geocodes_aux)[colnames(geocodes_aux)=="location"] <- "locacion"
# dado que existen varias ciudades que queremos reemplazar, tenemos hacer un loop 
for (id in 1:nrow(geocodes_aux)){
  demanda_gas3$lat[demanda_gas3$locacion %in% geocodes_aux$locacion[id]] <- geocodes_aux$lat[id]
  demanda_gas3$lon[demanda_gas3$locacion %in% geocodes_aux$locacion[id]] <- geocodes_aux$lon[id]
}


####

write.csv(demanda_gas3, file="demanda_gasolina_centro.csv", row.names=FALSE)
demanda_gas3 <- melt(demanda_gas3, id.vars=c("locacion", "Petrolifero", "lon", "lat"), variable.name="ano", value.name="mbd")
demanda_gas3$ano <- as.numeric(str_sub(demanda_gas3$ano, start=2, end=5))

gg <- ggplot(demanda_gas3, aes(x=ano, y=mbd))
gg + geom_point(aes(color=Petrolifero)) + facet_wrap(~locacion) + scale_x_continuous(breaks=seq(from=1993, to=2017, by=3)) + theme(axis.text.x = element_text(angle=90))


## Cargar la informaci?n de prospectiva
prospectiva.wide <- read.csv("G:/Proyectos/Prospectiva 2018-2032/Prospectiva/Para SENER/Base prospectiva 2018-2032.csv")
prospectiva.long <- melt(prospectiva.wide, id=c("tipo", "ronda", "actividad", "activo", "provincia", "ubic", "p_prin_tipo_h", "hidroc_principal", "clasificacion", "licitacion", "bloque", "empresa", "escenario", "concepto") )
colnames(prospectiva.long)[colnames(prospectiva.long)=="value"] <- "montodiario"
colnames(prospectiva.long)[colnames(prospectiva.long)=="variable"] <- "periodo"

# filtramos para quedarnos s?lo con aceite y escenario medio
prospectiva.long %>% filter(concepto=='aceite_mbd' & escenario=='MEDIO')


## visualizar la informaci?n
ggplot(tot, aes(x=Crudo, y=producto_mbd, fill=Crudo)) + geom_point(aes(colour=factor(Producto))) + facet_wrap(~Refineria)

