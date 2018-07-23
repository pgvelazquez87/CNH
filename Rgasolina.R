install.packages("dplyr")
install.packages("tidyr")
install.packages("reshape")
install.packages("ggplot2")
install.packages("qdapRegex")

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
p_producto$Producto <- gsub("ó", "o", p_producto$Producto)
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

#install_packages("ggmap")
library(devtools)
library(rjson)
library(ggmap)
library(lattice)


# cargar la informaci?n de coordenadas
demanda_gas$locacion <- paste(demanda_gas$ciudad, demanda_gas$estado)
demanda_gas[,3:28][is.na(demanda_gas[,3:28])] <-0
aggregate(cbind(`1993`, `1994`, `1995`, `1996`, `1997`, `1998`, `1999`, `2000`, `2001`,`2002`, `2003`, `2004`, `2018`) ~ estado, demanda_gas, sum)
demanda_gas2 <- as.data.frame(demanda_gas)[!demanda_gas$Petrolifero!= c("Gasolinas", "Turbosina", "Diesel"),]
demanda_gas2 <- melt(demanda_gas2, id.vars=c("Superintendencia", "Petrolifero", "estado", "ciudad", "locacion"), variable.name="ano", value.name="mbd")
demanda_gas$lon <- geocode(demanda_gas$locacion, output="latlona", source="google")[1]
demanda_gas$lat <- geocode(demanda_gas$locacion, output="latlona", source="dsk")[2]
gg <- ggplot(demanda_gas2, aes(x=variable, y=value))
gg + geom_point(aes(color=Petrolifero)) + facet_wrap(~estado)




## Cargar la informaci?n de prospectiva
prospectiva.wide <- read.csv("G:/Proyectos/Prospectiva 2018-2032/Prospectiva/Para SENER/Base prospectiva 2018-2032.csv")
prospectiva.long <- melt(prospectiva.wide, id=c("tipo", "ronda", "actividad", "activo", "provincia", "ubic", "p_prin_tipo_h", "hidroc_principal", "clasificacion", "licitacion", "bloque", "empresa", "escenario", "concepto") )
colnames(prospectiva.long)[colnames(prospectiva.long)=="value"] <- "montodiario"
colnames(prospectiva.long)[colnames(prospectiva.long)=="variable"] <- "periodo"

# filtramos para quedarnos s?lo con aceite y escenario medio
prospectiva.long %>% filter(concepto=='aceite_mbd' & escenario=='MEDIO')


## visualizar la informaci?n
ggplot(tot, aes(x=Crudo, y=producto_mbd, fill=Crudo)) + geom_point(aes(colour=factor(Producto))) + facet_wrap(~Refineria)

