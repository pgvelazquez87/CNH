#################################################################################
# Conectar a base de datos de Oracle                                            #
# https://www.linkedin.com/pulse/connect-oracle-database-r-rjdbc-tianwei-zhang/ #
# 25 de octubre de 2018                                                         #
#################################################################################

options(java.parameters = "-Xmx8048m")
library(RJDBC)

jdbcDriver <- JDBC("oracle.jdbc.OracleDriver",classPath="Z:\\Proyectos\\ojdbc6.jar")
valid <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//172.16.120.3:1521/cnih", "cmde_valid", "valid17")

balance_gas <- dbReadTable(valid, "BALANCE_GAS")
head(balance_gas)

produccion_campos <- dbReadTable(valid, "PRODUCCION_CAMPOS")
head(produccion_campos)

produccion_estado <- dbReadTable(valid,"PRODUCCION_ESTADO")


##Graficar la producción

library(dplyr)
library(ggplot2)

produccion_campos$fecha <- as.Date(produccion_campos$FECHA)
produccion_campos$LATITUD <- as.numeric(produccion_campos$LATITUD)
produccion_campos$LONGITUD <- as.numeric(produccion_campos$LONGITUD)
head(produccion_campos)

prodmes <- as.data.frame(produccion_campos %>% group_by(format(produccion_campos$fecha, "%Y-%m"), CAMPO_PORTAL) %>% summarize(aceite = sum(PETROLEO_MBD), gas=sum(GAS_MMPCD), latitud=mean(LATITUD), longitud = mean(LONGITUD)))
colnames(prodmes)[1] <- "fecha"
prodmes$fecha <- as.Date(as.yearmon(prodmes$fecha))
View(prodmes)

ggplot(data=prodmes[prodmes$fecha > as.Date('2000-01-01') & prodmes$aceite>50, ], aes(x=fecha, y=aceite, group=CAMPO_PORTAL, color=CAMPO_PORTAL)) + 
  geom_point() + geom_line() + scale_colour_hue(name="Campo") + xlab("Año") + ylab("miles de barriles diarios") + labs(title="Producción mensual de aceite") + 
  theme_minimal() + theme(legend.key.size = unit(0.5, "line"), legend.position= c(0.7,0.8), legend.key = element_rect(colour="transparent"))
  

ggplot(data=prodmes[prodmes$fecha > as.Date('2000-01-01') & prodmes$gas>150, ], aes(x=fecha, y=gas, group=CAMPO_PORTAL, color=CAMPO_PORTAL)) + 
  geom_point() + geom_line() + scale_colour_hue(name="Campo") + xlab("Año") + ylab("millones de pies cúbicos diarios") + labs(title="Producción mensual de gas") + 
  theme_minimal() + theme(legend.key.size = unit(0.4, "line"), legend.position= c(0.9,0.9), legend.key = element_rect(colour="transparent"))


library(ggmap)
library(rgdal)

