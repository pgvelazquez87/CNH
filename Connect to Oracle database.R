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


library(dplyr)
library(ggplot2)

produccion_campos$fecha <- as.Date(produccion_campos$FECHA)
head(produccion_campos)

prodaceite <- as.data.frame(produccion_campos %>% group_by(format(produccion_campos$fecha, "%Y-%m"), CAMPO_PORTAL) %>% summarize(aceite = sum(PETROLEO_MBD)))
colnames(prodaceite)[1] <- "fecha"
prodaceite$fecha <- as.Date(as.yearmon(prodaceite$fecha))
View(prodaceite)

ggplot(data=prodaceite[prodaceite$fecha > as.Date('2000-01-01') & prodaceite$aceite>50, ], aes(x=fecha, y=aceite)) + geom_point(aes(color = CAMPO_PORTAL)) + scale_shape_discrete(name="Campo")
