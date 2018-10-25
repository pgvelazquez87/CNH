#################################################################################
# Conectar a base de datos de Oracle                                            #
# https://www.linkedin.com/pulse/connect-oracle-database-r-rjdbc-tianwei-zhang/ #
# 25 de octubre de 2018                                                         #
#################################################################################


library(RJDBC)

jdbcDriver <- JDBC("oracle.jdbc.OracleDriver",classPath="Z:\\Proyectos\\ojdbc6.jar")
valid <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//172.16.120.3:1521/cnih", "cmde_valid", "valid17")

balance_gas <- dbReadTable(valid, "BALANCE_GAS")
head(balance_gas)
