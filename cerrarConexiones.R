library(RMySQL)
conexiones_abiertas <- dbListConnections(MySQL())

# Cerrar todas las conexiones
lapply(conexiones_abiertas, dbDisconnect)
