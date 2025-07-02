library(tidyverse)
library(readxl)
library(writexl)


#TRAEMOS LAS BASES GENERADAS POR "01_Filtrado_Limpieza_de_datos.R" DE LA CARPETA OUTPUT
base_combinada_transacciones <- read_excel("C:/Users/JP1/Documents/GitHub/TP-FINAL/output/base_combinada_transacciones.xlsx" )
prestamo_usd <- read_excel("C:/Users/JP1/Documents/GitHub/TP-FINAL/output/monto_prestamos_usd.xlsx" )

#JUNTAMOS LA BASE DE LOS PRESTAMOS CON LAS TRANSACCIONES:

venta_alq_uva <- left_join(base_combinada_transacciones, prestamo_usd , by="año") 



#REGRESION VENTAS
venta_uva <- venta_alq_uva %>% 
  filter(transaccion== "Venta") #SACAMOS LOS DATOS DE ALQUILER
regresion_venta = lm( precio_promedio~ prestamos, data=venta_uva )
summary(regresion_venta)

#REGRESION ALQUILER
alq_uva <- venta_alq_uva %>% 
  filter(transaccion== "Alquiler") #SACAMOS LOS DATOS DE VENTA

regresion_alq = lm(precio_promedio ~ prestamos  , data=alq_uva)
summary(regresion_alq)