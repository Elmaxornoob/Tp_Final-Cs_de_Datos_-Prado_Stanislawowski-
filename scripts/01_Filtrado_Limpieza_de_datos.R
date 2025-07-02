library(tidyverse)
library(readxl)
library(stringr)
library(tidygeocoder)
library(sf)
library(leaflet)
library(writexl)

base_venta <- read.csv("C:/Users/JP1/Documents/GitHub/TP-FINAL/raw/precio-venta-deptos.csv" , sep = ";",stringsAsFactors = FALSE)
base_alquiler <- read.csv("C:/Users/JP1/Documents/GitHub/TP-FINAL/raw/precio-alquiler-deptos.csv" , sep = ";",stringsAsFactors = FALSE)


#FILTROS DE LA VENTA

base_venta <- base_venta %>% 
  filter(año>2014) %>% #FILTRO DATOS A PARTIR DE 2014 PARA OBTENER EL MAYOR NUMERO POSIBLE DE DATOS SIN NA
  group_by(comuna , año ) %>% #AGRUPAMOS POR COMUNA Y AÑO
  summarise( #SACAMOS EL PRECIOS PROMEDIO DE CADA AÑO PARA CADA COMUNA
    precio_promedio = mean(precio_prom , na.rm= TRUE) , #TRUE para evitar que el resultado sea NA si hay un NA
    precio_mediana = median(precio_prom , na.rm=TRUE)
  ) %>% 
  mutate(transaccion = "Venta") %>% 
  drop() 




#FILTROS DE ALQUILER
base_alquiler <- base_alquiler %>%
  rename("año" = "anio")

base_alquiler <- base_alquiler %>% 
  filter(año>2014) %>% #FILTRO HASTA 2014 PARA OBTENER EL MAYOR NUMERO POSIBLE DE DATOS SIN NA
  group_by(comuna , año ) %>% #SACAMOS EL PRECIOS PROMEDIO DE CADA AÑO PARA CADA COMUNA
  summarise(
    precio_promedio = mean(precio_prom , na.rm= TRUE)/12 , #DEJAMOS EL ALQUILER MENSUAL EN USD, MENSUAL ES UNA METRICA MAS USADA QUE ANUAL
    precio_mediana = median(precio_prom , na.rm=TRUE)/12
  ) %>% 
  drop() %>% 
  mutate(transaccion = "Alquiler")

base_combinada_transacciones <-full_join(base_venta, base_alquiler , by=NULL , copy=FALSE )


write_xlsx(base_combinada_transacciones, file.path("C:/Users/JP1/Documents/GitHub/TP-FINAL/output", "base_combinada_transacciones.xlsx"))


#ARRANCA EL CODIGO PARA LA DOLARIZACION DEL PRESTAMO UVA

base_prestamos <- read.csv("C:/Users/JP1/Documents/GitHub/TP-FINAL/input/montos-creditos-hipotecarios-uva.csv" , sep = ";",stringsAsFactors = FALSE)
tipo_cambio <- read.csv2("C:/Users/JP1/Documents/GitHub/TP-FINAL/input/Serie_dolar_combinada.csv" , sep = ";",stringsAsFactors = FALSE)

#RENOMBRO PARA PODER HACER UN JOIN
tipo_cambio <- rename(tipo_cambio , año_mes = anio_mes)

#HACEMOS EL JOIN Y JUNTAMOS CREDITOS UVA CON TIPO DE CAMBIO POR MES
prestamo_usd  <- left_join(base_prestamos , tipo_cambio , by="año_mes")  
prestamo_usd <- prestamo_usd %>% 
  mutate(
    monto_pesos = Monto.operado.prestamos.de.UVA..en.miles.de.pesos.*1000, #paso el valro de miles de pesos a pesos
    prestamo_dolar = monto_pesos/ cierre, #generamos columna de prestamos en usd al dividir el monto en pesos por el tc
    año = substr(año_mes, start = 1, stop = 4) #PARA PASAR DE FORMATO "2016-05" A "2016"
  ) 


#Hacemos el monto anual en USD de prestamos 
prestamo_usd<- prestamo_usd %>%
  mutate(año = as.numeric(as.character(año))) %>% 
  select("año_mes" , "año" , "prestamo_dolar") %>% 
  group_by(año) %>%  
  summarise(prestamos = sum(prestamo_dolar))


write_xlsx(prestamo_usd, file.path("C:/Users/JP1/Documents/GitHub/TP-FINAL/output", "monto_prestamos_usd.xlsx"))







