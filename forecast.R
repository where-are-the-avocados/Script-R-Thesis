# ------------------INSTALACIÓN DE PAQUETES DE INVESTIGACIÓN--------------------
install.packages("tidyverse",
                 "lubridate",
                 "forecast",
                 "readxl")

update.packages()
#--------------------REVISIÓN DE SESIÓN DE RSTUDIO------------------------------
sessionInfo()
options(scipen = 99999)

# Fija el ambiente de trabajo del proyecto
setwd("E:/Repo GitHub/Repo-GitHub")

# Entrega el ambiente de trabajo del proyecto
getwd()
#------------------CARGAR LIBRERIAS DE PAQUETES DE INVESTIGACIÓN----------------
library(tidyverse)
library(stargazer)
library(lubridate)
library(forecast)

#-----------------DESARROLLO DEL CÓDIGO PARA LA INVESTIGACIÓN-------------------
# Importación de los Datasets
library(readr)
sample_submission <-
  read_csv(
    "Dataset/sample_submission.csv",
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    na = "NA",
    comment = "#"
  )
View(sample_submission)

test <- read_csv(
  "Dataset/test.csv",
  locale = locale(decimal_mark = ",", grouping_mark = "."),
  na = "NA",
  comment = "#"
)
View(test)

train <- read_csv(
  "Dataset/train.csv",
  locale = locale(decimal_mark = ",", grouping_mark = "."),
  na = "NA",
  comment = "#"
)
View(train)

#-------------------------DESCRIPCIÓN DE Los DATASETS--------------------------#

# Descripción de los archivos

# train.csv - Entrenamiento de los datos
# test.csv - Datos de prueba
# sample_submission.csv - Ventas e ID

# Data fields
# date - (Fecha) Fecha de ventas, no hay efectos por feriados o tiendas cerradas.
# store - (ID Tienda) Store ID
# item - (ID Producto)
# sales - (Ventas) Número de productos vendidos en una tienda específica en una fecha específica.

# Funciones que permiten identificar la estructura del Dataset
head(train)
tail(train)
structure(train)
glimpse(train)
str(train)
dim(train)
nrow(train)
ncol(train)
ls(train)
names(train)
summary(train)
glimpse(summary(train))
names(train)

# Tendencia Central
mean(train$sales)
median(train$sales)

# Medidas de Variabilidad
range(train$date)
var(train$sales)
sd(train$sales)

#------------------------------LIMPIEZA DEL DATASET----------------------------#

# Revisamos si el dataset tiene valores NA
colSums(is.na(train))             # No tiene valores NA
colSums(is.na(test))              # No tiene valores NA
colSums(is.na(sample_submission)) # No tiene valores NA

# Ajustamos las fechas de los datasets al formato de la ISO 8601
test$date <- as.Date(test$date, format = "%Y/%m/%d")
print(test) # La columna date tiene formato <date>

train$date <- as.Date(train$date, format = "%Y/%m/%d")
print(train) # La columna date tiene formato <date>

#----------------------------ANÁLISIS DE LOS DATASETS---------------------------#

# Histograma de las ventas, distribuidas por su precio
ggplot(train, aes(x = sales)) +
  geom_histogram(fill = "slateblue") +
  ggtitle("Histograma de Precios de Venta") + xlab("Precio") + ylab("Cantidad") +
  scale_x_continuous(breaks = seq(0, 300, 10)) +
  scale_y_continuous(breaks = seq(0, 120000, 10000))

# Creamos un vector que nos muestre las ventas totales por día
Sum_fecha <-  select(train, sales, date) %>%
  group_by(date) %>%
  summarise(sales = sum(sales))

# Imprimimos el vector para revisarlo
print(Sum_fecha)

# Convertimos la columnda date a <date>
Sum_fecha$date <- as.Date(Sum_fecha$date, format = "%Y-%m-%d")

# Imprimimos para confirmar que funcionó
print(Sum_fecha)

# Creamos un gráfico que nos muestra el crecimiento de las ventas por fechas
ggplot(Sum_fecha, aes(x = date, y = sales)) +
  geom_line() +
  labs(
    title = "Crecimiento de las ventas",
    x = "Fechas",
    y = "Precio de Venta",
    subtitle = "Se encuentra subdividido por día"
  ) +
  scale_x_date(date_labels = "%Y/%b", date_breaks = "3 month") +
  scale_y_continuous(breaks = seq(0, 50000, 5000)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Creamos un vector que nos muestre la tasa de crecimiento por fecha
Sum_fecha$tasa = c(0, 100 * diff(Sum_fecha$sales) / Sum_fecha[-nrow(Sum_fecha),]$sales)

# Imprimimos el vector con la nueva columna
print(Sum_fecha)

# Creamos un gráfico que nos muestre la nueva columna creada
ggplot(Sum_fecha, aes(x = date, y = tasa)) +
  geom_point(aes(group = 1), size = 1) + ggtitle("Tasa de crecimiento del Precio de Venta",
                                                 subtitle = "Implica la diferencia entre el total de ventas del día, con su antecesor inmediato") +
  xlab("Fechas") + ylab("Tasas") +
  geom_hline(yintercept = 0, col = "blue")  +
  scale_y_continuous(
    breaks = seq(-50, 50, 10),
    labels = function(x)
      paste0(x, "%")
  ) +
  scale_x_date(date_labels = "%Y/%b", date_breaks = "3 month") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Creamos un vector que nos muestre las ventas totales por mes
Meses <-  select(train, sales, date) %>%
  group_by(month = floor_date(date, 'month')) %>%
  summarise(sum_of_sales = sum(sales))

print(Meses)

# Creamos un gráfico que nos muestra el crecimiento de las ventas por fechas
ggplot(Meses, aes(x = month, y = sum_of_sales)) +
  geom_line() +
  labs(
    title = "Crecimiento mes a mes de las ventas",
    x = "Fechas",
    y = "Precio de Venta",
    subtitle = "El lapso se encuentra subdividido por trimestres"
  ) +
  scale_x_date(date_labels = "%Y/%b", date_breaks = "3 month") + scale_y_continuous(
    breaks = seq(450000, 1500000, 100000),
    labels = function(x)
      format(x, big.mark = ".", scientific = FALSE)
  ) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Creamos un vector que nos muestre la tasa de crecimiento por fecha
Meses$tasa = c(0, 100 * diff(Meses$sum_of_sales) / Meses[-nrow(Meses),]$sum_of_sales)

ggplot(Meses, aes(x = month, y = tasa)) +
  geom_line(aes(group = 1)) + ggtitle("Tasa de crecimiento del precio de venta",
                                      subtitle = "Implica la diferencia entre el total de ventas del mes, con su antecesor inmediato") +
  xlab("Fechas") + ylab("Tasas") +
  geom_hline(yintercept = 0) + scale_x_date(date_labels = "%Y/%b", date_breaks = "3 month") +
  scale_y_continuous(
    breaks = seq(-50, 50, 10),
    labels = function(x)
      paste0(x, "%")
  ) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Creamos un vector que nos muestre las ventas totales por mes
Años <-  select(train, sales, date) %>%
  group_by(Año = floor_date(date, "year")) %>%
  summarise(Total_de_Ventas = sum(sales))

print(Años)

# Creamos un gráfico que nos muestra el crecimiento de las ventas por fechas
ggplot(Años, aes(x = Año, y = Total_de_Ventas)) +
  geom_line(col="red", size=1.5) +
  labs(title = "Crecimiento año a año de las ventas",
       x = "Fechas",
       y = "Montos",
       subtitle="Comprende el total de todas las tiendas al cierre del periodo (31 de diciembre)") +
  scale_x_date(date_labels = "%Y", date_breaks = "year") +
  scale_y_continuous(
    labels = function(x)
      format(x, big.mark = ".", scientific = FALSE),
    breaks = seq(7000000, 12000000, 500000)
  ) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Creamos un vector que nos muestre la tasa de crecimiento por fecha
Años$Tasa = c(0, 100 * diff(Años$Total_de_Ventas) / Años[-nrow(Años),]$Total_de_Ventas)

#Imprimimos el vector actualizado
print(Años)

# Creamos un gráfico que nos muestre la tasa de crecimiento de las ventas, año a año
ggplot(Años, aes(x = Año, y = Tasa)) +
  geom_line() + ggtitle("Tasa de crecimiento del precio de venta") +
  xlab("Fechas") + ylab("Tasas") +
  geom_point() + scale_x_date(date_labels = "%Y", date_breaks = "years") +
  scale_y_continuous(
    breaks = seq(0, 20, 2),
    labels = function(x)
      paste0(x, "%")
  ) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------------------------------TIENDAS E ITEMS---------------------------------

# Para llevar a cabo la aplicación de forecast, utilizaremos los vectores
# creados en la sección anterior

#-------------EJERCICIO 1
Ventas_TS <- ts(Sum_fecha$sales,
                start=2013,
                frequency=365)

print(Ventas_TS)

ggseasonplot(Ventas_TS, 
             xlab="Tiempo",
             ylab="Ventas") + 
  labs(title="Evolución de las ventas diarias", 
       subtitle = "Comparación año tras año del total de ventas acumuladas") +
  scale_y_continuous(breaks= seq(0,100000,5000)) +
  scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1),labels=c("Enero","Marzo","Junio","Septiembre","Diciembre"))

plot(forecast(Ventas_TS))
#-------------EJERCICIO 2

Sum_fecha_TS <-
  ts(
    Sum_fecha$tasa,
    start = 2013, 
    frequency = 365
  )
# Aplicamos test de Dickey-Fuller
# p-valor pequeño -> Rechazar H0 y la serie SÍ es estacionaria
# p-valor > 0,05 -> No rechazar H0 y la seríe NO es estacionaria
tseries::adf.test(diff(Sum_fecha_TS))

# Al ser estacionaria aplicamos el modelo ETS para pronosticar
autoplot(forecast(Sum_fecha_TS),
         xlab="Periodos",
         ylab="Tasa",
         main="Pronóstico de las tasas de crecimiento diarias")

#-------------EJERCICIO 3

# Ahora pronosticaremos por partes, en este caso, será
Tienda1 <- train %>% filter(store == 1, item == 1)

# Imprimimos el vector
print(Tienda1)

# Generamos una serie de tiempo, es importante que el formato sea ts para
# que forecast pueda usarse

Tienda1_TS <-
  ts(
    Tienda1$sales,
    start = 2013,
    frequency = 365
  )

# Descomposición de los datos
autoplot(decompose(Tienda1_TS))
# data:
# trend:
# seasonal:
# remainder: 

# 

autoplot(log(Tienda1_TS))

# Esta es la diferencia entre los valores
diff(Tienda1_TS)

# Generamos un plot de las diferencias
autoplot(diff(Tienda1_TS),
         xlab = "Tiempo",
         ylab = "Monto",
         main = "Primera diferencia de los Ingresos Netos")

# Aplicamos test de Dickey-Fuller
# p-valor pequeño -> Rechazar H0 y la serie SÍ es estacionaria
# p-valor > 0,05 -> No rechazar H0 y la seríe NO es estacionaria
tseries::adf.test(diff(Tienda1_TS))

# Al ser una serie de tiempo estacionaria, utilizamos la función forecast() la cual
# nos permite pronosticar utilizando el modelo ETS
autoplot(forecast(Tienda1_TS),
         xlab="Fechas",
         ylab="Ventas",
         main="Pronóstico del item 1 en la tienda 1") 

#-------------EJERCICIO 4

Meses_TS <-  ts(
  Meses$sum_of_sales,
  start = 2013,
  frequency = 12
)

print(Meses_TS)
tseries::adf.test(diff(Meses_TS))
plot(forecast(Meses_TS))

#-------------EJERCICIO 4


#--------------------------------FINAL------------------------------------------
# Liberación de memoria
gc()

# Limpieza del environment
rm(list = ls())
