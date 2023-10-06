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
    "Dataset/Dataset2/sample_submission.csv",
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    na = "NA",
    comment = "#"
  )
View(sample_submission)

test <- read_csv(
  "Dataset/Dataset2/test.csv",
  locale = locale(decimal_mark = ",", grouping_mark = "."),
  na = "NA",
  comment = "#"
)
View(test)

train <- read_csv(
  "Dataset/Dataset2/train.csv",
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

#------------------------------------TIENDAS------------------------------------


#----------------------------------FORECASTING----------------------------------

train_sample <- data.table::setDT(train)[store == 1 & item == 45]
print(train_sample)
# Crear una serie de tiempo

Ventas_ts <- ts(train_sample[,c("sales")],     
           start = c(2013,1),
           frequency = 365)

print(Ventas_ts)

autoplot(Ventas_ts)

ggseasonplot(Ventas_ts)

gglagplot(Ventas_ts)
ggAcf(Ventas_ts)

fcnv <- naive(Ventas_ts, h = 90)
autoplot(fcnv)

fcsnv <- snaive(Ventas_ts, h = 90)
autoplot(fcsnv)

fcses <- ses(Ventas_ts, h = 90)
autoplot(fcses)

# Create a training set using subset()
train <- subset(Ventas_ts, end = length(Ventas_ts) - 365)

# Compute SES and naive forecasts, save to fcses and fcnaive
fcses <- ses(train, h = 365)
fcsnaive <- snaive(train, h = 365)

# Calculate forecast accuracy measures
accuracy(fcses, Ventas_ts)


#Running the Seasonal Naive forecasting with the accuracy summary  

accuracy(fcsnaive, Ventas_ts)


fcholt <- holt(Ventas_ts, h =90)
autoplot(fcholt)
checkresiduals(fcholt)

fchws <- hw(Ventas_ts, seasonal = "multiplicative", h = 90)

fcets <- ets(Ventas_ts)
autoplot(forecast(Ventas_ts))
checkresiduals(Ventas_ts)

fcarima <- auto.arima(Ventas_ts)
fcarima %>% forecast(h = 90) %>% autoplot()
checkresiduals(fcarima)
#--------------------------------FINAL------------------------------------------
# Liberación de memoria
gc()

# Limpieza del environment
rm(list = ls())
