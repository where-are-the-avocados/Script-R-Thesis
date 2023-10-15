# ------------------INSTALACIÓN DE PAQUETES DE INVESTIGACIÓN--------------------
install.packages("tidyverse",
                 "lubridate",
                 "forecast",
                 "readxl",
                 "summarytools")

update.packages()
#--------------------REVISIÓN DE SESIÓN DE RSTUDIO------------------------------
sessionInfo()
options(scipen = 99999) # Permite aumentar la cantidad de dígitos de los números en los gráficos

# Fija el ambiente de trabajo del proyecto
setwd("E:/Repo GitHub/Repo-GitHub")

# Entrega el ambiente de trabajo del proyecto
getwd()

#------------------CARGAR LIBRERIAS DE PAQUETES DE INVESTIGACIÓN----------------
library(tidyverse)
library(stargazer)
library(lubridate)
library(forecast)
library(summarytools)

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

# Generalmente, se dividen los datasets en dos partes diferentes:
# el training data, tiene por objetivo estimar los parametros para un método
# de pronóstico, y el test data es para evaluar su exactitud; dado que el
# test data no es utilizado para hacer pronósticos, debería ser de utilidad para
# ser un indicador fiable sobre qué tan bien se ajustan los modelos a los datos.

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

#------------------------------SUMMARYTOOLS------------------------------------#

freq(train$sales)        # Genera una tabla de frecuencias estadísticas para la columna ventas
descr(train)             # Genera una descripción a modo resumen, de los valores estadísticos de todas las columnas
stview(dfSummary(train)) # Resumen general del dataframe, lo generamos en un gráfico con la función stview()
stview(dfSummary(train), file = "resumen.html") # Alternativamente, podemos exportar los datos como un archivo HTML

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

# Histograma de las ventas, distribuidas por su frecuencia
ggplot(train, aes(x = sales)) +
  geom_histogram(col = "black",
                 fill = "steelblue",
                 bins = 30) +
  ggtitle("Histograma de Precios de Venta") + xlab("Ventas") + ylab("Frecuencia") +
  scale_x_continuous(breaks = seq(0, 300, 10)) +
  scale_y_continuous(breaks = seq(0, 120000, 10000),
                     labels = function(x)
                       format(x, big.mark = ".", scientific = FALSE))

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
    y = "Ventas",
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
  geom_point(aes(group = 1), size = 1) +
  geom_smooth(method = "lm", col = "red") + ggtitle("Tasa de crecimiento del Precio de Venta",
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
  group_by(month = floor_date(date, "month")) %>%
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
  scale_x_date(date_labels = "%Y/%b", date_breaks = "3 month") +
  scale_y_continuous(
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
  geom_hline(yintercept = 0,
             col = "blue",
             linetype = "dotted") + scale_x_date(date_labels = "%Y/%b", date_breaks = "3 month") +
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
  geom_line(col = "red", size = 1.5) +
  labs(
    title = "Crecimiento año a año de las ventas",
    x = "Fechas",
    y = "Montos",
    subtitle = "Comprende el total de todas las tiendas al cierre del periodo (31 de diciembre)"
  ) +
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

# Agrupación de ventas de Enero 2015

Enero2015 <- train %>%
  select(date, store, item, sales) %>%
  filter(between(date, as.Date("2015-01-01"), as.Date("2015-01-31"))) %>%
  group_by(Dia = floor_date(date, "day")) %>%
  summarise(Total_Ventas = sum(sales))

print(Enero2015)

ggplot(Enero2015, aes(x = Dia, y = Total_Ventas)) + geom_col() +
  scale_x_date(breaks = Enero2015$Dia,
               date_labels = "%a-%d",
               date_breaks = "day") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Ventas del mes de enero de 2015",
       y = "Ventas",
       x = "Día") +
  scale_y_continuous(breaks = seq(0, 25000, 2000))

Meses2015 <- train %>%
  select(date, store, item, sales) %>%
  filter(between(date, as.Date("2015-01-01"), as.Date("2015-12-31"))) %>%
  group_by(Mes = floor_date(date, "month")) %>%
  summarise(Total_Ventas = sum(sales))

print(Meses2015)

ggplot(Meses2015, aes(x = Mes, y = Total_Ventas)) + geom_col() +
  scale_x_date(breaks = Meses2015$Mes,
               date_labels = "%b",
               date_breaks = "month") +
  labs(title = "Ventas del año 2015",
       y = "Ventas",
       x = "Mes") + scale_y_continuous(labels = function(x)
                                         format(x, big.mark = ".", scientific = FALSE),
                                       breaks=seq(0,1200000,150000)) + theme_bw()

#-------------------------------FORECAST----------------------------------------
#forecast(object, h=ifelse(object$m>1, 2*object$m, 10),
#         level=c(80,95), fan=FALSE, simulate=FALSE, bootstrap=FALSE,
#         npaths=5000, PI=TRUE, lambda=object$lambda, biasadj=NULL, ...)

#object=




#-------------------------------TIENDAS E ITEMS---------------------------------

# Para llevar a cabo la aplicación de forecast, utilizaremos los vectores
# creados en la sección anterior

# Creamos una función que permite formatear las fechas con el mes, con el fin
# de dar gráficos más detallados
Fechas_funcion <- function(x) {
  format(date_decimal(x), "%b-%Y")
}

#-------------EJERCICIO 1

Ventas_TS <- ts(Sum_fecha$sales, # Tomamos la columna ventas del df
                start = 2013,    # Le indicamos que inicia el 2013, ts() calcula automaticamente el final
                frequency = 365) # Como los datos son diarios, le indicamos 365

# Si son datos anuales, frequency = 1
# Si son datos mensuales, es frequency = 12
# Si son datos trimestrales, frequency = 4
# Si son datos semanales, frequency = 52

print(Ventas_TS)

# Generamos un gráfico que compare las ventas diarias entre todos los años
ggseasonplot(Ventas_TS,
             xlab = "Tiempo",
             ylab = "Ventas") +
  labs(title = "Evolución de las ventas diarias",
       subtitle = "Comparación año tras año del total de ventas acumuladas") +
  scale_y_continuous(breaks = seq(0, 100000, 5000)) +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("Enero", "Marzo", "Junio", "Septiembre", "Diciembre")
  ) + scale_color_discrete(name = "Años")

# Ahora revisamos los datos del dataframe para poder generar un pronóstico

# Autocorrelación
ggAcf(Ventas_TS)

# La autocorrelación es una parte fundamental del analisis exploratorio de los datos
# permite identificar los patrones y revisar aleatoriedad en los datos.
# Básicamente es la similitud entre las observaciones en función del desfase temporal
# existente entre cada una de ellas.


# En función del gráfico, podemos establecer que los datos decrecen y crecen de manera
# estacional, con una leve tendencia a aumentar los desfases. No existen variaciones significativas.
# Los altos decrecen levemente.

# Aplicamos test de Dickey-Fuller
# p-valor pequeño -> Rechazar H0 y la serie SÍ es estacionaria
# p-valor > 0,05 -> No rechazar H0 y la seríe NO es estacionaria
tseries::adf.test(diff(Ventas_TS)) #p-value=0.01, es menor a 0.05, por ende la serie sí es estacionaria

# Generamos un pronóstico
autoplot(forecast(Ventas_TS),
         xlab = "Fechas",
         ylab = "Cantidad",
         main = "Pronóstico de las ventas diarias para los próximos dos periodos") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20),labels = Fechas_funcion) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
#-------------EJERCICIO 2

TasaMensual_TS <- ts(Meses$tasa,
                     start = 2013,
                     frequency = 12)

# Aplicamos test de Dickey-Fuller
# p-valor pequeño -> Rechazar H0 y la serie SÍ es estacionaria
# p-valor > 0,05 -> No rechazar H0 y la seríe NO es estacionaria
tseries::adf.test(diff(TasaMensual_TS)) #p-value=0.01, es menor a 0.05, por ende la serie sí es estacionaria

autoplot(decompose(TasaMensual_TS))

autoplot(
  forecast(TasaMensual_TS, h = 12),
  xlab = "Fecha",
  ylab = "Tasa Mensual de Crecimiento",
  main = "Pronóstico de la tasa mensual de crecimiento"
) +
  scale_x_continuous(breaks = seq(2013, 2020, 1)) +
  geom_hline(yintercept = 0,
             col = "slateblue",
             linetype = "dotted") +
  scale_y_continuous(
    breaks = seq(-50, 50, 5),
    labels = function(x)
      paste0(x, "%")
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                     labels = Fechas_funcion) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------------EJERCICIO 2

# Ahora pronosticaremos por partes, en este caso, será
Tienda1 <- train %>% filter(store == 1, item == 1) %>%
  group_by(date = floor_date(date, "day")) %>%
  summarise(Total_de_Ventas = sum(sales))

# Imprimimos el vector
print(Tienda1)

View(Tienda1)
# Generamos una serie de tiempo, es importante que el formato sea ts para
# que forecast pueda usarse

Tienda1_TS <-
  ts(Tienda1$Total_de_Ventas,
     start = c(2013, 1),
     frequency = 365)

print(Tienda1_TS)
# Descomposición de los datos
autoplot(decompose(Tienda1_TS))

# data:
# trend:
# seasonal:
# remainder:

#

checkresiduals(Tienda1_TS)

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


# Para series de tiempo con una alta estacionalidad, se puede utilizar el
# método naive, este consiste en que cada predicción sea igual al último valor
# de la última observación.

# Aplicamos la función naive
autoplot(naive(Tienda1_TS, h = 90))

# Método naive con estacionalidad
autoplot(snaive(Tienda1_TS, h = 90))

# Al ser una serie de tiempo estacionaria, utilizamos la función forecast() la cual
# nos permite pronosticar utilizando el modelo ETS
autoplot(
  forecast(Tienda1_TS, h = 365),
  # Pronosticamos un semestre, es decir 181 días
  xlab = "Fechas",
  ylab = "Ventas",
  main = "Pronóstico del item 1 en la tienda 1"
) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                     labels = Fechas_funcion) + theme(axis.text.x = element_text(angle = 45, hjust = 1))


#-------------EJERCICIO 3

Meses_TS <-
  ts(Meses$sum_of_sales, # Indicamos la columna que queremos pronosticar
     start = 2013,       # Indicamos el año de inicio
     frequency = 12)     # Al ser datos mensuales, la frecuencia es 12

print(Meses_TS) # Imprimimos el dataframe creado

# Este es un ploteo que permite destacar los patrones de estacionalidad, dato a dato.
ggsubseriesplot(Meses_TS) + ylab("Ventas") + xlab("Meses") + ggtitle("Gráfico de subseries estacionales: Ventas mensuales totales") +
  scale_y_continuous(
    breaks = seq(0, 1200000, 100000),
    labels = function(x)
      format(x, big.mark = ".", scientific = FALSE)
  )

# La línea azul representa la media para cada mes. De este modo, la función permite
# detectar patrones de temporada agrupados en totales, en este caso, se observa que
# la estacionalidad se cumple para todos los años, siendo julio el mes con más ventas.

# Aplicamos test de Dickey-Fuller
# p-valor pequeño -> Rechazar H0 y la serie SÍ es estacionaria
# p-valor > 0,05 -> No rechazar H0 y la seríe NO es estacionaria
tseries::adf.test(Meses_TS)

# Revisamos los datos de manera descompuesta
autoplot(decompose(Meses_TS))

# Al verificar que existe estacionalidad, generamos un pronóstico para los próximos dos periodos
autoplot(
  forecast(Meses_TS, h = 12),
  xlab = "Años",
  ylab = "Ventas",
  main = "Pronóstico de ventas agrupadas mes a mes"
) +
  scale_y_continuous(
    breaks = seq(0, 2000000, 100000),
    labels = function(x)
      format(x, big.mark = ".", scientific = FALSE)
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                     labels = Fechas_funcion) + theme(axis.text.x = element_text(angle = 45, hjust = 1))


# --------------EJERCICIO 4


#--------------------------------FINAL------------------------------------------
# Liberación de memoria
gc()

# Limpieza del environment
rm(list = ls())