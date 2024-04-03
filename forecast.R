# ------------------INSTALACIÓN DE PAQUETES DE INVESTIGACIÓN--------------------

# install.packages("tidyverse", # Paquete para hacer y manipular graficos y datos
                 # "lubridate", # Permite trabajar con fechas
                 # "forecast",  # Permite hacer pronósticos
                 # "readxl",     # Funciones para cargar archivos
                 # "summarytools",  # Estadística descriptiva
                 # "zoo")    # Herrramientas para modificar fechas

# update.packages()

#--------------------REVISIÓN DE SESIÓN DE RSTUDIO------------------------------
sessionInfo()

options(scipen = 99999) # Permite aumentar la cantidad de dígitos de los números en los gráficos

# Fija el ambiente de trabajo del proyecto
setwd("E:/Repo GitHub/Repo-GitHub")

# Entrega el ambiente de trabajo del proyecto
getwd()

#------------------CARGAR LIBRERIAS DE PAQUETES DE INVESTIGACIÓN----------------
library(tidyverse)
library(lubridate)
library(forecast)
library(summarytools)
library(zoo)
library(ghibli)
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
# el training data, tiene por objetivo estimar los parámetros para un método
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
range(train$sales)
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

# Extracción del año y el mes del año
# Estas funciones del paquete zoo serán de utilidad más adelante, en conjunto
# con el paquete dplyr, se agruparán los datos en función del mes y del año.

print(train) # Observamos que se han creado dos columnas con información extraida de la columna date

# Función que permite renombrar las columnas
colnames(train) <- c("Fechas", "Tienda", "Producto", "Ventas")

print(train)

train$Año = year(train$Fechas)        # Devuelve el año de una fecha
train$Mes = as.yearmon(train$Fechas)  # Devuelve el mes de una fecha

# Creamos una función que permite formatear las fechas con el mes, con el fin
# de dar gráficos más detallados

Fechas_funcion <- function(x) {
  format(date_decimal(x), "%b-%Y")
}

# Creamos otra forma para representar las fechas de otro modo

Fechas_funcion_2 <- function(x) {
  format(date_decimal(x), "%Y-%m-%d")
}

# En caso de necesitar otra forma, simplemente crear otra función y cambiar el formato
# y retirar los #

# Fechas_funcion_2 <- function(x) {                 
# format(date_decimal(x), "[INSERTAR ACA]")
# }

# Creamos otra función que nos permite añadir un "." como separador de miles
# en los gráficos que se crearán a futuro. También es posible añadir el mismo 
# código de la función, no obstante, usarla ahorra espacio y mejora la aplicabilidad.

separadores_funcion = function(x) {
  format(x, big.mark = ".", scientific = FALSE)
}

#----------------------------ANÁLISIS DE LOS DATASETS---------------------------#

# Histograma de las ventas, distribuidas por su frecuencia
ggplot(train, aes(x = Ventas)) +
  geom_histogram(col = "black",
                 fill = "steelblue",
                 bins = 30) +
  ggtitle("Histograma de Ventas") + xlab("Ventas") + ylab("Frecuencia") +
  scale_x_continuous(breaks = seq(0, 300, 10)) +
  scale_y_continuous(
    breaks = seq(0, 120000, 10000),
    labels = function(x)
      format(x, big.mark = ".", scientific = FALSE)) 

# Gráfico de línea de las ventas, [ESTE GRÁFICO TARDA MUCHO EN CARGAR]
ggplot(train, aes(y = Ventas, x = Fechas)) +
  geom_line() +
  ggtitle("Ventas") + 
  scale_y_continuous(breaks = seq(0, 300, 10))

# Creamos un objeto que nos muestre las ventas totales por día
Sum_fecha <-  select(train, Ventas, Fechas) %>%
  group_by(Fechas) %>%
  summarise(Ventas = sum(Ventas))

# Imprimimos el vector para revisarlo
print(Sum_fecha)

# Convertimos la columna date a <date>
Sum_fecha$date <- as.Date(Sum_fecha$Fechas, format = "%Y-%m-%d")

# Imprimimos para confirmar que funcionó
print(Sum_fecha)

# Creamos un gráfico que nos muestra el crecimiento de las ventas por fechas
ggplot(Sum_fecha, aes(x = Fechas, y = Ventas)) +
  geom_line() +
  labs(
    title = "Crecimiento de las ventas",
    x = "Fechas",
    y = "Ventas",
    subtitle = "Se encuentra subdividido por día"
  ) +
  scale_x_date(date_labels = "%Y/%b", date_breaks = "3 month") +
  scale_y_continuous(breaks = seq(0, 50000, 5000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Creamos un vector que nos muestre la tasa de crecimiento por fecha
Sum_fecha$Tasa = c(0, 100 * diff(Sum_fecha$Ventas) / Sum_fecha[-nrow(Sum_fecha),]$Ventas)

# Imprimimos el vector con la nueva columna
print(Sum_fecha)

# Creamos un gráfico que nos muestre la nueva columna creada
ggplot(Sum_fecha, aes(x = Fechas, y = Tasa)) +
  geom_point(aes(group = 1), size = 1) +
  geom_smooth(method = "lm") + ggtitle("Tasa de crecimiento de las ventas",
                                       subtitle = "Implica la diferencia entre el total de ventas del día, con su antecesor inmediato") +
  xlab("Fechas") + ylab("Tasas") +
  geom_hline(yintercept = 0)  +
  scale_y_continuous(
    breaks = seq(-50, 50, 10),
    labels = function(x)
      paste0(x, "%")
  ) +
  scale_x_date(date_labels = "%Y/%b", date_breaks = "3 month") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Creamos un vector que nos muestre las ventas totales por mes
Meses <-  select(train, Ventas, Fechas) %>%
  group_by(Mes = floor_date(Fechas, "month")) %>%
  summarise(Total_de_Ventas = sum(Ventas))

print(Meses)

# Creamos un gráfico que nos muestra el crecimiento de las ventas por fechas
ggplot(Meses, aes(x = Mes, y = Total_de_Ventas)) +
  geom_line() +
  labs(
    title = "Crecimiento mes a mes de las ventas",
    x = "Fechas",
    y = "Ventas",
    subtitle = "Comprende el lapso entre el 2013 al 2017"
  ) +
  scale_x_date(date_labels = "%Y/%b", date_breaks = "3 month") +
  scale_y_continuous(
    breaks = seq(450000, 1500000, 100000),
    labels = function(x)
      format(x, big.mark = ".", scientific = FALSE)
  ) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Creamos un vector que nos muestre la tasa de crecimiento por fecha
Meses$Tasa = c(0, 100 * diff(Meses$Total_de_Ventas) / Meses[-nrow(Meses),]$Total_de_Ventas)

ggplot(Meses, aes(x = Mes, y = Tasa)) +
  geom_line(aes(group = 1), color = "#ae93be") + ggtitle("Tasa de crecimiento mensual de las ventas",
                                                         subtitle = "Implica la diferencia entre el total de ventas del mes, con su antecesor inmediato") +
  xlab("Fechas") + ylab("Tasas") +
  geom_hline(
    yintercept = 0,
    col = "#00afaf",
    size = 1,
    linetype = "dotted"
  ) + scale_x_date(date_labels = "%Y/%b", date_breaks = "3 month") +
  scale_y_continuous(
    breaks = seq(-50, 50, 10),
    labels = function(x)
      paste0(x, "%")
  ) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Creamos un vector que nos muestre las ventas totales por mes
Años <-  select(train, Ventas, Fechas) %>%
  group_by(Año = floor_date(Fechas, "year")) %>%
  summarise(Total_de_Ventas = sum(Ventas))

print(Años)

# Creamos un gráfico que nos muestra el crecimiento de las ventas por fechas
ggplot(Años, aes(x = Año, y = Total_de_Ventas)) +
  geom_line(col = "red", size = 1) +
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
  geom_line() + ggtitle("Tasa de crecimiento de las ventas") +
  xlab("Fechas") + ylab("Tasas") +
  geom_point() + scale_x_date(date_labels = "%Y", date_breaks = "years") +
  scale_y_continuous(
    breaks = seq(0, 20, 2),
    labels = function(x)
      paste0(x, "%")
  ) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Agrupación de ventas de Enero 2015
Enero2015 <- train %>%
  select(Fechas, Tienda, Producto, Ventas) %>%
  filter(between(Fechas, as.Date("2015-01-01"), as.Date("2015-01-31"))) %>%
  group_by(Dia = floor_date(Fechas, "day")) %>%
  summarise(Total_Ventas = sum(Ventas))

# Imprimimos el objeto
print(Enero2015)

# Gráfico de las ventas totales de enero 2015
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
  select(Fechas, Tienda, Producto, Ventas) %>%
  filter(between(Fechas, as.Date("2015-01-01"), as.Date("2015-12-31"))) %>%
  group_by(Mes = floor_date(Fechas, "month")) %>%
  summarise(Total_Ventas = sum(Ventas))

print(Meses2015)

ggplot(Meses2015, aes(x = Mes, y = Total_Ventas)) + geom_col() +
  scale_x_date(breaks = Meses2015$Mes,
               date_labels = "%b",
               date_breaks = "month") +
  labs(title = "Ventas del año 2015",
       y = "Ventas",
       x = "Mes") + scale_y_continuous(
         labels = function(x)
           format(x, big.mark = ".", scientific = FALSE),
         breaks = seq(0, 1200000, 150000)
       ) + theme_bw()

# Ventas por tienda
unique(train$Tienda)
Tiendas <- aggregate(Ventas ~ Tienda + Año, train, mean)
print(Tiendas)

# Tienda Dataset
Tienda_Dataset <- Tiendas

print(Tienda_Dataset)

# Cambiamos la columna a character para diferenciarlo unas de otras fácilmente
Tienda_Dataset$Tienda <- as.character(Tienda_Dataset$Tienda)

# Crecimiento promedio de las tiendas
ggplot(Tienda_Dataset, aes(x = Año, y = Ventas, group = Tienda)) +
  geom_line(aes(col = Tienda),
            size = 1,
            show.legend = TRUE) +
  labs(title = "Crecimiento promedio de las tiendas entre los años 2013 a 2017",
       y = "Ventas") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), labels = Fechas_funcion) 

# Ventas por item
unique(train$Producto)

Productos <- aggregate(Ventas ~ Producto + Año, train, mean)

print(Productos)

# Cambiamos la columna a character para diferenciarlo unas de otras fácilmente
Productos_Dataset <- Productos
Productos_Dataset$Producto <-
  as.character(Productos_Dataset$Producto)

# Generamos un gráfico
ggplot(Productos_Dataset, aes(group = Producto)) +
  geom_line(aes(x = Año, y = Ventas, col = Producto),
            size = 1,
            show.legend = TRUE) +
  labs(title = "Crecimiento de las ventas de productos entre el periodos 2013 - 2017",
       y = "Ventas") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), labels = Fechas_funcion) 
#-------------------------------FORECASTING---------------------------------

# Para llevar a cabo la aplicación de forecast, utilizaremos los vectores
# creados en la sección anterior

#-------------EJERCICIO 1

Ventas_TS <- ts(Sum_fecha$Ventas, # Tomamos la columna ventas del df
                start = 2013,    # Le indicamos que inicia el 2013, ts() calcula automaticamente el final
                frequency = 365) # Como los datos son diarios, le indicamos 365

# Si son datos anuales, frequency = 1
# Si son datos trimestrales, frequency = 4
# Si son datos mensuales, es frequency = 12
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

Pacf(Ventas_TS)
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

# Descomposición del vector
autoplot(decompose(Ventas_TS))

# Generamos un pronóstico
autoplot(forecast(Ventas_TS),
         xlab = "Fechas",
         ylab = "Cantidad",
         main = "Pronóstico de las ventas diarias para los próximos dos periodos") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20), labels = Fechas_funcion) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------------EJERCICIO 2

TasaMensual_TS <- ts(Meses$Tasa,
                     start = 2013,
                     frequency = 12)

# Aplicamos test de Dickey-Fuller
# p-valor pequeño -> Rechazar H0 y la serie SÍ es estacionaria
# p-valor > 0,05 -> No rechazar H0 y la seríe NO es estacionaria
tseries::adf.test(diff(TasaMensual_TS)) #p-value=0.01, es menor a 0.05, por ende la serie sí es estacionaria

# Descomponemos la serie temporal
autoplot(decompose(TasaMensual_TS))

# Revisamos el nivel de residuo existente en la serie temporal
checkresiduals(TasaMensual_TS)

# Generamos el pronóstico para los próximos doce meses
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

#-------------EJERCICIO 3

# Ahora pronosticaremos por partes, en este caso, será
Tienda1 <- train %>% filter(Tienda == 1, Producto == 1) %>%
  group_by(Fecha = floor_date(Fechas, "day")) %>%
  summarise(Total_de_Ventas = sum(Ventas))

# Imprimimos el vector
print(Tienda1)

View(Tienda1)

# Suavizado
ggplot(Tienda1, aes(y = Total_de_Ventas, x = Fecha)) +
  geom_point() + geom_smooth() +
  labs(
    title = "Ventas diarias del item 1 en la tienda 1 con suavizado",
    y = "Total de Ventas",
    x = "Fechas",
    subtitle = "Utiliza geom_point y geom_smooth"
  ) +
  scale_x_date(
    date_labels = "%Y/%b",
    labels = Tienda1$Fecha,
    breaks = scales::pretty_breaks(n = 10)
  )

# Caja y bigote
ggplot(Tienda1, aes(y = Total_de_Ventas)) + geom_boxplot(fill = "orange") +
  labs(
    title = "Diagrama de caja y bigotes",
    subtitle = "Corresponde a las ventas diarias",
    x = "Total de ventas",
    y = "Densidad"
  ) + theme_minimal()

# Generamos una serie de tiempo, es importante que el formato sea ts para
# que forecast pueda usarse
Tienda1_TS <-
  ts(Tienda1$Total_de_Ventas,
     start = c(2013, 1),
     frequency = 365)

print(Tienda1_TS)

# Descomposición de los datos
autoplot(decompose(Tienda1_TS))

# Revisamos el nivel de residuo
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
autoplot(naive(Tienda1_TS, h = 90),
         main = "Pronóstico para los próximos tres meses") +
  labs(subtitle = "Calculado mediante el método naive",
       x = "Tiempo",
       y = "Ventas")

# Método naive con estacionalidad
autoplot(snaive(Tienda1_TS, h = 90, lambda = 0),
         main = "Pronóstico para los próximos tres meses") +
  labs(subtitle = "Calculado mediante el método Snaive",
       x = "Tiempo",
       y = "Ventas")


# Al ser una serie de tiempo estacionaria, utilizamos la función forecast() la cual
# nos permite buscar el mejor método de pronóstico
autoplot(
  forecast(Tienda1_TS, h = 365),
  xlab = "Fechas",
  ylab = "Ventas",
  main = "Pronóstico del item 1 en la tienda 1"
) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                     labels = Fechas_funcion) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------------EJERCICIO 4

Meses_TS <-
  ts(Meses$Total_de_Ventas,
     # Indicamos la columna que queremos pronosticar
     start = 2013,
     # Indicamos el año de inicio
     frequency = 12)     # Al ser datos mensuales, la frecuencia es 12

print(Meses_TS) # Imprimimos el dataframe creado

# Este es un ploteo que permite destacar los patrones de estacionalidad, dato a dato.
ggsubseriesplot(Meses_TS) + ylab("Ventas") + xlab("Meses") +
  ggtitle("Gráfico de subseries estacionales: Ventas mensuales totales") +
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

# --------------EJERCICIO 5
# Creamos un subset con el item 5
# Usaremos este item porque tiene un evento en particular que rompe con la estacionalidad
# De este modo, mostraremos la comparación entre los distintos métodos de pronóstico.

Tienda2 <- train %>% filter(Producto == 5) %>%
  group_by(Semanas = floor_date(Fechas, "week")) %>% # Son las ventas semanales
  summarise(Venta_Semanal = sum(Ventas))

# Creamos una serie de tiempo para pronosticar
Tienda2_TS <- ts(Tienda2$Venta_Semanal,
                 start = 2013,
                 frequency = 52) # Este es el comando para semanas

# Creamos un pronostico con la función más básica de forecast
autoplot(
  forecast(Tienda2_TS, h = 52),
  main = "Pronóstico de ventas semanales",
  xlab = "Tiempo",
  ylab = "Ventas semanales",
  subtitle = "Corresponde únicamente al producto 5"
) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20),
                     labels = Fechas_funcion) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 2000, 250))

# Con snaive, podemos visualizar que el pronostico es diferente, dado que
# basa los movimientos futuros en relación al último periodo
autoplot(
  snaive(Tienda2_TS, h = 52),
  main = "Pronóstico de ventas semanales con Snaive",
  xlab = "Tiempo",
  ylab = "Ventas semanales"
) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20),
                     labels = Fechas_funcion) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 5000, 250)) +
  labs(subtitle = "Corresponde únicamente al producto 5")

#----------------------EJERCICIO 6

# Extraemos las ventas trimestrales
Tienda3 <- train %>%
  group_by(Trimestre = ceiling_date(Fechas, "quarter")) %>% # Trimestrales
  summarise(Ventas_Trimestrales = sum(Ventas))

# Imprimimos el valor
print(Tienda3)

# Conocemos mejor los datos
ggplot(Tienda3, aes(x = Trimestre, y = Ventas_Trimestrales)) +
  geom_line(col = "darkblue") +
  scale_x_date(date_labels = "%Y/%b", breaks = Tienda3$Trimestre) +
  scale_y_continuous(breaks = seq(0, 3000000, 250000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Ventas Trimestrales", title = "Evolución de las ventas trimestrales") +
  geom_point(col = "darkblue") +
  scale_y_continuous(
    labels = function(x)
      format(x, big.mark = ".", scientific = FALSE),
    breaks = seq(1500000, 3250000, 250000)
  )

# Caja y bigote
ggplot(Tienda3, aes(y = Ventas_Trimestrales)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Diagrama de caja y bigote de las ventas trimestrales",
       x = "Densidad", y = "Ventas") +
  scale_y_continuous(breaks = seq(0, 3000000, 250000))

# Creamos una serie de tiempo
Tienda3_TS <- ts(Tienda3$Ventas_Trimestrales,
                 start = 2013,
                 frequency = 4)

# Imprimimos
print(Tienda3_TS)

# Revisamos los niveles de residuo
checkresiduals(Tienda3_TS)

# Descomponemos la serie temporal
autoplot(decompose(Tienda3_TS))

# Obtenemos la primera diferencia
autoplot(diff(Tienda3_TS))

# Creamos un pronóstico
autoplot(forecast(Tienda3_TS))

# Creamos un pronóstico mejorado
autoplot(forecast(Tienda3_TS, h = 4),
         main = "Pronóstico de ventas trimestrales") +
  scale_y_continuous(
    labels = function(x)
      format(x, big.mark = ".", scientific = FALSE)
  ) + labs(y = "Ventas trimestrales", x = "Tiempo",
           subtitle = "Contempla un pronóstico para los próximos cuatro trimestres") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15), labels = Fechas_funcion) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#------------EJERCICIO 7

# Dataset a utilizar
print(Meses)

# Creamos una serie de tiempo de la tasa de crecimiento mensual, tomando un
# corte desde enero del año 2013 hasta diciembre del año 2015
Tasas_Mes_TS <- ts(
  Meses$Tasa,
  start = c(2013, 1),
  end = c(2015, 12),
  frequency = 12
)

# Imprimimos el dataset
print(Tasas_Mes_TS)

# Descomponemos el dataset
autoplot(decompose(Tasas_Mes_TS))

# Ajustamos el modelo
# Lo que buscamos es predecir los próximos dos años y compararlos con los datos
# reales, de este modo, podremos corroborar el desempeño que tiene la función
# forecast.
autoplot(forecast(auto.arima(Tasas_Mes_TS)))

# Comparamos la predicción con los datos actuales, para ello 
# extraemos lo restante del dataset
Tasas_Mes2016 <- Meses[Meses$Mes >= "2016-01-01", ]

# Revisamos el objeto nuevo
print(Tasas_Mes_TS_2)

# Con dplyr agrupamos los datos de manera mensual, al igual que el objeto previamente
# mostrado
Tasas_Mes2016_2 <-
  select(Tasas_Mes2016, Mes, Total_de_Ventas, Tasa) %>%
  group_by(Mes = floor_date(Mes, "month")) %>%
  summarise(Total_de_Ventas = sum(Total_de_Ventas))

# Creamos una columna con la tasa de crecimiento
Tasas_Mes2016_2$Tasa = c(0, 100 * diff(Tasas_Mes2016_2$Total_de_Ventas) / Tasas_Mes2016_2[-nrow(Tasas_Mes2016_2),]$Total_de_Ventas)

# La imprimimos
print(Tasas_Mes2016_2)

# Dicho objeto debe ser pasado a una serie temporal
Tasas_Mes_TS_2 <- ts(Tasas_Mes2016_2$Tasa,
                     start = 2016,
                     frequency = 12)

# Decomposición de la serie temporal
autoplot(decompose(Tasas_Mes_TS_2))

# La imprimimos
print(Tasas_Mes_TS)

#-------------------------------COMPARACIÓN---------------------------------------
# Generamos el pronóstico para el primer objeto, el que comprende entre el
# 2013 hasta el año 2015
autoplot(forecast(auto.arima(Tasas_Mes_TS)))

gglagplot(Tasas_Mes_TS)

# Esta función permite identificar si un objeto es un forecast o no
is.forecast(Tasas_Mes_TS) # Arroja falso

# Función Acf
Acf(Tasas_Mes_TS)

# Esta función permite ver cuantos dias tiene cada mes de la serie de tiempo 
monthdays(Tasas_Mes_TS)

# Superponemos los datos reales versus el pronóstico
autoplot(forecast(auto.arima(Tasas_Mes_TS))) +
  autolayer(Tasas_Mes_TS_2) +
  labs(
    x = "Fechas",
    y = "Tasas de crecimiento",
    title = "Comparación de la tasa de crecimiento mensual real versus pronosticada",
    subtitle = "Fue realizado aplicando el modelo ARIMA(0,0,0)(0,1,0)"
  ) +
  scale_y_continuous(
    breaks = seq(-50, 50, 10),
    labels = function(x)
      paste0(x, "%")
  ) + theme(legend.position = "none")

# ------------------------FUNCIONES EXTRA

# Muestra el rango y mediana de cada mes
ggmonthplot(Tienda2_TS)

# 
ggtsdisplay(Tienda3_TS)

# Genera histograma de la serie temporal
gghistogram(Tasas_Mes_TS_2)

# Distribución de los meses según orden
monthdays(Meses_TS)

# Otro ejemplo según orden
monthdays(TasaMensual_TS)

# Entrega información sobre la media
meanf(Meses_TS)
autoplot(meanf(Meses_TS))

#  Entrega información en texto del dato, la tendencia la estacionalidad y el desecho
mstl(TasaMensual_TS)

#  Aplica test de ocsb
ocsb.test(Meses_TS)

# Devuelve una matriz con variables de prueba
seasonaldummy(Tienda1_TS)

# Devuelve si un objeto es acf
is.acf(TasaMensual_TS)

# Permite generar plots de forecast, da el mismo resultado que la función autoplot()
geom_forecast()

# Random walk forecast
rwf(Meses_TS)
autoplot(rwf(Meses_TS))

# ¿Es forecast?
Objeto <- forecast(Ventas_TS)
is.forecast(Objeto)

# Devuelve el periodo de la frecuencia dominante de una serie temporal. 
# Para datos estacionales el período estacional. 
# Para datos cíclicos, devuelve la longitud media del ciclo.
findfrequency(Ventas_TS)

# -------------------------------------MISC-------------------------------------
install.packages("ghibli")
library(ghibli)

Productos <- train %>% group_by(Tienda) %>% 
  arrange(Tienda) 

print(Productos)

ggplot(Productos, aes(x=Tienda,
                      y=Ventas,
                      group=Tienda,
                      fill=Tienda)) + 
  geom_boxplot() + labs(title="Comparación de ventas por tienda",
                        subtitle="Aplicando gráfico de caja y bigote") +
  scale_x_continuous(breaks=seq(0, 10, 1)) + theme(legend.position = "none") +
  scale_fill_ghibli_c("PonyoMedium")

autoplot(stl(Meses_TS, s.window = 'periodic'), ts.colour = 'red')

# Descomposición de la serie de tiempo. Se almacena en el objeto fit
fit <- decompose(Meses_TS, type='additive')

# Lo graficamos
autoplot(fit) + theme_bw()

# Utilizamos la función autolayer()
autoplot(Meses_TS, series="Serie tiempo") + 
  autolayer(trendcycle(fit), series="Tendencia") +
  labs(title = "Evolución de las ventas mensuales con tendencia",      
       x = "Tiempo",
       y = "Ventas",
       subtitle="Incorpora los datos reales con la tendencia, obtenida a través de la función decompose()") + 
  theme_minimal() + scale_y_continuous(
    labels = separadores_funcion)

# Serie temporal con ajuste estacional
autoplot(Meses_TS, series="Serie temporal") + 
  autolayer(seasadj(fit), series="Estacionalidad ajustada") +
  labs(title = "Evolución de las ventas mensuales con tendencia",      
       x = "Tiempo",
       y = "Ventas",
       subtitle="Incorpora los datos reales con la estacionalidad ajustada, obtenida a través de la función decompose()") + 
  theme_minimal() + scale_y_continuous(
    labels = separadores_funcion)

# Función para determinar si es ACF
is.acf(Acf(Meses_TS))

# Medición de precisión de los pronósticos
accuracy(ets(Meses_TS))
accuracy(auto.arima(Meses_TS))
#---------------------------------TESTEO DE LOS DATOS---------------------------
# Esta es una segunda oportunidad donde el usuario puede medir la factibilidad
# de los pronósticos en base a los datos de test. Que son los próximos periodos 
# en cuestión.
View(test)

# Funciones que permiten identificar la estructura del Dataset
head(test)
tail(test)
structure(test)
glimpse(test)
str(test)
dim(test)
nrow(test)
ncol(test)
ls(test)
names(test)
summary(test)
glimpse(summary(test))
names(test)

# Reutilizamos el forecast previo para conocer el pronóstico de los siguientes 
# tres meses en adelante, vale decir, enero, febrero y marzo del año 2018.

# Guardamos el ejemplo del forecast como un objeto, dado que trabajaramos con 
# él más adelante

forecast_ejemplo <-  forecast(Ventas_TS, h=90)
print(forecast_ejemplo)

sales_meanf <- meanf(Ventas_TS, h=90)
print(sales_meanf)

sales_rwf <- rwf(Ventas_TS, h=90)
print(sales_rwf)

sales_snaive <- snaive(Ventas_TS, h=90)
print(sales_rwf)

# Aplicamos la prueba de test para corroborar que efectivamente existe un 
# forecasting debidamente aplicado a los datos.

autoplot(forecast_ejemplo) +
  autolayer(sales_meanf, series = "Media", PI = FALSE) +
  autolayer(sales_rwf, series = "Naive", PI = FALSE) +
  autolayer(sales_snaive, series ="Snaive", PI = FALSE) +
  labs(x="Tiempo",
       y="Ventas")  +
  guides(colour=guide_legend(title=" "))  +
  scale_y_continuous(
    breaks = seq(0, 50000, 5000),
    labels = separadores_funcion) 

# Medimos la precisión del pronóstico
accuracy(sales_meanf)
accuracy(sales_rwf)
accuracy(sales_snaive)

# MUESTRA 2

print(sample_submission)
print(test)

#--------------------------------FINAL------------------------------------------
# Liberación de memoria
gc()

# Limpieza del environment
# rm(list = ls())

