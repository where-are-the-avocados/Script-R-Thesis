# PRESENTACION DEFENSA

#-----------------------------CARGA PAQUETES-----------------------------------
install.packages("tidyverse", # Paquete para hacer y manipular gráficos y datos
                 "lubridate", # Permite trabajar con date
                 "forecast",  # Permite hacer pronósticos
                 "readxl",     # Funciones para cargar archivos
                 "summarytools",  # Estadística descriptiva
                 "zoo",     # Herramientas para modificar date
                 "ghibli")  # Estética  

# Carga de librerías
library(tidyverse)
library(lubridate)
library(forecast)
library(summarytools)
library(zoo)
library(ghibli)

# Carga de datasets al environment

test_show <- read_csv("Dataset/test.csv")
View(test)

train_show <- read_csv("Dataset/train.csv")
View(train)

# Analisis breve
glimpse(train_show)
head(train_show)
tail(train_show)

# Tendencia Central
mean(train_show$sales)
median(train_show$sales)

# Medidas de Variabilidad
range(train_show$date)
range(train_show$sales)
var(train_show$sales)
sd(train_show$sales)

#-------------------------------- DATA ANALYTICS -------------------------------

# Histograma de ventas
ggplot(train_show, aes(x = sales)) + geom_histogram()

# Creamos un objeto que nos muestre las sales totales por día
Ventas_diarias <-  select(train_show, sales, date) %>%
  group_by(date) %>%
  summarise(Ventas_total = sum(sales))

View(Ventas_diarias)

# Grafico
ggplot(Ventas_diarias, aes(x = date, y = Ventas_total)) +
  geom_line() 

# Lo convertimos a una serie temporal
Ventas_diarias_TS <- ts(Ventas_diarias$Ventas_total, start = 2013, frequency = 365)
print(Ventas_diarias_TS)
View(train)

# Generamos un gráfico que compare las ventas diarias entre todos los años
ggseasonplot(Ventas_diarias_TS) 

# Tests para detectar estacionalidad
tseries::adf.test(diff(Ventas_diarias_TS))

#----------AQUI COMIENZA FORECAST--------------

# Descomposición de la serie temporal
autoplot(decompose(Ventas_diarias_TS))

# Funcion accuracy
accuracy(stlm(Ventas_diarias_TS))
accuracy(arima(Ventas_diarias_TS))

# Funcion checkresiduals
checkresiduals(Ventas_diarias_TS)

# Funcion acf
acf(Ventas_diarias_TS)

# FORECAST

# METODO NAIVE
naive(Ventas_diarias_TS, h = 90)
autoplot(naive(Ventas_diarias_TS, h = 90))

# METODO SNAIVE
snaive(Ventas_diarias_TS, h = 90)
autoplot(snaive(Ventas_diarias_TS, h = 90))

# METODO GENERAL
forecast(Ventas_diarias_TS, h = 90)

autoplot(forecast(Ventas_diarias_TS, h = 90))

# TEST
autoplot(forecast(Ventas_diarias_TS, h = 90)) +
  autolayer(PRUEBA_TS, series="Test dataset")  +
  guides(colour = guide_legend(title = "Información"))

# SEGUNDO EJEMPLO

# Creamos un vector que nos muestre las ventas totales por mes

Ventas_Mensuales <-  select(train_show, sales, date) %>%
  group_by(date = floor_date(date, "month")) %>%
  summarise(Ventas_Mensuales = sum(sales))

View(Ventas_Mensuales)

# Analizamos los datos
ggplot(Ventas_Mensuales, aes(x = date, y = Ventas_Mensuales)) +
  geom_line() 

# Serie de tiempo mensual
Ventas_Mensuales_TS <- ts(Ventas_Mensuales$Ventas_Mensuales, start = 2013, frequency = 12) 
print(Ventas_Mensuales_TS)

# Pronostico serie de tiempo mensual
forecast(Ventas_Mensuales_TS, h=3)
autoplot(forecast(Ventas_Mensuales_TS, h=3))

# MISC
monthdays(Ventas_Mensuales_TS)
