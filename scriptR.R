# ------------------INSTALACIÓN DE PAQUETES DE INVESTIGACIÓN--------------------
# install.packages("tidyverse",
#                 "stargazer",
#                 "lubridate",
#                 "forecast",
#                 "readxl")

# update.packages()
#--------------------REVISIÓN DE SESIÓN DE RSTUDIO------------------------------
sessionInfo()
options(scipen = 99999)

#------------------CARGAR LIBRERIAS DE PAQUETES DE INVESTIGACIÓN----------------
library(tidyverse)
library(stargazer)
library(lubridate)
library(forecast)
library(readxl)

#-----------------DESARROLLO DEL CÓDIGO PARA LA INVESTIGACIÓN-------------------
# Importación de los Datasets

HitesActivo <- read_excel("HITES.xlsx", sheet = "activo1")
View(HitesActivo)

HitesPasivo <- read_excel("HITES.xlsx", sheet = "pasivo1")
View(HitesPasivo)

HitesPatrimonio <- read_excel("HITES.xlsx", sheet = "patrimonio1")
View(HitesPatrimonio)

HitesEstResultados <- read_excel("HITES.xlsx", sheet = "resultado1")
View(HitesEstResultados)

ls() # Revisa que todo esté cargado


#-------------------ANÁLISIS DE Los DATASETS DE HITES S.A.---------------------#

# Funciones que permiten identificar la estructura del Dataset
head(HitesActivo)
tail(HitesActivo)
structure(HitesActivo)
glimpse(HitesActivo)
str(HitesActivo)
dim(HitesActivo)
nrow(HitesActivo)
ncol(HitesActivo)
ls(HitesActivo)
names(HitesActivo)
summary(HitesActivo)
glimpse(summary(HitesActivo))
names(HitesActivo)

# Tendencia Central
mean(HitesActivo$`Activo total`)
median(HitesActivo$`Activo total`)

# Medidas de Variabilidad
range(HitesActivo$`Activo total`)
var(HitesActivo$`Activo total`)
sd(HitesActivo$`Activo total`)

#------------------------------LIMPIEZA DEL DATASET-----------------------------


#------------------------------ANALISIS DE DATOS--------------------------------
#--------------------------RATIOS FINANCIEROS-----------------------------------



#------------------ANALISIS DEL DATASET DE ACTIVOS DE HITES S.A.----------------

# Evolución del Activo Total de Hites S.A.
ggplot(data = HitesActivo, aes(y = `Activo total`, x = Fechas)) +
  geom_line(col = "#00f8ff", size = 0.5) + geom_point(col = "#00bfc4", size = 2) +
  ggtitle("Evolución del Activo Total de Hites S.A.",
          subtitle = "Corresponde a las variaciones en el monto para los intervalos entre el año 2009 y el 2023") +
  theme(
    axis.text.x = element_text(
      angle = 35,
      vjust = 1,
      hjust = 1
    ),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    plot.subtitle = element_text(
      size = 8,
      face = "italic",
      vjust = -1
    )
  ) +
  xlab("Fecha de corte") + ylab("Monto") +
  scale_y_continuous(
    breaks = seq(0, 500000000, 50000000),
    labels = function(x)
      format(
        x,
        big.mark = ".",
        decimal.mark = ",",
        scientific = FALSE
      )
  ) + scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 year")

# Ajustamos la fecha del dataset al formato de la ISO 8601
HitesActivo$Fechas = as.Date(HitesActivo$Fechas, format = "%Y/%m/%d")

# Guardamos la secuencia de tiempo, la frecuencia indica que es trimestral,
# y que es 4 veces al año, empieza el primer trimestre del 2010 y termina el
# primer trimestre del 2023

Efectivo_fcst = ts(
  HitesActivo$Efectivo,
  start = c(2010, 1),
  end = c(2023, 1),
  frequency = 4
)

# Muestra de resultados
print(Efectivo_fcst)

# Generamos un pronóstico para la evolución de los activos totales
autoplot(
  forecast(Efectivo_fcst),
  xlab = "Tiempo",
  ylab = "Monto",
  main = "Pronóstico para la cuenta Efectivo de Hites S.A."
) + scale_y_continuous(
  labels = function(x)
    format(x, big.mark = ".", scientific = FALSE)
) +  scale_x_continuous(
  breaks = c(
    2010,
    2011,
    2012,
    2013,
    2014,
    2015,
    2016,
    2017,
    2018,
    2019,
    2020,
    2021,
    2022,
    2023,
    2024,
    2025
  )
)

#-----------ANALISIS DEL DATASET DE PASIVOS DE HITES S.A.-----------------------

#-----------ANALISIS DEL DATASET DE PATRIMONIO DE HITES S.A.--------------------

#-----------ANALISIS DEL DATASET DE ESTADO DE RESULTADOS DE HITES S.A.----------

# Extraemos los Ingresos netos, partiendo por el año 2010 con una frecuencia de
# cuatro veces por año.

Ing_net_ts <-
  ts(HitesEstResultados$`+Ingresos netos...5`, 2010, frequency = 4)

# Imprimimos los datos antes de modelarlos
print(Ing_net_ts)

# Gráfico de la evolución de los ingresos netos de Hites S.A.
ggplot(data = HitesEstResultados, aes(y = `+Ingresos netos...5`,
                                      x = Fechas)) +
  geom_line(col = "#addd8e", size = 0.5) +  geom_point(col = "#85aa6d", size = 2) +
  ggtitle("Evolución de los Ingresos Netos de Hites S.A.",
          subtitle = "Corresponde a las variaciones en el monto para los intervalos entre el año 2010 y 2022") +
  theme(
    axis.text.x = element_text(
      angle = 35,
      vjust = 1,
      hjust = 1
    ),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    plot.subtitle = element_text(
      size = 8,
      face = "italic",
      vjust = -1
    )
  ) + xlab("Fecha de corte") +
  ylab("Monto")  + scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "years")

# Generamos un gráfico que muestra un pronóstico para el movimiento de los
# ingresos netos de Hites S.A.

autoplot(forecast(Ing_net_ts),
         xlab = "Periodo",
         ylab = "Monto",
         main = "Pronóstico para los Ingresos Netos de Hites S.A.") +
  scale_y_continuous(
    labels = function(x)
      format(x, big.mark = ".", scientific = FALSE)
  )

#--------------------------------FINAL------------------------------------------
# Liberación de memoria
gc()
