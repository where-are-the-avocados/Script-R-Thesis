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

library(readxl)
Hites_EEFF <- read_excel("Hites_EEFF.xlsx",
                         sheet = "activo1")
View(Hites_EEFF)

ls() # Revisa que todo esté cargado

#-------------------ANÁLISIS DE Los DATASETS DE HITES S.A.---------------------#

# Funciones que permiten identificar la estructura del Dataset
head(Hites_EEFF)
tail(Hites_EEFF)
structure(Hites_EEFF)
glimpse(Hites_EEFF)
str(Hites_EEFF)
dim(Hites_EEFF)
nrow(Hites_EEFF)
ncol(Hites_EEFF)
ls(Hites_EEFF)
names(Hites_EEFF)
summary(Hites_EEFF)
glimpse(summary(Hites_EEFF))
names(Hites_EEFF)

# Tendencia Central
mean(Hites_EEFF$`Activo total`)
median(Hites_EEFF$`Activo total`)

# Medidas de Variabilidad
range(Hites_EEFF$`Activo total`)
var(Hites_EEFF$`Activo total`)
sd(Hites_EEFF$`Activo total`)

#------------------------------LIMPIEZA DEL DATASET-----------------------------
# Ajustamos la fecha del dataset al formato de la ISO 8601
Hites_EEFF$Fechas = as.Date(Hites_EEFF$Fechas, format = "%Y/%m/%d")

seq(as.Date("2010-03-31"), as.Date("2023-03-31"), by = "quarter")
#------------------------------ANALISIS DE DATOS--------------------------------
#--------------------------RATIOS FINANCIEROS-----------------------------------

Liquidez <- Hites_EEFF %>% select(Fechas,
                                  `Activo Corriente`,
                                  `Pasivo Corriente`) %>%
  mutate(Liquidez = `Activo Corriente` / `Pasivo Corriente`)

ggplot(data = Liquidez, aes(x = Fechas, y = Liquidez)) +
  geom_line(col = "#77dd66", size = 0.5) + geom_point(col = "#4d9042", size = 2) +
  ggtitle("Evolución del ratio de liquidez de Hites S.A.",
          subtitle = "Corresponde a las variaciones en el ratio para los intervalos entre el año 2010 y el 2023") +
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
  xlab("Fecha de corte") + ylab("Ratio") +
  scale_y_continuous(
    breaks = seq(0, 500000000, 50000000),
    labels = function(x)
      format(
        x,
        big.mark = ".",
        decimal.mark = ",",
        scientific = FALSE
      )
  ) + scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 year")

# Guardamos un pronóstico del ratio de liquidez
frcst_liquidez <- Liquidez %>% select(Liquidez)

# Generamos un gráfico para el pronóstico
autoplot(forecast(ts(
  frcst_liquidez,
  start = c(2010, 1),
  frequency = 4
)),
xlab = "Fecha de corte",
ylab = "Ratio",
main = "Pronóstico para el ratio de liquidez de Hites S.A.") +
  scale_y_continuous(breaks = round(seq(
    min(frcst_liquidez$Liquidez),
    max(frcst_liquidez$Liquidez),
    by = 0.5
  ), 1))

#------------------ANALISIS DEL DATASET DE ACTIVOS DE HITES S.A.----------------

# Evolución del Activo Total de Hites S.A.
ggplot(data = Hites_EEFF, aes(y = `Activo total`, x = Fechas)) +
  geom_line(col = "#00f8ff", size = 0.5) + geom_point(col = "#00bfc4", size = 2) +
  ggtitle("Evolución del Activo Total de Hites S.A.",
          subtitle = "Corresponde a las variaciones en el monto para los intervalos entre el año 2010 y el 2023") +
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
  ) + scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 year")

# Guardamos la secuencia de tiempo, la frecuencia indica que es trimestral,
# y que es 4 veces al año, empieza el primer trimestre del 2010 y termina el
# primer trimestre del 2023

Efectivo_fcst = ts(
  Hites_EEFF$Efectivo,
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
)

#-----------ANALISIS DEL DATASET DE PASIVOS DE HITES S.A.-----------------------

# Evolución del Pasivo Total de Hites S.A.
ggplot(data = Hites_EEFF, aes(y = `Pasivo total`, x = Fechas)) +
  geom_line(col = "#ff5252", size = 0.5) + geom_point(col = "#ffbaba", size = 2) +
  ggtitle("Evolución del Pasivo Total de Hites S.A.",
          subtitle = "Corresponde a las variaciones en el monto para los intervalos entre el año 2010 y el 2023") +
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

# Guardamos la secuencia de tiempo, la frecuencia indica que es trimestral,
# y que es 4 veces al año, empieza el primer trimestre del 2010 y termina el
# primer trimestre del 2023

Proveedores_fcst = ts(
  Hites_EEFF$`Provedores corrientes`,
  start = c(2010, 1),
  end = c(2023, 1),
  frequency = 4
)

# Muestra de resultados
print(Proveedores_fcst)

# Generamos un pronóstico para la evolución de los activos totales
autoplot(
  forecast(Proveedores_fcst),
  xlab = "Tiempo",
  ylab = "Monto",
  main = "Pronóstico para la cuenta Proveedores de Hites S.A."
) + scale_y_continuous(
  labels = function(x)
    format(x, big.mark = ".", scientific = FALSE)
)

#-----------ANALISIS DEL DATASET DE PATRIMONIO DE HITES S.A.--------------------

#-----------ANALISIS DEL DATASET DE ESTADO DE RESULTADOS DE HITES S.A.----------

# Extraemos los Ingresos netos, partiendo por el año 2010 con una frecuencia de
# cuatro veces por año.

Ing_net_ts <-
  ts(Hites_EEFF$`+Ingresos netos...63`,
     start = c(2010, 1),
     frequency = 4)

# Imprimimos los datos antes de modelarlos
print(Ing_net_ts)

# Generamos un gráfico que muestra un pronóstico para el movimiento de los
# ingresos netos de Hites S.A.

autoplot(
  forecast(Ing_net_ts),
  facets = TRUE,
  xlab = "Periodo",
  ylab = "Monto",
  main = "Pronóstico para los Ingresos Netos de Hites S.A."
) +
  scale_y_continuous(
    labels = function(x)
      format(x, big.mark = ".", scientific = FALSE)
  )

# Vemos un resumen sobre el pronóstico del Ingreso Neto
summary(forecast(Ing_net_ts))

# Gráfico de la evolución de los ingresos netos de Hites S.A.
ggplot(data = Hites_EEFF, aes(y = `+Ingresos netos...5`,
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
  ylab("Monto")  + scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "year")

#--------------------------------FINAL------------------------------------------
# Liberación de memoria
gc()
