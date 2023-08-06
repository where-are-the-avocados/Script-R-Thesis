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
print(Hites_EEFF)
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
#Hites_EEFF$Fechas = as.Date(Hites_EEFF$Fechas, format = "%Y/%m/%d")

#--------------------------RATIOS FINANCIEROS-----------------------------------
Ratios <- Hites_EEFF %>% select(Fechas,
                                `Activo Corriente`,
                                `Pasivo Corriente`,
                                `Inventarios corrientes`) %>%
  mutate(
    Liquidez = `Activo Corriente` / `Pasivo Corriente`,
    Prueba_acida = (`Activo Corriente` - `Inventarios corrientes`) / `Pasivo Corriente`,
    Capital_neto = `Activo Corriente` - `Pasivo Corriente`
  )

# Graficos de los Ratios Financieros

ggplot(data = Ratios, aes(x = Fechas, y = Liquidez)) +
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
    labels = function(x)
      format(
        x,
        big.mark = ".",
        decimal.mark = ",",
        scientific = FALSE
      )
  ) + scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 year")

# Evolución de la prueba ácida
ggplot(data = Ratios, aes(x = Fechas, y = Prueba_acida)) +
  geom_line(col = "#ff859a", size = 0.5) + geom_point(col = "#b25d6b", size = 2) +
  ggtitle("Evolución de la razón ácida de Hites S.A.",
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
    labels = function(x)
      format(
        x,
        big.mark = ".",
        decimal.mark = ",",
        scientific = FALSE
      )
  ) + scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 year")

# Evolución del capital neto
ggplot(data = Ratios, aes(x = Fechas, y = Capital_neto)) +
  geom_line(col = "#93b4ce", size = 0.5) + geom_point(col = "#aed5f4", size = 2) +
  ggtitle("Evolución del Capital Neto de Hites S.A.",
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
    labels = function(x)
      format(
        x,
        big.mark = ".",
        decimal.mark = ",",
        scientific = FALSE
      )
  ) + scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 year")

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
  ) + scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 year")

#--------ANALISIS DEL DATASET DE PASIVOS y PATRIMONIO DE HITES S.A.-------------

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

#-----------ANALISIS DEL DATASET DE ESTADO DE RESULTADOS DE HITES S.A.----------

# Gráfico de la evolución de los ingresos netos de Hites S.A.
ggplot(data = Hites_EEFF, aes(y = `+Ingresos netos...63`,
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

#--------------------------------FORECASTING------------------------------------

#INGRESOS NETOS DE HITES S.A.
Ing_net_ts <-
  ts(
    Hites_EEFF$`+Ingresos netos...63`,
    start = c(2010, 1),
    end = c(2023, 1),
    frequency = 4
  )

# Imprimimos los datos antes de modelarlos
print(Ing_net_ts)

# Descomposición
autoplot(decompose(Ing_net_ts))
autoplot(log(Ing_net_ts))

# Esta es la diferencia entre los valores
diff(Ing_net_ts)

# Generamos un plot de las diferencias
autoplot(diff(Ing_net_ts),
         xlab = "Tiempo",
         ylab = "Monto",
         main = "Primera diferencia de los Ingresos Netos")

# Niveles de residuos
acf(Ing_net_ts)

# Niveles de significancia
pacf(Ing_net_ts)

# Vemos un resumen sobre el pronóstico del Ingreso Neto
forecast(Ing_net_ts)

# Consulta mejor modelo para los datos
arima_ing.net <-
  auto.arima(
    Ing_net_ts,
    stepwise = FALSE,
    approximation = FALSE,
    trace = TRUE
  )

print(arima_ing.net)

# Revisamos los resultados
checkresiduals(arima_ing.net)

# Generamos un gráfico que muestra un pronóstico para el movimiento de los
# ingresos netos de Hites S.A. Con un grado de confianza del 95%

autoplot(
  forecast(arima_ing.net),
  facets = TRUE,
  xlab = "Periodo",
  ylab = "Monto",
  main = "Pronóstico para los Ingresos Netos de Hites S.A."
) +
  scale_y_continuous(
    labels = function(x)
      format(x, big.mark = ".", scientific = FALSE)
  ) +  scale_x_continuous(breaks = seq(2010, 2026, 1))

#----------------- Pronóstico del ratio de liquidez

liq_rt <- ts(
  Ratios$Liquidez,
  start = c(2010, 1),
  end = c(2023, 1),
  frequency = 4
)

# Imprimimos los datos antes de modelarlos
print(liq_rt)

# Descomposición
autoplot(decompose(liq_rt))
autoplot(log(liq_rt))

# Esta es la diferencia entre los valores
diff(liq_rt)

# Generamos un plot de las diferencias
autoplot(diff(liq_rt),
         xlab = "Tiempo",
         ylab = "Ratio",
         main = "Primera diferencia del ratio de liquidez")

# Generamos un pronóstico en base a ETS, dado que no es un modelo estacionario

autoplot(
  forecast(liq_rt),
  facets = TRUE,
  xlab = "Periodo",
  ylab = "Ratio",
  main = "Pronóstico para el ratio de liquidez de Hites S.A."
) +
  scale_y_continuous(breaks = seq(0, 4, 0.5)) +
  scale_x_continuous(breaks = seq(2010, 2026, 1)) 

#--------------------------------FINAL------------------------------------------
# Liberación de memoria
gc()

# Limpieza del environment
rm(list = ls())
