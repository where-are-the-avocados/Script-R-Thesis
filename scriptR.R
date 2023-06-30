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
#------------------ANÁLISIS DEL DATASET DE ACTIVOS DE HITES---------------------
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

# Limpieza del Dataset

as.Date(HitesActivo$Fechas)
glimpse(HitesActivo)

# Tendencia Central
mean(HitesActivo$`Activo total`)
median(HitesActivo$`Activo total`)

# Medidas de Variabilidad
range(HitesActivo$`Activo total`)
var(HitesActivo$`Activo total`)
sd(HitesActivo$`Activo total`)

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

#-----------ANALISIS DEL DATASET DE PASIVOS DE HITES S.A.-----------------------

#-----------ANALISIS DEL DATASET DE PATRIMONIO DE HITES S.A.--------------------

#-----------ANALISIS DEL DATASET DE ESTADO DE RESULTADOS DE HITES S.A.----------

# Extraemos los ingresos netos al cierre de cada periodo del dataset
Result_neto2 <- as.integer(
  c(
    183910997,
    227512020,
    253830651,
    271056964,
    275077376,
    280349317,
    302489675,
    311870350,
    316708928,
    324743257,
    291189414,
    403820306,
    368393931
  )
)

# Creamos un vector con todas las fechas de cierre entre el 2010 y el 2022
Result_fecha <-
  seq(as.Date("2010-12-31"), as.Date("2022-12-31"), by = "1 year")


# Las unimos para tener un vector que muestre los resultados netos al cierre de cada periodo
Resultados_df <- data.frame(Result_fecha,
                            Result_neto2)
print(Resultados_df)

# Gráfico de la evolución de los ingresos netos de Hites S.A.
ggplot(data = Resultados_df, aes(y = Result_neto2, x = Result_fecha)) +
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
    )) + xlab("Fecha de corte") + 
  ylab("Monto")  + scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 year")

# Pronósticos de Ingresos para Hites S.A.
fcst_rneto <- forecast(Result_neto2)

print(fcst_rneto)

autoplot(
  fcst_rneto,
  xlab = "Fecha",
  ylab = "Monto",
  na.rm = TRUE) +
  ggtitle("Pronóstico de la evolución de los Ingresos Netos de Hites S.A.",
          subtitle = "Gráfico elaborado en base a modelos ARIMA")

