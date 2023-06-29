# ---------------INSTALACIÓN DE PAQUETES DE INVESTIGACIÓN------------------
# install.packages("tidyverse",
#                 "stargazer",
#                 "lubridate",
#                 "forecast",
#                 "readxl")

#----------------REVISIÓN DE SESIÓN DE RSTUDIO-----------------------------
sessionInfo()
options(scipen = 99999)
update.packages()

#----------------CARGAR LIBRERIAS DE PAQUETES DE INVESTIGACIÓN-------------
library(tidyverse)
library(stargazer)
library(lubridate)
library(forecast)
library(readxl)

#---------------DESARROLLO DEL CÓDIGO PARA LA INVESTIGACIÓN----------------
# Importación de la Base de Datos
library(readxl)
Hites <- read_excel("economatica.xlsx",
                    sheet = "Hoja1")
View(Hites)

# Funciones que permiten identificar la estructura del Dataset
head(Hites)
tail(Hites)
structure(Hites)
glimpse(Hites)
str(Hites)
dim(Hites)
nrow(Hites)
ncol(Hites)
ls(Hites)
names(Hites)
summary(Hites)

# Limpieza del Dataset

as.Date(Hites$Cuentas)
glimpse(Hites)

# Análisis de Datos

# Tendencia Central
mean(Hites$`Activo total...2`)
median(Hites$`Activo total...2`)

# Medidas de Variabilidad
range(Hites$`Activo total...2`)
var(Hites$`Activo total...2`)
sd(Hites$`Activo total...2`)

ggplot(data = Hites, aes(y = `Activo total...2`, x = Cuentas)) +
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
