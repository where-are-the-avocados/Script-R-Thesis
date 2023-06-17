# ---------------INSTALACIÓN DE PAQUETES DE INVESTIGACIÓN------------------
# install.packages("tidyverse",
#                 "styler",
#                 "stargazer",
#                 "lubridate",
#                 "styler",
#                 "forecast",
#                 "lintr")
#----------------REVISIÓN DE SESIÓN DE RSTUDIO-----------------------------
sessionInfo()
options(scipen = 99999)
#----------------CARGAR LIBRERIAS DE PAQUETES DE INVESTIGACIÓN-------------
library(tidyverse)
library(styler)
library(stargazer)
library(lubridate)
library(forecast)


#---------------DESARROLLO DEL CÓDIGO PARA LA INVESTIGACIÓN----------------
# Importación de la Base de Datos
library(readr)
periodo2022 <-
  read_delim(
    "DataBase/2022-sociedades-por-fecha-rut-constitucion.csv",
    delim = ";",
    escape_double = FALSE,
    trim_ws = TRUE
  )
View(periodo2022)

?  ? forecast
?  ? stringr

summary(str_length(periodo2022$ID))
str_to_title(periodo2022$`Razon Social`)

length(unique(periodo2022$ID)) # No existen valores duplicados
length(unique(periodo2022$RUT)) # No existen valores duplicados

# Valores más repetidos
tail(names(sort(table(
  periodo2022$`Comuna Tributaria`
))), 1)
tail(names(sort(table(
  periodo2022$`Region Tributaria`
))), 1)
tail(names(sort(table(
  periodo2022$`Codigo de sociedad`
))), 1)

# REVISAR A MAYOR PROFUNDIDAD
min(periodo2022$Capital)
max(periodo2022$Capital)
summary(periodo2022$Capital)
tail(names(sort(table(
  periodo2022$Capital
))), 1) # Un millon es el valor mas repetido

length(unique(periodo2022$Capital)) # Existen valores repetidos en el capital, solamente 1701 son unicos

# Es la misma funcion pero una es con Z y la otra con S ??????
summarize(periodo2022)
summarise(periodo2022)

distinct(periodo2022$RUT)

is.na(periodo2022$Capital)

View(periodo2022)

periodo2022 <-
  periodo2022 %>% rename(Año = Anio) # Corrige el error en el nombre
View(periodo2022)

# FUNCIONES EXTRA

cantidad_na <- sum(is.na(periodo2022))
print(cantidad_na)

fila_na <- which(apply(is.na(periodo2022), 1, any))
print(fila_na)

valor_na <- periodo2022[which(rowSums(is.na(periodo2022)) > 0), ]
View(valor_na)

# FORECAST

# AIRPASSENGERS
# LYNX
# ELECTRICITY
# NILE

# LUBRIDATE
periodo2022$`Fecha de actuacion (1era firma)` <-
  ymd(periodo2022$`Fecha de actuacion (1era firma)`)

periodo2022$`Fecha de aprobacion x SII` <-
  ymd(periodo2022$`Fecha de aprobacion x SII`)

periodo2022$`Fecha de registro (ultima firma)` <-
  ymd(periodo2022$`Fecha de registro (ultima firma)`)

periodo2022 <- periodo2022[order(periodo2022$`Fecha de actuacion (1era firma)`), ]

format_ISO8601(periodo2022$`Fecha de actuacion (1era firma)`)
format_iso8601(periodo2022$`Fecha de aprobacion x SII`)
format_ISO8601(periodo2022$`Fecha de registro (ultima firma)`)
print(periodo2022)
View(periodo2022)
# STRINGR

digito_verificador <- str_extract(periodo2022$RUT,"\\d$")
print(digito_verificador)


# GGPLOT2



