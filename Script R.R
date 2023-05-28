# ---------------INSTALACIÓN DE PAQUETES DE INVESTIGACIÓN------------------
install.packages("ggplot2",
                 "dplyr",
                 "stringr",
                 "lubridate",
                 "forecast",
                 "stargazer")
#----------------CARGAR LIBRERIAS DE PAQUETES DE INVESTIGACIÓN-------------
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(forecast)
library(stargazer)
#---------------DESARROLLO DEL CÓDIGO PARA LA INVESTIGACIÓN----------------
dim(mtcars)
names(mtcars)
hist(mtcars$mpg,
     col='steelblue',
     main='Histogram',
     xlab='mpg',
     ylab='Frequency')
