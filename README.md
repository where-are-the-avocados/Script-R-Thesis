## Data analytics in auditing through the forecast package

## General Objective

To identify the relevance and applicability of the forecast package to be applied in the area of auditing, through a case study applied to the accounting of an entity.

## Specific Objectives

- To generate an algorithm that can be used as a study and as a basis to be applied in subsequent studies.
- To evaluate the potential and capabilities that forecast has for the generation of forecasts in the area of auditing.
  audit.
- Achieve basic skills in forecasting algorithm models, such as ARIMA, ETS and Naive.

## Resources used

For the purposes of this particular research, RStudio was used with the following libraries:
Most of explanations are included in the code, but in spanish since it is my native language.
```
# Instalación paquetes
 install.packages("tidyverse",
                 "lubridate",
                 "forecast",
                 "readxl",
                 "zoo",
                 "ghibli")

 update.packages()

# Carga de paquetes
    library(tidyverse)
    library(lubridate)
    library(forecast)
    library(readxl)
    library(zoo)
    library(ghibli)
```
