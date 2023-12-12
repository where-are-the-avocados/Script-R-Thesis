library(tidyverse)

library(readr)
vgsales <- read_csv("Dataset/stringr/vgsales.csv")
View(vgsales)

colSums(is.na(vgsales))             # No tiene valores NA

vgsales %>% filter(str_detect(Year, "N/A"))

vgsales$Publisher %>% str_to_upper()
vgsales$Publisher %>% str_to_title()
vgsales$Publisher %>% str_to_lower()

vgsales %>% filter(str_detect(Publisher, "^P"))


vgsales %>% filter(str_detect(Name, "A$"))


str_remove(vgsales$Year, "N/A")

Nombres <- select(vgsales, Name)

Nombres %>% head() %>% str_length()

Trunc <- str_trunc(Nombres$Name, 10, "left")

str_trim(Nombres$Name)

# -------------------------------EJEMPLOS----------------------------------
Ejemplo <- c("Inventario", "Cuentas por cobrar","Impuestos por pagar",
             "Clientes","Gastos Financieros","Instrumentos Financieros",
             "Activo Total","Pasivo Total","Patrimonio")
str_extract(Ejemplo,"\\g")

str_split(Ejemplo,"\\g")

str_extract(Ejemplo,"\\g")

str_pad(Ejemplo, 10)

Ejemplo <- c("La Manzana S.A","RUT: 71.229.587-4","Teléfono: +569 5445 7353")
Ejemplo_2 <- "(71.229.587-4)"
str_locate(Ejemplo,Ejemplo_2)

str_detect(Ejemplo, Ejemplo_2)

str_to_upper(Ejemplo)

str_trim(Ejemplo)


Ejemplo_2 <- str_c("Cuentas por cobrar",
                   "Inventario",
                   "Ventas",
                   "Instrumentos financieros",
                   "Gastos financieros")

str_to_upper(Ejemplo_2)

cat(str_wrap(Ejemplo_2,width=10))
Ejemplo %>% str_trunc(10) 
str_pad(x, 10)
str_length(Ejemplo)
str_sub(Ejemplo, 3)
str_dup(Ejemplo, 3)

str_extract(Ejemplo,"\\d")
