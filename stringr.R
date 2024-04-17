library(tidyverse)

library(readr)
vgsales <- read_csv("E:/Universidad/Tesis/vgsales.csv")
View(vgsales)

colSums(is.na(vgsales))             # No tiene valores NA

vgsales %>% filter(str_detect(Year, "N/A"))

# Lleva todos los nombres a mayusculas
vgsales$Publisher %>% str_to_upper()

# Lleva todos los nombres a título
vgsales$Publisher %>% str_to_title()

# Lleva todos los nombres a minusculas
vgsales$Publisher %>% str_to_lower()

# Extrae el Publisher que comienza con la letra P
vgsales %>% filter(str_detect(Publisher, "^P"))

# Extrae el Publisher que termina con la letra A
vgsales %>% filter(str_detect(Name, "A$"))

# Remueve filas con valores llamados "N/A"
str_remove(vgsales$Year, "N/A")

Nombres <- select(vgsales, Name) 
print(Nombres)

Trunc <- str_trunc(Nombres$Name, 10, "left")
print(Trunc)

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

str_trim(Ejemplo)

# Creación de un conjunto de datos de ejemplo
datos_facturas <- data.frame(
  Numero_Factura = c("F001", "F002", "F003", "F001", "F004", "F002"))

# Identificar facturas duplicadas con stringr
facturas_duplicadas <- 
  datos_facturas[duplicated(str_extract(datos_facturas$Numero_Factura, "\\d+")), ]

# Mostrar las facturas duplicadas
print(facturas_duplicadas)

# \\: Esto se utiliza para escapar el carácter \, ya que en R las barras invertidas
# se usan para escapar caracteres especiales en las cadenas de texto. 
# Por lo tanto, \\ representa una sola barra invertida en la expresión regular.
# \d: Esto es una secuencia de escape que coincide con cualquier dígito del 0 al 9.
# +: Esto indica que el patrón \d puede aparecer una o más veces en la cadena.
# En resumen, la expresión regular "\\d+" busca cualquier secuencia de uno o más 
# dígitos en una cadena de texto. En el contexto del ejemplo que te proporcioné, 
# se utiliza para extraer los números de las facturas (eliminando el prefijo "F") 
# para luego identificar las facturas duplicadas en el conjunto de datos.

# Ejemplo de nombres de cuentas
nombres_cuentas <- c("Cuenta de Ahorros", "Cuenta Corriente", "Cuenta de Inversiones",
                     "Inventario", "Cuentas por cobrar","Impuestos por pagar",
                     "Clientes","Gastos Financieros","Instrumentos Financieros",
                     "Activo Total","Pasivo Total","Patrimonio")

# Buscar nombres de cuentas que contienen la palabra "Cuenta"
cuentas_con_cuenta <- nombres_cuentas[str_detect(nombres_cuentas, "Cuenta")]
print(cuentas_con_cuenta)

# Ejemplo de datos de números de teléfono
numeros_telefono <- c("123-456-7890", "56947129491", "998007458", "953789569")
print(numeros_telefono)

# Filtrar números de teléfono válidos
formato_valido <- numeros_telefono[str_detect(numeros_telefono, "^\\d{3}-\\d{3}-\\d{4}$")]
print(formato_valido)

# Filtrar números de teléfono inválidos (ENTREGA AQUELLOS DATOS ERRÓNEOS)
formato_invalidos <- numeros_telefono[!str_detect(numeros_telefono, "^\\d{3}-\\d{3}-\\d{4}$")]
print(formato_invalidos)

# Corregir números de teléfono inválidos
numeros_corregidos <- str_replace_all(numeros_telefono, "(\\d{3})(\\d{3})(\\d{3})", "\\1-\\2-\\3")
print(numeros_corregidos)

