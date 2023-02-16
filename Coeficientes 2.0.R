# Cargar librerías

library(readxl)
library(dplyr)

# Leer datos desde un archivo xlsx

datos <- read_excel("C:\\Users\\rpm0a\\OneDrive\\Documentos\\RepTemplates\\ZonasMetro\\Bases temporales\\BL_zm99.xlsx")

# Crear vector subsec_mun

subsec_mun <- datos %>% group_by(cvegeo, cve_sub) %>% summarize(po=sum(po))

# Crear vector tot_mun

tot_mun <- datos %>% group_by(cvegeo) %>% summarize(po=sum(po))

# Crear vector subsec_zm

subsec_zm <- datos %>% group_by(CVE_ZM, cve_sub) %>% summarize(po=sum(po))

# Crear vector tot_zm

tot_zm <- datos %>% group_by(CVE_ZM) %>% summarize(po=sum(po))

# Agregar columna "po_tot_zm" al dataframe "tot_zm"

tot_zm$po_tot_zm <- aggregate(po ~ CVE_ZM, data = subsec_zm, sum)$po

# Agregar columna "po_subsec_zm" al dataframe "subsec_zm"

subsec_zm$po_subsec_zm <- aggregate(po ~ CVE_ZM + cve_sub, data = subsec_zm, sum)$po

# Realizar la operación

resultado_operacion <- ((tot_zm$po)*(subsec_mun$po)) / ((subsec_zm$po)*tot_mun$po)
View(resultado_operacion)

# Agregar columna "cve_sub" a "resultados"

resultados <- cbind(resultados, cve_sub = subsec_mun$cve_sub)

# Crear dataframe "resultados"

resultados <- data.frame(cvegeo = subsec_mun$cvegeo, operacion = resultado_operacion)


View(resu)


# Agregar los resultados al dataframe "resultados"

resultados$operacion <- resultado_operacion
