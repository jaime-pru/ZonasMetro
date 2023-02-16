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

geo, cve_sub = datos_combinados$cve_sub, operacion = operacion)


