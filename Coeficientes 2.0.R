# Cargar librerías

library(readxl)
library(dplyr)

# Leer datos desde un archivo xlsx

datos <- read_excel("C:\\Users\\rpm0a\\OneDrive\\Documentos\\RepTemplates\\ZonasMetro\\Bases temporales\\BL_zm99.xlsx")

# Crear vector subsec_mun

subsec_mun <- datos %>% group_by(cvegeo, CVE_ZM, cve_sub, CVE_ZM) %>% summarize(po=sum(po))


# Crear vector tot_mun

tot_mun <- datos %>% group_by(CVE_ZM, cvegeo) %>% summarize(po=sum(po))

# Crear vector subsec_zm

subsec_zm <- datos %>% group_by(CVE_ZM, cve_sub) %>% summarize(po=sum(po))


# Crear vector tot_zm

tot_zm <- datos %>% group_by(CVE_ZM) %>% summarize(po=sum(po))


# Combinar data frames

combinado <- full_join(subsec_mun, tot_mun, by = "cvegeo") %>%
  full_join(subsec_zm, by = c("cve_sub", "CVE_ZM"))

# Calcular división

combinado <- combinado %>%
  mutate(division = po.x / po.y)

# Seleccionar columnas de interés

combinado <- combinado %>% select(cvegeo, CVE_ZM, cve_sub, division)
View(combinado)

#Guardar xlsx

library(openxlsx)

write.xlsx(combinado, "COMBINADO.xlsx")










# Crear vector resultado (QL)

resultado <- (subsec_mun$po / tot_mun$po) / (subsec_zm$po / tot_zm$po)

# Combinar los resultados en un data frame

resultados <- data.frame(cvegeo = subsec_mun$cvegeo, cve_sub = subsec_mun$cve_sub, QL = resultado)

View(resultados)

#Guardar xlsx

library(openxlsx)

write.xlsx(resultados, "resultados.xlsx")


# Crear vector resultado (PR)

PR <- (subsec_mun$po / subsec_zm$po) 

# Combinar los resultados en un data frame

PR <- data.frame(cvegeo = subsec_mun$cvegeo, cve_sub = subsec_mun$cve_sub, operacion = resultado)

View(PR)

