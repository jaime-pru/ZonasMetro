# Estimación de coeficientes

# Cargar paqueterias

library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(writexl)


# Carga tu base de datos ancha

data <- readxl::read_xlsx("C:\\Users\\rpm0a\\OneDrive\\Documentos\\RepTemplates\\ZonasMetro\\Bases temporales\\BL_zm99.xlsx")

View(data)

# Separar datos geográficos y económicos

datos_geo <- data %>% select(cvegeo, cv_mun, nom_mun, cv_ent, nom_ent, CVE_ZM, NOM_ZM) %>% distinct()
datos_econ <- data %>% select(cvegeo, CVE_ZM, cve_sub, ue, af, fb, pb, po, re, va)

# Calcular coeficientes QL

QL <- datos_econ %>%
  group_by(CVE_ZM, cvegeo, cve_sub) %>%
  summarise(subsec_mun = sum(po, na.rm = TRUE),
            tot_mun = sum(po, na.rm = TRUE),
            subsec_zm = sum(po, na.rm = TRUE),
            tot_zm = sum(po, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(CVE_ZM, cvegeo) %>%
  mutate(QL = sum(subsec_mun) / sum(tot_mun) / (sum(subsec_zm) / sum(tot_zm))) %>%
  ungroup() %>%
  select(CVE_ZM, cvegeo, cve_sub, QL) %>%
  distinct(CVE_ZM, cvegeo, cve_sub, .keep_all = TRUE) %>%
  mutate(QL = round(QL, 3))

View(QL)

QL <- datos_econ %>%
  group_by(CVE_ZM, cvegeo, cve_sub) %>%
  summarise(subsec_mun = sum(po, na.rm = TRUE)) %>%
  group_by(cvegeo, cve_sub) %>%
  mutate(tot_mun = sum(subsec_mun, na.rm = TRUE)) %>%
  group_by(CVE_ZM, cve_sub) %>%
  mutate(subsec_zm = sum(subsec_mun, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(CVE_ZM, cvegeo) %>%
  mutate(tot_zm = sum(subsec_mun, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(QL = subsec_mun / tot_mun / (subsec_zm / tot_zm)) %>%
  select(CVE_ZM, cvegeo, cve_sub, QL) %>%
  distinct()

# Crea un archivo de Excel y escribe el objeto QL en la hoja de datos

# Nombre del archivo
nombre_archivo <- "QL.xlsx"

# Escribir objeto en archivo Excel
write.xlsx(QL, file = nombre_archivo, rowNames = FALSE)
