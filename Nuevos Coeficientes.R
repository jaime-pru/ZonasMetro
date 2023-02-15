# Estimación de coeficientes

# Cargar paqueterias

library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)

# Carga tu base de datos ancha

data <- readxl::read_xlsx("C:\\Users\\rpm0a\\OneDrive\\Documentos\\RepTemplates\\ZonasMetro\\Bases temporales\\BL_zm99.xlsx")

View(data)

# Separar datos geográficos y económicos

datos_geo <- data %>% select(cvegeo, cv_mun, nom_mun, cv_ent, nom_ent, CVE_ZM, NOM_ZM) %>% distinct()
datos_econ <- data %>% select(cvegeo, CVE_ZM, cve_sub, ue, af, fb, pb, po, re, va)

# Calcular coeficientes QL

QL <- datos_econ %>%
  group_by(CVE_ZM, cvegeo,cve_sub) %>%
  summarise(subsec_mun = sum(po, na.rm = TRUE),
            tot_mun = sum(po, na.rm = TRUE)
            subsec_zm = sum(po, na.rm = TRUE),
            tot_zm = sum(po, na.rm = TRUE)) %>%
  mutate(QL = subsec_mun / tot_mun / (subsec_zm / tot_zm)) %>%
  select(CVE_ZM, cvegeo, QL)

View(QL)

