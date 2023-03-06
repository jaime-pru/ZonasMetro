# Cargar librerías

library(readxl)
library(dplyr)

# Leer datos desde un archivo xlsx

datos <- read_excel("C:\\Users\\rpm0a\\OneDrive\\Documentos\\RepTemplates\\ZonasMetro\\Bases temporales\\Bases Agrupadas\\BLzm99.xlsx")

# Agrupando (Suma de cada sector)

datosa <- datos %>%
  group_by(cvegeo, cve_sec) %>%
  summarise(ue = sum(ue),
            af = sum(af),
            fb = sum(fb),
            pb = sum(pb),
            po = sum(po),
            re = sum(re),
            va = sum(va)) %>%
  mutate(sector = case_when(cve_sec %in% c(11, 21) ~ "primario",
                            cve_sec == 23 ~ "construcción",
                            cve_sec %in% c(31, 32, 33) ~ "manufactura",
                            cve_sec %in% c(43, 46) ~ "comercio",
                            TRUE ~ "servicios"))
View(datosa)

# Agrupando (Suma de cada sector)

datosb <- datosa %>%
  group_by(cvegeo, sector) %>%
  summarise(ue = sum(ue),
            af = sum(af),
            fb = sum(fb),
            pb = sum(pb),
            po = sum(po),
            re = sum(re),
            va = sum(va))

View(datosb)
  