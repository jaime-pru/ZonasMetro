# Cargar librerías

library(readxl)
library(dplyr)

# Leer datos desde un archivo xlsx

datos <- read_excel("C:\\Users\\rpm0a\\OneDrive\\Documentos\\RepTemplates\\ZonasMetro\\Bases temporales\\Bases Agrupadas\\BLzm19.xlsx")

# Agrupando (Suma de cada sector)

datosa <- datos %>%
  group_by(cvegeo, cve_sec) %>%
  summarise(ue = sum(ue, na.rm = TRUE),
            af = sum(af, na.rm = TRUE),
            fb = sum(fb, na.rm = TRUE),
            pb = sum(pb, na.rm = TRUE),
            po = sum(po, na.rm = TRUE),
            re = sum(re, na.rm = TRUE),
            va = sum(va, na.rm = TRUE)) %>%
  mutate(sect = case_when(cve_sec %in% c(11, 21) ~ "pri",
                            cve_sec == 23 ~ "con",
                            cve_sec %in% c(31, 32, 33) ~ "man",
                            cve_sec %in% c(43, 46) ~ "com",
                            TRUE ~ "ser"))
View(datosa)

# Agrupando (Suma de cada sector)

datosb <- datosa %>%
  group_by(cvegeo, sect) %>%
  summarise(ue = sum(ue, na.rm = TRUE),
            af = sum(af, na.rm = TRUE),
            fb = sum(fb, na.rm = TRUE),
            pb = sum(pb, na.rm = TRUE),
            po = sum(po, na.rm = TRUE),
            re = sum(re, na.rm = TRUE),
            va = sum(va, na.rm = TRUE))

View(datosb)

# Trayendo datos originales

datosagrupados <- datosb
datosagrupados$CVE_ZM <- datos$CVE_ZM[match(datosb$cvegeo, datos$cvegeo)]
datosagrupados$NOM_ZM <- datos$NOM_ZM[match(datosb$cvegeo, datos$cvegeo)]
datosagrupados$cv_mun <- datos$cv_mun[match(datosb$cvegeo, datos$cvegeo)]
datosagrupados$nom_mun <- datos$nom_mun[match(datosb$cvegeo, datos$cvegeo)]
datosagrupados$cv_ent <- datos$cv_ent[match(datosb$cvegeo, datos$cvegeo)]
datosagrupados$nom_ent <- datos$nom_ent[match(datosb$cvegeo, datos$cvegeo)]

# Ordenando columnas

datosagrupados <- datosagrupados[c("CVE_ZM", "NOM_ZM", "cvegeo", "cv_mun", "nom_mun", "cv_ent", 
                                   "nom_ent", "sect", "ue", "af", "fb", "pb", "po", "re", "va")]


View(datosagrupados)

# Guardar archivo

library(openxlsx)

write.xlsx(datosagrupados, "BLzm19a.xlsx")

  