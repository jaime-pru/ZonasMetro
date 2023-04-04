# Cargar Librerias

library(readxl)
library(dplyr)
library(tidyr)

# Cargar archivo excel

datos <- read_excel("C:\\Users\\rpm0a\\OneDrive\\Documentos\\RepTemplates\\ZonasMetro\\Bases temporales\\Bases Largas Finales\\BLzm19_final.xlsx")

# Seleccionar solo las columnas relevantes
df <- datos %>% 
  select(cvegeo, CVE_ZM, NOM_ZM, cv_mun, nom_mun, cv_ent, nom_ent, cve_sub, 
         ue, af, fb, pb, po, re, va, QLue, QLaf, QLfb, QLpb, QLpo, QLre, QLva, 
         PRue, PRaf, PRfb, PRpb, PRpo, PRre, PRva, HHue, HHaf, HHfb, HHpb, HHpo, 
         HHre, HHva, IHHue, IHHaf, IHHfb, IHHpb, IHHpo, IHHre, IHHva)

# Obtener valores únicos de cvegeo

df_distinct <- df %>% distinct(cvegeo)

# Agrupar valores por cve_sub

df_wide <- df %>% 
  pivot_wider(names_from = cve_sub, values_from = c("ue", "af", "fb", "pb", "po", "re", "va", 
                                                    "QLue", "QLaf", "QLfb", "QLpb", "QLpo", "QLre", "QLva", 
                                                    "PRue", "PRaf", "PRfb", "PRpb", "PRpo", "PRre", "PRva",
                                                    "HHue", "HHaf", "HHfb", "HHpb", "HHpo", "HHre", "HHva",
                                                    "IHHue", "IHHaf", "IHHfb", "IHHpb", "IHHpo", "IHHre", "IHHva"), 
              names_glue = "{.value}_{cve_sub}")

# Unir valores únicos de cvegeo con datos agrupados por cve_sub

zm19 <- left_join(df_distinct, df_wide, by = "cvegeo")
View(zm19)

# Guardar como archivo xlsx

library(openxlsx)
write.xlsx(zm19, "zm19.xlsx")
