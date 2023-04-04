# Cargar Librerias

library(readxl)
library(dplyr)
library(tidyr)

# Cargar archivo excel

datos <- read_excel("C:\\Users\\leope\\Documents\\RepTemplates\\ZonasMetro\\Bases temporales 2\\Base Largas Finales ZM\\BLZM99_final.xlsx")

# Seleccionar solo las columnas relevantes

df <- datos %>% 
  select(CVE_ZM, NOM_ZM, sect, ue, af, fb, pb, po, re, va, QLue, QLaf, QLfb, QLpb, 
         QLpo, QLre, QLva, PRue, PRaf, PRfb, PRpb, PRpo, PRre, PRva, HHue, HHaf, 
         HHfb, HHpb, HHpo, HHre, HHva, IHHue, IHHaf, IHHfb, IHHpb, IHHpo, IHHre, IHHva)

# Obtener valores únicos de cvegeo

df_distinct <- df %>% distinct(CVE_ZM)

# Agrupar valores por cve_sub

df_wide <- df %>% 
  pivot_wider(names_from = sect, values_from = c("ue", "af", "fb", "pb", "po", "re", "va", 
                                                    "QLue", "QLaf", "QLfb", "QLpb", "QLpo", "QLre", "QLva", 
                                                    "PRue", "PRaf", "PRfb", "PRpb", "PRpo", "PRre", "PRva",
                                                    "HHue", "HHaf", "HHfb", "HHpb", "HHpo", "HHre", "HHva",
                                                    "IHHue", "IHHaf", "IHHfb", "IHHpb", "IHHpo", "IHHre", "IHHva"), 
              names_glue = "{.value}.{sect}_99")

# Unir valores únicos de cvegeo con datos agrupados por cve_sub

ZM99 <- left_join(df_distinct, df_wide, by = "CVE_ZM")
View(ZM99)

# Guardar como archivo xlsx

library(openxlsx)
write.xlsx(ZM99, "ZM99.xlsx")
