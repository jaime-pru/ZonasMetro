# Cargar librerías

library(readxl)
library(dplyr)

# Leer datos desde un archivo xlsx

datos <- read_excel("C:\\Users\\leope\\Documents\\RepTemplates\\ZonasMetro\\Bases temporales\\BL_zm99.xlsx")

# Crear vector subsec_mun

subsec_mun <- datos %>% group_by(cvegeo, CVE_ZM, cve_sub) %>% summarize(po=sum(po))
View(subsec_mun)

# Crear vector tot_mun

tot_mun <- datos %>% group_by(CVE_ZM, cvegeo) %>% summarize(po=sum(po))

# Crear vector subsec_zm

subsec_zm <- datos %>% group_by(CVE_ZM, cve_sub) %>% summarize(po=sum(po))


# Crear vector tot_zm

tot_zm <- datos %>% group_by(CVE_ZM) %>% summarize(po=sum(po))

# Numerador

subsec_mun_div <- left_join(subsec_mun, tot_mun, by = c("cvegeo" = "cvegeo", "CVE_ZM" = "CVE_ZM")) %>% 
  mutate(po = po.x/po.y) %>% 
  select(-po.x, -po.y)
View(subsec_mun_div)

# Denominador

# Unir vectores subsec_zm y tot_zm por CVE_ZM
subsec_tot_zm <- left_join(subsec_zm, tot_zm, by = "CVE_ZM")

# Dividir los valores de subsec_zm entre los valores de tot_zm por CVE_ZM
subsec_tot_zm_div <- subsec_tot_zm %>%
  mutate(po_div = po.x / po.y) %>% 
  select(-po.x, -po.y)
View(subsec_tot_zm_div)

# Resultado final QL

# Unir subsec_mun_div y subsec_tot_zm_div por CVE_ZM y cve_sub

QL <- left_join(subsec_mun_div, subsec_tot_zm_div, by = c("CVE_ZM", "cve_sub"))

# Dividir po de subsec_mun_div entre po_div de subsec_tot_zm_div 

QL <- QL %>% mutate(QL = po / po_div) %>% 
  select(-po, -po_div)

View(QL)
View(datos)

#Guardar xlsx

library(openxlsx)

write.xlsx(QL, "resultados2.xlsx")


