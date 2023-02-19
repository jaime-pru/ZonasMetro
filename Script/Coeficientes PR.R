# Cargar librerías

library(readxl)
library(dplyr)

# Leer datos desde un archivo xlsx

datos <- read_excel("C:\\Users\\leope\\Documents\\RepTemplates\\ZonasMetro\\Bases temporales\\BL_zm99.xlsx")

# Crear vector subsec_mun

subsec_mun <- datos %>% group_by(cvegeo, cve_sub, CVE_ZM) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                                        af = sum(af, na.rm = TRUE),  
                                                                        fb = sum(fb, na.rm = TRUE), 
                                                                        pb = sum(pb, na.rm = TRUE), 
                                                                        po = sum(po, na.rm = TRUE), 
                                                                        re = sum(re, na.rm = TRUE), 
                                                                        va = sum(va, na.rm = TRUE))

# Crear vector subsec_zm

subsec_zm <- datos %>% group_by(CVE_ZM, cve_sub) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                               af = sum(af, na.rm = TRUE),  
                                                               fb = sum(fb, na.rm = TRUE), 
                                                               pb = sum(pb, na.rm = TRUE), 
                                                               po = sum(po, na.rm = TRUE), 
                                                               re = sum(re, na.rm = TRUE), 
                                                               va = sum(va, na.rm = TRUE))

  
# Crear vector con la división de las variables correspondientes

PR <- subsec_mun %>% 
  left_join(subsec_zm, by = c("cve_sub", "CVE_ZM")) %>% 
  mutate(PRue = ue.x / ue.y,
         PRaf = af.x / af.y,
         PRfb = fb.x / fb.y,
         PRpb = pb.x / pb.y,
         PRpo = po.x / po.y,
         PRre = re.x / re.y,
         PRva = va.x / va.y) %>% 
  select(cvegeo, cve_sub, CVE_ZM, PRue, PRaf, PRfb, PRpb, PRpo, PRre, PRva)

View(PR)

