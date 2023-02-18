# Cargar librerías

library(readxl)
library(dplyr)

# Leer datos desde un archivo xlsx

datos <- read_excel("C:\\Users\\leope\\Documents\\RepTemplates\\ZonasMetro\\Bases temporales\\BL_zm99.xlsx")

# Crear vector subsec_mun
subsec_mun <- datos %>% group_by(cvegeo, CVE_ZM, cve_sub) %>% summarize(ue = sum(ue), af = sum(af), fb = sum(fb), pb = sum(pb), po = sum(po), re = sum(re), va = sum(va))

# Crear vector tot_mun
tot_mun <- datos %>% group_by(CVE_ZM, cvegeo) %>% summarize(ue = sum(ue), af = sum(af), fb = sum(fb), pb = sum(pb), po = sum(po), re = sum(re), va = sum(va))

# Crear vector subsec_zm
subsec_zm <- datos %>% group_by(CVE_ZM, cve_sub) %>% summarize(ue = sum(ue), af = sum(af), fb = sum(fb), pb = sum(pb), po = sum(po), re = sum(re), va = sum(va))

# Crear vector tot_zm
tot_zm <- datos %>% group_by(CVE_ZM) %>% summarize(ue = sum(ue), af = sum(af), fb = sum(fb), pb = sum(pb), po = sum(po), re = sum(re), va = sum(va))

# Numerador

subsec_mun_div <- left_join(subsec_mun, tot_mun, by = c("cvegeo" = "cvegeo", "CVE_ZM" = "CVE_ZM")) %>% 
  mutate(ue = ue.x/ue.y, af = af.x/af.y, fb = fb.x/fb.y, pb = pb.x/pb.y, po = po.x/po.y, re = re.x/re.y, va = va.x/va.y) %>% 
  select(-ue.x, -ue.y, -af.x, -af.y, -fb.x, -fb.y, -pb.x, -pb.y, -po.x, -po.y, -re.x, -re.y, -va.x, -va.y)

# Denominador

# Unir vectores subsec_zm y tot_zm por CVE_ZM

subsec_tot_zm <- left_join(subsec_zm, tot_zm, by = "CVE_ZM")

# Dividir los valores de subsec_zm entre los valores de tot_zm por CVE_ZM
subsec_tot_zm_div <- subsec_tot_zm %>%
  mutate(ue.div = ue.x/ue.y, af.div = af.x/af.y, fb.div = fb.x/fb.y, pb.div = pb.x/pb.y, po.div = po.x/po.y, re.div = re.x/re.y, va.div = va.x/va.y) %>% 
  select(-ue.x, -ue.y, -af.x, -af.y, -fb.x, -fb.y, -pb.x, -pb.y, -po.x, -po.y, -re.x, -re.y, -va.x, -va.y)

# Resultado final QL

# Unir subsec_mun_div y subsec_tot_zm_div por CVE_ZM y cve_sub

QL <- left_join(subsec_mun_div, subsec_tot_zm_div, by = c("CVE_ZM", "cve_sub"))

# Dividir cada variable de subsec_mun_div entre la variable correspondiente de subsec_tot_zm_div

QL <- QL %>% mutate(QLue = ue / ue.div, QLaf = af / af.div, QLfb = fb/fb.div, QLpb = pb/pb.div, QLpo = po/po.div, QLre= re/re.div, QLva = va/va.div) %>% 
  select(-ue, -ue.div, -af, -af.div, -fb, -fb.div, -fb, -fb.div, -pb, -pb.div, -po, -po.div, -re, -re.div, -va, -va.div)

View(QL)