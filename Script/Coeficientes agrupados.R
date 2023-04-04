# Cargar librerías

library(readxl)
library(dplyr)

# Leer datos desde un archivo xlsx

datos <- read_excel("C:\\Users\\rpm0a\\OneDrive\\Documentos\\RepTemplates\\ZonasMetro\\Bases temporales\\Bases Agrupadas\\Bases Largas Agrupadas\\BLzm19a.xlsx")


# Crear vector subsec_mun

subsec_mun <- datos %>% group_by(cvegeo, sect, CVE_ZM) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                                        af = sum(af, na.rm = TRUE),  
                                                                        fb = sum(fb, na.rm = TRUE), 
                                                                        pb = sum(pb, na.rm = TRUE), 
                                                                        po = sum(po, na.rm = TRUE), 
                                                                        re = sum(re, na.rm = TRUE), 
                                                                        va = sum(va, na.rm = TRUE))
# Crear vector tot_mun

tot_mun <- datos %>% group_by(cvegeo, CVE_ZM) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                            af = sum(af, na.rm = TRUE),  
                                                            fb = sum(fb, na.rm = TRUE), 
                                                            pb = sum(pb, na.rm = TRUE), 
                                                            po = sum(po, na.rm = TRUE), 
                                                            re = sum(re, na.rm = TRUE), 
                                                            va = sum(va, na.rm = TRUE))

# Crear vector subsec_zm

subsec_zm <- datos %>% group_by(sect) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                             af = sum(af, na.rm = TRUE),  
                                                             fb = sum(fb, na.rm = TRUE), 
                                                             pb = sum(pb, na.rm = TRUE), 
                                                             po = sum(po, na.rm = TRUE), 
                                                             re = sum(re, na.rm = TRUE), 
                                                             va = sum(va, na.rm = TRUE))

# Crear vector tot_zm

tot_zm <- datos %>%  summarize(ue = sum(ue, na.rm = TRUE), 
                                        af = sum(af, na.rm = TRUE),  
                                        fb = sum(fb, na.rm = TRUE), 
                                        pb = sum(pb, na.rm = TRUE), 
                                        po = sum(po, na.rm = TRUE), 
                                        re = sum(re, na.rm = TRUE), 
                                        va = sum(va, na.rm = TRUE))


# Numerador

subsec_mun_div <- left_join(subsec_mun, tot_mun, by = c("cvegeo" = "cvegeo", "CVE_ZM" = "CVE_ZM")) %>% 
  mutate(ue = ue.x/ue.y, af = af.x/af.y, fb = fb.x/fb.y, pb = pb.x/pb.y, po = po.x/po.y, re = re.x/re.y, va = va.x/va.y) %>% 
  select(-ue.x, -ue.y, -af.x, -af.y, -fb.x, -fb.y, -pb.x, -pb.y, -po.x, -po.y, -re.x, -re.y, -va.x, -va.y)

# Denominador

# Replicar la única fila de tot_zm para tener la misma cantidad de filas que subsec_zm

tot_zm_rep <- tot_zm[rep(1, nrow(subsec_zm)), ]

# Dividir subsec_zm entre tot_zm_rep y agregar la columna sect

subsec_tot_zm_div <- cbind(subsec_zm[, "sect"], subsec_zm[, c("ue", "af", "fb", "pb", "po", "re", "va")] / tot_zm_rep[, c("ue", "af", "fb", "pb", "po", "re", "va")])


# Resultado final QL

# Unir subsec_mun_div y subsec_tot_zm_div por CVE_ZM y cve_sub

QL <- left_join(subsec_mun_div, subsec_tot_zm_div, by = c("sect"))

# Dividir cada variable de subsec_mun_div entre la variable correspondiente de subsec_tot_zm_div

QL <- QL %>% mutate(QLue = ue.x / ue.y, QLaf = af.x / af.y, QLfb = fb.x/fb.y, QLpb = pb.x/pb.y, QLpo = po.x/po.y, QLre= re.x/re.y, QLva = va.x/va.y) %>% 
  select(-ue.x, -ue.y, -af.x, -af.y, -fb.x, -fb.y, -pb.x, -pb.y, -po.x, -po.y, -re.x, -re.y, -va.x, -va.y)


# Estimar coeficiente PR

PR <- subsec_mun %>% 
  left_join(subsec_zm, by = c("sect")) %>% 
  mutate(PRue = ue.x / ue.y,
         PRaf = af.x / af.y,
         PRfb = fb.x / fb.y,
         PRpb = pb.x / pb.y,
         PRpo = po.x / po.y,
         PRre = re.x / re.y,
         PRva = va.x / va.y) %>% 
  select(cvegeo, sect, CVE_ZM, PRue, PRaf, PRfb, PRpb, PRpo, PRre, PRva)


# Estimar coeficiente HH

# Replicar la única fila de tot_zm para tener la misma cantidad de filas que tot_mun

tot_zm_rep2 <- tot_zm[rep(1, nrow(tot_mun)), ]

# Estimar la parte que se resta

# Dividir subsec_zm entre tot_zm_rep y agregar la columna sect

resta <- cbind(tot_mun[, c("cvegeo", "CVE_ZM")], tot_mun[, c("ue", "af", "fb", "pb", "po", "re", "va")] / tot_zm_rep2[, c("ue", "af", "fb", "pb", "po", "re", "va")])


# Estimar HH

HH <- PR %>% 
  left_join(resta, by = c("cvegeo", "CVE_ZM")) %>% 
  mutate(HHue = PRue - ue,
         HHaf = PRaf - af,
         HHfb = PRfb - fb,
         HHpb = PRpb - pb,
         HHpo = PRpo - po,
         HHre = PRre - re,
         HHva = PRva - va) %>% 
  select(cvegeo, sect, CVE_ZM, HHue,HHaf, HHfb, HHpb, HHpo, HHre, HHva)


# Estimar IHH

IHH <- HH %>%
  mutate_at(vars(HHue, HHaf, HHfb, HHpb, HHpo, HHre, HHva), ~ 1 - .) %>%
  rename_with(~ paste0("IHH", gsub("HH", "", .)), starts_with("HH"))


# Unir datos 

BLzm19a_final <- left_join(datos, QL, by = c("cvegeo", "sect", "CVE_ZM")) %>%
  left_join(PR, by = c("cvegeo", "sect", "CVE_ZM")) %>%
  left_join(HH, by = c("cvegeo", "sect", "CVE_ZM")) %>%
  left_join(IHH, by = c("cvegeo", "sect", "CVE_ZM"))

View(BLzm19a_final)

# Guardar archivo

library(openxlsx)

write.xlsx(BLzm19a_final, "BLzm19a_final.xlsx")



