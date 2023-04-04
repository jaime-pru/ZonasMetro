# Cargar librerías

library(readxl)
library(dplyr)

# Leer datos desde un archivo xlsx

datos <- read_excel("C:\\Users\\leope\\Documents\\RepTemplates\\ZonasMetro\\Bases temporales 2\\BLZM04.xlsx")
datos2 <- read_excel("C:\\Users\\leope\\Documents\\RepTemplates\\ZonasMetro\\Bases temporales 2\\Totales Nacionales\\Agrupadas\\ZMTN04a.xlsx")

# Crear vector subsec_mun

subsec_mun <- datos %>% group_by(CVE_ZM, sect) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                                        af = sum(af, na.rm = TRUE),  
                                                                        fb = sum(fb, na.rm = TRUE), 
                                                                        pb = sum(pb, na.rm = TRUE), 
                                                                        po = sum(po, na.rm = TRUE), 
                                                                        re = sum(re, na.rm = TRUE), 
                                                                        va = sum(va, na.rm = TRUE))
# Crear vector tot_mun

tot_mun <- datos %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                            af = sum(af, na.rm = TRUE),  
                                                            fb = sum(fb, na.rm = TRUE), 
                                                            pb = sum(pb, na.rm = TRUE), 
                                                            po = sum(po, na.rm = TRUE), 
                                                            re = sum(re, na.rm = TRUE), 
                                                            va = sum(va, na.rm = TRUE))

# Crear vector subsec_zm

subsec_zm <- datos2 %>% group_by(sect) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                             af = sum(af, na.rm = TRUE),  
                                                             fb = sum(fb, na.rm = TRUE), 
                                                             pb = sum(pb, na.rm = TRUE), 
                                                             po = sum(po, na.rm = TRUE), 
                                                             re = sum(re, na.rm = TRUE), 
                                                             va = sum(va, na.rm = TRUE))

# Crear vector tot_zm

tot_zm <- datos2 %>%  summarize(ue = sum(ue, na.rm = TRUE), 
                                        af = sum(af, na.rm = TRUE),  
                                        fb = sum(fb, na.rm = TRUE), 
                                        pb = sum(pb, na.rm = TRUE), 
                                        po = sum(po, na.rm = TRUE), 
                                        re = sum(re, na.rm = TRUE), 
                                        va = sum(va, na.rm = TRUE))


# Numerador

# Replicar la única fila de tot_mun para tener la misma cantidad de filas que subsec_mun

tot_mun_rep <- tot_mun[rep(1, nrow(subsec_mun)), ]

# Dividir subsec_zm entre tot_zm_rep y agregar la columna sect

subsec_mun_div <- cbind(subsec_mun[, c("CVE_ZM", "sect")], subsec_mun[, c("ue", "af", "fb", "pb", "po", "re", "va")] / tot_mun_rep[, c("ue", "af", "fb", "pb", "po", "re", "va")])

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

View(QL)

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
  select(CVE_ZM, sect, PRue, PRaf, PRfb, PRpb, PRpo, PRre, PRva)

View(PR)

# Estimar coeficiente HH

# Estimar la parte que se resta


# Dividir subsec_zm entre tot_zm_rep y agregar la columna sect

resta <-  tot_mun[, c("ue", "af", "fb", "pb", "po", "re", "va")] / tot_zm[, c("ue", "af", "fb", "pb", "po", "re", "va")]

View(resta2)

# Replicar la única fila de tot_zm para tener la misma cantidad de filas que subsec_zm

resta2 <- resta[rep(1, nrow(PR)), ]

# Estimar HH

HH <- cbind(PR[, c("CVE_ZM", "sect")], PR[, c("PRue", "PRaf", "PRfb", "PRpb", "PRpo", "PRre", "PRva")] - resta2[, c("ue", "af", "fb", "pb", "po", "re", "va")])

# Cambiando nombres a las variables

new_names <- c("CVE_ZM", "sect", "HHue", "HHaf", "HHfb", "HHpb", "HHpo", "HHre", "HHva")
colnames(HH) <- new_names

View(HH)

# Estimar IHH

IHH <- HH %>%
  mutate_at(vars(HHue, HHaf, HHfb, HHpb, HHpo, HHre, HHva), ~ 1 + .) %>%
  rename_with(~ paste0("IHH", gsub("HH", "", .)), starts_with("HH"))


# Unir datos 

BLZM99_final <- left_join(datos, QL, by = c("CVE_ZM", "sect")) %>%
  left_join(PR, by = c("CVE_ZM", "sect")) %>%
  left_join(HH, by = c("CVE_ZM", "sect")) %>%
  left_join(IHH, by = c("CVE_ZM", "sect"))

View(BLZM99_final)

# Guardar archivo

library(openxlsx)

write.xlsx(BLZM99_final, "BLZM99_final.xlsx")



