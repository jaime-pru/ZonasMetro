# Cargar paqueterias

library(readxl)
library(dplyr)

# Carga tu base de datos ancha

data <- readxl::read_xlsx("C:\\Users\\rpm0a\\OneDrive\\Documentos\\RepTemplates\\ZonasMetro\\Bases temporales\\zm99.xlsx")

# Agrupamos por CVE_ZM

grouped_data <- group_by(data, CVE_ZM)


# Aplica la función de agregación que deseas (promedio, suma, etc.) a las variables de interés

grouped_data <- grouped_data %>% 
  filter(n() >= 10)

cov_matrices <- lapply(grouped_data, function(group) {
  cov(group[, grep("^ue_|^af_|^fb_|^pb_|^po_|^re_|^va_", names(group))], use = "complete.obs")})

#

cov_matrices <- lapply(grouped_data, function(group) {
  cov(group[, grep("^ue_|^af_|^fb_|^pb_|^po_|^re_|^va_", names(group))])})