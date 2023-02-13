# Cargar paqueterias

library(readxl)
library(dplyr)

# Carga tu base de datos ancha

data <- readxl::read_xlsx("C:\\Users\\rpm0a\\OneDrive\\Documentos\\RepTemplates\\ZonasMetro\\Bases temporales\\zm99.xlsx")

# Agrupamos por CVE_ZM

grouped_data <- group_by(data, CVE_ZM)


# Aplica la función de agregación que deseas (promedio, suma, etc.) a las variables de interés

grouped_data <- summarise_at(grouped_data, vars(starts_with("ue_"), starts_with("af_"), starts_with("fb_"), 
                starts_with("pb_"), starts_with("po_"), starts_with("re_"), starts_with("va_")), mean, na.rm = TRUE)

#

cov_matrices <- lapply(grouped_data, function(group) {
  cov(group[, grep("^ue_|^af_|^fb_|^pb_|^po_|^re_|^va_", names(group))])})