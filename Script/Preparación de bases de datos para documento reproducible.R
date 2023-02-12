#Preparación de bases de datos para documento reproducible

# Cargar librerias

library(readxl)
library(dplyr)

# Cargar base

zm04 <- read_excel("RepTemplates/ZonasMetro/Bases temporales/zm04.xlsx")

# Agrupar los datos por clave de zona metropolitana

grouped_data <- group_by(zm04, CVE_ZM, NOM_ZM)
View(grouped_data)

# Seleccionar solo las variables que comienzan con "ue_", "af_", "fb_", "pb_", "po_", "re_" o "va_"

filtered_data <- grouped_data %>% select(matches("^ue_|^af_|^fb_|^pb_|^po_|^re_|^va_"))

# Calcular los subtotales para las variables seleccionadas

subtotals <- summarise_all(filtered_data, sum, na.rm = TRUE)

View(subtotals)

# Guardar como una nueva hoja

library(openxlsx)

write.xlsx(subtotals, file = "zm04.xlsx", sheetName = "Totales ZM", append = TRUE)
