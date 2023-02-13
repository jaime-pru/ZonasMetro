# Cargar paqueterias

library(readxl)

# Carga tu base de datos ancha

data <- readxl::read_xlsx("C:\\Users\\rpm0a\\OneDrive\\Documentos\\RepTemplates\\ZonasMetro\\Bases temporales\\zm99.xlsx")

# Agrupamos por CVE_ZM

grouped_data <- group_by(data, CVE_ZM)