# cargar la librería openxlsx

library(openxlsx)

# cargar la base de datos
datos <- read.xlsx("ruta/de/tu/archivo/excel.xlsx", sheetIndex = 1)

# obtener una lista de las zonas metropolitanas
zonas <- unique(datos$CVE_ZM)

# iterar sobre cada zona metropolitana
for (i in 1:length(zonas)) {
  
  # seleccionar los datos para la zona actual
  datos_filtrados <- subset(datos, CVE_ZM == zonas[i])
  
  # escribir los datos en un nuevo archivo excel
  write.xlsx(datos_filtrados, file = paste0("ruta/de/tu/nuevo/archivo/excel_", zonas[i], ".xlsx"), sheetName = "datos", colNames = TRUE, rowNames = FALSE, append = FALSE)
  
}

