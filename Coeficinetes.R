# Cargar paqueterias

library(spatstat)
library(readxl)

# Carga tu base de datos ancha

data <- readxl::read_xlsx("C:\\Users\\rpm0a\\OneDrive\\Documentos\\RepTemplates\\ZonasMetro\\Bases temporales\\zm99.xlsx")

# Agrupa la base de datos por Zona Metropolitana

grouped_data <- split(data, data$CVE_ZM)

# Crea una lista para guardar los resultados

ql_results <- list()

# Itera sobre cada grupo de datos (cada Zona Metropolitana)

for (group in names(grouped_data)) 
  
  # Crea un objeto tipo ppp para cada grupo
  
ppp_group <- as.ppp(grouped_data[[group]][, c("longitud", "latitud")], 
                    marks=cbind(grouped_data[[group]]$ue_, grouped_data[[group]]$af_, 
                    grouped_data[[group]]$fb_, grouped_data[[group]]$pb_, grouped_data[[group]]$po_, 
                    grouped_data[[group]]$re_, grouped_data[[group]]$va_), 
                    window=owin(c(0, 1), c(0, 1)))

  
  # Estima el coeficiente QL para cada grupo
  ql_group <- ql.ppp(ppp_group)
  
  # Guarda el resultado en la lista
  ql_results[[group]] <- ql_group
}

# Guarda los resultados en un data.frame
ql_df <- data.frame(Zona_Metropolitana=names(ql_results), QL=unlist(ql_results))
