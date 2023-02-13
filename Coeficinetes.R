# Cargar paqueterias

library(readxl)
library(dplyr)

# Carga tu base de datos ancha

data <- readxl::read_xlsx("C:\\Users\\rpm0a\\OneDrive\\Documentos\\RepTemplates\\ZonasMetro\\Bases temporales\\zm99.xlsx")

# Agrupamos por CVE_ZM

grouped_data <- group_by(data, CVE_ZM)

# Resumir las variables de interés por la media

grouped_data <- summarise_at(grouped_data, vars(starts_with("ue_"), starts_with("af_"), starts_with("fb_"), 
                            starts_with("pb_"), starts_with("po_"), starts_with("re_"), 
                            starts_with("va_")), mean, na.rm = TRUE)

# Filtrar las variables de interés

filtered_data <- grouped_data[, grep("^ue_|^af_|^fb_|^pb_|^po_|^re_|^va_", names(grouped_data))]

# Calcular la matriz de covarianza para cada grupo

cov_matrices <- sapply(split(filtered_data, grouped_data$CVE_ZM), function(group) {
  cov(group)
})

# Calcular los coeficientes QL respecto a su zona metropolitana

ql_coefs <- lapply(grouped_data, function(group) {
  n <- ncol(group)
  if (is.integer(n)) {
    ql_coefs <- numeric(n)
    for (i in 1:n) {
      ql_coefs[i] <- as.numeric(group[i, ] %*% solve(cov_matrices[[i]]) %*% t(group[i, ]))
    }
    return(ql_coefs)
  } else {
    return(NULL)
  }
})

View(ql_coefs)