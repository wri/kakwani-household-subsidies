rm(list=ls())

# =========================================================
# KAKWANI, SUITS Y EFECTO DE AUMENTO DEL 10%
# EN EL PRECIO DE 4 BIENES
# ENIGH - R STUDIO
# =========================================================

# =========================================================
# 1. INSTALAR PAQUETES (SOLO PRIMERA VEZ)
# =========================================================

# install.packages(c(
#   "tidyverse",
#   "ineq",
#   "DescTools",
#   "writexl"
# ))

library(tidyverse)
library(ineq)
library(DescTools)
library(writexl)

# =========================================================
# 2. CARGAR ARCHIVO CSV
# =========================================================

enigh <- read.csv("concentradohogar.csv")

# =========================================================
# 3. DEFINIR VARIABLES
# =========================================================

ingreso <- enigh$ing_cor
factor  <- enigh$factor

bien1 <- enigh$alimentos
bien2 <- enigh$combus
bien3 <- enigh$energia
bien4 <- enigh$publico

# =========================================================
# 4. CREAR BASE LIMPIA
# =========================================================

base <- data.frame(
  ingreso,
  factor,
  bien1,
  bien2,
  bien3,
  bien4
)

base <- base %>%
  filter(
    !is.na(ingreso),
    !is.na(factor),
    ingreso > 0
  )

# =========================================================
# 5. FUNCION INDICE DE CONCENTRACION
# =========================================================

concentration_index <- function(y, rankvar, weights = NULL){
  
  df <- data.frame(y, rankvar, weights)
  
  # eliminar NA
  df <- df[
    complete.cases(df) &
      df$y >= 0 &
      df$rankvar > 0,
  ]
  
  # ordenar por ingreso
  df <- df[order(df$rankvar), ]
  
  # pesos
  if(is.null(weights)){
    weights <- rep(1, nrow(df))
  } else {
    weights <- df$weights
  }
  
  y <- df$y
  
  # evitar division entre cero
  if(sum(y * weights) == 0){
    return(NA)
  }
  
  # poblacion acumulada
  cw <- cumsum(weights) / sum(weights)
  
  # gasto acumulado
  cy <- cumsum(y * weights) / sum(y * weights)
  
  # agregar origen
  cw <- c(0, cw)
  cy <- c(0, cy)
  
  # area bajo curva
  B <- sum(
    diff(cw) *
      (
        head(cy, -1) +
          tail(cy, -1)
      ) / 2
  )
  
  # indice de concentracion
  C <- 1 - 2 * B
  
  return(C)
}

# =========================================================
# 6. FUNCION SUITS (FORMULA ORIGINAL)
# =========================================================

suits_index <- function(y, income, weights = NULL){
  
  df <- data.frame(y, income, weights)
  
  # limpiar datos
  df <- df[
    complete.cases(df) &
      df$y >= 0 &
      df$income > 0,
  ]
  
  # ordenar por ingreso
  df <- df[order(df$income), ]
  
  # pesos
  if(is.null(weights)){
    weights <- rep(1, nrow(df))
  } else {
    weights <- df$weights
  }
  
  y <- df$y
  income <- df$income
  
  # totales
  total_tax <- sum(y * weights)
  total_income <- sum(income * weights)
  
  # evitar division entre cero
  if(total_tax == 0 | total_income == 0){
    return(NA)
  }
  
  # ingreso acumulado
  Y <- cumsum(income * weights) / total_income
  
  # impuesto/gasto acumulado
  T <- cumsum(y * weights) / total_tax
  
  # agregar origen
  Y <- c(0, Y)
  T <- c(0, T)
  
  # formula original de suits
  S <- 1 - sum(
    (T[-1] + T[-length(T)]) *
      diff(Y)
  )
  
  return(S)
}

# =========================================================
# 7. FUNCION CURVA DE CONCENTRACION
# =========================================================

plot_concentration <- function(
    y,
    ingreso,
    weights = NULL,
    titulo = ""
){
  
  df <- data.frame(
    y,
    ingreso,
    weights
  )
  
  # limpiar
  df <- df[
    complete.cases(df) &
      df$y >= 0 &
      df$ingreso > 0,
  ]
  
  # ordenar
  df <- df[order(df$ingreso), ]
  
  # pesos
  if(is.null(weights)){
    weights <- rep(1, nrow(df))
  } else {
    weights <- df$weights
  }
  
  # poblacion acumulada
  cw <- cumsum(weights) / sum(weights)
  
  # gasto acumulado
  cy <- cumsum(df$y * weights) /
    sum(df$y * weights)
  
  # agregar origen
  cw <- c(0, cw)
  cy <- c(0, cy)
  
  # graficar
  plot(
    cw,
    cy,
    type = "l",
    lwd = 2,
    col = "red",
    xlab = "Población acumulada",
    ylab = "Gasto acumulado",
    main = titulo
  )
  
  # linea igualdad
  abline(0,1,lty=2)
}

# =========================================================
# 8. GINI DEL INGRESO
# =========================================================

gini_ingreso <- Gini(
  base$ingreso,
  weights = base$factor
)

print(gini_ingreso)

# =========================================================
# 9. ESCENARIO:
# AUMENTO DEL 10% EN PRECIOS
# =========================================================

aumento <- 0.10

# impacto monetario adicional
base$impacto_bien1 <- base$bien1 * aumento
base$impacto_bien2 <- base$bien2 * aumento
base$impacto_bien3 <- base$bien3 * aumento
base$impacto_bien4 <- base$bien4 * aumento

# =========================================================
# 10. INDICES DE CONCENTRACION
# =========================================================

C1 <- concentration_index(
  base$impacto_bien1,
  base$ingreso,
  base$factor
)

C2 <- concentration_index(
  base$impacto_bien2,
  base$ingreso,
  base$factor
)

C3 <- concentration_index(
  base$impacto_bien3,
  base$ingreso,
  base$factor
)

C4 <- concentration_index(
  base$impacto_bien4,
  base$ingreso,
  base$factor
)

# =========================================================
# 11. KAKWANI
# =========================================================

K1 <- C1 - gini_ingreso
K2 <- C2 - gini_ingreso
K3 <- C3 - gini_ingreso
K4 <- C4 - gini_ingreso

# =========================================================
# 12. SUITS
# =========================================================

S1 <- suits_index(
  base$impacto_bien1,
  base$ingreso,
  base$factor
)

S2 <- suits_index(
  base$impacto_bien2,
  base$ingreso,
  base$factor
)

S3 <- suits_index(
  base$impacto_bien3,
  base$ingreso,
  base$factor
)

S4 <- suits_index(
  base$impacto_bien4,
  base$ingreso,
  base$factor
)

# =========================================================
# 13. RESULTADOS FINALES
# =========================================================

resultado_final <- data.frame(
  
  Bien = c(
    "Food & Beverage",
    "Gasoline",
    "Electricity",
    "Public Transportation"
  ),
  
  Aumento_Precio = "+10%",
  
  Concentracion = c(
    C1,
    C2,
    C3,
    C4
  ),
  
  Gini = gini_ingreso,
  
  Kakwani = c(
    K1,
    K2,
    K3,
    K4
  ),
  
  Suits = c(
    S1,
    S2,
    S3,
    S4
  ),
  
  Interpretacion_Kakwani = c(
    ifelse(K1 > 0,"Progresive","Regresive"),
    ifelse(K2 > 0,"Progresive","Regresive"),
    ifelse(K3 > 0,"Progresive","Regresive"),
    ifelse(K4 > 0,"Progresive","Regresive")
  ),
  
  Interpretacion_Suits = c(
    ifelse(S1 > 0,"Progresive","Regresive"),
    ifelse(S2 > 0,"Progresive","Regresive"),
    ifelse(S3 > 0,"Progresive","Regresive"),
    ifelse(S4 > 0,"Progresive","Regresive")
  )
)

print(resultado_final)

# =========================================================
# 14. EXPORTAR RESULTADOS
# =========================================================

write_xlsx(
  list(
    Resultados = resultado_final
  ),
  "Resultados_ENIGH_Impacto_10.xlsx"
)

# =========================================================
# 15. CURVA DE LORENZ DEL INGRESO
# =========================================================

Lc_ingreso <- Lc(
  base$ingreso,
  weights = base$factor
)

plot(
  Lc_ingreso,
  main = "Curva de Lorenz - Ingreso",
  col = "blue",
  lwd = 2
)

abline(0,1,lty=2)

# =========================================================
# 16. CURVAS DE CONCENTRACION
# =========================================================

plot_concentration(
  base$impacto_bien1,
  base$ingreso,
  base$factor,
  "+ 10% - Food & Beverage"
)

plot_concentration(
  base$impacto_bien2,
  base$ingreso,
  base$factor,
  "+10% - Gasoline"
)

plot_concentration(
  base$impacto_bien3,
  base$ingreso,
  base$factor,
  "+10% - Electricity"
)

plot_concentration(
  base$impacto_bien4,
  base$ingreso,
  base$factor,
  "+10% - Public Transportation"
)

# =========================================================
# INTERPRETACION
# =========================================================
# KAKWANI > 0  = PROGRESIVO
# KAKWANI < 0  = REGRESIVO
#
# SUITS > 0    = PROGRESIVO
# SUITS < 0    = REGRESIVO
#
# El aumento del 10% representa:
# gasto adicional = gasto del hogar * 10%
# =========================================================