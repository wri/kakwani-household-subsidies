rm(list=ls())

# =========================================================
# KAKWANI, SUITS Y EFECTO DE AUMENTO DEL 10%
# EN EL PRECIO DE 4 BIENES
# HCES INDIA - R STUDIO
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
library(haven)
library(dplyr)

# =========================================================
# 2. LLAVE ÚNICA DE HOGAR
#    Combinación de 6 variables que identifica
#    de forma unívoca a cada hogar en la HCES
# =========================================================

LLAVE_HOGAR <- c(
  "FSU_Serial_No",
  "Sub_sample",
  "Sample_SU_No",
  "Sample_Sub_Division_No",
  "Second_Stage_Stratum_No",
  "Sample_Household_No"
)

# Multiplier va aparte: es una variable de ponderación,
# no forma parte de la llave de identificación
LLAVE_JOIN <- c(LLAVE_HOGAR, "Multiplier")

# =========================================================
# 3. CARGAR Y PREPARAR DATOS
# =========================================================

# ==============================================================================
# 3.1 INGRESO/GASTO TOTAL — Level 15 (Sections A2, B2, C2)
#     Un hogar aparece hasta 3 veces (una por visita/cuestionario).
#     El group_by + sum colapsa a 1 fila por hogar.
# ==============================================================================

ingreso_raw <- read_dta("LEVEL - 15 (Section 1_1, A2,B2  C2).dta")

# Verificar nombres de columnas:
# names(ingreso_raw)

ingreso <- ingreso_raw |>
  rename(Multiplier = MULTIPLIER) |>
  filter(SECTION %in% c("A2", "B2", "C2")) |>
  group_by(across(all_of(LLAVE_JOIN))) |>
  summarise(
    ingreso_total = sum(MONTHLY_CONSUMPTION_EXP, na.rm = TRUE),
    .groups = "drop"
  )

# Diagnóstico: debe dar 0 duplicados
dupl_ingreso <- ingreso |>
  count(across(all_of(LLAVE_HOGAR))) |>
  filter(n > 1) |>
  nrow()
cat("Duplicados en ingreso:", dupl_ingreso, "\n")

# ==============================================================================
# 3.2 ELECTRICIDAD — Level 08 (Section 8_1)
#     Item Code 332 = Electricity
# ==============================================================================

energia_raw <- read_dta("LEVEL - 08 (Section 8_1).dta")

# names(energia_raw)

energia <- energia_raw |>
  filter(Item_Code_8_1 == 332) |>
  group_by(across(all_of(LLAVE_JOIN))) |>
  summarise(
    gasto_electricidad = sum(Total_consumption_value_rs, na.rm = TRUE),
    .groups = "drop"
  )

# ==============================================================================
# 3.3 COMBUSTIBLES Y TRANSPORTE — Level 09 (Sections 9, 10, 11)
#     Gasolinas:          512 = Petrol for vehicle; 513 = Diesel for vehicle
#     Transporte público: 024 = Railway fare; 025 = Bus fare (work);
#                         504 = Bus fare (school); 505 = Bus/tram fare (work);
#                         506 = Bus/tram fare (school)
# ==============================================================================

publico_og_raw <- read_dta("LEVEL - 09 (Section 9  10  11).dta")

# names(publico_og_raw)

# ---- Gasolinas ---------------------------------------------------------------
gasolinas <- publico_og_raw |>
  filter(Item_Code_9_1_to_11_4 %in% c(512, 513)) |>
  group_by(across(all_of(LLAVE_JOIN))) |>
  summarise(
    gasto_gasolinas = sum(Value_Rs_9_1_to_11_4, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Transporte público ------------------------------------------------------
transporte_pub <- publico_og_raw |>
  filter(Item_Code_9_1_to_11_4 %in% c(024,505)) |>
  group_by(across(all_of(LLAVE_JOIN))) |>
  summarise(
    gasto_transporte_pub = sum(Value_Rs_9_1_to_11_4, na.rm = TRUE),
    .groups = "drop"
  )

# ==============================================================================
# 3.4 ALIMENTOS — Level 05 (Sections 5 & 6)
#     Se suman todos los ítems del nivel sin filtro adicional
# ==============================================================================

alimentos_raw <- read_dta("LEVEL - 05 ( Sec 5  6).dta")

# names(alimentos_raw)

alimentos <- alimentos_raw |>
  group_by(across(all_of(LLAVE_JOIN))) |>
  summarise(
    gasto_alimentos = sum(Total_Consumption_Value, na.rm = TRUE),
    .groups = "drop"
  )

# ==============================================================================
# 3.5 UNIÓN DE TODAS LAS BASES
#     full_join conserva TODOS los hogares aunque no tengan registro
#     en algún módulo (el gasto queda en NA → se reemplaza con 0)
# ==============================================================================

base_integrada <- ingreso |>
  full_join(energia,        by = LLAVE_JOIN) |>
  full_join(gasolinas,      by = LLAVE_JOIN) |>
  full_join(transporte_pub, by = LLAVE_JOIN) |>
  full_join(alimentos,      by = LLAVE_JOIN)

# Reemplazar NA con 0 en columnas de gasto
enigh <- base_integrada |>
  mutate(across(
    c(ingreso_total, gasto_electricidad, gasto_gasolinas,
      gasto_transporte_pub, gasto_alimentos),
    ~ replace_na(.x, 0)
  ))

# =========================================================
# 4. DEFINIR VARIABLES DE TRABAJO
# =========================================================

ingreso <- enigh$ingreso_total
factor  <- enigh$Multiplier

bien1 <- enigh$gasto_alimentos
bien2 <- enigh$gasto_gasolinas
bien3 <- enigh$gasto_electricidad
bien4 <- enigh$gasto_transporte_pub

# =========================================================
# 5. CREAR BASE LIMPIA
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

cat("Hogares en base final:", nrow(base), "\n")

# =========================================================
# 6. FUNCIÓN ÍNDICE DE CONCENTRACIÓN
# =========================================================

concentration_index <- function(y, rankvar, weights = NULL){
  
  df <- data.frame(y, rankvar, weights)
  
  df <- df[
    complete.cases(df) &
      df$y >= 0 &
      df$rankvar > 0,
  ]
  
  df <- df[order(df$rankvar), ]
  
  if(is.null(weights)){
    weights <- rep(1, nrow(df))
  } else {
    weights <- df$weights
  }
  
  y <- df$y
  
  if(sum(y * weights) == 0){
    return(NA)
  }
  
  cw <- cumsum(weights) / sum(weights)
  cy <- cumsum(y * weights) / sum(y * weights)
  
  cw <- c(0, cw)
  cy <- c(0, cy)
  
  B <- sum(
    diff(cw) *
      (head(cy, -1) + tail(cy, -1)) / 2
  )
  
  C <- 1 - 2 * B
  
  return(C)
}

# =========================================================
# 7. FUNCIÓN SUITS (FÓRMULA ORIGINAL)
# =========================================================

suits_index <- function(y, income, weights = NULL){
  
  df <- data.frame(y, income, weights)
  
  df <- df[
    complete.cases(df) &
      df$y >= 0 &
      df$income > 0,
  ]
  
  df <- df[order(df$income), ]
  
  if(is.null(weights)){
    weights <- rep(1, nrow(df))
  } else {
    weights <- df$weights
  }
  
  y      <- df$y
  income <- df$income
  
  total_tax    <- sum(y * weights)
  total_income <- sum(income * weights)
  
  if(total_tax == 0 | total_income == 0){
    return(NA)
  }
  
  Y <- cumsum(income * weights) / total_income
  T <- cumsum(y * weights) / total_tax
  
  Y <- c(0, Y)
  T <- c(0, T)
  
  S <- 1 - sum(
    (T[-1] + T[-length(T)]) * diff(Y)
  )
  
  return(S)
}

# =========================================================
# 8. FUNCIÓN CURVA DE CONCENTRACIÓN
# =========================================================

plot_concentration <- function(y, ingreso, weights = NULL, titulo = ""){
  
  df <- data.frame(y, ingreso, weights)
  
  df <- df[
    complete.cases(df) &
      df$y >= 0 &
      df$ingreso > 0,
  ]
  
  df <- df[order(df$ingreso), ]
  
  if(is.null(weights)){
    weights <- rep(1, nrow(df))
  } else {
    weights <- df$weights
  }
  
  cw <- cumsum(weights) / sum(weights)
  cy <- cumsum(df$y * weights) / sum(df$y * weights)
  
  cw <- c(0, cw)
  cy <- c(0, cy)
  
  plot(
    cw, cy,
    type = "l", lwd = 2, col = "red",
    xlab = "Accumulated Population",
    ylab = "Accumulated Expenditure",
    main = titulo
  )
  
  abline(0, 1, lty = 2)
}

# =========================================================
# 9. GINI DEL INGRESO/GASTO TOTAL
# =========================================================

gini_ingreso <- Gini(
  base$ingreso,
  weights = base$factor
)

print(paste("Gini:", round(gini_ingreso, 4)))

# =========================================================
# 10. ESCENARIO: AUMENTO DEL 10% EN PRECIOS
# =========================================================

aumento <- 0.10

base$impacto_bien1 <- base$bien1 * aumento
base$impacto_bien2 <- base$bien2 * aumento
base$impacto_bien3 <- base$bien3 * aumento
base$impacto_bien4 <- base$bien4 * aumento

# =========================================================
# 11. ÍNDICES DE CONCENTRACIÓN
# =========================================================

C1 <- concentration_index(base$impacto_bien1, base$ingreso, base$factor)
C2 <- concentration_index(base$impacto_bien2, base$ingreso, base$factor)
C3 <- concentration_index(base$impacto_bien3, base$ingreso, base$factor)
C4 <- concentration_index(base$impacto_bien4, base$ingreso, base$factor)

# =========================================================
# 12. KAKWANI
# =========================================================

K1 <- C1 - gini_ingreso
K2 <- C2 - gini_ingreso
K3 <- C3 - gini_ingreso
K4 <- C4 - gini_ingreso

# =========================================================
# 13. SUITS
# =========================================================

S1 <- suits_index(base$impacto_bien1, base$ingreso, base$factor)
S2 <- suits_index(base$impacto_bien2, base$ingreso, base$factor)
S3 <- suits_index(base$impacto_bien3, base$ingreso, base$factor)
S4 <- suits_index(base$impacto_bien4, base$ingreso, base$factor)

# =========================================================
# 14. RESULTADOS FINALES
# =========================================================

resultado_final <- data.frame(
  
  Bien = c(
    "Food & Beverage",
    "Gasoline",
    "Electricity",
    "Public Transportation"
  ),
  
  Aumento_Precio = "+10%",
  
  Concentracion = c(C1, C2, C3, C4),
  
  Gini = gini_ingreso,
  
  Kakwani = c(K1, K2, K3, K4),
  
  Suits = c(S1, S2, S3, S4),
  
  Interpretacion_Kakwani = c(
    ifelse(K1 > 0, "Progressive", "Regressive"),
    ifelse(K2 > 0, "Progressive", "Regressive"),
    ifelse(K3 > 0, "Progressive", "Regressive"),
    ifelse(K4 > 0, "Progressive", "Regressive")
  ),
  
  Interpretacion_Suits = c(
    ifelse(S1 > 0, "Progressive", "Regressive"),
    ifelse(S2 > 0, "Progressive", "Regressive"),
    ifelse(S3 > 0, "Progressive", "Regressive"),
    ifelse(S4 > 0, "Progressive", "Regressive")
  )
)

print(resultado_final)

# =========================================================
# 15. EXPORTAR RESULTADOS
# =========================================================

write_xlsx(
  list(Resultados = resultado_final),
  "Resultados_India_Impacto_10.xlsx"
)

# =========================================================
# 16. CURVA DE LORENZ DEL INGRESO/GASTO TOTAL
# =========================================================

Lc_ingreso <- Lc(
  base$ingreso,
  weights = base$factor
)

plot(
  Lc_ingreso,
  main = "Lorenz Curve - Total Expenditure",
  col  = "blue",
  lwd  = 2
)

abline(0, 1, lty = 2)

# =========================================================
# 17. CURVAS DE CONCENTRACIÓN
# =========================================================

plot_concentration(
  base$impacto_bien1, base$ingreso, base$factor,
  "+10% - Food & Beverage"
)

plot_concentration(
  base$impacto_bien2, base$ingreso, base$factor,
  "+10% - Gasoline"
)

plot_concentration(
  base$impacto_bien3, base$ingreso, base$factor,
  "+10% - Electricity"
)

plot_concentration(
  base$impacto_bien4, base$ingreso, base$factor,
  "+10% - Public Transportation"
)

# =========================================================
# 18. EXPENDITURE SHARES BY DECILE (STACKED BAR CHART)
# =========================================================

# --- 18a. Crear deciles de gasto ponderados ---
base <- base %>%
  arrange(ingreso) %>%
  mutate(
    cum_weight   = cumsum(factor),
    total_weight = sum(factor),
    decil        = cut(
      cum_weight / total_weight,
      breaks         = seq(0, 1, by = 0.1),
      labels         = 1:10,
      include.lowest = TRUE
    )
  )

# --- 18b. Calcular "Otros" por hogar ---
# Otros = gasto total - los 4 bienes analizados
base <- base %>%
  mutate(
    gasto_total = bien1 + bien2 + bien3 + bien4,
    otros       = pmax(ingreso - gasto_total, 0)
  )

# --- 18c. Shares ponderados por decil ---
shares_decil <- base %>%
  group_by(decil) %>%
  summarise(
    w           = sum(factor),
    Food        = sum(bien1 * factor) / sum(ingreso * factor) * 100,
    Fuels       = sum(bien2 * factor) / sum(ingreso * factor) * 100,
    Electricity = sum(bien3 * factor) / sum(ingreso * factor) * 100,
    PublicTrans = sum(bien4 * factor) / sum(ingreso * factor) * 100,
    Others      = sum(otros * factor) / sum(ingreso * factor) * 100,
    .groups = "drop"
  ) %>%
  select(-w)

# --- 18d. Formato largo para ggplot ---
shares_long <- shares_decil %>%
  pivot_longer(
    cols      = c(Food, Fuels, Electricity, PublicTrans, Others),
    names_to  = "Categoria",
    values_to = "Share"
  ) %>%
  mutate(
    Categoria = factor(
      Categoria,
      levels = c("Others", "PublicTrans", "Electricity", "Fuels", "Food")
    )
  )

# --- 18e. Paleta de colores ---
colores <- c(
  "Food"        = "#1F4E79",
  "Fuels"       = "#4E7C3F",
  "Electricity" = "#7B2D42",
  "PublicTrans" = "#D4A017",
  "Others"      = "#8DB4C8"
)

etiquetas <- c(
  "Food"        = "Food & Beverage",
  "Fuels"       = "Vehicle's Fuels",
  "Electricity" = "Electricity",
  "PublicTrans" = "Public Transportation",
  "Others"      = "Others"
)

# --- 18f. Gráfica ---
grafica_shares <- ggplot(
  shares_long,
  aes(x = decil, y = Share, fill = Categoria)
) +
  geom_bar(stat = "identity", width = 0.75) +
  scale_fill_manual(
    values = colores,
    labels = etiquetas,
    guide  = guide_legend(nrow = 2)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),
    limits = c(0, 110),
    expand = c(0, 0)
  ) +
  scale_x_discrete(labels = 1:10) +
  labs(
    title = "Expenditure shares by decile",
    x     = NULL,
    y     = "%",
    fill  = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background    = element_rect(fill = "#D6E8F0", color = NA),
    panel.background   = element_rect(fill = "#D6E8F0", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "white", linewidth = 0.5),
    plot.title         = element_text(hjust = 0.5, face = "bold"),
    legend.position    = "bottom",
    legend.key.size    = unit(0.5, "cm"),
    axis.ticks.x       = element_blank()
  )

print(grafica_shares)



# =========================================================
# INTERPRETACIÓN
# =========================================================
# KAKWANI > 0  = PROGRESIVO  (los ricos pagan proporcionalmente más)
# KAKWANI < 0  = REGRESIVO   (los pobres pagan proporcionalmente más)
#
# SUITS > 0    = PROGRESIVO
# SUITS < 0    = REGRESIVO
#
# El aumento del 10% representa:
# gasto adicional = gasto del hogar * 10%
# =========================================================
