rm(list = ls())

# =============================================================================
# KAKWANI, SUITS Y EFECTO DE AUMENTO DEL 10% EN EL PRECIO DE 4 BIENES
# HCES ETIOPÍA 2015-2016
# =============================================================================
#
#
# ARCHIVOS REQUERIDOS (en el directorio de trabajo):
#   • HCES_2015_2016_DEMOGRAPHY.dta
#   • HCES_2015_2016_Expenditure.dta
#
# ESTRUCTURA CONFIRMADA DEL ARCHIVO DE EXPENDITURE:
#   ITEMC   = Código COICOP del ítem (8 dígitos)
#   TOP1    = División COICOP  (1-13)
#   TOP2    = Grupo COICOP
#   TOP3    = Clase COICOP
#   VALUE   = Gasto anual en Birr (ETB)
#   FOOD    = 1 → alimento  /  2 → no alimento
#   WGT     = Ponderador muestral
#
#
# LLAVE DE HOGAR (8 variables geográficas, presentes en AMBOS archivos):
#   CQ11 = Killil / Región   CQ12 = Zone    CQ13 = Wereda   CQ14 = Town
#   CQ15 = K/Ketema          CQ16 = Kebele  CQ17 = EA (PSU) CQ18 = Sel. HH
# =============================================================================


# =============================================================================
# 1. PAQUETES
# =============================================================================

# install.packages(c("tidyverse", "ineq", "DescTools", "writexl", "haven"))

library(tidyverse)
library(ineq)
library(DescTools)
library(writexl)
library(haven)


# =============================================================================
# 2. LLAVE ÚNICA DE HOGAR
# =============================================================================

LLAVE_HOGAR <- c("CQ11","CQ12","CQ13","CQ14","CQ15","CQ16","CQ17","CQ18")


# =============================================================================
# 3. CARGAR Y PREPARAR DATOS
# =============================================================================

# -----------------------------------------------------------------------------
# 3.1  DEMOGRAFÍA — gasto total del hogar y ponderador
#      Nivel INDIVIDUAL → se toma la primera fila por hogar (los valores de
#      gasto/ponderador son constantes para todos los miembros del mismo hogar)
# -----------------------------------------------------------------------------

demog_raw <- read_dta("HCES_2015_2016_DEMOGRAPHY.dta")

demog <- demog_raw |>
  group_by(across(all_of(LLAVE_HOGAR))) |>
  slice(1) |>
  ungroup() |>
  select(
    all_of(LLAVE_HOGAR),
    WGT,                          # ponderador muestral
    RATE,                         # factor de expansión
    ur,                           # 1 = Urbano / 2 = Rural
    REP,                          # estrato de reporte (50 categorías)
    CQ17,                         # EA = PSU para diseño muestral
    ingreso_total = TOTEXPP,   # gasto total anual del hogar (ETB)
    gasto_pc      = TOTEXPP_HH,      # gasto per cápita (ETB)
    gasto_ae      = TOTEXPP_AD,   # gasto por adulto equivalente (ETB)
    quintil       = EXPCC         # quintil de gasto (1-5)
  )

cat("Hogares en demografía:", nrow(demog), "\n")


# -----------------------------------------------------------------------------
# 3.2  EXPENDITURE — detalle por categorías de gasto
#      Se cargan solo las columnas necesarias para ahorrar memoria
# -----------------------------------------------------------------------------


exp_raw <- read_dta(
  "HCES_2015_2016_Expenditure.dta"
  ,col_select = c(
    "CQ11","CQ12","CQ13","CQ14","CQ15","CQ16","CQ17","CQ18",
    "ITEMC","TOP1","TOP2","TOP3","TOP4","TOP5","VALUE","FOOD"
  )
)

cat("Filas en expenditure:", nrow(exp_raw), "\n")

# attr(exp_raw$TOP3,"labels")
# attr(exp_raw$TOP1,"labels")
# attr(exp_raw$TOP5,"labels")
# ---- 3.2a  Alimentos y bebidas -----------------------------------------------
# TOP1 == 1  → Alimentos (División COICOP 01)
# TOP1 == 2  → Bebidas  alcohólicas y tabaco (División COICOP 02)


alimentos <- exp_raw |>
  filter(TOP1 %in% c(1, 2)) |>
  group_by(across(all_of(LLAVE_HOGAR))) |>
  summarise(gasto_alimentos = sum(VALUE, na.rm = TRUE), .groups = "drop")


# ---- 3.2b  Combustibles domésticos ------------------------------------------
# TOP3 == 772 Fuels and lubricants for personal transport equipment 
combustible <- exp_raw |>
  filter(TOP3 == 722) |>
  group_by(across(all_of(LLAVE_HOGAR))) |>
  summarise(gasto_combustible = sum(VALUE, na.rm = TRUE), .groups = "drop")


# ---- 3.2c  Electricidad ------------------------------------------------------

electricidad <- exp_raw |>
  filter(TOP5 == 441011 ) |>
  group_by(across(all_of(LLAVE_HOGAR))) |>
  summarise(gasto_electricidad = sum(VALUE, na.rm = TRUE), .groups = "drop")


# ---- 3.2d  Transporte público -------------------------------------------------
# TOP3== 731 Road Transport

transporte <- exp_raw |>
  filter(TOP3 == 731) |>
  group_by(across(all_of(LLAVE_HOGAR))) |>
  summarise(gasto_transporte = sum(VALUE, na.rm = TRUE), .groups = "drop")

# attr(exp_raw$TOP3,"labels")

# -----------------------------------------------------------------------------
# 3.3  MERGE: demografía + 4 categorías de gasto
# -----------------------------------------------------------------------------

base_integrada <- demog |>
  left_join(alimentos,    by = LLAVE_HOGAR) |>
  left_join(combustible,  by = LLAVE_HOGAR) |>
  left_join(electricidad, by = LLAVE_HOGAR) |>
  left_join(transporte,   by = LLAVE_HOGAR)

# Hogares sin gasto en algún ítem → reemplazar NA con 0
enigh <- base_integrada |>
  mutate(across(
    c(gasto_alimentos, gasto_combustible, gasto_electricidad, gasto_transporte),
    ~ replace_na(.x, 0)
  ))



# =============================================================================
# 4. BASE DE TRABAJO LIMPIA
# =============================================================================

base <- enigh |>
  select(
    ingreso_total,
    factor  = WGT,
    bien1   = gasto_alimentos,
    bien2   = gasto_combustible,
    bien3   = gasto_electricidad,
    bien4   = gasto_transporte,
    ur, REP, quintil
  ) |>
  filter(
    !is.na(ingreso_total),
    !is.na(factor),
    ingreso_total > 0
  )

cat("\nHogares en base final:", nrow(base), "\n")


# =============================================================================
# 5. ESCENARIO: IMPACTO DE UN AUMENTO DEL 10% EN PRECIOS
# =============================================================================

aumento <- 0.10

base <- base |>
  mutate(
    impacto_bien1 = bien1 * aumento,
    impacto_bien2 = bien2 * aumento,
    impacto_bien3 = bien3 * aumento,
    impacto_bien4 = bien4 * aumento
  )


# =============================================================================
# 6. FUNCIÓN: ÍNDICE DE CONCENTRACIÓN
# =============================================================================

concentration_index <- function(y, rankvar, weights = NULL) {
  df <- data.frame(y, rankvar, weights)
  df <- df[complete.cases(df) & df$y >= 0 & df$rankvar > 0, ]
  df <- df[order(df$rankvar), ]
  if (is.null(weights)) weights <- rep(1, nrow(df)) else weights <- df$weights
  y <- df$y
  if (sum(y * weights) == 0) return(NA)
  cw <- c(0, cumsum(weights) / sum(weights))
  cy <- c(0, cumsum(y * weights) / sum(y * weights))
  B  <- sum(diff(cw) * (head(cy, -1) + tail(cy, -1)) / 2)
  return(1 - 2 * B)
}


# =============================================================================
# 7. FUNCIÓN: SUITS INDEX
# =============================================================================

suits_index <- function(y, income, weights = NULL) {
  df <- data.frame(y, income, weights)
  df <- df[complete.cases(df) & df$y >= 0 & df$income > 0, ]
  df <- df[order(df$income), ]
  if (is.null(weights)) weights <- rep(1, nrow(df)) else weights <- df$weights
  y      <- df$y
  income <- df$income
  total_tax    <- sum(y * weights)
  total_income <- sum(income * weights)
  if (total_tax == 0 | total_income == 0) return(NA)
  Y <- c(0, cumsum(income * weights) / total_income)
  T <- c(0, cumsum(y * weights)      / total_tax)
  return(1 - sum((T[-1] + T[-length(T)]) * diff(Y)))
}


# =============================================================================
# 8. FUNCIÓN: CURVA DE CONCENTRACIÓN
# =============================================================================

plot_concentration <- function(y, ingreso, weights = NULL, titulo = "") {
  df <- data.frame(y, ingreso, weights)
  df <- df[complete.cases(df) & df$y >= 0 & df$ingreso > 0, ]
  df <- df[order(df$ingreso), ]
  if (is.null(weights)) weights <- rep(1, nrow(df)) else weights <- df$weights
  cw <- c(0, cumsum(weights)        / sum(weights))
  cy <- c(0, cumsum(df$y * weights) / sum(df$y * weights))
  plot(cw, cy, type = "l", lwd = 2, col = "red",
       xlab = "Accumulated Population (ranked by total expenditure)",
       ylab = "Accumulated Expenditure Share", main = titulo)
  abline(0, 1, lty = 2, col = "grey40")
  legend("topleft",
         legend = c("Concentration curve", "45° line"),
         col = c("red","grey40"), lty = c(1,2), lwd = 2, bty = "n", cex = 0.85)
}


# =============================================================================
# 9. GINI DEL GASTO TOTAL
# =============================================================================

gini_ingreso <- Gini(base$ingreso_total, weights = base$factor)
cat("\nGini (gasto total del hogar):", round(gini_ingreso, 4), "\n")


# =============================================================================
# 10. ÍNDICES DE CONCENTRACIÓN
# =============================================================================

C1 <- concentration_index(base$impacto_bien1, base$ingreso_total, base$factor)
C2 <- concentration_index(base$impacto_bien2, base$ingreso_total, base$factor)
C3 <- concentration_index(base$impacto_bien3, base$ingreso_total, base$factor)
C4 <- concentration_index(base$impacto_bien4, base$ingreso_total, base$factor)


# =============================================================================
# 11. KAKWANI = Índice de Concentración - Gini
# =============================================================================

K1 <- C1 - gini_ingreso
K2 <- C2 - gini_ingreso
K3 <- C3 - gini_ingreso
K4 <- C4 - gini_ingreso


# =============================================================================
# 12. SUITS
# =============================================================================

S1 <- suits_index(base$impacto_bien1, base$ingreso_total, base$factor)
S2 <- suits_index(base$impacto_bien2, base$ingreso_total, base$factor)
S3 <- suits_index(base$impacto_bien3, base$ingreso_total, base$factor)
S4 <- suits_index(base$impacto_bien4, base$ingreso_total, base$factor)


# =============================================================================
# 13. TABLA DE RESULTADOS
# =============================================================================

resultado_final <- data.frame(
  Bien = c(
    "Food & Beverage",
    "Vehicle's Fuel",
    "Energy",
    "Public Transportation"
  ),
  Aumento_Precio    = "+10%",
  Concentracion     = round(c(C1, C2, C3, C4), 4),
  Gini              = round(gini_ingreso, 4),
  Kakwani           = round(c(K1, K2, K3, K4), 4),
  Suits             = round(c(S1, S2, S3, S4), 4),
  Interp_Kakwani    = ifelse(c(K1,K2,K3,K4) > 0, "Progressive", "Regressive"),
  Interp_Suits      = ifelse(c(S1,S2,S3,S4) > 0, "Progressive", "Regressive")
)

print(resultado_final)


# =============================================================================
# 14. EXPORTAR RESULTADOS A EXCEL
# =============================================================================

write_xlsx(
  list(Resultados = resultado_final),
  "Resultados_Etiopia_Impacto_10pct.xlsx"
)

cat("\nArchivo exportado: Resultados_Etiopia_Impacto_10pct.xlsx\n")


# =============================================================================
# 15. CURVA DE LORENZ (gasto total del hogar)
# =============================================================================

Lc_gasto <- Lc(base$ingreso_total, weights = base$factor)

plot(
  Lc_gasto,
  main = "Lorenz Curve — Total Household Expenditure\nEthiopia HCES 2015/16",
  col  = "blue", lwd = 2,
  xlab = "Cumulative population share",
  ylab = "Cumulative expenditure share"
)
abline(0, 1, lty = 2, col = "grey40")


# =============================================================================
# 16. CURVAS DE CONCENTRACIÓN (una por bien)
# =============================================================================


plot_concentration(base$impacto_bien1, base$ingreso_total, base$factor,
                   "+10% — Food & Beverage")
plot_concentration(base$impacto_bien2, base$ingreso_total, base$factor,
                   "+10% —  Fuels")
plot_concentration(base$impacto_bien3, base$ingreso_total, base$factor,
                   "+10% — Electricity")
plot_concentration(base$impacto_bien4, base$ingreso_total, base$factor,
                   "+10% — Public Transportation")




# =============================================================================
# 17. EXPENDITURE SHARES POR DECIL (stacked bar)
# =============================================================================

# ---- 17a. quintiles ponderados -------------------------------------------------
base <- base |>
  arrange(ingreso_total) |>
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

# ---- 17b. "Otros" = lo que no cae en los 4 bienes ---------------------------
base <- base |>
  mutate(
    gasto_4bienes = bien1 + bien2 + bien3 + bien4,
    otros         = pmax(ingreso_total - gasto_4bienes, 0)
  )

# ---- 17c. Shares ponderados por quintil ----------------------------------------
shares_decil <- base |>
  group_by(decil) |>
  summarise(
    Food        = sum(bien1 * factor) / sum(ingreso_total * factor) * 100,
    Fuels       = sum(bien2 * factor) / sum(ingreso_total * factor) * 100,
    Electricity = sum(bien3 * factor) / sum(ingreso_total * factor) * 100,
    PublicTrans = sum(bien4 * factor) / sum(ingreso_total * factor) * 100,
    Others      = sum(otros * factor) / sum(ingreso_total * factor) * 100,
    .groups     = "drop"
  )

# ---- 17d. Formato largo ------------------------------------------------------
shares_long <- shares_decil |>
  pivot_longer(
    cols      = c(Food, Fuels, Electricity, PublicTrans, Others),
    names_to  = "Categoria",
    values_to = "Share"
  ) |>
  mutate(
    Categoria = factor(
      Categoria,
      levels = c("Others", "PublicTrans", "Electricity", "Fuels", "Food")
    )
  )

# ---- 17e. Paleta y etiquetas -------------------------------------------------
colores <- c(
  "Food"        = "#1F4E79",
  "Fuels"       = "#4E7C3F",
  "Electricity" = "#7B2D42",
  "PublicTrans" = "#D4A017",
  "Others"      = "#8DB4C8"
)

etiquetas <- c(
  "Food"        = "Food & Beverage",
  "Fuels"       = "Vehicle's Fuel",
  "Electricity" = "Electricity",
  "PublicTrans" = "Public Transportation",
  "Others"      = "Others"
)

# ---- 17f. Gráfica ------------------------------------------------------------
grafica_shares <- ggplot(
  shares_long,
  aes(x = decil, y = Share, fill = Categoria)
) +
  geom_bar(stat = "identity", width = 0.75) +
  scale_fill_manual(values = colores, labels = etiquetas,
                    guide = guide_legend(nrow = 2)) +
  scale_y_continuous(breaks = seq(0, 110, by = 10),
                     limits = c(0, 110), expand = c(0, 0)) +
  scale_x_discrete(labels = 1:10) +
  labs(
    title    = "Expenditure shares by decile — Ethiopia HCES 2015/16",
    subtitle = "Weighted by household sampling weight (WGT)",
    x        = "Expenditure decile",
    y        = "% of total household expenditure",
    fill     = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background    = element_rect(fill = "#D6E8F0", color = NA),
    panel.background   = element_rect(fill = "#D6E8F0", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "white", linewidth = 0.5),
    plot.title         = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle      = element_text(hjust = 0.5, color = "grey30"),
    legend.position    = "bottom",
    legend.key.size    = unit(0.5, "cm"),
    axis.ticks.x       = element_blank()
  )

print(grafica_shares)


# =============================================================================
# NOTAS DE INTERPRETACIÓN
# =============================================================================
#
# GINI: 0 = igualdad perfecta | 1 = desigualdad máxima
#
# KAKWANI = Índice_Concentración(bien) − Gini(gasto_total)
#   > 0  → PROGRESIVO  (hogares ricos gastan proporcionalmente más en ese bien)
#   < 0  → REGRESIVO   (hogares pobres cargan proporcionalmente más)
#   = 0  → Neutral
#
# SUITS:
#   > 0  → PROGRESIVO  |  < 0  → REGRESIVO
#   Comparte signo con Kakwani; magnitudes comparables entre bienes
#
#
# ELECTRICIDAD en Etiopía:
#   Cobertura limitada (~44% de hogares en 2015/16) y concentrada
#   en zonas urbanas; se espera que sea PROGRESIVO