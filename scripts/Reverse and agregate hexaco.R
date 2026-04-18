# Packs and read ----------------------------------------------------------

df <- readRDS("HEXACO_clean_240.rds")

df_items <- df

# Revert likert 1-7
invert <- function(x) {
  ifelse(is.na(x), NA, 8 - x)
}

# =====(REVERSED KEYS)https://ipip.ori.org/newhexaco_pi_key.htm ==========

reversed <- list(
  HSinc  = 2:10,     
  HFair  = 6:10,
  HGree  = 3:10,
  HMode  = 5:10,
  
  EFear  = 6:10,
  EAnxi  = 6:10,
  EDepe  = integer(0),   
  ESent  = 6:10,
  
  XExpr  = 6:10,
  XSocB  = 6:10,
  XSoci  = 6:10,
  XLive  = 9:10,
  
  AForg  = 5:10,
  AGent  = 5:10,
  AFlex  = 3:10,
  APati  = 6:10,
  
  COrga  = 6:10,
  CDili  = 6:10,
  CPerf  = 9:10,
  CPrud  = 4:10,
  
  OAesA  = 6:10,
  OInqu  = 7:10,
  OCrea  = 7:10,    
  OUnco  = 6:10
)

#======inverting reversed keys ======

facets <- names(reversed)

for (facet in facets) {
  items <- paste0(facet, 1:10)           # ex: HSinc1 até HSinc10
  
  for (pos in reversed[[facet]]) {
    col_name <- items[pos]
    df_items[[col_name]] <- invert(df_items[[col_name]])
  }
}

metadata <- c("V1_valida", "V2_valida", "country", "elapse")

df_final <- df_items[, c(names(df)[1:240], metadata)]

saveRDS(df_final, "HEXACO_240_itens_invertidos.rds")
write.csv(df_final, "HEXACO_240_itens_invertidos.csv", row.names = FALSE, na = "")

# ============== create faceted Dataframe =================

df_facetas <- df_final

# List of 24 facets
facet_names <- c("HSinc", "HFair", "HGree", "HMode",
                 "EFear", "EAnxi", "EDepe", "ESent",
                 "XExpr", "XSocB", "XSoci", "XLive",
                 "AForg", "AGent", "AFlex", "APati",
                 "COrga", "CDili", "CPerf", "CPrud",
                 "OAesA", "OInqu", "OCrea", "OUnco")
#  Create facets by average , fm = "ml" and CFA work better this way
for (facet in facet_names) {
  items <- paste0(facet, 1:10)                
  df_facetas[[facet]] <- rowMeans(df_final[, items], na.rm = TRUE)
}

metadata <- c("V1_valida", "V2_valida", "country", "elapse")
df_facetas_final <- df_facetas[, c(facet_names, metadata)]

print(dim(df_facetas_final))    # 24 + 4 columns , ok!
print(names(df_facetas_final))

summary(df_facetas_final[, facet_names])

# saving the files 

saveRDS(df_facetas_final, "HEXACO_facetas.rds")
write.csv(df_facetas_final, "HEXACO_facetas.csv", 
          row.names = FALSE, na = "")

# ====Aggregating by FACTOR (useful for 2nd order CFA) ===== 

df_fatores <- df_facetas_final

# 6 factors (By facet average)
df_fatores$H <- rowMeans(df_facetas_final[, c("HSinc", "HFair", "HGree", "HMode")], na.rm = TRUE)
df_fatores$E <- rowMeans(df_facetas_final[, c("EFear", "EAnxi", "EDepe", "ESent")], na.rm = TRUE)
df_fatores$X <- rowMeans(df_facetas_final[, c("XExpr", "XSocB", "XSoci", "XLive")], na.rm = TRUE)
df_fatores$A <- rowMeans(df_facetas_final[, c("AForg", "AGent", "AFlex", "APati")], na.rm = TRUE)
df_fatores$C <- rowMeans(df_facetas_final[, c("COrga", "CDili", "CPerf", "CPrud")], na.rm = TRUE)
df_fatores$O <- rowMeans(df_facetas_final[, c("OAesA", "OInqu", "OCrea", "OUnco")], na.rm = TRUE)

colunas_finais <- c(
  "HSinc", "HFair", "HGree", "HMode",
  "EFear", "EAnxi", "EDepe", "ESent",
  "XExpr", "XSocB", "XSoci", "XLive",
  "AForg", "AGent", "AFlex", "APati",
  "COrga", "CDili", "CPerf", "CPrud",
  "OAesA", "OInqu", "OCrea", "OUnco",
  "H", "E", "X", "A", "C", "O",
  "V1_valida", "V2_valida", "country", "elapse"
)

df_fatores_final <- df_fatores[, colunas_finais]

saveRDS(df_fatores_final, "HEXACO_factors.rds")
write.csv(df_fatores_final, "HEXACO_factors.csv", 
          row.names = FALSE, na = "")

