# AFE by facet ------------------------------------------------------------
library(psych)
library(GPArotation)

df_facetas <- readRDS("HEXACO_facets.rds")

df_items <- df_facetas[,1:24]
# to make sure everything is a number

df_items <- as.data.frame(lapply(df_items, as.numeric))

str(df_items)

resultado_kmo <- KMO(df_items)
print(resultado_kmo)

cortest.bartlett(df_items)

#parallel analysis
set.seed(123)
fa.parallel(df_items, fa = "fa", n.iter = 50, main = "Parallel: df_items")

# factor analysis

modelo_hexaco <- fa(df_items, 
                    nfactors = 6,      
                    rotate = "oblimin", 
                    fm = "minres")

print(modelo_hexaco$loadings, cut = 0.3, sort = TRUE) # 3 cross-loadings

# correlation matrix

cor_fatores <- modelo_hexaco$Phi
# renaming for readability
colnames(cor_fatores) <- rownames(cor_fatores) <- c("Extro(MR1)", "Amabil(MR6)", "Emocion(MR3)", "Consc(MR4)", "Open(MR5)", "Honest(MR2)")

print(round(cor_fatores, 2))
# We can see that Honesty and Amiability should be 2 separated factors

#McDonald's Omega (reliability)

# 1. eXtroversion facets
itens_extroversao <- df_items[, c("XExpr", "XSocB", "XSoci", "XLive")]

# 2. The omega
conf_extroversao <- omega(itens_extroversao, nfactors = 1)

# 3. The omega.tot (for that factor)
print(conf_extroversao$omega.tot)

set.seed(123)

# 2. A list for every facet name

fatores_lista <- list(
  Extroversion = c("XExpr", "XSocB", "XSoci", "XLive"),
  Amability = c("AForg", "AGent", "AFlex", "APati"),
  Emocionality = c("EFear", "EAnxi", "EDepe", "ESent"),
  Conscientiouness = c("COrga", "CDili", "CPerf", "CPrud"),
  Openess = c("OAesA", "OInqu", "OCrea", "OUnco"),
  Honesty= c("HSinc", "HFair", "HGree", "HMode")
)

# 3. A loop do calculate omega for each one
resultados_confiavel <- lapply(fatores_lista, function(itens) {
  df_temp <- df_items[, itens]
  res <- omega(df_temp, nfactors = 1, plot = FALSE)
  return(res$omega.tot)
})

# 4. Results
unlist(resultados_confiavel)     # to see the list as a vector

# Openess is "problematic" if we remove a facet, would omega improve?

itens_abertura <- df_items[, c("OAesA", "OInqu", "OCrea", "OUnco")]
analise_abertura <- alpha(itens_abertura) # alpha() psych "if item deleted"

print(analise_abertura$alpha.drop) # nothing should be dropped

# communalities : How much of the variance is explained by the model?

h2_valores <- round(modelo_hexaco$communality[1:24], 3)
print(h2_valores)

# Ex: OInqu has 42% explained by Hexaco model

# describe for skew , kurtosis and all else

print(describe(df_items))