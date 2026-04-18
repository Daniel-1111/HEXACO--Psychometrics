# Packages and READ -----------------------------------------------------

library(psych)
library(GPArotation)
library(tidyverse)
library(readr)
library(magick)

data <- read_rds("HEXACO_240_reversed_items.rds")

dim(data)                   
colnames(data)               
glimpse(data) 

# DF with without metadata for the AFE

items <- data %>% 
  select(-V1_valida, -V2_valida, -country, -elapse)

# Adequacy of the sample tests --------------------------------------------

#are the items fatorable?
kmo_result <- KMO(items)
print(kmo_resultMSA)

#Is my correlation matrix the Identity matrix?

bartlett_result <- cortest.bartlett(items)
print(bartlett_result)

# Parallel analysis
set.seed(123)   # For Reproducibility

parallel_result <- fa.parallel(items, 
                               fm = "minres", 
                               fa = "fa", 
                               n.iter = 50, 
                               main = "Parallel Analysis - HEXACO 240 itens (N = 17656)",
                               ylabel = "Eigenvalues")


cat("\nParallel Analysis number of suggested factors:\n")
print(parallel_result$nfact)

# 6 factor model, we will go with promax since it's usual and our kmo is good.
# minres for lots of data and EFA , ML for cleaner data and CFA.
efa_6_promax <- fa(items, 
                   nfactors = 6, 
                   fm = "minres",     
                   rotate = "promax",  
                   scores = "none")


print(efa_6_promax, 
      digits = 2, 
      sort = TRUE,      # by loading size
      cutoff = 0.30)  

#variance and communalities
print(efa_6_promax$Vaccounted, digits = 2)

# Correlation matrix (Phi matrix)
cat("\nCorrelation matrix:\n")
print(round(efa_6_promax$Phi, 2))

# Comunalidades (h²) from lower to higher
communalities <- efa_6_promax$communality
communalities_sorted <- sort(communalities, decreasing = FALSE)

# Mostrar as 15 comunalidades mais baixas 
cat("=== 15 lower communalities ===\n")
print(round(communalities_sorted[1:15], 3))

# Mostrar as 10 comunalidades mais altas
cat("\n=== 10 higher communalities ===\n")
print(round(tail(communalities_sorted, 10), 3))

# Resumo estatístico das comunalidades
cat("\nCommunalities summary:\n")
summary(round(communalities, 3))


# 15 factor model ---------------------------------------------------------

# since this is an EFA , let's pretend we don't know its 24 , but go to 15 and 
# see if the comunalities improve.

set.seed(123)

efa_15 <- fa(items, 
             nfactors = 15, 
             fm = "minres", 
             rotate = "promax", 
             scores = "none")

# main output of the model
print(efa_15, digits = 2, sort = TRUE, cutoff = 0.32)

print(efa_15$Vaccounted, digits = 2)

summary(efa_15$communality)

img <- image_read("improvement 6 to 15 factors.png")

print(img)

image_write(img, 
            path = "tabela_comparacao_6_vs_15.pdf", 
            format = "pdf")








