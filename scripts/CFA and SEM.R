# CFA  --------------------------------------------------------------------
library(lavaan)
#Reminder all my hexaco files have clean data. Otherwise na.omit, bad resp, etc
df_facets <- readRDS("HEXACO_facets.rds")

facets_only <- df_facets[, c(
  "HSinc", "HFair", "HGree", "HMode", 
  "EFear", "EAnxi", "EDepe", "ESent", 
  "XExpr", "XSocB", "XSoci", "XLive", 
  "AForg", "AGent", "AFlex", "APati", 
  "COrga", "CDili", "CPerf", "CPrud", 
  "OAesA", "OInqu", "OCrea", "OUnco"
)]    # exclude metadata

# my model for the CFA
hexaco_model <- '
  Honesty    =~ HSinc + HFair + HGree + HMode
  Emotional  =~ EFear + EAnxi + EDepe + ESent
  Extraver   =~ XExpr + XSocB + XSoci + XLive
  Agreeable  =~ AForg + AGent + AFlex + APati
  Conscience =~ COrga + CDili + CPerf + CPrud
  Openness   =~ OAesA + OInqu + OCrea + OUnco
'
# Maximum likelihood robust: Ml because we don't have 240 columns anymore, 
# Robust because its standard for large samples
fit_cfa <- cfa(hexaco_model, 
               data = facets_only, 
               std.lv = TRUE, 
               estimator = "MLR")

summary(fit_cfa, 
        fit.measures = TRUE,   # Pay attention to CFI, TLI, RMSEA
        standardized = TRUE,   # Para ver a coluna Std.all
        rsquare = TRUE)

# The adjustments were low, but it is what it is. CFA assumes simple structure facets 
# and that's not true but using modification indexes while good for learning is not
# good scientific practice. And that leave space for stuff such as PNA.


# SEM ---------------------------------------------------------------------

model_sem <- '
  Honesty    =~ HSinc + HFair + HGree + HMode
  Emotional  =~ EFear + EAnxi + EDepe + ESent
  Extraver   =~ XExpr + XSocB + XSoci + XLive
  Agreeable  =~ AForg + AGent + AFlex + APati
  Conscience =~ COrga + CDili + CPerf + CPrud
  Openness   =~ OAesA + OInqu + OCrea + OUnco

  
  elapse ~ Conscience + Honesty + Emotional + Extraver + Agreeable + Openness
'


fit_sem <-sem(model_sem, data = df_facets, estimator = "MLR")

summary(fit_sem, standardized = TRUE, rsquare = TRUE) # estimate in seconds

# lets normalize the elapsed time

df_facets$elapse_z <- as.numeric(scale(df_facets$elapse))  # Z for z score 
# one new column on df_facets

#new model now with elapse with average = 0 and sd = 1
model_sem_z <- '
  Honesty    =~ HSinc + HFair + HGree + HMode
  Emotional  =~ EFear + EAnxi + EDepe + ESent
  Extraver   =~ XExpr + XSocB + XSoci + XLive
  Agreeable  =~ AForg + AGent + AFlex + APati
  Conscience =~ COrga + CDili + CPerf + CPrud
  Openness   =~ OAesA + OInqu + OCrea + OUnco

  elapse_z ~ Conscience + Honesty + Emotional + Extraver + Agreeable + Openness
'
fit_sem_z <- sem(model_sem_z, data = df_facets, estimator = "MLR")
summary(fit_sem_z, standardized = TRUE, rsquare = TRUE)
 # inspect(fit_sem_z, "rsquare")  alternatively

# the estimates are now on SD units harder to read but no "warning" from R

# We were expecting conscience to be the stronger one, but agreeableness was the 
# stronger positive one and openness the stronger of ALL and negative !!!

#R-square of elapse-Z   = 1.8% "the personality of somebody explains 1.8% of the
# elapsed time.

# some visuals
library(semPlot)
semPaths(fit_sem_z, 
         whatLabels = "std", 
         layout = "tree", 
         edge.label.cex = 0.8, 
         curvePivot = TRUE, 
         fade = FALSE)

# 1. Extract scores from participants
df_scores <- as.data.frame(lavPredict(fit_sem_z))

# 2. New column to the SCORE dataframe, not df_facets
df_scores$elapse_z <- df_facets$elapse_z

# 3. Configure R to show graphs side by side
par(mfrow = c(1, 2))

# --- PLOT 1: Openness (The Accelerator) ---
plot(df_scores$Openness, df_scores$elapse_z, 
     main = "Openness vs Time",
     xlab = "Factor Openness (Latent)", ylab = "Time (Z-score)",
     pch = 16, col = rgb(0.1, 0.2, 0.8, 0.05)) # Transparent blue high "n"
abline(lm(elapse_z ~ Openness, data = df_scores), col = "red", lwd = 3)
text(x = 0, y = 3, labels = "Beta = -0.128", col = "red", font = 2)

# --- PLOT 2: Amiability (The Brake) ---
plot(df_scores$Agreeable, df_scores$elapse_z, 
     main = "Amiability vs Time",
     xlab = "Factor Amiability (Latent)", ylab = "Time (Z-score)",
     pch = 16, col = rgb(0.1, 0.8, 0.2, 0.05)) # Green transparent
abline(lm(elapse_z ~ Agreeable, data = df_scores), col = "darkgreen", lwd = 3)
text(x = 1.5, y = 3, labels = "Beta = 0.110", col = "darkgreen", font = 2)

# 4. Reset layout for 1x1
par(mfrow = c(1, 1))

