# Begin -------------------------------------------------------------------
library(readr)      
library(dplyr)
library(psych)  

df <- read_delim("raw_data.csv", 
                 delim = "\t", 
                 col_names = TRUE,   
                 trim_ws = TRUE,
                 na = c("", " ", "NA")) 

names(df)[241:244] <- c("V1_valida", "V2_valida", "country", "elapse")

# cleaning
  # 3 minutes for 240 items? 216 hours??? CUT!

df_clean <- df %>%
  filter(
    V1_valida >= 6,          # 6 is recommended
    V2_valida >= 6,
    elapse > 600,  
    elapse < 3600,
    
    apply(select(., 1:240), 1, var, na.rm = TRUE) > 0.5, # bad respondents var
    
    # responses different than 1 to 7 just in case
    rowSums(select(., 1:240) < 1 | select(., 1:240) > 7, na.rm = TRUE) <= 5,
    !is.na(country) | TRUE   
  )


#  Empty cells on country
df_clean$country <- na_if(df_clean$country, "") 
df_clean <- df_clean %>%
  filter(!is.na(country))

df_clean %>%
  count(country, sort = TRUE) %>%
  slice_head(n = 10)         # 10 countries with most participants

library(ggplot2)

ggplot(df_clean, aes(x = elapse / 60)) +
  geom_histogram(bins = 60, fill = "steelblue", color = "white") +
  labs(title = "Distribution of time to take Hexaco test",
       x = "Time (minutes)",
       y = "Frequency") +
  theme_minimal()

#The skewness for time is normal in that kind of test, lets keep 3600 seconds

summary(df_clean$elapse / 60)
quantile(df_clean$elapse / 60, probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99))

# savin the CLEAN dataset

# 1: CSV (easy to open anywhere Excel, Python, SPSS, etc.)
write_csv(df_clean, "HEXACO_clean.csv")

# Opção 2: RDS (Recomended for R)
saveRDS(df_clean, "HEXACO_clean.rds")




