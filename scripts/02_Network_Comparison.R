# Packages and opening file -----------------------------------------------

library(NetworkComparisonTest)
library(dplyr)
library(bootnet)
library(qgraph)

raw_data <- read.csv("HEXACO_facets.csv",header = TRUE)

# Check the number of participants per country
# This will show you the top countries in your dataset
sort(table(raw_data$country), decreasing = TRUE) |> head(10)

# Subset the data for Australia (AU)
# We filter by country and select only the 24 facets
data_AU <- raw_data %>% 
  filter(country == "AU") %>% 
  select(1:24)

#  Subset the data for United Kingdom (GB)
data_GB <- raw_data %>% 
  filter(country == "GB") %>% 
  select(1:24)

# Run the Network Comparison Test
# This function compares: 
# - Global Strength (Is one group more "connected" than the other?)
# - Network Structure (Is the map of connections different?)
# - Specific Edges (Is one specific link significantly different?)

comparison_results <- NCT(data_AU, data_GB, 
                          it = 1000, 
                          test.edges = TRUE, 
                          edges = "all")
summary(comparison_results)

# NETWORK INVARIANCE TEST Test statistic M: 0.1289695 p-value 0.1968032 
#  The personality of GB and AU is very close

#GLOBAL STRENGTH INVARIANCE TEST Global strength per group:  15.50438 16.85843 
#Test statistic S:  1.354047 p-value 0.3086913 (The strength of their connections
# is about the same too)


# Do the networks and Plots! -------------------------------------------------

# 0. --- DATA DICTIONARY  ---

#  Names of the 24 facets
node_names <- c("HSinc", "HFair", "HGree", "HMode", 
                "EFear", "EAnxi", "EDepe", "ESent", 
                "XExpr", "XSocB", "XSoci", "XLive", 
                "AForg", "AGent", "AFlex", "APati", 
                "COrga", "CDili", "CPerf", "CPrud", 
                "OAesA", "OInqu", "OCrea", "OUnco")

#  Grouping by the 6 HEXACO factors
hexaco_groups <- list(
  Honesty_Humility = 1:4,
  Emotionality = 5:8,
  Extraversion = 9:12,
  Agreeableness = 13:16,
  Conscientiousness = 17:20,
  Openness = 21:24
)

# 1. Estimate Australia (AU) and Great Britain (GB) Networks
network_AU <- estimateNetwork(data_AU, 
                              default = "EBICglasso") # Same method as global
network_GB <- estimateNetwork(data_GB, 
                              default = "EBICglasso")

# 2. Define a fixed layout so the nodes don't move between plots
# We will use the Australia network as the coordinate reference
L <- network_AU$layout 

# 3. Open a PDF file
pdf("Comparison_AU_vs_GB.pdf", width = 14, height = 7)

# 4. Split the screen into 1 row and 2 columns
par(mfrow=c(1,2)) 

# 5. Plot Australia (Left side)
plot(network_AU, 
     layout = L, 
     title = "Australia (AU)", 
     groups = hexaco_groups, 
     palette = "colorblind", 
     labels = node_names,
     vsize = 7)

# 6. Plot United Kingdom (Right side)
plot(network_GB, 
     layout = L, 
     title = "United Kingdom (GB)", 
     groups = hexaco_groups, 
     palette = "colorblind", 
     labels = node_names,
     vsize = 7)

# 7. Close and save the file
dev.off()

# Lets compare countries more distinct cultural-wise --------------------------
# Australia and Philippines

# 1. Filter data for Philippines
data_PH <- raw_data %>% 
  filter(country == "PH") %>% 
  select(1:24)

network_PH <- estimateNetwork(data_PH, default = "EBICglasso")

# 2. Run the Comparison Test (AU vs PH)
# This might show more structural differences than the previous test
comparison_AU_PH <- NCT(data_AU, data_PH, 
                        it = 1000, 
                        test.edges = TRUE, 
                        edges = "all")

summary(comparison_AU_PH)

# ---------The cultural contrast PLOT ----------

#  Open the PDF file
pdf("Cultural_Contrast_AU_vs_PH.pdf", width = 14, height = 7)

#  Split the screen
par(mfrow = c(1, 2))

#  Plot Australia (Left side)
plot(network_AU, 
     layout = L, 
     title = "Western Culture (Australia)", 
     groups = hexaco_groups, 
     palette = "colorblind", 
     labels = node_names,
     vsize = 7)

#  Plot Philippines (Right side)
plot(network_PH, 
     layout = L, 
     title = "Eastern Culture (Philippines)", 
     groups = hexaco_groups, 
     palette = "colorblind", 
     labels = node_names,
     vsize = 7)

#  Close the file
dev.off()

# Lets finish by comparing 3 countries ------------------------------------
# lets add Brasil as an intermediate country between the culture of AU and PH

data_BR <- raw_data %>% 
  filter(country == "BR") %>% 
  select(1:24)

network_BR <- estimateNetwork(data_BR, default = "EBICglasso")

comparison_AU_BR <- NCT(data_AU, data_BR, it = 1000, test.edges = TRUE, edges = "all")

summary(comparison_AU_BR)

# PLOT!

pdf("Global_Personality_Comparison.pdf", width = 21, height = 7) 
par(mfrow = c(1, 3)) 

# 1. Australia
plot(network_AU, layout = L, title = "Australia (Western)", 
     groups = hexaco_groups, palette = "colorblind", labels = node_names, vsize = 7)

# 2. Philippines
plot(network_PH, layout = L, title = "Philippines (Eastern)", 
     groups = hexaco_groups, palette = "colorblind", labels = node_names, vsize = 7)

# 3. Brazil
plot(network_BR, layout = L, title = "Brazil (Latin)", 
     groups = hexaco_groups, palette = "colorblind", labels = node_names, vsize = 7)

dev.off()

#  PNG for GitHub

png("Culture_Contrast_AU_vs_PH.png", width = 1600, height = 800, res = 200)

# 3. Configure plot layout: 1 row, 2 columns + increased margins
# mar = c(bottom, left, top, right)
par(mfrow = c(1, 2), mar = c(3, 3, 5, 8)) 

# 4. Plot Australia (Left)
plot(network_AU, 
     layout = L, 
     title = "Australia (Western Context)", 
     groups = hexaco_groups, 
     palette = "colorblind", 
     labels = node_names,
     vsize = 8,
     legend.cex = 0.4,       # Smaller legend font to avoid overlap
     legend.margin = 0.15)   # Pushes the legend away from the nodes

# 5. Plot Philippines (Right)
plot(network_PH, 
     layout = L, 
     title = "Philippines (Eastern Context)", 
     groups = hexaco_groups, 
     palette = "colorblind", 
     labels = node_names,
     vsize = 8,
     legend.cex = 0.4, 
     legend.margin = 0.15)

# 6. Finalize file
dev.off()
