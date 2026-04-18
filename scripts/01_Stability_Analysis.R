# packages ----------------------------------------------------------------

library(bootnet)
library(qgraph)

raw_data <- read.csv("HEXACO_facets.csv", header = TRUE)

#  Inspect the data
# 'head' shows the first 6 rows. 
# 'dim' shows how many rows (participants) and columns (facets) we have.

colnames(raw_data)

# Check if the data is numeric (it must be for PNA)
str(raw_data)

# We create a new object called 'facets_only' 
# We select all rows (empty before the comma) and columns 1 to 24
facets_only <- raw_data[, 1:24]

# Let's check if it worked. It should have only 24 variables now.
dim(facets_only)


# Starting the PNA --------------------------------------------------------

# Estimate the network structure
# 'default = "EBICglasso"' is the standard for psychological research.
# It applies a penalty to small correlations to keep the map clean.
network_model <- estimateNetwork(facets_only, default = "EBICglasso")

# Plot the network

# 1. Create a vector with the names of the facets
# We extract this directly from the dataframe columns
node_names <- colnames(facets_only)

# 2. Define the groups (factors)
# This tells R which node belongs to which 'family'
# Each factor has 4 facets in your dataset
hexaco_groups <- list(
  Honesty = 1:4,
  Emotionality = 5:8,
  Extraversion = 9:12,
  Agreeableness = 13:16,
  Conscientiousness = 17:20,
  Openness = 21:24
)

# 3. Plot with colors and labels
# We use 'vsize' to make the nodes a bit bigger so the text fits
plot(network_model, 
     layout = "spring",
     labels = node_names,     # Now it uses the actual names
     groups = hexaco_groups,  # Adds colors based on the groups above
     palette = "colorblind",  # Good practice for academic publications
     legend = TRUE,           # Shows which color is which factor
     vsize = 7,               # Increases node size for readability
     shape = "circle")               

# 4. Calculate and plot centrality indices
# We'll focus on Strength, as it is the most stable for psychological data
centralityPlot(network_model, 
               include = c("Strength", "Closeness", "Betweenness"),
               orderBy = "Strength") # This sorts the plot by the most 'powerful' node

# 5. Stability tests "what-if i took a sample? the leader will still be the leader?"
# Warning: This can take a few minutes because it recalculates the network many times.
# We will do 100 iterations (for a quick test, usually 1000 is used for papers)

stability_test <- bootnet(network_model, 
                          nBoots = 100, 
                          type = "nonparametric")

# Plot the stability of the edges (lines)
plot(stability_test)

# it's hard to see anything, since it tests all the possible combinations between each facet.
#  C(24,2) = 276 combinations!


# centrability-stability this time 1000 iterations ------------------------

# 1. Stability of Centrality Indices
# This drops participants and checks if the results stay the same
stability_centrality <- bootnet(network_model, 
                                nBoots = 1000, 
                                type = "case") # Note we use 'case' here!

# 2. Plotting the result
plot(stability_centrality)

# The stability doesn't drop even when 75% of the original sample is removed!
# Prudence (CPrud) remains on the top

# 3. Calculating the CS-Coefficient
# This is a single number that summarizes how stable your network is.
corStability(stability_centrality)

# Saving Plots ------------------------------------------------------------

# This creates a vector file where you can zoom infinitely.
pdf("HEXACO_Network_Final.pdf", width = 12, height = 9)
plot(network_model, 
     layout = "spring", 
     groups = hexaco_groups, 
     labels = node_names, 
     palette = "colorblind", 
     legend = TRUE, 
     vsize = 7, 
     shape = "circle")
dev.off()

# --- HIGH-RES IMAGE (PNG for GitHub) ---
# This will look crisp on a computer screen.
png("HEXACO_Network_Preview.png", width = 1800, height = 1400, res = 200)
plot(network_model, 
     layout = "spring", 
     groups = hexaco_groups, 
     labels = node_names, 
     palette = "colorblind", 
     legend = TRUE, 
     vsize = 7, 
     shape = "circle")
dev.off()

pdf("HEXACO_Centrality_Indices.pdf", width = 8, height = 10)
centralityPlot(network_model, include = "Strength", orderBy = "Strength")
dev.off()

pdf("HEXACO_Centrality_Stability.pdf", width = 10, height = 6)
plot(stability_centrality)
dev.off()
