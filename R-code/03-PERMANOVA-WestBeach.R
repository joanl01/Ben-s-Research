# Load required packages 
pacman::p_load(readxl, tidyverse, vegan, PERMANOVA)
# Read in dataset
data<- read_excel(here::here("datasets", "JJ_DATA.xlsx"))
# Remove the empty spaces
data<- data |> na.omit()

# Filter the data to only keep West Beach East and West Beach West
data <- data |> filter(
  `Sampling Group` != "Semaphore"
)

# Select only the columns related to species abundance (P1 to P20)
species_data <- data[, 3:22]  # Columns P1 to P20 should be species abundance columns

# Remove rows where all species abundances are zero
species_data <- species_data[rowSums(species_data) > 0, ]

# Filter the original `data` dataframe to match the rows of `species_data`
data <- data[rowSums(data[, 3:22]) > 0, ] 

# Check for missing values (NAs) and remove them
if(any(is.na(species_data))) {
  species_data <- na.omit(species_data)  # Remove rows with missing values
  data <- na.omit(data)  # Also remove corresponding rows from `data`
}


###############################################################################
species_data <- species_data |> as.matrix()

# Create a factor for Sampling Group and Terrain Type interaction
group_factor <- interaction(data$`Sampling Group`, data$`Terrain Type`)

# Inspect the group factor
# table(group_factor)


# Perform PERMANOVA to test the effect of Sampling Group and Terrain Type
permanova_results <- adonis2(species_data ~ `Sampling Group` * `Terrain Type`, data = data, permutations = 999)

# Print the results
print(permanova_results)

# Test the effect of Sampling Group
permanova_group <- adonis2(species_data ~ `Sampling Group`, data = data, permutations = 999)
print(permanova_group)

# Test the effect of Terrain Type
permanova_terrain <- adonis2(species_data ~ `Terrain Type`, data = data, permutations = 999)
print(permanova_terrain)


# NMDS visualization of species composition
nmds <- metaMDS(species_data, distance = "bray", k = 2)

# Plot the NMDS
plot(nmds, type = "t", main = "NMDS of Species Composition")

# Add group information to the plot
ordiplot(nmds, display = "sites", type = "t")
ordihull(nmds, group_factor, draw = "polygon", col = "transparent", border = "black")


#####################################################################################
# Perform PCA using ggplot
pca <- prcomp(species_data, center = TRUE, scale. = TRUE)

# Extract PCA scores for the first two components (PC1 and PC2)
pca_data <- as.data.frame(pca$x)
pca_data$group <- group_factor  # Add the grouping factor

# Plot the PCA using ggplot2
ggplot(pca_data, aes(x = PC1, y = PC2, color = group)) +
  geom_point(size = 5) +
  labs(title = "PCA Plot of Species Composition", x = "PC1", y = "PC2") +
  theme_bw() + harrypotter::scale_colour_hp(house = "ravenclaw", discrete = TRUE) +
  theme(legend.title = element_blank(), legend.position = "bottom",
        text = element_text(size = 20))
# Save PCA plot in folder 
ggsave(here::here("pretty-pictures", "PCA-plot-no-Semaphore.pdf"),
       plot = last_plot(),
       width = 12,
       height = 9,
       units = "in")
