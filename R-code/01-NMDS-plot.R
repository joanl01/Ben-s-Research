# Load required packages 
pacman::p_load(readxl, tidyverse)
# Read in dataset
data <- read_excel(here::here("datasets", "JJ_DATA.xlsx"))
# Remove the empty spaces
data <- data |> na.omit()

###############################################################################
# Select only the columns related to species abundance (P1 to P20)
species_data <- data[, 3:22]  # Columns P1 to P20 should be species abundance columns

# Remove rows where all species abundances are zero
species_data <- species_data[rowSums(species_data) > 0, ]

# Filter the original `data` dataframe to match the rows of `species_data`
data_filtered <- data[rowSums(data[, 3:22]) > 0, ]

# Check for missing values (NAs) and remove them
if(any(is.na(species_data))) {
  species_data <- na.omit(species_data)  # Remove rows with missing values
  data_filtered <- na.omit(data_filtered)  # Also remove corresponding rows from `data_filtered`
}

# Calculate Bray-Curtis dissimilarity matrix
bray_curtis <- vegdist(species_data, method = "bray")

# Perform NMDS
nmds_result <- metaMDS(bray_curtis, k = 2, trymax = 100)

# Create a dataframe for plotting with NMDS coordinates and group info
nmds_data <- data.frame(nmds_result$points)
nmds_data$group <- data_filtered$`Sampling Group`  # Add Sampling Group info (from filtered data)

# add Terrain Type if to distinguish by terrain
nmds_data$terrain <- data_filtered$`Terrain Type`

# Create the NMDS plot using ggplot
ggplot(nmds_data, aes(x = MDS1, y = MDS2, color = group, shape = terrain)) +
  geom_point(size = 4) + 
  labs(title = "NMDS Plot", x = "NMDS1", y = "NMDS2", col = "Group", shape = "Terrain") +
  theme_bw() + theme(legend.position = "bottom", text = element_text(size = 20)) + 
  harrypotter::scale_color_hp(house = "ravenclaw", discrete = TRUE)
# Save the NMDS plot in the figure directory
ggsave(here::here("pretty-pictures", "NMDS_plot.pdf"),
       plot = last_plot(),
       width = 16,
       height = 9,
       units = "in")

