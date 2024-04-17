# Load data and libraries
library(ratdat)
library(ggplot2)

# Store ratdat in a variable rat_data.
complete.data <- ratdat::complete

# Check out the different features in the dataset.
#names(complete.data)

# Subset the data to only include rodents.
rodent.data <- subset(complete.data, complete.data$taxa == "Rodent")

# Check unique record entries for rodent species.
#unique(rodent.data$species)

# Check unique record entries for rodent sex.
#unique(rodent.data$sex)

# Remove rows where sex records are empty.
rodent.data.MF <- rodent.data[rodent.data$sex == "M" | rodent.data$sex == "F", ]

# Check unique record entries after cleaning.
#unique(rodent.data.MF$sex)

# Remove rows where hindfoot_length is NA.
rodent.data.MF.HF <- rodent.data.MF[!is.na(rodent.data.MF$hindfoot_length), ]

# Create a bar plot to display proportions of males vs females for each species.
ggplot(rodent.data.MF.HF, aes(x = species, fill = sex)) +
  # Add bar plot.
  geom_bar(aes(y = after_stat(count/sum(count))), position = "fill") +
  # Scale y to display percentages instead.
  scale_y_continuous(labels = scales::percent) +
  # Adjust fill colors manually.
  scale_fill_manual(values = c("F"="pink","M"="skyblue")) + 
  # Add text labels on the bars showing the actual counts.
  geom_text(
    # Position text labels at the middle of each bar.
    aes(y = after_stat(count/sum(count)/2), label = after_stat(count)), 
    # Center text labels within each filled section.
    position = position_fill(vjust = 0.5), 
    # Ensure count is calculated for text labels.
    stat = "count",
    # Set color of text labels.
    color = "black",
    # Adjust angle of text labels verticaly for better readability.
    angle = 90
  ) +
  # Add labels.
  labs(x = "Rodent species", 
       y = "Percentage", 
       title = paste0("Sample size of rodents (n = ",
                     nrow(rodent.data.MF.HF),
                     ")")) +
  # Add horizontal line at 50% to asses species ratios.
  geom_hline(yintercept = .5, linetype = "dashed") + 
  # Applying a minimal theme.
  theme_minimal() +
  # Rotate x-axis labels to 45 degrees.
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))  

# We realize 'intermedius' has to be dropped (not informative for group analysis).
# Also, we need to re-sample observed species that show biased sex proportions for 
# proper analysis of the groups.

# Eliminate 'intermedius' out of the study
rodent.data.MF.HF.clean <- rodent.data.MF.HF[rodent.data.MF.HF$species != "intermedius", ]

# Initialize an empty vector to store species names that meet re-sampling criteria.
species.names.resample <- c()

# Initializing a data frame that will include the final re-sampled data combined with other non-sampled data.
rodent.data.MF.HF.clean.final <- data.frame()

# Setting a seed for reproducibility before re-sampling.
set.seed(123)

# Loop through species names in main data.
for (species in unique(rodent.data.MF.HF.clean$species)) {

  # Subsetting for species
  species.subset <- rodent.data.MF.HF.clean[rodent.data.MF.HF.clean$species == species, ]
  
  # Subsetting for males (M) and females (F).
  species.subset.M <- species.subset[species.subset$sex == "M", ]
  species.subset.F <- species.subset[species.subset$sex == "F", ]

  # Check if the size of one group is not more than twice the size of the other.
  if(nrow(species.subset.M) >= 2*nrow(species.subset.F) || nrow(species.subset.F) >= 2*nrow(species.subset.M)){
   
    # If the condition is true, store the species name.
    species.names.resample <- c(species.names.resample, species)

    # Identify the larger group for downsampling.
    if (nrow(species.subset.M) > nrow(species.subset.F)) {
      # Resample males without replacement to match the size of the female group.
      downsampled.indices <- sample(nrow(species.subset.M), nrow(species.subset.F), replace = FALSE)
      species.subset.M <- species.subset.M[downsampled.indices, ]
    } 
    if (nrow(species.subset.M) < nrow(species.subset.F)) {
      # Resample females without replacement to match the size of the male group
      downsampled.indices <- sample(nrow(species.subset.F), nrow(species.subset.M), replace = FALSE)
      species.subset.F <- species.subset.F[downsampled.indices, ]
    }

    # Combine the resampled subsets.
    species.sex.combined <- rbind(species.subset.M, species.subset.F)
    
    # Append the balanced subset back into the balanced dataset.
    rodent.data.MF.HF.clean.final <- rbind(rodent.data.MF.HF.clean.final, species.sex.combined)
  }
}

# Identify non-resampled species.
non.resampled.species <- setdiff(unique(rodent.data.MF.HF.clean$species), species.names.resample)

# Extract data for non-resampled species
non.resampled.data <- rodent.data.MF.HF.clean[rodent.data.MF.HF.clean$species %in% non.resampled.species, ]

# Append the non-resampled data to the final resampled dataframe
rodent.data.MF.HF.clean.final <- rbind(rodent.data.MF.HF.clean.final, non.resampled.data)

# Plot balanced sample sizes between sex groups across species.
ggplot(rodent.data.MF.HF.clean.final, aes(x = species, fill = sex)) +
  # Add bar plot.
  geom_bar(aes(y = after_stat(count/sum(count))), position = "fill") +
  # Scale y to display percentages instead.
  scale_y_continuous(labels = scales::percent) +
  # Adjust fill colors manually.
  scale_fill_manual(values = c("F"="pink","M"="skyblue")) + 
  # Add text labels on the bars showing the actual counts.
  geom_text(
    # Position text labels at the middle of each bar.
    aes(y = after_stat(count/sum(count)/2), label = after_stat(count)), 
    # Center text labels within each filled section.
    position = position_fill(vjust = 0.5), 
    # Ensure count is calculated for text labels.
    stat = "count",
    # Set color of text labels.
    color = "black",
    # Adjust angle of text labels vertically for better readability.
    angle = 90
  ) +
  # Add labels.
  labs(x = "Rodent species", 
       y = "Percentage", 
       title = paste0("Sample size of rodents after resampling (n = ",
                     nrow(rodent.data.MF.HF.clean.final),
                     ")"))+
  # Add horizontal line at 50% to asses species ratios.
  geom_hline(yintercept = .5, linetype = "dashed") + 
  # Applying a minimal theme.
  theme_minimal() +
  # Rotate x-axis labels to 45 degrees.
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
