library(tidyverse)
library(scales)
library(viridis)

# ==========================================
# 1. BIOLOGICAL PARAMETERS & RELEASE NUMBERS
# ==========================================

# Baseline annual hatchery releases (Totaling roughly 5 Billion)
# You can update these numbers to match the exact latest year in your dataset
releases <- data.frame(
  Species = c("Pink", "Chum", "Sockeye", "Coho", "Chinook"),
  Smolts_Released = c(1.5e9, 2.8e9, 300e6, 200e6, 200e6) 
)

# Species-specific biological matrix (Ocean Ages 1 to 5)
# M: Natural Mortality | m: Maturation (spawning exit) | Weight_kg | Q_B: Consumption/Biomass
params <- tribble(
  ~Species, ~Age, ~M,  ~m,  ~Weight_kg, ~Q_B,
  # Pink (2-year life cycle, 100% spawn at age 2)
  "Pink",   1,    3.22, 0.0,  0.15,       8.0,
  "Pink",   2,    0.22, 1.0,  1.50,       5.0,
  "Pink",   3,    0.0, 1.0,  0.00,       0.0,
  "Pink",   4,    0.0, 1.0,  0.00,       0.0,
  "Pink",   5,    0.0, 1.0,  0.00,       0.0,
  
  # Chum (Stretches to 5 years)
  "Chum",   1,    3.51, 0.0,  0.15,       8.0,
  "Chum",   2,    0.22, 0.0,  1.00,       5.0,
  "Chum",   3,    0.22, 0.3,  2.50,       4.0,
  "Chum",   4,    0.22, 0.6,  4.00,       3.5,
  "Chum",   5,    0.22, 1.0,  5.50,       3.0,
  
  # Sockeye
  "Sockeye",1,    3.22, 0.0,  0.20,       7.0,
  "Sockeye",2,    0.22, 0.1,  1.20,       5.0,
  "Sockeye",3,    0.22, 0.5,  2.50,       4.0,
  "Sockeye",4,    0.22, 1.0,  3.50,       3.5,
  "Sockeye",5,    0.0, 1.0,  0.00,       0.0,
  
  # Coho (Usually out by Ocean Age 3)
  "Coho",   1,    2.81, 0.0,  0.30,       7.0,
  "Coho",   2,    0.22, 0.1,  1.50,       5.0,
  "Coho",   3,    0.22, 1.0,  3.50,       4.0,
  "Coho",   4,    0.0, 1.0,  0.00,       0.0,
  "Coho",   5,    0.0, 1.0,  0.00,       0.0,
  
  # Chinook (Lower early mortality, biggest weight, late maturation)
  "Chinook",1,    3.91, 0.0,  0.20,       7.0,
  "Chinook",2,    0.22, 0.05, 1.50,       5.0,
  "Chinook",3,    0.22, 0.2,  4.00,       4.0,
  "Chinook",4,    0.22, 0.5,  7.00,       3.5,
  "Chinook",5,    0.22, 1.0,  10.00,      3.0
)

# ==========================================
# 2. RUN THE COHORT SIMULATION
# ==========================================

run_species_model <- function(target_species) {
  # Get data for this specific species
  sp_params <- params %>% filter(Species == target_species) %>% arrange(Age)
  sp_smolts <- releases %>% filter(Species == target_species) %>% pull(Smolts_Released)
  
  N_at_sea <- numeric(5)
  Biomass_kg <- numeric(5)
  Consumption_kg <- numeric(5)
  
  N_at_sea[1] <- sp_smolts
  
  for (a in 1:5) {
    Biomass_kg[a] <- N_at_sea[a] * sp_params$Weight_kg[a]
    Consumption_kg[a] <- Biomass_kg[a] * sp_params$Q_B[a]
    
    if (a < 5) {
      N_at_sea[a+1] <- N_at_sea[a] * exp(-sp_params$M[a]) * (1 - sp_params$m[a])
    }
  }
  
  sp_params %>%
    mutate(
      Numbers_Alive = N_at_sea,
      Standing_Biomass_MT = Biomass_kg / 1000,
      Prey_Consumed_MT = Consumption_kg / 1000
    )
}

# Apply the function across all 5 species and combine into one dataframe
all_species_results <- map_dfr(releases$Species, run_species_model)

# ==========================================
# 3. SUMMARY & VISUALIZATION
# ==========================================

# Print the Grand Total
total_prey <- sum(all_species_results$Prey_Consumed_MT)
cat("\nTotal Estimated Prey Consumed by Hatchery Cohort:\n")
cat(comma(round(total_prey)), "Metric Tons Annually\n\n")

# Print Summary Table by Species (Now including Smolts Released)
species_summary <- all_species_results %>%
  group_by(Species) %>%
  summarize(
    # Grab the initial number of fish entering the ocean (Age 1)
    Smolts_Released = max(Numbers_Alive[Age == 1]), 
    Total_Biomass_MT = sum(Standing_Biomass_MT),
    Total_Prey_MT = sum(Prey_Consumed_MT)
  ) %>%
  arrange(desc(Total_Prey_MT))

# Print with comma formatting for readability
print(species_summary %>% mutate(across(where(is.numeric), ~comma(round(.)))))

# Plot the Ecological Footprint by Species and Age
p_footprint <- ggplot(all_species_results %>% filter(Prey_Consumed_MT > 0), 
                      aes(x = Age, y = Prey_Consumed_MT / 1e6, fill = Species)) +
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.2) +
  scale_fill_viridis_d(option = "mako") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "The Ecological Footprint of Hatchery Salmon",
    subtitle = "Millions of Metric Tons of prey consumed annually, by ocean age",
    x = "Ocean Age",
    y = "Prey Consumed (Millions of MT)",
    fill = "Species"
  ) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "right", plot.margin = margin(10, 20, 10, 20))

# Save the plot
ggsave("Analysis_6_Footprint.png", p_footprint, width = 8, height = 8, units="in", dpi = 300, bg = "white")

# ==========================================
# 4. ECOLOGICAL SCALE COMPARISONS
# ==========================================

# Reference baselines (in Metric Tons)
pollock_annual_harvest_mt <- 1.5e6  # US Total Allowable Catch for Alaska Pollock
pollock_living_biomass_mt <- 5.5e6  # Estimated living biomass in Eastern Bering Sea
caribou_annual_forage_mt  <- 1.8    # Annual forage requirement for an adult Alaskan Caribou

# Calculate the multipliers based on the dynamically generated total footprint
pollock_harvest_multiplier <- total_prey / pollock_annual_harvest_mt
pollock_biomass_multiplier <- total_prey / pollock_living_biomass_mt
caribou_equivalent         <- total_prey / caribou_annual_forage_mt

# Print the jaw-dropping comparisons to the console
cat("\n======================================================\n")
cat("ECOLOGICAL SCALE OF HATCHERY CONSUMPTION:\n")
cat("======================================================\n\n")

cat("MARINE COMPARISON (Alaska Pollock):\n")
cat("- Hatchery salmon consume", round(pollock_harvest_multiplier, 1), 
    "times MORE biomass than the entire U.S. commercial harvest of Alaska Pollock.\n")
cat("- They consume roughly", round(pollock_biomass_multiplier, 1), 
    "times the ENTIRE living biomass of the Eastern Bering Sea pollock stock.\n\n")

cat("TERRESTRIAL COMPARISON (Alaskan Caribou):\n")
cat("- To match this level of grazing on land, you would have to unleash a herd of\n  ", 
    comma(round(caribou_equivalent)), "Caribou onto the tundra (over 2x the global wild population).\n")
cat("======================================================\n")

# ==========================================
# IMPLICIT MARINE SURVIVAL CALCULATIONS
# ==========================================

survival_summary <- all_species_results %>%
  group_by(Species) %>%
  mutate(
    # Fish that survive natural mortality (M) and then mature (m)
    Mature_Returns = Numbers_Alive * exp(-M) * m
  ) %>%
  summarize(
    Smolts_Released = max(Numbers_Alive[Age == 1]),
    Total_Returns = sum(Mature_Returns)
  ) %>%
  mutate(
    Marine_Survival_Rate = Total_Returns / Smolts_Released,
    Survival_Percent = percent(Marine_Survival_Rate, accuracy = 0.01)
  ) %>%
  # FIX: Sort the data BEFORE selecting the final columns
  arrange(desc(Marine_Survival_Rate)) %>%
  select(Species, Smolts_Released, Total_Returns, Survival_Percent) 

cat("\n======================================================\n")
cat("IMPLICIT MARINE SURVIVAL RATES:\n")
cat("======================================================\n")
print(survival_summary %>% mutate(across(where(is.numeric), ~comma(round(.)))))
cat("======================================================\n")
