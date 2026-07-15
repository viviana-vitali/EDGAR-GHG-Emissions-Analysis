# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)

# Load the data
file_path <- "EDGAR_2024_GHG_booklet_2024.xlsx"
ghg_totals_by_country <- read_excel(file_path, sheet = "GHG_totals_by_country")

# Chart 1a: Evolution of GHG Growth in the Euro Area, and EU27

# List of countries in the Euro Area 
euro_area_countries <- c("Austria", "Belgium", "Cyprus", "Estonia", "Finland", 
                         "France", "Germany", "Greece", "Ireland", "Italy", 
                         "Latvia", "Lithuania", "Luxembourg", "Malta", 
                         "Netherlands", "Portugal", "Slovakia", "Slovenia", 
                         "Spain")

# Filter and aggregate data for Euro Area
euro_area_data <- ghg_totals_by_country %>%
  filter(Country %in% euro_area_countries) %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"), names_to = "Year", values_to = "Emissions") %>%
  group_by(Year) %>%
  summarise(Emissions = sum(Emissions)) %>%
  mutate(Country = "Euro Area")

# Extract EU27 data
eu27_data <- ghg_totals_by_country %>%
  filter(Country == "EU27") %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"), names_to = "Year", values_to = "Emissions")

# Extract GLOBAL TOTAL data
global_total_data <- ghg_totals_by_country %>%
  filter(Country == "GLOBAL TOTAL") %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"), names_to = "Year", values_to = "Emissions")

# Combine Euro Area, EU27 and World data
ghg_combined <- bind_rows(euro_area_data, eu27_data, global_total_data)

# Calculate year-over-year GHG growth
ghg_combined <- ghg_combined %>%
  group_by(Country) %>%
  arrange(Year) %>%
  mutate(Growth = (Emissions - lag(Emissions)) / lag(Emissions) * 100)

# Plot GHG growth for Euro Area, EU27 and World
ggplot(ghg_combined, aes(x = as.numeric(Year), y = Growth, color = Country, group = Country)) +
  geom_line(size = 0.75) +
  scale_color_manual(values = c("#77DD77", "#89CFF0", "#FFB347")) +
  labs(title = "Evolution of GHG growth in the Euro Area (EA), European Union (EU27) and worldwide",
       x = "Year",
       y = "GHG Growth (%)",
       color = "Region") +
  theme_minimal()
