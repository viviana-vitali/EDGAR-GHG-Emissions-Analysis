# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(gridExtra)  # for arranging plots

# Load the data
file_path <- "EDGAR_2024_GHG_booklet_2024.xlsx"
ghg_totals_by_country <- read_excel(file_path, sheet = "GHG_totals_by_country")

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

# Calculate year-over-year GHG growth for Euro Area
euro_area_growth <- euro_area_data %>%
  arrange(Year) %>%
  mutate(Growth = (Emissions - lag(Emissions)) / lag(Emissions) * 100)

# Calculate year-over-year GHG growth for EU27
eu27_growth <- eu27_data %>%
  arrange(Year) %>%
  mutate(Growth = (Emissions - lag(Emissions)) / lag(Emissions) * 100)

# Calculate year-over-year GHG growth for GLOBAL TOTAL
global_total_growth <- global_total_data %>%
  arrange(Year) %>%
  mutate(Growth = (Emissions - lag(Emissions)) / lag(Emissions) * 100)

# Create individual plots for each region
p1 <- ggplot(euro_area_growth, aes(x = as.numeric(Year), y = Growth, group = 1)) +
  geom_line(color = "#77DD77", size = 1) +
  labs(title = "Year-over-Year GHG Growth: Euro Area", 
       x = "Year", 
       y = "GHG Growth (%)") +
  theme_minimal()

p2 <- ggplot(eu27_growth, aes(x = as.numeric(Year), y = Growth, group = 1)) +
  geom_line(color = "#AEC6CF", size = 1) +
  labs(title = "Year-over-Year GHG Growth: EU27", 
       x = "Year", 
       y = "GHG Growth (%)") +
  theme_minimal()

p3 <- ggplot(global_total_growth, aes(x = as.numeric(Year), y = Growth, group = 1)) +
  geom_line(color = "#FFB347", size = 1) +
  labs(title = "Year-over-Year GHG Growth: GLOBAL TOTAL", 
       x = "Year", 
       y = "GHG Growth (%)") +
  theme_minimal()

# Arrange the three plots horizontally
grid.arrange(p1, p2, p3, ncol = 1)  # ncol=1 places them in a vertical stack (rectangular arrangement)


calculate_mean_and_variance <- function(df) {
  return(
    df %>%
      summarise(
        mean = mean(Growth, na.rm = TRUE),
        variance = var(Growth, na.rm = TRUE)
      )
  )
}

euro_area_mean_and_variance <- calculate_mean_and_variance(euro_area_growth)
eu27_mean_and_variance <- calculate_mean_and_variance(eu27_growth)
global_total_mean_and_variance <- calculate_mean_and_variance(global_total_growth)

print(euro_area_mean_and_variance)
print(eu27_mean_and_variance)
print(global_total_mean_and_variance)
