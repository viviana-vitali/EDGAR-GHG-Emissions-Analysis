# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(countrycode)

# Load the data
file_path <- "EDGAR_2024_GHG_booklet_2024.xlsx"
ghg_totals_by_country <- read_excel(file_path, sheet = "GHG_proportion_by_country")

# Classify countries by continent
data <- ghg_totals_by_country %>%
  mutate(
    Continent = countrycode(Country, "country.name", "continent"),
    Continent = ifelse(is.na(Continent), countrycode(`EDGAR Country Code`, "wb", "continent"), Continent)
  )

# Handle unmatched rows
data <- data %>%
  mutate(
    Continent = case_when(
      Country == "Serbia and Montenegro" ~ "Europe",
      TRUE ~ Continent
    )
  ) %>%
  filter(!is.na(Continent))  # Remove rows without a continent classification

# Prepare data for bar chart
bar_chart <- data %>%
  select(-c(`EDGAR Country Code`, `Continent`))

# Transform data from wide to long format
ghg_long <- bar_chart %>%
  pivot_longer(
    cols = -Country,  # All columns except "Country"
    names_to = "Year",  # Convert column names to "Year"
    values_to = "GHG_Contribution"  # Corresponding values to "GHG_Contribution"
  ) %>%
  mutate(Year = as.numeric(Year))  # Convert Year to numeric

# Aggregate data by continent
ghg_long_by_continent <- ghg_long %>%
  left_join(data %>% select(Country, Continent), by = "Country") %>%
  group_by(Year, Continent) %>%
  summarise(
    Total_GHG_Contribution = sum(GHG_Contribution, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Year) %>%
  mutate(
    Total_World_GHG = sum(Total_GHG_Contribution, na.rm = TRUE),
    Continent_Contribution_Percentage = (Total_GHG_Contribution / Total_World_GHG) * 100
  ) %>%
  ungroup()

# Plot the data
ggplot(ghg_long_by_continent, aes(x = Year, y = Continent_Contribution_Percentage, fill = Continent)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "GHG Emissions Contribution by Continent (Percentage)",
    x = "Year",
    y = "Contribution to Total World GHG Emissions (%)",
    fill = "Continent"
  ) +
  scale_fill_brewer(palette = "Set3", name = "Continent") +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
