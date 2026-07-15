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

# Identify major contributors
major_countries <- ghg_long %>%
  group_by(Country) %>%
  summarise(Average_Contribution = mean(GHG_Contribution, na.rm = TRUE)) %>%
  arrange(desc(Average_Contribution)) %>%
  slice_head(n = 15) %>%  # Top 15 contributors
  pull(Country)

# Classify countries
ghg_long <- ghg_long %>%
  mutate(Country_Group = ifelse(Country %in% major_countries, Country, "Other"))

# Normalize GHG contribution by year
ghg_long <- ghg_long %>%
  group_by(Year) %>%
  mutate(
    Total_GHG_Contribution = sum(GHG_Contribution, na.rm = TRUE),
    GHG_Contribution_Percentage = (GHG_Contribution / Total_GHG_Contribution) * 100
  ) %>%
  ungroup()

# Final year in the dataset
final_year <- max(ghg_long$Year, na.rm = TRUE)

# Contribution order for the final year
final_year_contributions <- ghg_long %>%
  filter(Year == final_year) %>%
  group_by(Country) %>%
  summarise(Final_Contribution = sum(GHG_Contribution, na.rm = TRUE)) %>%
  arrange(desc(Final_Contribution)) %>%
  pull(Country)

# Reorder Country_Group factor levels
ghg_long$Country_Group <- factor(ghg_long$Country_Group, levels = c(final_year_contributions, "Other"))

# Plot the data
ggplot(ghg_long, aes(x = Year, y = GHG_Contribution_Percentage, fill = Country_Group)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "GHG Emissions Contribution by Country (Percentage)",
    x = "Year",
    y = "Contribution to Total World GHG Emissions (%)",
    fill = "Country Group"
  ) +
  scale_fill_manual(
    values = c(RColorBrewer::brewer.pal(12, "Set3"), "lightgray", "lightpink", "lightblue", "lightgreen"),
    name = "Country Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
