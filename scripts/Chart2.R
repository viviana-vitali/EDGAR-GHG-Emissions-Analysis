# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)

# Load the data
world_bank_class <- read_excel("CLASS1.xlsx", sheet = "List of economies")
ghg_per_capita_by_country <- read_excel("EDGAR_2024_GHG_booklet_2024.xlsx", sheet = "GHG_per_capita_by_country") 


# Join the two datasets on the country names
merged_data <- ghg_per_capita_by_country %>%
  left_join(world_bank_class, by = c("Country" = "Economy")) %>%
  select(-c("Region", "Lending category"))

# Handle rows not matched in the first join
unmatched_data <- merged_data %>%
  filter(is.na(`Income group`)) %>%
  select(-`Income group`) # Remove the unmatched "Income group" column

# Second join for unmatched rows
additional_matches <- unmatched_data %>%
  left_join(world_bank_class, by = c("EDGAR Country Code" = "Code")) %>%
  select(-c("Region", "Lending category","Code","Economy"))

final_data <- merged_data %>%
  filter(!is.na(`Income group`)) %>% # Keep rows matched in the first join
  bind_rows(additional_matches) %>% # Add rows matched in the second join
  select(-c("Code"))

unmatched_final <- final_data %>% filter(is.na(`Income group`))

# Assign manual Income group classifications
complete_data <- final_data %>%
  mutate(`Income group` = case_when(
    Country == "Anguilla" ~ "Not classified",
    Country == "Cook Islands" ~ "Not classified",
    Country == "Western Sahara" ~ "Not classified",
    Country == "Falkland Islands" ~ "Not classified",
    Country == "Guadeloupe" ~ "High income",
    Country == "French Guiana" ~ "High income",
    Country == "Martinique" ~ "High income",
    Country == "Réunion" ~ "High income",
    Country == "Serbia and Montenegro" ~ "Upper middle income",
    Country == "Saint Helena, Ascension and Tristan da Cunha" ~ "Not classified",
    Country == "Saint Pierre and Miquelon" ~ "Not classified",
    Country == "Venezuela" ~ "Not classified",
    TRUE ~ `Income group` # Keep existing values for other countries
  ))

ghg_per_capita_by_country_cleaned <- complete_data %>%
  filter(!is.na(`Income group`) & `Income group` != "Not classified")

# For the countries you've listed:
# 
# Cook Islands: The Cook Islands is not a member of the World Bank and therefore does not have an official income classification.
# Western Sahara: Western Sahara is a disputed territory and is not classified by the World Bank.
# Falkland Islands: The Falkland Islands (Islas Malvinas) are a British Overseas Territory and are not individually classified by the World Bank.
# 
# Guadeloupe: Guadeloupe is an overseas region of France and is considered part of the French economy, which is classified as high-income.
# French Guiana: French Guiana is an overseas region of France and shares France's high-income classification.
# Martinique: Martinique is an overseas region of France and is classified as high-income, in line with France.
# Réunion: Réunion is an overseas region of France and shares the high-income classification of France.
# 
# Serbia and Montenegro: Serbia and Montenegro separated into two independent countries in 2006. As of the latest classifications, Serbia is classified as an upper-middle-income economy, while Montenegro is classified as an upper-middle-income economy.
# 
# Saint Helena, Ascension and Tristan da Cunha: This is a British Overseas Territory and is not individually classified by the World Bank.
# Saint Pierre and Miquelon: Saint Pierre and Miquelon is a self-governing territorial overseas collectivity of France and is not individually classified by the World Bank.
# Venezuela: Due to the unavailability of recent data, Venezuela has been unclassified by the World Bank since the 2021 fiscal year. 

# Reshape the data
ghg_per_capita_summary <- ghg_per_capita_by_country_cleaned %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"), names_to = "Year", values_to = "EmissionsPerCapita") %>%
  group_by(`Income group`, Year) %>% # Group by Income Group and Year
  summarise(MeanEmissionsPerCapita = mean(EmissionsPerCapita, na.rm = TRUE), .groups = "drop")

# Plot emissions per capita by income group
ggplot(ghg_per_capita_summary, aes(x = as.numeric(Year), y = MeanEmissionsPerCapita, color = `Income group`, group = `Income group`)) +
  geom_line(size = 1) +
  labs(title = "GHG Emissions Per Capita by Income Group", 
       x = "Year", 
       y = "Mean Emissions Per Capita (Mt CO2-eq)", 
       color = "Income Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
