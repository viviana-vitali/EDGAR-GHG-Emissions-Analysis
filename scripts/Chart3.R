# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(countrycode)
library(tidyverse)

# Load the data
file_path <- "EDGAR_2024_GHG_booklet_2024.xlsx"
ghg_totals_by_country <- read_excel(file_path, sheet = "GHG_proportion_by_country")

# Classify countries by continent
data <- ghg_totals_by_country %>%
  mutate(Continent = countrycode(Country, "country.name", "continent"))

# Handle rows not matched in the first join
unmatched_data <- data %>%
  filter(is.na(`Continent`))

# Second join for unmatched rows
additional_matches <- unmatched_data %>%
  mutate(Continent = countrycode(`EDGAR Country Code`, "wb", "continent"))

final_data <- data %>%
  filter(!is.na(`Continent`)) %>% # Keep rows matched in the first join
  bind_rows(additional_matches)

unmatched_final <- final_data %>% filter(is.na(`Continent`))

# Assign manual Income group classifications
complete_data <- final_data %>%
  mutate(`Continent` = case_when(
    Country == "Serbia and Montenegro" ~ "Europe",
    TRUE ~ `Continent` # Keep existing values for other countries
  ))

ghg_totals_by_country <- complete_data %>%
  filter(!is.na(`Continent`))

# Data for bar chart 
bar_chart <- ghg_totals_by_country %>%
  select(-c("EDGAR Country Code","Continent"))

# Melt the data from wide to long format
ghg_long <- bar_chart %>%
  pivot_longer(
    cols = -`Country`,  # All columns except "Country"
    names_to = "Year",  # Convert column names (years) into a new column "Year"
    values_to = "GHG_Contribution"  # Values from those columns go here
  )

# Convert Year to numeric for proper sorting
ghg_long$Year <- as.numeric(ghg_long$Year)

# Filter out "GLOBAL TOTAL" and identify major contributors
major_countries <- ghg_long %>%
  group_by(Country) %>%
  summarise(Average_Contribution = mean(GHG_Contribution, na.rm = TRUE)) %>%
  arrange(desc(Average_Contribution)) %>%
  slice_head(n = 15) %>%  # Top 10 contributors
  pull(Country)  # Extract country names

# Create a new column to classify major and other countries
ghg_long <- ghg_long %>%
  mutate(
    Country_Group = ifelse(Country %in% major_countries, Country, "Other")
  )

# Normalize GHG contribution by year to get the percentage of total emissions
ghg_long <- ghg_long %>%
  group_by(Year) %>%
  mutate(
    Total_GHG_Contribution = sum(GHG_Contribution, na.rm = TRUE),
    GHG_Contribution_Percentage = GHG_Contribution / Total_GHG_Contribution * 100
  ) %>%
  ungroup()

# Get the final year in the dataset
final_year <- max(ghg_long$Year, na.rm = TRUE)

# Summarize the GHG contribution for the final year and sort countries by their share
final_year_contributions <- ghg_long %>%
  filter(Year == final_year) %>%
  group_by(Country) %>%
  summarise(Final_Contribution = sum(GHG_Contribution, na.rm = TRUE)) %>%
  arrange(desc(Final_Contribution)) %>%
  pull(Country)

# Reorder the Country_Group based on the final year contribution order
ghg_long$Country_Group <- factor(ghg_long$Country, levels = c(final_year_contributions, "Other"))

# Plot the normalized data
ggplot(ghg_long, aes(x = Year, y = GHG_Contribution_Percentage, fill = Country_Group)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Stacked Bar Chart of GHG Emissions Contribution by Country (Percentage)",
    x = "Year",
    y = "Contribution to Total World GHG Emissions (%)",
    fill = "Country"
  ) +
  scale_fill_manual(
    values = c(RColorBrewer::brewer.pal(12, "Set3"), "lightgray", "lightpink", "lightblue", "lightgreen"),
    name = "Country",
    breaks = c(major_countries, "Other")  # Show only major countries in the legend
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# 
# 
# # Check the data structure and total contributions for each year
# summary(ghg_long)  # This will help you identify any NAs or unexpected values
# 
# # Summarize by year to see the total GHG contribution per year
# total_by_year <- ghg_long %>%
#   group_by(Year) %>%
#   summarise(Total_Contribution = sum(GHG_Contribution, na.rm = TRUE))
# 
# # Check if the sums make sense for each year
# print(total_by_year)
# 
# # Plot the data to check the structure of the stacked bar chart
# ggplot(ghg_long, aes(x = Year, y = GHG_Contribution, fill = Country_Group)) +
#   geom_bar(stat = "identity", position = "stack") +
#   labs(
#     title = "Stacked Bar Chart of GHG Emissions Contribution by Country",
#     x = "Year",
#     y = "Contribution to Total World GHG Emissions",
#     fill = "Country"
#   ) +
#   scale_fill_manual(
#     values = c(RColorBrewer::brewer.pal(12, "Set3"), "grey", "red", "blue", "green"),
#     name = "Country",
#     breaks = c(major_countries, "Other")  # Show only major countries in the legend
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "right",
#     legend.title = element_text(face = "bold"),
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )