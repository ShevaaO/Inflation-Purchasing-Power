# ------------------------------------------------------------
# Project: Food inflation vs overall inflation in EU countries
# Dataset: Eurostat HICP annual data, 2016–2025
# ------------------------------------------------------------

#Load packages
library(tidyverse)
library(dplyr)

# ------------------------------------------------------------
# 1. Load data
# ------------------------------------------------------------

hicp <- read.csv("prc_hicp_aind__custom_21166576_linear.csv")

# ------------------------------------------------------------
# 2. Basic data description
# ------------------------------------------------------------

# Row and column count
dim(hicp)

# Column names
names(hicp)

# Structure of the dataset
str(hicp)

# First rows
head(hicp)

# Check key variables
unique(hicp$coicop)
unique(hicp$geo)
unique(hicp$TIME_PERIOD)
unique(hicp$unit)

# Count observations by category
table(hicp$coicop)

# Count observations by year
table(hicp$TIME_PERIOD)

# Count countries
length(unique(hicp$geo))

# ------------------------------------------------------------
# 3. Cleaning narrative
# ------------------------------------------------------------

# Rename variables (for easier work)
hicp_clean <- hicp %>%
  rename(
    country = geo,
    year = TIME_PERIOD,
    inflation = OBS_VALUE,
    category = coicop
  )

# Keep only relevant columns
hicp_clean <- hicp_clean %>%
  select(country, year, category, inflation, unit)

# Check missing values in the variables used for analysis
colSums(is.na(hicp_clean))

# Create period variable
hicp_clean <- hicp_clean %>%
  mutate(
    period = case_when(
      year >= 2016 & year <= 2020 ~ "2016-2020",
      year >= 2021 & year <= 2025 ~ "2021-2025",
      TRUE ~ NA_character_
    )
  )

# Check whether all observations were assigned to one of the two periods
table(hicp_clean$period, useNA = "ifany")

# Convert from long to wide format
hicp_final <- hicp_clean %>%
  pivot_wider(
    names_from = category,
    values_from = inflation
  )

# Rename inflation variables
hicp_final <- hicp_final %>%
  rename(
    overall_inflation = `All-items HICP`,
    food_inflation = Food
  )

# Create comparison variables:
# food_inflation_gap = food inflation minus overall inflation
# food_overall_ratio = food inflation divided by overall inflation
hicp_final <- hicp_final %>%
  mutate(
    food_inflation_gap = food_inflation - overall_inflation,
    
    # Full ratio, including negative overall inflation
    food_overall_ratio = food_inflation / overall_inflation,
    
    # Safer ratio, only for positive overall inflation
    food_overall_ratio_positive = ifelse(
      overall_inflation > 0,
      food_inflation / overall_inflation,
      NA
    )
  )

# Check final dataset
dim(hicp_final)
head(hicp_final)
summary(hicp_final)

# ------------------------------------------------------------
# 4. Outlier check
# ------------------------------------------------------------
# We inspect unusually high or low inflation observations.
# We do not automatically remove them, because high inflation after 2020 is economically meaningful for this project.

highest_food_inflation <- hicp_final %>%
  arrange(desc(food_inflation)) %>%
  select(country, year, period, overall_inflation, food_inflation, food_inflation_gap) %>%
  head(10)

highest_food_inflation


highest_overall_inflation <- hicp_final %>%
  arrange(desc(overall_inflation)) %>%
  select(country, year, period, overall_inflation, food_inflation, food_inflation_gap) %>%
  head(10)

highest_overall_inflation


lowest_food_inflation <- hicp_final %>%
  arrange(food_inflation) %>%
  select(country, year, period, overall_inflation, food_inflation, food_inflation_gap) %>%
  head(10)

lowest_food_inflation

# ------------------------------------------------------------
# 5. Descriptive statistics
# ------------------------------------------------------------
# These are needed for the report:
# mean, median, standard deviation, minimum, and maximum.
# ------------------------------------------------------------
# Check problematic ratio values
# ------------------------------------------------------------

# The food_overall_ratio is not defined when overall_inflation equals zero,
# because division by zero creates Inf or -Inf.
# These observations stay in the dataset, but they are not used
# when calculating the average food_overall_ratio.

problem_ratio_values <- hicp_final %>%
  filter(overall_inflation == 0) %>%
  select(
    country,
    year,
    period,
    overall_inflation,
    food_inflation,
    food_overall_ratio
  )

problem_ratio_values

descriptive_stats <- hicp_final %>%
  group_by(period) %>%
  summarise(
    n = n(),
    
    mean_overall = mean(overall_inflation, na.rm = TRUE),
    median_overall = median(overall_inflation, na.rm = TRUE),
    sd_overall = sd(overall_inflation, na.rm = TRUE),
    min_overall = min(overall_inflation, na.rm = TRUE),
    max_overall = max(overall_inflation, na.rm = TRUE),
    
    mean_food = mean(food_inflation, na.rm = TRUE),
    median_food = median(food_inflation, na.rm = TRUE),
    sd_food = sd(food_inflation, na.rm = TRUE),
    min_food = min(food_inflation, na.rm = TRUE),
    max_food = max(food_inflation, na.rm = TRUE),
    
    mean_gap = mean(food_inflation_gap, na.rm = TRUE),
    median_gap = median(food_inflation_gap, na.rm = TRUE),
    sd_gap = sd(food_inflation_gap, na.rm = TRUE),
    min_gap = min(food_inflation_gap, na.rm = TRUE),
    max_gap = max(food_inflation_gap, na.rm = TRUE),
    
    mean_ratio = mean(food_overall_ratio[is.finite(food_overall_ratio)], na.rm = TRUE),
    median_ratio = median(food_overall_ratio[is.finite(food_overall_ratio)], na.rm = TRUE)
  )

descriptive_stats


# Main comparison table for the central finding

period_comparison <- hicp_final %>%
  group_by(period) %>%
  summarise(
    avg_overall_inflation = mean(overall_inflation, na.rm = TRUE),
    avg_food_inflation = mean(food_inflation, na.rm = TRUE),
    avg_food_inflation_gap = mean(food_inflation_gap, na.rm = TRUE),
    avg_food_overall_ratio = mean(food_overall_ratio[is.finite(food_overall_ratio)], na.rm = TRUE),
    n = n()
  )

period_comparison

# ------------------------------------------------------------
# 6. Visualisation 1: Histogram
# ------------------------------------------------------------
# This histogram shows the distribution of the food inflation gap.
# The gap is food inflation minus overall inflation.
# Positive values mean that food inflation was higher than overall inflation.

graph_histogram <- ggplot(hicp_final, aes(x = food_inflation_gap)) +
  geom_histogram(bins = 25, color = "white") +
  facet_wrap(~ period) +
  labs(
    title = "Distribution of Food Inflation Gap in EU Countries",
    subtitle = "Food inflation minus overall inflation, 2016–2020 vs 2021–2025",
    x = "Food inflation gap, percentage points",
    y = "Number of country-year observations"
  ) +
  theme_minimal()

graph_histogram
# ------------------------------------------------------------
# 7. Visualisation 2: Scatterplot
# ------------------------------------------------------------

# This scatterplot shows the relationship between overall inflation and food inflation. A positive slope means that countries and years with higher overall inflation also tend to have higher food inflation.

graph_scatter <- ggplot(
  hicp_final,
  aes(x = overall_inflation, y = food_inflation, color = period)
) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relationship between Overall Inflation and Food Inflation",
    subtitle = "EU countries, 2016–2025",
    x = "Overall inflation, annual average rate of change (%)",
    y = "Food inflation, annual average rate of change (%)",
    color = "Period"
  ) +
  theme_minimal()

graph_scatter

# ------------------------------------------------------------
# 8. Visualisation 3: Line chart
# ------------------------------------------------------------

# This graph shows the average development of overall inflation and food inflation across EU countries over time.

eu_average_by_year <- hicp_final %>%
  group_by(year) %>%
  summarise(
    average_overall_inflation = mean(overall_inflation, na.rm = TRUE),
    average_food_inflation = mean(food_inflation, na.rm = TRUE)
  )

eu_average_long <- eu_average_by_year %>%
  pivot_longer(
    cols = c(average_overall_inflation, average_food_inflation),
    names_to = "inflation_type",
    values_to = "inflation_rate"
  ) %>%
  mutate(
    inflation_type = case_when(
      inflation_type == "average_overall_inflation" ~ "Overall inflation",
      inflation_type == "average_food_inflation" ~ "Food inflation"
    )
  )

graph_line <- ggplot(
  eu_average_long,
  aes(x = year, y = inflation_rate, color = inflation_type)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = 2016:2025
  ) + 
  labs(
    title = "Average Overall and Food Inflation in EU Countries",
    subtitle = "Annual average rate of change, 2016–2025",
    x = "Year",
    y = "Inflation rate (%)",
    color = "Inflation type"
  ) +
  theme_minimal()

graph_line

# ------------------------------------------------------------
# 9.Boxplot
# ------------------------------------------------------------

# This graph directly compares the food inflation gap between the two periods.

graph_boxplot <- ggplot(hicp_final, aes(x = period, y = food_inflation_gap)) +
  geom_boxplot() +
  labs(
    title = "Food Inflation Gap by Period",
    subtitle = "Food inflation minus overall inflation in EU countries",
    x = "Period",
    y = "Food inflation gap, percentage points"
  ) +
  theme_minimal()

graph_boxplot
# ------------------------------------------------------------
# 10. Central finding: Correlation
# ------------------------------------------------------------

# Correlation shows whether food inflation and overall inflation move together across countries and years.

correlation_total <- cor(
  hicp_final$overall_inflation,
  hicp_final$food_inflation,
  use = "complete.obs"
)

correlation_total


# Correlation by period

correlation_by_period <- hicp_final %>%
  group_by(period) %>%
  summarise(
    correlation = cor(overall_inflation, food_inflation, use = "complete.obs"),
    n = n()
  )

correlation_by_period

