#packages
library(tidyverse)
library(dplyr)

#dataset load
hicp <- read.csv("prc_hicp_aind__custom_21166576_linear.csv")

#count of rows and variables(columns)
dim(hicp)

#column names
names(hicp)

#structure of dataset
str(hicp)

#key variables
unique(hicp$coicop)
unique(hicp$geo)
unique(hicp$TIME_PERIOD)
unique(hicp$unit)

#number of observations(hicp/food)
table(hicp$coicop)

#to ensure that there are actually 27 countries in the dataset
length(unique(hicp$geo))

#renaming variables for easier work
hicp_clean <- hicp %>%
  rename(
    country = geo,
    year = TIME_PERIOD,
    inflation = OBS_VALUE,
    category = coicop
  )

#filter the data we actually need for our analysis
hicp_clean <- hicp_clean %>%
  select(country, year, category, inflation, unit)

#check for NA
colSums(is.na(hicp_clean))

#divide the data to 2 time periods
hicp_clean <- hicp_clean %>%
  mutate(
    period = case_when(
      year >= 2016 & year <= 2020 ~ "2016-2020",
      year >= 2021 & year <= 2025 ~ "2021-2025",
      TRUE ~ NA_character_
    )
  )

#category as two separete columns
hicp_final <- hicp_clean %>%
  pivot_wider(
    names_from = category,
    values_from = inflation
  )

#renaming the inflation variables
hicp_final <- hicp_final %>%
  rename(
    overall_inflation = `All-items HICP`,
    food_inflation = Food
  )

#creating two new variables to compare inflation
hicp_final <- hicp_final %>%
  mutate(
    food_inflation_gap = food_inflation-overall_inflation,
    food_overall_ratio = food_inflation/overall_inflation
  )

#check for outlying data
hicp_final %>%
  slice_max(food_inflation, n = 10) %>%
  select(country, year, period, overall_inflation, food_inflation, food_inflation_gap)

hicp_final %>%
  slice_max(overall_inflation, n = 10) %>%
  select(country, year, period, overall_inflation, food_inflation, food_inflation_gap)

hicp_final %>%
  slice_min(food_inflation, n = 10) %>%
  select(country, year, period, overall_inflation, food_inflation, food_inflation_gap)

hicp_final %>%
  slice_min(overall_inflation, n = 10) %>%
  select(country, year, period, overall_inflation, food_inflation, food_inflation_gap)

#checking the problematic values for the ratio, as when the overall inflation is equal to 0, the ratio is infinite, we will not use them
hicp_final %>% 
  filter(overall_inflation == 0) %>% 
  print(n = Inf)

hicp_no_NA <- hicp_final %>%
  mutate(food_overall_ratio = ifelse(is.finite(food_overall_ratio), food_overall_ratio, NA))

mainstats <- hicp_no_NA %>%
  group_by(period) %>%
  summarise(
    n = n(),
    across(
      c(overall_inflation, food_inflation, food_inflation_gap, food_overall_ratio),
      list(mean = mean, median = median, sd = sd, min = min, max = max),
      na.rm = TRUE,
      .names = "{.fn}_{.col}"
    )
  )
print(mainstats, width = Inf)

#main comparison table
hicp_final %>%
  group_by(period) %>%
  summarise(
    across(
      c(overall_inflation, food_inflation, food_inflation_gap), 
      ~ mean(.x, na.rm = TRUE), 
      .names = "avg_{.col}"
    ),
    avg_food_overall_ratio = mean(food_overall_ratio[is.finite(food_overall_ratio)], na.rm = TRUE),
    n = n()
  )

#counting the ratio between the overall inflation and food inflation
hicp_final %>%
  group_by(period) %>%
  summarise(
    avg_overall_inflation = mean(overall_inflation, na.rm = TRUE),
    avg_food_inflation = mean(food_inflation, na.rm = TRUE),
    avg_food_inflation_gap = mean(food_inflation_gap, na.rm = TRUE),
    food_to_overall_ratio = avg_food_inflation / avg_overall_inflation,
    n = n()
  )
  
#histogram - food inflation gap by time periods
ggplot(hicp_final, aes(x = food_inflation_gap)) +
  geom_histogram(bins = 25, color = "white") +
  facet_wrap(~ period) +
  labs(
    title = "Distribution of Food Inflation Gap in EU Countries",
    subtitle = "Food inflation minus overall inflation, 2016–2020 vs 2021–2025",
    x = "Food inflation gap, percentage points",
    y = "Number of country-year observations"
  ) +
  theme_minimal()

#scatterplot - relationship between overall inflation and food inflation
ggplot(
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

#graph - average inflation (overall/food) by year across the EU countries
avg_by_year <- hicp_final %>%
  group_by(year) %>%
  summarise(
    average_overall_inflation = mean(overall_inflation, na.rm = TRUE),
    average_food_inflation = mean(food_inflation, na.rm = TRUE)
  )

avg_by_year_for_easier_work <- avg_by_year %>%
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

ggplot(
  avg_by_year_for_easier_work,
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

#boxplot of inflation gap by period
ggplot(hicp_final, aes(x = period, y = food_inflation_gap)) +
  geom_boxplot() +
  labs(
    title = "Food Inflation Gap by Period",
    subtitle = "Food inflation minus overall inflation in EU countries",
    x = "Period",
    y = "Food inflation gap, percentage points"
  ) +
  theme_minimal()

#correlation of food & overall inflations
cor(
  hicp_final$overall_inflation,
  hicp_final$food_inflation,
  use = "complete.obs"
)

#correlation by period
hicp_final %>%
  group_by(period) %>%
  summarise(
    correlation = cor(overall_inflation, food_inflation, use = "complete.obs"),
    n = n()
  )