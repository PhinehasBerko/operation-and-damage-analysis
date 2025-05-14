# load library

library(readxl)
library(ggplot2)
library(tidyr)
library(scales)
library(dplyr)
library(ggcorrplot)
library(lubridate)
library(gt)

#load data

dataset <- read_excel("data/combined data copy.xlsx")

#display data

View(dataset)

#Clean and restructure

dataset <- dataset[, -1]

dataset <- dataset %>%
  rename(
         human_factor =`Human_Factor`,
         production =`EXPECED HOURLY PRODUCTIVITY OF EQUIPMENT (BCM)`,
         damage_cost =`COST OF DAMAGED COMPONENT`,
         repair_time = `MEAN TIME TO REPAIR (HRS)`,
         downtime =`EQUIPMENT DOWNTIME (HRS)`,
         revenue_loss =`REVENUE (PRODUCTION) LOST ($)`) %>%
  filter(human_factor %in% c("0", "1")) %>%
  mutate(
    human_factor = as.integer(human_factor),
    date = as.Date(DATE),
    year = year(date),
    production = suppressWarnings(as.numeric(production))
  ) %>%
  drop_na(DATE, damage_cost, downtime, revenue_loss)

# Summary of total cumulative metrics
summary_components <- dataset %>%
  summarise(
    `Total Downtime (hrs/days)` = sum(downtime, na.rm = TRUE),
    `Total Revenue Loss ($)` = sum(revenue_loss, na.rm = TRUE),
    `Total Damage Cost ($)` = sum(damage_cost, na.rm = TRUE),
    `Total Mean Time to Repair (hrs/days)` = sum(repair_time, na.rm = TRUE)
  )

summary_components %>%
  gt() %>%
  tab_header(
    title = "Summary of Cumulative Equipment Damage Impact"
  ) %>%
  fmt_number(
    columns = everything(),
    decimals = 2,
    use_seps = TRUE
  )

# Summary of Average cumulative metrics
summary_avg_comp <- dataset %>%
  summarise(
    `Avg Downtime (hrs/days)` = mean(downtime, na.rm = TRUE),
    `Avg Revenue Loss ($)` = mean(revenue_loss, na.rm = TRUE),
    `Avg Damage Cost ($)` = mean(damage_cost, na.rm = TRUE),
    `Avg Mean Time to Repair (hrs/days)` = mean(repair_time, na.rm = TRUE)
  )

summary_avg_comp %>%
  gt() %>%
  tab_header(
    title = "Summary of Average Cumulative Equipment Damage"
  ) %>%
  fmt_number(
    columns = everything(),
    decimals = 2,
    use_seps = TRUE
  )

# =========================================
# 🎯 Objective 1: Human Factor Contribution
# =========================================

## Summary Table
summary_table <- dataset %>%
  group_by(human_factor) %>%
  summarise(
    count = n(),
    avg_cost = mean(damage_cost, na.rm = TRUE),
    avg_downtime = mean(downtime, na.rm = TRUE),
    avg_revenue_loss = mean(revenue_loss, na.rm = TRUE),
    avg_production = mean(production,na.rm = TRUE)
  )

print(summary_table)

summary_table %>%
  gt() %>%
  tab_header(
    title = "Summary of Equipment Damage by Human Factor"
  ) %>%
  fmt_number(columns = 3:6, decimals = 1)

## Bar Plot: Incidents by Human Factor
ggplot(dataset, aes(factor(human_factor))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Count of Incidents by Human Factor",
       x = "Human Factor (1 = Yes, 0 = No)", y = "Incident Count") +
  theme_minimal()

## Box Plot: Cost by Human Factor
ggplot(dataset, aes(x = factor(human_factor), y = damage_cost)) +
  geom_boxplot(fill = c("#E69F00", "#56B4E9")) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Cost of Damage by Human Factor",
       x = "Human Factor", y = "Cost of Damage ($)") +
  theme_minimal()

#Count number of incidents per year
yearly_incidents <- dataset%>%
  group_by(year) %>%
  summarise(incidents = n())

# Plot
ggplot(yearly_incidents, aes(x = factor(year), y = incidents)) +
  geom_col(fill = "#0073C2FF") +
  labs(title = "Number of Damage Incidents per Year",
       x = "Year", y = "Number of Incidents") +
  theme_minimal()

# Summarize total revenue loss per year
revenue_yearly <- dataset %>%
  group_by(year) %>%
  summarise(total_revenue_loss = sum(revenue_loss, na.rm = TRUE))

#Plot ======== DONE -----
ggplot(revenue_yearly, aes(x = factor(year), y = total_revenue_loss)) +
  geom_col(fill = "#D55E00") +
  labs(title = "Total Revenue Loss per Year Due to Equipment Damage",
       x = "Year",
       y = "Total Revenue Loss ($)") +
  theme_minimal()

summary_components <- dataset %>%
  summarise(
    total_downtime = sum(downtime, na.rm = TRUE),
    total_revenue_loss = sum(revenue_loss, na.rm = TRUE),
    total_damage_cost = sum(damage_cost, na.rm = TRUE),
    total_mttr = sum(repair_time, na.rm = TRUE)
  )

print(summary_components)
# =========================================
# 🎯 Objective 2: Damage vs Production
# =========================================

## Correlation Matrix
cor_matrix <- dataset %>%
  select(damage_cost, downtime, revenue_loss) %>%
  cor(use = "complete.obs")
print(cor_matrix)

# Create correlation matrix
cor_matrix <- cor(dataset[, c("damage_cost", "downtime", "revenue_loss")], use = "complete.obs")

# Plot
ggcorrplot(cor_matrix, method = "square", lab = TRUE,
           title = "Correlation Between Damage Metrics")


## Scatter Plot: Downtime vs Revenue Loss
ggplot(dataset, aes(x = downtime, y = revenue_loss)) +
  geom_point(color = "darkred", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Downtime vs. Revenue Loss",
       x = "Downtime (Hrs)", y = "Revenue Lost ($)") +
  theme_minimal()

## Linear Regression Model
model <- lm(revenue_loss ~ downtime + cost_damage, data = dataset)
summary(model)

## Productivity vs Damage
ggplot(dataset, aes(x = production, y = damage_cost)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Expected Productivity vs Damage Cost",
       x = "Productivity (BCM/hr)", y = "Damage Cost ($)") +
  theme_minimal()

#check cumulative cost of damage component
#total equipment downtime
#total revenue lost
#total mean time to repair

# avg value for all the above

# yearly analysis 

# plot a graph for number of incidents on yearly basis.
# a graph of yearly incident count vs revenue lost.
# revenue lost by year
