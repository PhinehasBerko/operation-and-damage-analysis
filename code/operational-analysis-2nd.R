# load library

library(readxl)
library(ggplot2)
library(tidyr)
library(scales)
library(dplyr)
library(ggcorrplot)
library(lubridate)
library(gt)
library(stringr)
library(data.tree)
library(DiagrammeR)
library(rsvg)
library(janitor)

df<- read_excel("data/Revised combined data 2.xlsx",skip = 1)[, -1]


View(df)


#Clean and restructure

df <- df[, -1]
df <- df %>%
  rename(
    human_error =`Human_Errors`,
    production =`EXPECED HOURLY PRODUCTIVITY OF EQUIPMENT (BCM)`,
    damage_cost =`COST OF DAMAGED COMPONENT ($)`,
    repair_time = `MEAN TIME TO REPAIR (HRS)`,
    downtime =`EQUIPMENT DOWNTIME (HRS)`,
    revenue_loss =`REVENUE (PRODUCTION) LOST ($)`,
    incident_description = `INCIDENT DESCRIPTION`) %>%
  filter( human_error %in% c("0", "1")) %>%
  mutate(
    
    human_error = as.integer( human_error),
  # date = as.Date(DATE),
  # year = year(date),
    production = suppressWarnings(as.numeric(production)),
    extracted_time = str_extract(incident_description, "\\b\\d{1,2}:\\d{2}\\b"),
    time_parsed = lubridate::hm(extracted_time),
    hour = hour(time_parsed),
    hour_label = format(strptime(hour, format="%H"), format="%I:00 %p"),
    shift = case_when(
      !is.na(hour) & hour >= 6 & hour < 18 ~ "Day Shift",
      !is.na(hour) ~ "Night Shift",
      TRUE ~ NA_character_
    )
  ) %>%
  drop_na(DATE, damage_cost, downtime, revenue_loss)


# Summary of total cumulative metrics
summary_components <- df %>%
  summarise(
    `Total Downtime (hrs/days)` = sum(downtime, na.rm = TRUE),
    `Total Revenue Loss ($)` = sum(revenue_loss, na.rm = TRUE),
    `Total Damage Cost ($)` = sum(damage_cost, na.rm = TRUE),
    `Total Mean Time to Repair (hrs/days)` = sum(repair_time, na.rm = TRUE)
  )

print(summary_components)

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
summary_avg_comp <- df %>%
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

#Count number of incidents per year
#yearly_incidents <- df%>%
#  group_by(year) %>%
#  summarise(incidents = n())

# Plot
#ggplot(yearly_incidents, aes(x = factor(year), y = incidents)) +
#  geom_col(fill = "#0073C2FF") +
#  labs(title = "Number of Damage Incidents per Year",
#       x = "Year", y = "Number of Incidents") +
#  theme_minimal()


# Summarize total revenue loss per year
#revenue_yearly <- df %>%
# group_by(year) %>%
#  summarise(total_revenue_loss = sum(revenue_loss, na.rm = TRUE))
#
##Plot 
#ggplot(revenue_yearly, aes(x = factor(year), y = total_revenue_loss)) +
#  geom_col(fill = "#D55E00") +
#  labs(title = "Total Revenue Loss per Year Due to Equipment Damage",
#       x = "Year",
#       y = "Total Revenue Loss ($)") +
#  theme_minimal()

# =========================================
# 🎯 Objective 1: Human Factor Contribution
# =========================================

## Bar Plot: Incidents by Human Error
ggplot(df, aes(factor(human_error))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Count of Incidents by Human Error",
       x = "Human Error (1 = Yes, 0 = No)", y = "Incident Count") +
  theme_minimal()


## Summary Table for key parameters by human error
summary_table <- df %>%
  group_by(human_error) %>%
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
    title = "Summary of Equipment Damage by Human Error"
  ) %>%
  fmt_number(columns = 3:6, decimals = 1)

## Box Plot: Cost of Damage by Human Error
ggplot(df, aes(x = factor(human_error), y = damage_cost)) +
  geom_boxplot(fill = c("#E69F00", "#56B4E9")) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Cost of Damage by Human Error",
       x = "Human Error", y = "Cost of Damage ($)") +
  theme_minimal()

## Box Plot: Revenue loss by Human Error
ggplot(df, aes(x = factor(human_error), y = revenue_loss)) +
  geom_boxplot(fill = c("#E69F00", "#56B4E9")) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Revenue loss by Human Error",
       x = "Human Error", y = "Revenue loss ($)") +
  theme_minimal()


# Test Analysis using T-test for human impact on revenue loss and production
head(df)
str(df$human_error)
copy_data <- df
View(copy_data)

copy_data$human_error <- as.factor(copy_data$human_error)

?t.test

# T-test: human impact on revenue loss
# check if there is a statically significance difference between
# incidents with or without human factor

# if p-value < 0.05 we will reject our null hypothesis
# if p-value > 0.05 then we will fail to reject our null hypothesis
t_test_result <- t.test(revenue_loss ~ human_error, data = copy_data)
print(t_test_result)



#The difference is not statistically significant at the 5% level (p = 0.3476). 
# This means we do not have strong evidence to conclude that human involvement 
# significantly increases revenue loss based on this df.


# T-test: human impact on production
t_test_result2 <- t.test(production ~ human_error, data = copy_data)
print(t_test_result2)

#We do not have enough evidence to conclude that incidents involving human error
# are associated with a statistically significant change in production output.

# Since T-test shows no statistical significant despite higher mean difference,
copy_data %>% 
  group_by(human_error) %>% 
  summarise(
    mean_production = mean(production, na.rm = TRUE),
    sd_production = sd(production, na.rm = TRUE),
    count = n()
  )

ggplot(copy_data, aes(x = human_error, y = production)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Production by Human Error", x = "Human Error", y = "Production")

# Statistical Test for Normality

# For production
ggplot(copy_data, aes(x = production, fill = human_error)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 20) +
  facet_wrap(~ human_error) +
  theme_minimal()


shapiro.test(copy_data$production[copy_data$human_error == "0"])
shapiro.test(copy_data$production[copy_data$human_error == "1"])


# For revenue_loss
ggplot(copy_data, aes(x = revenue_loss, fill = human_error)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 20) +
  facet_wrap(~ human_error) +
  theme_minimal()

shapiro.test(copy_data$revenue_loss[copy_data$human_error == "0"])
shapiro.test(copy_data$revenue_loss[copy_data$human_error == "1"])

# Hypothesis
#The null hypothesis of the Shapiro-Wilk test is that the data are 
#normally distributed.
# Since p-value for both groups are < 0.05 we reject the null hypothesis
# Hence both groups are not normally distributed


# Wilcoxon rank-sum test for production
wilcox.test(production ~ human_error, data = copy_data)

wilcox.test(revenue_loss ~ human_error, data = copy_data)

#The p-value of 0.01248 is less than 0.05, which means:
#There is a statistically significant difference in revenue loss between 
#incidents with and without human factor.
# Hence this evidence supports the idea that human factors contribute
# meaningfully to increased financial loss

wilcox.test(damage_cost ~ human_error, data = df)

# Since variance | Log Transformation
copy_data$log_production <- log(copy_data$production + 1)
t.test(log_production ~ human_error, data = copy_data)

# Apply Log Transformation 
copy_data$log_revenue_loss <- log(copy_data$revenue_loss + 1) 
t.test(log_revenue_loss ~ human_error, data = copy_data)
# Add 1 to avoid log(0)


shapiro.test(copy_data$log_revenue_loss[copy_data$human_error == "0"])
shapiro.test(copy_data$log_revenue_loss[copy_data$human_error == "1"])

# Apply Log Transformation 
copy_data$log_damage_cost <- log(copy_data$damage_cost + 1) 
t.test(log_damage_cost ~ human_error, data = copy_data)
# Add 1 to avoid log(0)


shapiro.test(copy_data$log_damage_cost[copy_data$human_error == "0"])
shapiro.test(copy_data$log_damage_cost[copy_data$human_error == "1"])



copy_data %>% 
  filter(production > quantile(production, 0.95, na.rm = TRUE)) %>%
  arrange(desc(production))

# =========================================
# 🎯 Objective 2: Damage vs Production
# =========================================

## Correlation Matrix
cor_matrix <- df %>%
  select(damage_cost, downtime, revenue_loss) %>%
  cor(use = "complete.obs")
print(cor_matrix)

# Create correlation matrix
cor_matrix <- cor(df[, c("damage_cost", "downtime", "revenue_loss","repair_time")], use = "complete.obs")

# Plot
ggcorrplot(cor_matrix, method = "square", lab = TRUE,
           title = "Correlation Between Key Metrics",
)
