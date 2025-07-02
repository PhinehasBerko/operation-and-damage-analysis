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

#Plot 
ggplot(revenue_yearly, aes(x = factor(year), y = total_revenue_loss)) +
  geom_col(fill = "#D55E00") +
  labs(title = "Total Revenue Loss per Year Due to Equipment Damage",
       x = "Year",
       y = "Total Revenue Loss ($)") +
  theme_minimal()

# =========================================
# 🎯 Objective 1: Human Factor Contribution
# =========================================

## Bar Plot: Incidents by Human Factor
ggplot(dataset, aes(factor(human_factor))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Count of Incidents by Human Factor",
       x = "Human Factor (1 = Yes, 0 = No)", y = "Incident Count") +
  theme_minimal()

## Summary Table for key parameters by human factor
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

## Box Plot: Cost of Damage by Human Factor
ggplot(dataset, aes(x = factor(human_factor), y = damage_cost)) +
  geom_boxplot(fill = c("#E69F00", "#56B4E9")) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Cost of Damage by Human Factor",
       x = "Human Factor", y = "Cost of Damage ($)") +
  theme_minimal()

## Box Plot: Revenue loss by Human Factor
ggplot(dataset, aes(x = factor(human_factor), y = revenue_loss)) +
  geom_boxplot(fill = c("#E69F00", "#56B4E9")) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Revenue loss by Human Factor",
       x = "Human Factor", y = "Revenue loss ($)") +
  theme_minimal()


# Test Analysis using T-test for human impact on revenue loss and production
head(dataset)
str(dataset$human_factor)
copy_data <- dataset
View(copy_data)

copy_data$human_factor <- as.factor(copy_data$human_factor)

?t.test

# T-test: human impact on revenue loss
# check if there is a statically significance difference between
# incidents with or without human factor

# if p-value < 0.05 we will reject our null hypothesis
# if p-value > 0.05 then we will fail to reject our null hypothesis
t_test_result <- t.test(revenue_loss ~ human_factor, data = copy_data)
print(t_test_result)



#The difference is not statistically significant at the 5% level (p = 0.3476). 
# This means we do not have strong evidence to conclude that human involvement 
# significantly increases revenue loss based on this dataset.


# T-test: human impact on production
t_test_result2 <- t.test(production ~ human_factor, data = copy_data)
print(t_test_result2)

#We do not have enough evidence to conclude that incidents involving human factors
# are associated with a statistically significant change in production output.

# Since T-test shows no statistical significant despite higher mean difference,
copy_data %>% 
  group_by(human_factor) %>% 
  summarise(
    mean_production = mean(production, na.rm = TRUE),
    sd_production = sd(production, na.rm = TRUE),
    count = n()
  )

ggplot(copy_data, aes(x = human_factor, y = production)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Production by Human Factor", x = "Human Factor", y = "Production")

# Statistical Test for Normality

# For production
ggplot(copy_data, aes(x = production, fill = human_factor)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 20) +
  facet_wrap(~ human_factor) +
  theme_minimal()


shapiro.test(copy_data$production[copy_data$human_factor == "0"])
shapiro.test(copy_data$production[copy_data$human_factor == "1"])


# For revenue_loss
ggplot(copy_data, aes(x = revenue_loss, fill = human_factor)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 20) +
  facet_wrap(~ human_factor) +
  theme_minimal()

shapiro.test(copy_data$revenue_loss[copy_data$human_factor == "0"])
shapiro.test(copy_data$revenue_loss[copy_data$human_factor == "1"])

# Hypothesis
#The null hypothesis of the Shapiro-Wilk test is that the data are 
#normally distributed.
# Since p-value for both groups are < 0.05 we reject the null hypothesis
# Hence both groups are not normally distributed


# Wilcoxon rank-sum test for production
wilcox.test(production ~ human_factor, data = copy_data)

wilcox.test(revenue_loss ~ human_factor, data = copy_data)

#The p-value of 0.01248 is less than 0.05, which means:
#There is a statistically significant difference in revenue loss between 
#incidents with and without human factor.
# Hence this evidence supports the idea that human factors contribute
# meaningfully to increased financial loss

wilcox.test(damage_cost ~ human_factor, data = copy_data)

# Since variance | Log Transformation
copy_data$log_production <- log(copy_data$production + 1)
t.test(log_production ~ human_factor, data = copy_data)

# Apply Log Transformation 
copy_data$log_revenue_loss <- log(copy_data$revenue_loss + 1) 
t.test(log_revenue_loss ~ human_factor, data = copy_data)
# Add 1 to avoid log(0)


shapiro.test(copy_data$log_revenue_loss[copy_data$human_factor == "0"])
shapiro.test(copy_data$log_revenue_loss[copy_data$human_factor == "1"])

# Apply Log Transformation 
copy_data$log_damage_cost <- log(copy_data$damage_cost + 1) 
t.test(log_damage_cost ~ human_factor, data = copy_data)
# Add 1 to avoid log(0)


shapiro.test(copy_data$log_damage_cost[copy_data$human_factor == "0"])
shapiro.test(copy_data$log_damage_cost[copy_data$human_factor == "1"])



copy_data %>% 
  filter(production > quantile(production, 0.95, na.rm = TRUE)) %>%
  arrange(desc(production))

# =========================================
# 🎯 Objective 2: Damage vs Production
# =========================================

## Correlation Matrix
cor_matrix <- dataset %>%
  select(damage_cost, downtime, revenue_loss) %>%
  cor(use = "complete.obs")
print(cor_matrix)

# Create correlation matrix
cor_matrix <- cor(dataset[, c("damage_cost", "downtime", "revenue_loss","repair_time")],method = "spearman", use = "complete.obs")

# Plot
ggcorrplot(cor_matrix, method = "square", lab = TRUE,
           title = "Correlation Between Key Metrics",
           )
?ggcorrplot

## Scatter Plot: Downtime vs Revenue Loss
ggplot(dataset, aes(x = downtime, y = revenue_loss)) +
  geom_point(color = "darkred", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Downtime vs. Revenue Loss",
       x = "Downtime (Hrs)", y = "Revenue Lost ($)") +
  theme_minimal()

## Linear Regression Model
model <- lm(revenue_loss ~ downtime + damage_cost, data = dataset)
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



# Revised data 
revised_data <- read_excel("data/Revised combined data.xlsx",skip = 1)

#display data

View(revised_data)

#Clean and restructure

revised_data <- revised_data[, -1]

View(revised_data)
revised_data <- revised_data %>%
  rename(
    human_error =`Human_Errors`,
    production =`EXPECED HOURLY PRODUCTIVITY OF EQUIPMENT (BCM)`,
    damage_cost =`COST OF DAMAGED COMPONENT`,
    repair_time = `MEAN TIME TO REPAIR (HRS)`,
    downtime =`EQUIPMENT DOWNTIME (HRS)`,
    revenue_loss =`REVENUE (PRODUCTION) LOST ($)`,
    incident_description = `INCIDENT DESCRIPTION`) %>%
  filter( human_error %in% c("0", "1")) %>%
  mutate(
    
    human_error = as.integer( human_error),
    date = as.Date(DATE),
    year = year(date),
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

# Count incidents per hour and shift
plot_data <- revised_data %>%
  filter(!is.na(hour_label), !is.na(shift)) %>%
  count(hour_label, shift) %>%
  mutate(hour_label = factor(hour_label, levels = format(strptime(0:23, format="%H"), "%I:00 %p")))
plot_data
# Plot
ggplot(plot_data, aes(x = hour_label, y = n, fill = shift)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_text(aes(label = n), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Day Shift" = "#9ACD32", "Night Shift" = "#3CB371")) +
  labs(
    title = "Classification Based on Time of Incident",
    x = "Time",
    y = "Number of Incident",
    fill = "Shift"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# Summary of total cumulative metrics
summary_components <- revised_data %>%
  group_by(human_error) %>%
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
summary_avg_comp <- revised_data %>%
  group_by(human_error) %>%
  summarise(
    `Avg Downtime (hrs/days)` = mean(downtime, na.rm = TRUE),
    `Avg Revenue Loss ($)` = mean(revenue_loss, na.rm = TRUE),
    `Avg Damage Cost ($)` = mean(damage_cost, na.rm = TRUE),
    `Avg Mean Time to Repair (hrs/days)` = mean(repair_time, na.rm = TRUE)
  )

summary_avg_comp %>%
  gt() %>%
  tab_header(
    title = "Summary of Average cumulative metrics"
  ) %>%
  fmt_number(
    columns = everything(),
    decimals = 2,
    use_seps = TRUE
  )
# Second Test
copy_data <- revised_data
copy_data$human_error <- as.factor(copy_data$human_error)

copy_data %>% 
  group_by(human_error) %>% 
  summarise(
    mean_production = mean(revenue_loss, na.rm = TRUE),
    sd_production = sd(revenue_loss, na.rm = TRUE),
    count = n(),
  )
View(copy_data)
# Statistical Test for Normality

# For Revenue Loss
ggplot(copy_data, aes(x = revenue_loss, fill = human_error)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 20) +
  facet_wrap(~ human_error) +
  theme_minimal()


shapiro.test(copy_data$revenue_loss[copy_data$human_error == "0"])
shapiro.test(copy_data$revenue_loss[copy_data$human_error == "1"])

# Non-parametric Test (Wilcoxon Test)
wilcox.test(revenue_loss ~ human_error, data = copy_data)

t.test(damage_cost ~ human_error, data = copy_data)

copy_data$log_revenue_loss <- log(copy_data$revenue_loss + 1) 
t.test(log_revenue_loss ~ human_error, data = copy_data)

# =================================================================
# Clean and count Active Failures (assumed in column 23)
active_counts <- revised_data %>%
  filter(!is.na(`ACTIVE FAILURES`)) %>%
  count(`ACTIVE FAILURES`) %>%
  rename(Active_Failures = `ACTIVE FAILURES`, Count = n)

# Clean and count Latent Conditions (assumed in column 25)
latent_counts <- revised_data %>%
  filter(!is.na(`LATENT CONDITIONS`)) %>%
  count(`LATENT CONDITIONS`) %>%
  rename(Latent_Conditions = `LATENT CONDITIONS`, Count = n)

# Plot Active Failures
ggplot(active_counts, aes(x = reorder(Active_Failures, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Distribution of Incident Types Within Active Failures", x = "Active Failure Type", y = "Count of Reported Incidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5,size = 12)
  )

# Plot Latent Conditions
ggplot(latent_counts, aes(x = reorder(Latent_Conditions, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(title = "Distribution of Incident Types Within  Latent Conditions", x = "Category of Failure", y = "Number of Incidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 11)
  )

count <- n(revised_data$`LATENT CONDITIONS`=="Technological Environment")
# ------------------------------------
# 🔢 Chi-Square Test: Active Failures
# ------------------------------------
chisq_active <- chisq.test(active_counts$Count)
chisq.test(active_counts$Count, simulate.p.value = TRUE, B = 10000)

cat("===== ACTIVE FAILURES CHI-SQUARE TEST =====\n")
print(chisq_active)
cat("\nObserved Counts:\n")
print(chisq_active$observed)
cat("\nExpected Counts:\n")
print(round(chisq_active$expected, 2))

# ------------------------------------
# 🔢 Chi-Square Test: Latent Conditions
# ------------------------------------
chisq_latent <- chisq.test(latent_counts$Count)

cat("\n\n===== LATENT CONDITIONS CHI-SQUARE TEST =====\n")
print(chisq_latent)
cat("\nObserved Counts:\n")
print(chisq_latent$observed)
cat("\nExpected Counts:\n")
print(round(chisq_latent$expected, 2))

View(revised_data)
View(data)

# Rename some columns
names(revised_data)[which(names(revised_data) == "Human_Errors")] <- "Human Errors"
names(revised_data)[which(names(revised_data) == "ACTIVE FAILURES")] <- "Active Failures"

# Build levels explicitly
data <- revised_data %>%
  mutate(
    BranchType = case_when(
      `Human Errors` == 1 ~ "Active Failures",
      `Human Errors` == 0 ~ "LATENT CONDITIONS",
      TRUE ~ "UNKNOWN"
    ),
    Level2 = case_when(
      `Human Errors` == 1 ~ `Active Failures`,
      `Human Errors` == 0 ~ `LATENT CONDITIONS`,
      TRUE ~ NA_character_
    ),
    Level3 = case_when(
      `Human Errors` == 1 ~ specific_active,
      `Human Errors` == 0 ~ specific_latent,
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Level2) & !is.na(Level3))

# Function to build and style a graph
build_graph <- function(branch_label, n = 5) {
  filtered <- data %>%
    filter(BranchType == branch_label) %>%
    distinct(Level2, Level3) %>%
    group_by(Level2) %>%
    slice_head(n = n) %>%
    ungroup() %>%
    mutate(pathString = paste("Human Errors", branch_label, Level2, Level3, sep = "/"))
  
  tree <- as.Node(filtered, pathName = "pathString")
  graph <- ToDiagrammeRGraph(tree)
  
  # Add left-to-right layout and bold black text
  graph <- add_global_graph_attrs(graph, "rankdir", "LR", "graph")
  graph <- add_global_graph_attrs(graph, "fontsize", "12", "node")
  graph <- add_global_graph_attrs(graph, "fontname", "Arial Bold", "node")
  graph <- add_global_graph_attrs(graph, "fontcolor", "black", "node")  # <-- Black text
  graph <- add_global_graph_attrs(graph, "shape", "box", "node")
  graph <- add_global_graph_attrs(graph, "style", "filled", "node")
  #graph <- add_global_graph_attrs(graph, "fillcolor", ifelse(branch_label == "ACTIVE FAILURES", "#FFDDC1", "#D1E8FF"), "node")
  
  return(graph)
}

# Build both graphs
active_graph <- build_graph("Active Failures", n = 2)
latent_graph <- build_graph("LATENT CONDITIONS", n = 2)

# Render graphs
render_graph(active_graph)
render_graph(latent_graph)

export_graph(active_graph, file_name = "active_failures_tree.png", file_type = "png", width = 1200, height = 800)

library(grid)
# Render graph
grid.newpage()
grid.text("Active Failures Tree", gp = gpar(fontface = "bold", cex = 2.5), y = 0.95)
render_graph(active_graph)

latent_data <- read_excel("data/Revised combined data - Copy.xlsx", skip = 1) %>%
  clean_names()

names(latent_data)[which(names(latent_data) == "organizational_factors_10")] <- "Organizational Factors"
names(latent_data)[which(names(latent_data) == "latent_conditions")] <- "Latent Conditions"

latent_data <- latent_data[, -1]
View(latent_data)

# Build levels explicitly
data <- latent_data %>%
  mutate(
    BranchType = case_when(
      `Organizational Factors` == 1 ~ "ACTIVE FAILURES",
      `Organizational Factors` == 0 ~ "Latent Conditions",
      TRUE ~ "UNKNOWN"
    ),
    Level2 = case_when(
      `Organizational Factors` == 0 ~ `Latent Conditions`,
      TRUE ~ NA_character_
    ),
    Level3 = case_when(
      `Organizational Factors` == 1 ~ specific_active,
      `Organizational Factors` == 0 ~ specific_latent,
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Level2) & !is.na(Level3))

# Function to build and style a graph
build_graph <- function(branch_label, n = 5) {
  filtered <- data %>%
    filter(BranchType == branch_label) %>%
    distinct(Level2, Level3) %>%
    group_by(Level2) %>%
    slice_head(n = n) %>%
    ungroup() %>%
    mutate(pathString = paste("Organizational Factors", branch_label, Level2, Level3, sep = "/"))
  
  tree <- as.Node(filtered, pathName = "pathString")
  graph <- ToDiagrammeRGraph(tree)
  
  # Add left-to-right layout and bold black text
  graph <- add_global_graph_attrs(graph, "rankdir", "LR", "graph")
  graph <- add_global_graph_attrs(graph, "fontsize", "14", "node")
  graph <- add_global_graph_attrs(graph, "fontname", "Arial Bold", "node")
  graph <- add_global_graph_attrs(graph, "fontcolor", "black", "node")  # <-- Black text
  graph <- add_global_graph_attrs(graph, "shape", "box", "node")
  graph <- add_global_graph_attrs(graph, "style", "filled", "node")
  #graph <- add_global_graph_attrs(graph, "fillcolor", ifelse(branch_label == "ACTIVE FAILURES", "#FFDDC1", "#D1E8FF"), "node")
  
  return(graph)
}

# Build both graphs
active_graph <- build_graph("ACTIVE FAILURES", n = 2)
latent_graph <- build_graph("Latent Conditions", n = 3)

# Render graphs
render_graph(active_graph)
render_graph(latent_graph)

kexport_graph(active_graph, file_name = "active_failures_tree.png", file_type = "png", width = 1200, height = 800)

library(grid)
# Render graph
grid.newpage()
grid.text("Active Failures Tree", gp = gpar(fontface = "bold", cex = 2.5), y = 0.95)
render_graph(active_graph)