library(FSA)
kw_data <- read_excel("data/Revised combined data 2.xlsx",skip = 1)[,-1]

View(kw_data)
# Rename some columns
names(revised_data)[which(names(revised_data) == "Human_Errors")] <- "Human Errors"
names(revised_data)[which(names(revised_data) == "ACTIVE FAILURES")] <- "Active Failures"

# STEP 2: View the column names to confirm their positions
names(kw_data)

# STEP 3: Rename columns for easier reference (adjust if your names differ)
colnames(kw_data)[which(names(kw_data) == "CLASSIFICATION BASED ON  SEVERITY OF INCIDENT")] <- "Severity"
colnames(kw_data)[which(names(kw_data) == "ACTIVE FAILURES")] <- "ActiveFailure"
colnames(kw_data)[which(names(kw_data) == "LATENT CONDITIONS")] <- "LatentCondition"
colnames(kw_data)[which(names(kw_data) == "Organizational Factors...10")] <- "OrganizationalFactor"
colnames(kw_data)[which(names(kw_data) == "Human_Errors")] <- "HumanErrors"

# STEP 4: Map severity levels to numeric scores
severity_map <- c("MINOR" = 1, "MAJOR" = 2, "CATASTROPHIC" = 3)

# ACTIVE FAILURES DATA
active_df <- kw_data %>%
  select(ActiveFailure, Severity) %>%
  filter(!is.na(ActiveFailure), !is.na(Severity)) %>%
  mutate(Severity_Score = severity_map[Severity])

# LATENT CONDITIONS DATA
latent_df <- kw_data %>%
  select(LatentCondition, Severity) %>%
  filter(!is.na(LatentCondition), !is.na(Severity)) %>%
  mutate(Severity_Score = severity_map[Severity])

# STEP 5: Kruskal-Wallis Test
kruskal_active <- kruskal.test(Severity_Score ~ ActiveFailure, data = active_df)
print(kruskal_active)

kruskal_latent <- kruskal.test(Severity_Score ~ LatentCondition, data = latent_df)
print(kruskal_latent)

# STEP 6: Optional Post-hoc test (if Kruskal-Wallis is significant)
dunnTest(Severity_Score ~ ActiveFailure, data = active_df, method = "bonferroni")
dunnTest(Severity_Score ~ LatentCondition, data = latent_df, method = "bonferroni")
