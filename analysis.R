# -----------------------------------------------------------------------------
# Filename:     ellahi_ibraheemsaqib_code.R
# Author:       Ibraheem Saqib
# Date:         12-29-2025
# 
# Description:  This script analyzes the 2016 Investigatory Stop Reports (ISR) 
#               data from the Chicago Police Department. 
#
# Inputs:       2016-ISR.csv (Main dataset)
#               ISR-Data-Dictionary.csv (Metadata)
#
# Output:       Statistical summaries and visualizations regarding police stops.
#
# Runtime ~ 8.2s
# -----------------------------------------------------------------------------

# Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, janitor, fixest, car, stargazer)

# Output Directory
if (!dir.exists("output")) {
  dir.create("output")
}

# -----------------------------------------------------------------------------
# 0. Data Preparation; Runtime ~ 5.9s
# -----------------------------------------------------------------------------

# Start Timer 0
t0_start <- Sys.time()

# Import data
isr_df <- read_csv("2016-ISR.csv")
data_dictionary <- read_csv("ISR-Data-Dictionary.csv")

glimpse(isr_df)

# Keep if "Investigatory Stop" and if officer has at least 25 stops
isr_filtered <- isr_df %>%
  filter(CONTACT_TYPE_DESCR == "Investigatory Stop") %>%
  group_by(FO_EMPLOYEE_ID) %>%
  filter(n() >= 25) %>%
  ungroup()

print(paste("Original rows:", nrow(isr_df)))
print(paste("Filtered rows:", nrow(isr_filtered)))

# Add indicator variables for search and contraband
isr_filtered <- isr_filtered %>%
  mutate(
    search_conducted = if_else(SEARCH_I == "Y", 1, 0, missing = 0),
    contraband_found = if_else(
      (WEAPON_OR_CONTRABAND_FOUND_I %in% "Y") | (SEARCH_CONTRABAND_FOUND_I %in% "Y"),
      1,
      0
    )
  )

table(isr_filtered$search_conducted, useNA = "ifany")
table(isr_filtered$contraband_found, useNA = "ifany")

# Fixed effects
isr_filtered <- isr_filtered %>%
  mutate(
    contact_datetime = dmy_hm(CONTACT_DATE),
    
    #helper variables
    
    stop_year = year(contact_datetime),
    stop_month = month(contact_datetime),

    is_weekend = if_else(wday(contact_datetime) %in% c(1, 7), 1, 0),
    
    quarter_of_day = case_when(
      CONTACT_HOUR >= 0 & CONTACT_HOUR < 6  ~ 1,
      CONTACT_HOUR >= 6 & CONTACT_HOUR < 12 ~ 2,
      CONTACT_HOUR >= 12 & CONTACT_HOUR < 18 ~ 3,
      TRUE                                  ~ 4
    ),
    
    # fixed effects
    fe_district_year_month = paste(DISTRICT, stop_year, stop_month, sep = "_"),
    fe_beat_weekend_quarter = paste(BEAT, is_weekend, quarter_of_day, sep = "_")
  )

head(select(isr_filtered, DISTRICT, contact_datetime, fe_district_year_month))
head(select(isr_filtered, BEAT, is_weekend, quarter_of_day, fe_beat_weekend_quarter))

# End Timer 0
t0_end <- Sys.time()

# -----------------------------------------------------------------------------
# Part 1. Racial disparities in search rates; Runtime ~ 0.32s
# -----------------------------------------------------------------------------

# Start Timer 1
t1_start <- Sys.time()

# 1. Race Variable
# -----------------------------------------------------------------------------
isr_filtered <- isr_filtered %>%
  mutate(
    subject_race = case_when(
      RACE_CODE_CD == "WHT" ~ "White",
      RACE_CODE_CD == "BLK" ~ "Black",
      # Combining White Hispanic (WWH), Black Hispanic (WBH), and general Hispanic (WHI)
      RACE_CODE_CD %in% c("WWH", "WHI", "WBH") ~ "Hispanic",
      # All other codes (API, I, P, U) and NAs go to "Other"
      TRUE ~ "Other"
    )
  )

# Create a summary table of the counts and percentages
race_distribution <- isr_filtered %>%
  count(subject_race) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  arrange(desc(n))

print("Distribution of Subject Race:")
print(race_distribution)

# 2. Differrences in search rates by subject race
# -----------------------------------------------------------------------------

# Set "White" as the reference level for the race variable
isr_filtered <- isr_filtered %>%
  mutate(subject_race = fct_relevel(subject_race, "White"))

# Estimate the Linear Probability Model (LPM)
# Outcome: search_conducted
# Predictor: subject_race
# Fixed Effects: District-Year-Month + Beat-Weekend-Quarter
# Standard Errors: Clustered by Officer (FO_EMPLOYEE_ID)
model_search_race <- feols(
  search_conducted ~ subject_race | fe_district_year_month + fe_beat_weekend_quarter,
  data = isr_filtered,
  cluster = ~FO_EMPLOYEE_ID
)

print("Regression Results: Search Rates by Subject Race")
etable(model_search_race)

# 3. Hypothesis Testing
# -----------------------------------------------------------------------------
# Test 1: White vs Black (Testing if Black coeff is 0)
test_wb <- linearHypothesis(model_search_race, "subject_raceBlack = 0")
print(test_wb)

# Test 2: Black vs Hispanic (Testing if Black coeff == Hispanic coeff)
test_bh <- linearHypothesis(model_search_race, "subject_raceBlack = subject_raceHispanic")
print(test_bh)


# 4. Regression Table
# -----------------------------------------------------------------------------

isr_reg_data <- isr_filtered %>%
  mutate(
    # convert to num, impute missing with average
    age_numeric = as.numeric(AGE),
    age_missing_flag = if_else(is.na(age_numeric), 1, 0),
    subject_age = if_else(is.na(age_numeric), mean(age_numeric, na.rm = TRUE), age_numeric),
    
    # convert NA to missing
    subject_sex = if_else(is.na(SEX_CODE_CD), "Missing", SEX_CODE_CD),
    subject_sex = as.factor(subject_sex)
  )

# Model 2 ensure consistent N
m2 <- feols(search_conducted ~ subject_race | fe_district_year_month + fe_beat_weekend_quarter, 
            cluster = ~FO_EMPLOYEE_ID, 
            data = isr_reg_data)

rows_kept <- m2$obs_selection[[1]]
isr_consistent <- isr_reg_data[rows_kept, ]

# Model 1: Search ~ Race (No FE)
m1 <- feols(search_conducted ~ subject_race, 
            cluster = ~FO_EMPLOYEE_ID, 
            data = isr_consistent)

# Model 3: Search ~ Race + Age + Sex + FE
m3 <- feols(search_conducted ~ subject_race + subject_sex + subject_age + age_missing_flag | fe_district_year_month + fe_beat_weekend_quarter, 
            cluster = ~FO_EMPLOYEE_ID, 
            data = isr_consistent)

# verify
obs_counts <- c(nobs(m1), nobs(m2), nobs(m3))

if (var(obs_counts) == 0) {
  print(paste("SUCCESS: All models have identical N =", obs_counts[1]))
} else {
  stop(paste("ERROR: Sample sizes differ! M1:", obs_counts[1], " M2:", obs_counts[2], " M3:", obs_counts[3]))
}


# Generate table

mean_y <- round(mean(isr_consistent$search_conducted), 3)

dict_labels <- c(
  "subject_raceBlack" = "Race: Black",
  "subject_raceHispanic" = "Race: Hispanic",
  "subject_raceOther" = "Race: Other",
  "subject_sexM" = "Sex: Male",
  "subject_sexMissing" = "Sex: Missing",
  "subject_sexX" = "Sex: Other/Unknown",
  "subject_age" = "Age",
  "age_missing_flag" = "Age (Missing Flag)"
)

etable(
  m1, m2, m3,
  tex = TRUE,                                # Export as LaTeX
  file = "output/search_race_regressions.tex",
  replace = TRUE,
  dict = dict_labels,                        # Apply clean labels
  # REMOVED se = "cluster" line! Models already have clustered SEs.
  fitstat = c("n", "r2"),                    # Report N and R-squared
  title = "Regression of Search Indicator on Subject Demographics",
  label = "tab:search_race",
  
  # Add the custom footer rows
  extralines = list(
    "^Design Fixed Effects?" = c("No", "Yes", "Yes"),
    "^Mean of Dep. Var." = c(mean_y, mean_y, mean_y)
  )
)

print("Regression table saved to output/search_race_regressions.tex")


# End Timer 1
t1_end <- Sys.time()

# -----------------------------------------------------------------------------
# Part 2. Measuring oﬀicer heterogeneity; Runtime ~ 1.26s
# -----------------------------------------------------------------------------

# Start Timer 2
t2_start <- Sys.time()

# 1. Adjusted Search Rates and Histogram
# -----------------------------------------------------------------------------

# Regress the search indicator on the fixed effects only
model_officer_fe <- feols(
  search_conducted ~ 1 | FO_EMPLOYEE_ID + fe_district_year_month + fe_beat_weekend_quarter,
  data = isr_filtered
)

# Extract fixed effects and isolate officer fixed effects
all_fixed_effects <- fixef(model_officer_fe)
officer_fe_values <- all_fixed_effects$FO_EMPLOYEE_ID

# Data frame for plotting
officer_fe_df <- data.frame(
  officer_id = names(officer_fe_values),
  adjusted_rate = as.numeric(officer_fe_values)
)

# Plot the distribution of the officer fixed effects
ggplot(officer_fe_df, aes(x = adjusted_rate)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 50) +
  theme_minimal() +
  labs(
    title = "Distribution of Officer Adjusted Search Rates",
    subtitle = "Estimated Officer Fixed Effects (controlled for Location and Time)",
    x = "Officer Fixed Effect (Deviation from Mean)",
    y = "Count of Officers"
  )

ggsave("output/officer_adjusted_search_rates.png", width = 8, height = 6)

# 2. Adjusted Search Rates Two Partitions and Bin Scatter Plot
# -----------------------------------------------------------------------------

set.seed(12345) # Ensure reproducibility

# Randomly assign stops to Partition 1 or 2 within each officer
isr_filtered <- isr_filtered %>%
  group_by(FO_EMPLOYEE_ID) %>%
  mutate(partition = sample(rep(c(1, 2), length.out = n()))) %>%
  ungroup()

# Estimate for P1 and P2
model_p1 <- feols(
  search_conducted ~ 1 | FO_EMPLOYEE_ID + fe_district_year_month + fe_beat_weekend_quarter,
  data = filter(isr_filtered, partition == 1)
)

model_p2 <- feols(
  search_conducted ~ 1 | FO_EMPLOYEE_ID + fe_district_year_month + fe_beat_weekend_quarter,
  data = filter(isr_filtered, partition == 2)
)

# Extract officer fixed effects
fe_p1 <- fixef(model_p1)$FO_EMPLOYEE_ID
fe_p2 <- fixef(model_p2)$FO_EMPLOYEE_ID

# Create dataframes and merge
df_p1 <- data.frame(officer_id = names(fe_p1), rate_p1 = as.numeric(fe_p1))
df_p2 <- data.frame(officer_id = names(fe_p2), rate_p2 = as.numeric(fe_p2))
# keep only officers present in both partitions
reliability_df <- inner_join(df_p1, df_p2, by = "officer_id")

# Create bins based on P1 rates (x-axis)
# Calculate the mean of X and Y within each bin
binscatter_data <- reliability_df %>%
  mutate(bin = ntile(rate_p1, 20)) %>% # Divide into 20 equal-sized bins
  group_by(bin) %>%
  summarise(
    mean_rate_p1 = mean(rate_p1),
    mean_rate_p2 = mean(rate_p2)
  )

# Create the plot
ggplot(binscatter_data, aes(x = mean_rate_p1, y = mean_rate_p2)) +
  geom_point(size = 3, color = "darkblue") + # The binned points
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") + # 45-degree line
  theme_minimal() +
  labs(
    title = "Split-Half Reliability of Officer Adjusted Search Rates",
    subtitle = "Binscatter Plot (20 Bins)",
    x = "Adjusted Search Rate (Partition 1)",
    y = "Adjusted Search Rate (Partition 2)"
  )

ggsave("output/officer_reliability_binscatter.png", width = 8, height = 6)

# 3. Regress adjusted rate from P2 on P1
# -----------------------------------------------------------------------------

# lm since this is a cross-sectional officer-level regression
model_reliability <- lm(rate_p2 ~ rate_p1, data = reliability_df)

print("Regression Results: Split-Half Reliability of Officer Search Rates")
summary(model_reliability)

stargazer(
  model_reliability,
  type = "latex",
  out = "output/reliability_regression.tex",
    title = "Reliability of Officer Adjusted Search Rates (Split-Sample)",
  label = "tab:reliability",
    covariate.labels = c("Adjusted Search Rate (Partition 1)"),
  dep.var.labels = "Adjusted Search Rate (Partition 2)",
    keep.stat = c("n", "rsq"),
  header = FALSE
)

print("Reliability regression table saved to output/reliability_regression.tex")

# End Timer 2
t2_end <- Sys.time()

# -----------------------------------------------------------------------------
# Part 3. Search rates and hit rates; Runtime ~ 0.72s
# -----------------------------------------------------------------------------

# Start Timer 3
t3_start <- Sys.time()

# 1. Create leave-one-out search rate
# -----------------------------------------------------------------------------

isr_filtered <- isr_filtered %>%
  group_by(FO_EMPLOYEE_ID) %>%
  mutate(
    total_officer_stops = n(),
    total_officer_searches = sum(search_conducted, na.rm = TRUE),
    # leave_one_out rate:
    lo_search = (total_officer_searches - search_conducted) / (total_officer_stops - 1)
  ) %>%
  ungroup() 

print("Summary of Leave-One-Out Search Rate:")
summary(isr_filtered$lo_search)

# Verify for a single officer
print("Sample Check (First 5 rows for one officer):")
isr_filtered %>% 
  filter(FO_EMPLOYEE_ID == isr_filtered$FO_EMPLOYEE_ID[1]) %>%
  select(FO_EMPLOYEE_ID, search_conducted, total_officer_stops, total_officer_searches, lo_search) %>%
  head(5)

# 2. Regress Contraband Indicator and Binscatter Plot
# -----------------------------------------------------------------------------

# Yield ~ Officer Search Rate + Fixed Effects
model_contraband <- feols(
  contraband_found ~ lo_search | fe_district_year_month + fe_beat_weekend_quarter,
  cluster = ~FO_EMPLOYEE_ID,
  data = isr_filtered
)

print("Regression Results: Contraband Yield on Search Rate")
etable(model_contraband, fitstat = c("n", "r2"))

# Binscatter plot
# partial out the fixed effects from both X and Y before plotting.

plot_data <- model_contraband$data

# Calculate residuals on fixed effects
res_y <- resid(feols(contraband_found ~ 1 | fe_district_year_month + fe_beat_weekend_quarter, data = isr_filtered))
res_x <- resid(feols(lo_search ~ 1 | fe_district_year_month + fe_beat_weekend_quarter, data = isr_filtered))

# Add back means (to make the axes interpretable)
mean_y <- mean(isr_filtered$contraband_found, na.rm = TRUE)
mean_x <- mean(isr_filtered$lo_search, na.rm = TRUE)

clean_plot_data <- data.frame(
  res_lo_search = res_x + mean_x,
  res_contraband = res_y + mean_y
)

# Create bins and summarize
bin_summary <- clean_plot_data %>%
  mutate(bin = ntile(res_lo_search, 20)) %>% # 20 bins
  group_by(bin) %>%
  summarise(
    mean_x = mean(res_lo_search),
    mean_y = mean(res_contraband)
  )

# Extract coeff and SE for label
coef_val <- coef(model_contraband)["lo_search"]
se_val <- se(model_contraband)["lo_search"]
label_text <- sprintf("Beta = %.4f\nSE = %.4f", coef_val, se_val)

# Plot
ggplot(bin_summary, aes(x = mean_x, y = mean_y)) +
  geom_point(size = 3, color = "darkblue", alpha = 0.7) +
  # Add the regression line (slope matches the coefficient)
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "solid") +
  # Add the text annotation
  annotate("text", x = min(bin_summary$mean_x), y = max(bin_summary$mean_y), 
           label = label_text, hjust = 0, vjust = 1, size = 5, fontface = "bold") +
  theme_minimal() +
  labs(
    title = "Relationship between Officer Search Intensity and Contraband Recovery",
    subtitle = "Binscatter of Residuals (Controlled for Location & Time)",
    x = "Officer Leave-One-Out Search Rate (Residualized)",
    y = "Contraband Found Indicator (Residualized)"
  )

ggsave("output/contraband_binscatter.png", width = 8, height = 6)

# 3. Regress lo_search on Demographics
# -----------------------------------------------------------------------------

isr_lo_reg <- isr_filtered %>%
  mutate(
    # conver to num and impute missing age with mean
    age_numeric = suppressWarnings(as.numeric(AGE)),
    age_missing_flag = if_else(is.na(age_numeric), 1, 0),
    subject_age = if_else(is.na(age_numeric), mean(age_numeric, na.rm = TRUE), age_numeric),
    
    # convert to char
    sex_temp = as.character(SEX_CODE_CD),
    subject_sex = if_else(is.na(sex_temp), "Missing", sex_temp),
    subject_sex = as.factor(subject_sex)
  )

# Regress lo_search on Race, Sex, Age (imputed), and Fixed Effects.

model_3_3 <- feols(
  lo_search ~ subject_race + subject_sex + subject_age + age_missing_flag | fe_district_year_month + fe_beat_weekend_quarter,
  cluster = ~FO_EMPLOYEE_ID,
  data = isr_lo_reg
)

print("Number of observations:")
print(nobs(model_3_3))

print("Regression Results:")
print(summary(model_3_3))

# Generate Table

dict_lo <- c(
  "subject_raceBlack"    = "Race: Black",
  "subject_raceHispanic" = "Race: Hispanic",
  "subject_raceOther"    = "Race: Other",
  "subject_sexM"         = "Sex: Male",
  "subject_sexX"         = "Sex: Other/Unknown",
  "subject_age"          = "Age",
  "age_missing_flag"     = "Age (Missing Flag)"
)

etable(
  model_3_3,
  tex = TRUE,
  file = "output/lo_search_demographics.tex",
  replace = TRUE,
  dict = dict_lo,
  fitstat = c("n", "r2"),
  title = "Correlation between Officer Search Rate and Subject Demographics",
  label = "tab:lo_search_demog"
)

print("Regression table saved to output/lo_search_demographics.tex")


# 4. Joint Significance Test for Race, Age, and Gender
# -----------------------------------------------------------------------------

full_model <- lm(search_conducted ~ subject_race + age_clean + sex_clean, data = isr_imputed)

# Restricted model (regressing on just the intercept)
restricted_model <- lm(search_conducted ~ 1, data = isr_imputed)

# Test if Race, Age, and Sex jointly explain the variation in search_conducted
joint_test_results <- anova(restricted_model, full_model)

# Report the Results
print(joint_test_results)

# View individual coefficients
summary(full_model)

# End Timer 3
t3_end <- Sys.time()
print(paste("Part 3 Runtime:", round(difftime(t3_end, t3_start, units = "secs"), 2), "seconds"))

# -----------------------------------------------------------------------------
# Runtimes
# -----------------------------------------------------------------------------

print(paste("0. Data Preparation Runtime:", round(difftime(t0_end, t0_start, units = "secs"), 2), "seconds"))
print(paste("Part 1 Runtime:", round(difftime(t1_end, t1_start, units = "secs"), 2), "seconds"))
print(paste("Part 2 Runtime:", round(difftime(t2_end, t2_start, units = "secs"), 2), "seconds"))
print(paste("Part 3 Runtime:", round(difftime(t3_end, t3_start, units = "secs"), 2), "seconds"))
print(paste("Total Script Runtime:", round(difftime(t3_end, t0_start, units = "secs"), 2), "seconds"))
