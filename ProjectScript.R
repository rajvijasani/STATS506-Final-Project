# SAScii package will automate the import of ASCII data (.dat file) according to the SAS program statement (.sas file)
# .sas file is the setup file with column locations and labels
# the SAScii package can read both the .dat and .sas files together, applying the formatting directly.
library(SAScii)
# survey package is needed to apply weights and design variables to the datasets to get accurate analysis
library(survey)
library(dplyr)
library(ggplot2)
library(broom)
library(ggeffects)

# Exporting the data
FemaleRespondent <- read.SAScii("data/2017_2019_FemRespData.dat",
                                "data/2017_2019_FemRespSetup.sas")
MaleRespondent <- read.SAScii("data/2017_2019_MaleData.dat",
                              "data/2017_2019_MaleSetup.sas")

# Reducing dataset to only important variables
df_FR <- FemaleRespondent[, c(
  "CASEID",
  "AGER",
  "RMARITAL",
  "HIEDUC",
  "LABORFOR",
  "INTENT",
  "SPWRKST",
  "WGT2017_2019",
  "SEST",
  "SECU"
)]
df_MR <- MaleRespondent[, c(
  "CASEID",
  "AGER",
  "RMARITAL",
  "HIEDUC",
  "LABORFOR",
  "INTENT",
  "SPWRKST",
  "WGT2017_2019",
  "SEST",
  "SECU"
)]

# Checking for missing values or other inconsistencies in FEMALE data
table(df_FR$AGER, useNA = "ifany")
sum(table(df_FR$AGER, useNA = "ifany"))
table(df_FR$RMARITAL, useNA = "ifany")
sum(table(df_FR$RMARITAL, useNA = "ifany"))
table(df_FR$HIEDUC, useNA = "ifany")
sum(table(df_FR$HIEDUC, useNA = "ifany"))
table(df_FR$LABORFOR, useNA = "ifany")
sum(table(df_FR$LABORFOR, useNA = "ifany"))
table(df_FR$INTENT, useNA = "ifany")
sum(table(df_FR$INTENT, useNA = "ifany"))
table(df_FR$SPWRKST, useNA = "ifany")
sum(table(df_FR$SPWRKST, useNA = "ifany"))
sum(is.na(df_FR$WGT2017_2019))
sum(is.na(df_FR$SEST))
sum(is.na(df_FR$SECU))

# Checking for missing values or other inconsistencies in MALE data
table(df_MR$AGER, useNA = "ifany")
sum(table(df_MR$AGER, useNA = "ifany"))
table(df_MR$RMARITAL, useNA = "ifany")
sum(table(df_MR$RMARITAL, useNA = "ifany"))
table(df_MR$HIEDUC, useNA = "ifany")
sum(table(df_MR$HIEDUC, useNA = "ifany"))
table(df_MR$LABORFOR, useNA = "ifany")
sum(table(df_MR$LABORFOR, useNA = "ifany"))
table(df_MR$INTENT, useNA = "ifany")
sum(table(df_MR$INTENT, useNA = "ifany"))
table(df_MR$SPWRKST, useNA = "ifany")
sum(table(df_MR$SPWRKST, useNA = "ifany"))
sum(is.na(df_MR$WGT2017_2019))
sum(is.na(df_MR$SEST))
sum(is.na(df_MR$SECU))

# Encoding variables to factors in FEMALE data
df_FR <- df_FR %>%
  mutate(
    RMARITAL = factor(
      RMARITAL,
      levels = 1:6,
      labels = c(
        "Married",
        "Living with partner",
        "Widowed",
        "Divorced",
        "Separated",
        "Never married"
      )
    ),
    HIEDUC = factor(
      HIEDUC,
      levels = 5:15,
      labels = c(
        "9th or less",
        "10th",
        "11th",
        "12th, no diploma",
        "High school",
        "Some college, no degree",
        "Associate",
        "Bachelor's",
        "Master's",
        "Doctorate",
        "Professional"
      )
    ),
    LABORFOR = factor(
      LABORFOR,
      levels = 1:9,
      labels = c(
        "Full-time",
        "Part-time",
        "On vacation",
        "Family leave",
        "Job looking",
        "In school",
        "Keeping house",
        "Caring for family",
        "Other"
      )
    ),
    INTENT = factor(
      INTENT,
      levels = 1:3,
      labels = c("Intends to have (more)", "Does not intend", "Uncertain")
    ),
    SPWRKST = factor(
      SPWRKST,
      levels = c(1, 5),
      labels = c("Yes", "No")
    )
  )

# Encoding variables to factors in MALE data
df_MR <- df_MR %>%
  mutate(
    RMARITAL = factor(
      RMARITAL,
      levels = 1:6,
      labels = c(
        "Married",
        "Living with partner",
        "Widowed",
        "Divorced",
        "Separated",
        "Never married"
      )
    ),
    HIEDUC = factor(
      HIEDUC,
      levels = 5:15,
      labels = c(
        "9th or less",
        "10th",
        "11th",
        "12th, no diploma",
        "High school",
        "Some college, no degree",
        "Associate",
        "Bachelor's",
        "Master's",
        "Doctorate",
        "Professional"
      )
    ),
    LABORFOR = factor(
      LABORFOR,
      levels = 1:9,
      labels = c(
        "Full-time",
        "Part-time",
        "On vacation",
        "Family leave",
        "Job looking",
        "In school",
        "Keeping house",
        "Caring for family",
        "Other"
      )
    ),
    INTENT = factor(
      INTENT,
      levels = 1:3,
      labels = c("Intends to have (more)", "Does not intend", "Uncertain")
    ),
    SPWRKST = factor(
      SPWRKST,
      levels = c(1, 5),
      labels = c("Yes", "No")
    )
  )

# Releveling reference categories in FEMALE data
# Setting 'High school' as the reference for HIEDUC
df_FR$HIEDUC <- relevel(df_FR$HIEDUC, ref = "High school")

# Setting 'Never married' as the reference for RMARITAL
df_FR$RMARITAL <- relevel(df_FR$RMARITAL, ref = "Never married")

# Setting 'Full-time' as the reference for LABORFOR
df_FR$LABORFOR <- relevel(df_FR$LABORFOR, ref = "Full-time")

# Setting 'High school' as the reference for HIEDUC
df_MR$HIEDUC <- relevel(df_MR$HIEDUC, ref = "High school")

# Releveling reference categories in MALE data
# Setting 'Never married' as the reference for RMARITAL
df_MR$RMARITAL <- relevel(df_MR$RMARITAL, ref = "Never married")

# Setting 'Full-time' as the reference for LABORFOR
df_MR$LABORFOR <- relevel(df_MR$LABORFOR, ref = "Full-time")

## Survey design
# Creating the survey design for FEMALE data
female_design <- svydesign(
  ids = ~ SECU,
  strata = ~ SEST,
  weights = ~ WGT2017_2019,
  data = df_FR,
  nest = TRUE
)

# Creating the survey design for MALE data
male_design <- svydesign(
  ids = ~ SECU,
  strata = ~ SEST,
  weights = ~ WGT2017_2019,
  data = df_MR,
  nest = TRUE
)

## Initial summaries and plots
# AGE
# Summarize age data for females
female_age_summary <- svytable(~ AGER, female_design)
female_age_df <- as.data.frame(female_age_summary)
female_age_df$Gender <- "Female"

# Summarize age data for males
male_age_summary <- svytable(~ AGER, male_design)
male_age_df <- as.data.frame(male_age_summary)
male_age_df$Gender <- "Male"

# Combine both datasets
age_df <- rbind(female_age_df, male_age_df)

# Bar chart for age by gender
ggplot(age_df, aes(x = AGER, y = Freq, fill = Gender)) +
  geom_histogram(stat = "identity", position = "dodge") +
  labs(title = "Age Distribution by Gender",
       x = "Age",
       y = "Weighted Count",
       fill = "Gender") +
  theme_minimal() +
  scale_fill_manual(values = c("Female" = "#4394E5", "Male" = "#87BB62")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# MARITAL STATUS
# Summarize marital status data for females
female_marital_summary <- svytable(~ RMARITAL, female_design)
female_marital_df <- as.data.frame(female_marital_summary)
female_marital_df$Gender <- "Female"

# Summarize marital status data for males
male_marital_summary <- svytable(~ RMARITAL, male_design)
male_marital_df <- as.data.frame(male_marital_summary)
male_marital_df$Gender <- "Male"

# Combine both datasets
marital_df <- rbind(female_marital_df, male_marital_df)

# Bar chart for marital status by gender
ggplot(marital_df, aes(
  x = factor(RMARITAL),
  y = Freq,
  fill = Gender
)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Marital Status by Gender",
       x = "Marital Status",
       y = "Weighted Count",
       fill = "Gender") +
  theme_minimal() +
  scale_fill_manual(values = c("Female" = "#4394E5", "Male" = "#87BB62")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# EDUCATION
# Summarize education data for females
female_edu_summary <- svytable(~ HIEDUC, female_design)
female_edu_df <- as.data.frame(female_edu_summary)
female_edu_df$Gender <- "Female"

# Summarize education data for males
male_edu_summary <- svytable(~ HIEDUC, male_design)
male_edu_df <- as.data.frame(male_edu_summary)
male_edu_df$Gender <- "Male"

# Combine both datasets
edu_df <- rbind(female_edu_df, male_edu_df)

# Bar chart for education level by gender
ggplot(edu_df, aes(
  x = factor(HIEDUC),
  y = Freq,
  fill = Gender
)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Education Level by Gender",
       x = "Education Level",
       y = "Weighted Count",
       fill = "Gender") +
  theme_minimal() +
  scale_fill_manual(values = c("Female" = "#4394E5", "Male" = "#87BB62")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# EMPLOYMENT
# Summarize employment data for females
female_labour_summary <- svytable(~ LABORFOR, female_design)
female_labour_df <- as.data.frame(female_labour_summary)
female_labour_df$Gender <- "Female"

# Summarize employment data for males
male_labour_summary <- svytable(~ LABORFOR, male_design)
male_labour_df <- as.data.frame(male_labour_summary)
male_labour_df$Gender <- "Male"

# Combine both datasets
labour_df <- rbind(female_labour_df, male_labour_df)

# Bar chart for labor force status by gender
ggplot(labour_df, aes(
  x = factor(LABORFOR),
  y = Freq,
  fill = Gender
)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Labor Force Status by Gender",
       x = "Labor Force Status",
       y = "Weighted Count",
       fill = "Gender") +
  theme_minimal() +
  scale_fill_manual(values = c("Female" = "#4394E5", "Male" = "#87BB62")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# INTENTION
# Summarize intent data for females
female_intent_summary <- svytable(~ INTENT, female_design)
female_intent_df <- as.data.frame(female_intent_summary)
female_intent_df$Gender <- "Female"

# Summarize intent data for males
male_intent_summary <- svytable(~ INTENT, male_design)
male_intent_df <- as.data.frame(male_intent_summary)
male_intent_df$Gender <- "Male"

# Combine both datasets
intent_df <- rbind(female_intent_df, male_intent_df)

# Bar chart for future childbearing intentions by gender
ggplot(intent_df, aes(
  x = factor(INTENT),
  y = Freq,
  fill = Gender
)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Intentions for Additional Children by Gender",
       x = "Intentions",
       y = "Weighted Count",
       fill = "Gender") +
  theme_minimal() +
  scale_fill_manual(values = c("Female" = "#4394E5", "Male" = "#87BB62")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Comparison analysis
# AGE vs. INTENTION
# Prepare data for females
female_age_intent_summary <- svyby( ~ INTENT, ~ AGER, female_design, svymean, na.rm = TRUE)
intent_columns <- grep("INTENT", colnames(female_age_intent_summary), value = TRUE)
# Create a tidy dataframe for plotting
female_age_intent_df <- data.frame(
  Age = female_age_intent_summary$AGER,
  Intents = as.numeric(female_age_intent_summary[, intent_columns[1]]),
  Does_Not_Intend = as.numeric(female_age_intent_summary[, intent_columns[2]]),
  Uncertain = as.numeric(female_age_intent_summary[, intent_columns[3]]),
  Gender = "Female"
)

# Reshape for plotting (long format)
female_age_intent_long <- tidyr::pivot_longer(
  female_age_intent_df,
  cols = c("Intents", "Does_Not_Intend", "Uncertain"),
  names_to = "Intent",
  values_to = "Probability"
)

# Prepare data for males
male_age_intent_summary <- svyby( ~ INTENT, ~ AGER, male_design, svymean, na.rm = TRUE)
# Create a tidy dataframe for plotting
male_age_intent_df <- data.frame(
  Age = male_age_intent_summary$AGER,
  Intents = as.numeric(male_age_intent_summary[, intent_columns[1]]),
  Does_Not_Intend = as.numeric(male_age_intent_summary[, intent_columns[2]]),
  Uncertain = as.numeric(male_age_intent_summary[, intent_columns[3]]),
  Gender = "Male"
)

# Reshape for plotting (long format)
male_age_intent_long <- tidyr::pivot_longer(
  male_age_intent_df,
  cols = c("Intents", "Does_Not_Intend", "Uncertain"),
  names_to = "Intent",
  values_to = "Probability"
)

# Combine the datasets
age_intent_df <- rbind(female_age_intent_long, male_age_intent_long)

# Plotting age vs intent by gender
ggplot(age_intent_df, aes(x = Age, y = Probability, color = Intent)) +
  geom_line(size = 1) +
  facet_wrap( ~ Gender) +
  labs(
    title = "Age vs. Intentions by Gender",
    x = "Age",
    y = "Probability",
    color = "Intent"
  ) +
  theme_minimal()

# MARITAL STATUS vs. INTENTION
# Summarize data for females
female_marital_intent <- svytable(~ RMARITAL + INTENT, female_design)
female_marital_intent_df <- as.data.frame(female_marital_intent)
female_marital_intent_df$Gender <- "Female"

# Summarize data for males
male_marital_intent <- svytable(~ RMARITAL + INTENT, male_design)
male_marital_intent_df <- as.data.frame(male_marital_intent)
male_marital_intent_df$Gender <- "Male"

# Combine both datasets
marital_intent_df <- rbind(female_marital_intent_df, male_marital_intent_df)

# Plotting Marital Status vs. Intentions by Gender
ggplot(marital_intent_df, aes(x = RMARITAL, y = Freq, fill = INTENT)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ Gender) +
  labs(title = "Marital Status vs. Intentions by Gender",
       x = "Marital Status",
       y = "Proportion",
       fill = "Intentions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# EDUCATION vs. INTENTION
# Summarize data for females
female_edu_intent <- svytable(~ HIEDUC + INTENT, female_design)
female_edu_intent_df <- as.data.frame(female_edu_intent)
female_edu_intent_df$Gender <- "Female"

# Summarize data for males
male_edu_intent <- svytable(~ HIEDUC + INTENT, male_design)
male_edu_intent_df <- as.data.frame(male_edu_intent)
male_edu_intent_df$Gender <- "Male"

# Combine both datasets
edu_intent_df <- rbind(female_edu_intent_df, male_edu_intent_df)

# Plotting Education vs. Intentions by Gender
ggplot(edu_intent_df, aes(x = HIEDUC, y = Freq, fill = INTENT)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ Gender) +
  labs(title = "Education Level vs. Intentions by Gender",
       x = "Education Level",
       y = "Proportion",
       fill = "Intentions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# EMPLOYMENT vs. INTENTION
# Summarize data for females
female_labour_intent <- svytable(~ LABORFOR + INTENT, female_design)
female_labour_intent_df <- as.data.frame(female_labour_intent)
female_labour_intent_df$Gender <- "Female"

# Summarize data for males
male_labour_intent <- svytable(~ LABORFOR + INTENT, male_design)
male_labour_intent_df <- as.data.frame(male_labour_intent)
male_labour_intent_df$Gender <- "Male"

# Combine both datasets
labour_intent_df <- rbind(female_labour_intent_df, male_labour_intent_df)

# Plotting Employment vs. Intentions by Gender
ggplot(labour_intent_df, aes(x = LABORFOR, y = Freq, fill = INTENT)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Gender) +
  labs(title = "Labor Force Status vs. Intentions by Gender",
       x = "Labor Force Status",
       y = "Weighted Count",
       fill = "Birth Intentions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# S/P EMPLOYMENT vs. INTENTION
# Filter female and male datasets for respondents with a partner (SPWRKST != "NA")
female_with_partner <- subset(female_design, SPWRKST != "NA")
male_with_partner <- subset(male_design, SPWRKST != "NA")

# Summarize data for females
female_partner_intent <- svytable(~ SPWRKST + INTENT, female_with_partner)
female_partner_intent_df <- as.data.frame(female_partner_intent)
female_partner_intent_df$Gender <- "Female"

# Summarize data for males
male_partner_intent <- svytable(~ SPWRKST + INTENT, male_with_partner)
male_partner_intent_df <- as.data.frame(male_partner_intent)
male_partner_intent_df$Gender <- "Male"

# Combine both datasets
partner_intent_df <- rbind(female_partner_intent_df, male_partner_intent_df)

# Plotting S/P Employment vs. Intentions by Gender
ggplot(partner_intent_df, aes(x = SPWRKST, y = Freq, fill = INTENT)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ Gender) +
  labs(title = "Spouse/Partner's Employment Status vs. Intentions by Gender",
       x = "Spouse/Partner's Employment Status",
       y = "Proportion",
       fill = "Intentions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Regression analysis
# subsetting data to remove the "Uncertain" category observations
female_binary <- subset(female_design, INTENT != "Uncertain")
male_binary <- subset(male_design, INTENT != "Uncertain")


# CORE Models
# Core regression for female dataset
female_core_model <- svyglm(INTENT ~ AGER + HIEDUC + RMARITAL + LABORFOR,
                            design = female_binary,
                            family = quasibinomial())

# Summary of the female core model
summary(female_core_model)

# Core regression for male dataset
male_core_model <- svyglm(INTENT ~ AGER + HIEDUC + RMARITAL + LABORFOR,
                          design = male_binary,
                          family = quasibinomial())

# Summary of the male core model
summary(male_core_model)

# EXTENDED Models
female_binary_partner <- subset(female_binary, SPWRKST != "NA")
male_binary_partner <- subset(male_binary, SPWRKST != "NA")
# Extended regression for female dataset
female_extended_model <- svyglm(
  INTENT ~ AGER + HIEDUC + RMARITAL + LABORFOR + SPWRKST,
  design = female_binary_partner,
  family = quasibinomial()
)

# Summary of the female extended model
summary(female_extended_model)

# Extended regression for male dataset
male_extended_model <- svyglm(
  INTENT ~ AGER + HIEDUC + RMARITAL + LABORFOR + SPWRKST,
  design = male_binary_partner,
  family = quasibinomial()
)

# Summary of the male extended model
summary(male_extended_model)

## Regression plots
# Extract odds ratios and confidence intervals for female core model
female_core_or <- tidy(female_core_model,
                       conf.int = TRUE,
                       exponentiate = TRUE)

# Plot the odds ratios for the female model
ggplot(female_core_or,
       aes(
         x = term,
         y = estimate,
         ymin = conf.low,
         ymax = conf.high
       )) +
  geom_pointrange() +
  coord_flip() +
  labs(title = "Odds Ratios for Female Core Model", x = "Predictor Variables", y = "Odds Ratio") +
  theme_minimal()

# Extract odds ratios and confidence intervals for male core model
male_core_or <- tidy(male_core_model,
                     conf.int = TRUE,
                     exponentiate = TRUE)

# Plot the odds ratios for the male model
ggplot(male_core_or,
       aes(
         x = term,
         y = estimate,
         ymin = conf.low,
         ymax = conf.high
       )) +
  geom_pointrange() +
  coord_flip() +
  labs(title = "Odds Ratios for Male Core Model", x = "Predictor Variables", y = "Odds Ratio") +
  theme_minimal()