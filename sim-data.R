library(dplyr)
library(purrr)
library(janitor)
library(dagitty)
library(ggplot2)
library(ggdag)

set.seed(123) # For reproducibility

n <- 1500

region <- sample(
  c("Northeast", "Midwest", "South", "West"),
  size = n,
  replace = TRUE,
  prob = c(17, 21, 38, 24) / 100
)

data <- tibble(region)

head(data)

raceEthnicity <- map_chr(region, function(reg) {
  sample(
    c("Black", "White", "Hispanic", "Asian", "Other"),
    size = 1,
    prob = case_when(
      reg == "Northeast" ~ c(14, 55, 22, 6, 3) / 100,
      reg == "Midwest"   ~ c(10, 70, 15, 3, 2) / 100,
      reg == "South"     ~ c(20, 50, 20, 5, 5) / 100,
      reg == "West"      ~ c(6, 40, 35, 15, 4) / 100,
      TRUE               ~ c(13, 58, 20, 6, 3) / 100 # Default (national average)
    )
  )
})

data <- data %>%
  mutate(raceEthnicity = raceEthnicity)


religion <- map_chr(region, function(reg) {
  sample(
    c("Christian", "Unaffiliated", "Jewish", "Muslim", "Hindu", "Other"),
    size = 1,
    prob = case_when(
      reg == "Northeast" ~ c(60, 25, 10, 1, 1, 3) / 100,
      reg == "Midwest"   ~ c(70, 20, 3, 1, 1, 5) / 100,
      reg == "South"     ~ c(80, 15, 2, 1, 0.5, 1.5) / 100,
      reg == "West"      ~ c(50, 35, 2, 1, 3, 9) / 100,
      TRUE               ~ c(63, 26, 2, 1, 1, 7) / 100 # Default (national average)
    )
  )
})

data <- data %>%
  mutate(religion = religion)

head(data)


# Generate parent latent variables
latent_vars_parent <- tibble(
  parentSelfReliance = rnorm(n, mean = 1, sd = 1),
  parentMotivation = rnorm(n, mean = 1, sd = 1),
  parentIntelligence = rnorm(n, mean = 1, sd = 1),
  parentCommunityEngagement = rnorm(n, mean = 1, sd = 1)
)

# Generate child latent variables correlated with parent at 0.5
latent_vars_child <- latent_vars_parent %>%
  mutate(
    childSelfReliance = 0.5 * parentSelfReliance + sqrt(1 - 0.5^2) * rnorm(n, mean = 0, sd = 1),
    childMotivation = 0.5 * parentMotivation + sqrt(1 - 0.5^2) * rnorm(n, mean = 0, sd = 1),
    childIntelligence = 0.5 * parentIntelligence + sqrt(1 - 0.5^2) * rnorm(n, mean = 0, sd = 1),
    childCommunityEngagement = 0.5 * parentCommunityEngagement + sqrt(1 - 0.5^2) * rnorm(n, mean = 0, sd = 1)
  ) %>%
  select(starts_with("child"))

# Combine parent and child variables
data <- bind_cols(data, latent_vars_parent, latent_vars_child)

head(data)

# Generate parentIncome using latent variables and 50% noise
data <- data %>%
  mutate(
    parentIncomeRaw = 0.25 * parentSelfReliance +
      0.25 * parentMotivation +
      0.25 * parentIntelligence +
      0.25 * parentCommunityEngagement +
      rnorm(n, mean = 0, sd = 1) * sqrt(0.5),
    
    # Apply transformations to make it right-skewed and set median ~60k
    parentIncome = exp(scale(parentIncomeRaw)) * 60000
  ) %>%
  mutate(
    parentIncome = round(parentIncome, 0) # Ensure income is rounded to integers
  )

head(data)

# Generate kids educational attainment
data <- data %>%
  mutate(
    # Base raw score for educational attainment
    educationRaw = 0.3 * (parentIncome / 60000) +   # Scale parentIncome to match other variables
      0.3 * childMotivation +
      0.3 * childIntelligence +
      0.2 * childSelfReliance +
      0.1 * childCommunityEngagement +
      rnorm(n, mean = 0, sd = 1) * sqrt(0.5), # Add 50% noise
    
    # Map raw scores to educational attainment levels
    educationalAttainment = case_when(
      educationRaw >= 2.5 ~ "AdvancedDegree",
      educationRaw >= 1.5 ~ "Bachelors",
      educationRaw >= 0.5 ~ "SomeCollege",
      educationRaw >= -0.5 ~ "HighSchool",
      TRUE ~ "LessThanHighSchool"
    )
  )

head(data)

# Map parent education to a numeric score
parentEducationScore <- case_when(
  data$educationalAttainment == "AdvancedDegree" ~ 4,
  data$educationalAttainment == "Bachelors" ~ 3,
  data$educationalAttainment == "SomeCollege" ~ 2,
  data$educationalAttainment == "HighSchool" ~ 1,
  data$educationalAttainment == "LessThanHighSchool" ~ 0,
  TRUE ~ NA_real_
)

# Generate type of job
data <- data %>%
  mutate(
    # Base score for job type likelihood
    jobScore = case_when(
      educationalAttainment == "AdvancedDegree" ~ 3,
      educationalAttainment == "Bachelors" ~ 2,
      educationalAttainment == "SomeCollege" ~ 1,
      educationalAttainment == "HighSchool" ~ 0.5,
      educationalAttainment == "LessThanHighSchool" ~ 0,
      TRUE ~ NA_real_
    ) +
      0.2 * case_when(
        region == "Northeast" ~ 0.3,
        region == "Midwest" ~ 0.1,
        region == "South" ~ -0.2,
        region == "West" ~ 0.1,
        TRUE ~ 0
      ) +
      0.1 * parentEducationScore +
      0.1 * (parentIncome / 60000) + # Scale parentIncome to a comparable range
      rnorm(n, mean = 0, sd = 0.5), # Add random noise
    
    # Map jobScore to jobType
    jobType = case_when(
      jobScore >= 2.5 ~ "Professional",
      jobScore >= 1.0 ~ "Trade",
      TRUE ~ "Unskilled"
    )
  )


data <- data %>%
  mutate(
    # Base score for income based on jobType
    jobIncomeComponent = case_when(
      jobType == "Professional" ~ 3,
      jobType == "Trade" ~ 2,
      jobType == "Unskilled" ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Base score for income based on educationalAttainment
    educationIncomeComponent = case_when(
      educationalAttainment == "AdvancedDegree" ~ 4,
      educationalAttainment == "Bachelors" ~ 3,
      educationalAttainment == "SomeCollege" ~ 2,
      educationalAttainment == "HighSchool" ~ 1,
      educationalAttainment == "LessThanHighSchool" ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Income calculation
    incomeRaw = 0.4 * jobIncomeComponent +
      0.4 * educationIncomeComponent +
      0.1 * (childMotivation + childIntelligence + childSelfReliance + childCommunityEngagement) +
      rnorm(n, mean = 0, sd = 1) * sqrt(0.1), # 10% random noise
    
    # Transform to make income realistic (median ~60k, right-skewed)
    income = exp(scale(incomeRaw)) * 60000
  ) %>%
  mutate(
    income = round(income, 0) # Round to nearest whole number
  )

# Insurance
data <- data %>%
  mutate(
    # Base probabilities for insurance based on jobType and income
    insurance = map2_chr(jobType, income, ~ {
      case_when(
        .x == "Professional" & .y > 50000 ~ sample(c("Private", "StateProvided", "None"), size = 1, prob = c(0.8, 0.15, 0.05)),
        .x == "Trade" & .y > 50000 ~ sample(c("Private", "StateProvided", "None"), size = 1, prob = c(0.6, 0.3, 0.1)),
        .x == "Trade" & .y <= 50000 ~ sample(c("Private", "StateProvided", "None"), size = 1, prob = c(0.4, 0.5, 0.1)),
        .x == "Unskilled" & .y > 30000 ~ sample(c("Private", "StateProvided", "None"), size = 1, prob = c(0.4, 0.5, 0.1)),
        .x == "Unskilled" & .y <= 30000 ~ sample(c("Private", "StateProvided", "None"), size = 1, prob = c(0.2, 0.6, 0.2)),
        TRUE ~ "None"
      )
    })
  )

data <- data %>%
  mutate(
    # Base score for dependents based on religion
    religionDependents = case_when(
      religion == "Unaffiliated" ~ -0.5,
      religion == "Christian" ~ 0.3,
      religion == "Jewish" ~ -0.2,
      religion == "Muslim" ~ 0.8,
      religion == "Hindu" ~ 0.6,
      religion == "Other" ~ 0.2,
      TRUE ~ 0
    ),
    
    # Education influence
    educationDependents = case_when(
      educationalAttainment == "AdvancedDegree" ~ 0.2,
      educationalAttainment == "Bachelors" ~ 0.1,
      educationalAttainment == "SomeCollege" ~ 0,
      educationalAttainment == "HighSchool" ~ 0.1,
      educationalAttainment == "LessThanHighSchool" ~ 0.2,
      TRUE ~ 0
    ),
    
    # Income influence (moderate negative correlation)
    incomeDependents = -0.5 * (income / 60000),
    
    # Raw number of dependents
    dependentsRaw = 2 + religionDependents + educationDependents + incomeDependents +
      rnorm(n, mean = 0, sd = 0.5), # Random noise for variability
    
    # Ensure dependents are non-negative integers
    numberOfDependents = pmax(0, round(dependentsRaw))
  )

head(data)


data <- data %>%
  mutate(
    # Base trust score with random noise
    trustRaw = rnorm(n, mean = 0, sd = 1),
    
    # Adjustment for education
    educationTrust = case_when(
      educationalAttainment == "AdvancedDegree" ~ 0.5,
      educationalAttainment == "Bachelors" ~ 0.4,
      educationalAttainment == "SomeCollege" ~ 0.2,
      educationalAttainment == "HighSchool" ~ 0.1,
      educationalAttainment == "LessThanHighSchool" ~ -0.2,
      TRUE ~ 0
    ),
    
    # Adjustment for income (scaled)
    incomeTrust = 0.3 * (income / 60000),
    
    # Adjustment for race/ethnicity
    raceTrust = case_when(
      raceEthnicity == "White" ~ 0.3,
      raceEthnicity == "Asian" ~ 0.2,
      raceEthnicity == "Hispanic" ~ -0.1,
      raceEthnicity == "Black" ~ -0.3,
      raceEthnicity == "Other" ~ -0.2,
      TRUE ~ 0
    ),
    
    # Combine all influences
    trustInHealthcareProvider = trustRaw + educationTrust + incomeTrust + raceTrust
  )

# Scale and normalize trust scores (optional)
data <- data %>%
  mutate(
    trustInHealthcareProvider = round((trustInHealthcareProvider - min(trustInHealthcareProvider)) / 
                                        (max(trustInHealthcareProvider) - min(trustInHealthcareProvider)) * 100, 1) # Scale to 0-100
  )

head(data)

data <- data %>%
  mutate(
    # Base score for proactive behavior
    proactiveHealthRaw = 0.4 * trustInHealthcareProvider / 100 + # Normalize trust score to [0, 1]
      0.3 * (income / 60000) + # Scale income
      case_when( # Education influence
        educationalAttainment == "AdvancedDegree" ~ 0.4,
        educationalAttainment == "Bachelors" ~ 0.3,
        educationalAttainment == "SomeCollege" ~ 0.2,
        educationalAttainment == "HighSchool" ~ 0.1,
        educationalAttainment == "LessThanHighSchool" ~ -0.2,
        TRUE ~ 0
      ) +
      rnorm(n, mean = 0, sd = 0.2), # Add random noise
    
    # Normalize the latent variable to range [0, 1]
    proactiveHealthSeekingBehavior = (proactiveHealthRaw - min(proactiveHealthRaw)) / 
      (max(proactiveHealthRaw) - min(proactiveHealthRaw))
  )

head(data)


data <- data %>%
  # Generate demographic and anthropometric variables
  mutate(
    age = round(rnorm(n, mean = 30, sd = 5)), # Maternal age
    height = rnorm(n, mean = 162, sd = 7.6), # Height in cm
    weight = rnorm(n, mean = 68, sd = 12) + # Weight in kg
      ifelse(income > 60000, -5, 5) + # Income adjustment
      ifelse(educationalAttainment %in% c("AdvancedDegree", "Bachelors"), -3, 3),
    bmi = weight / (height / 100)^2, # Calculate BMI
    
    # Obesity (based on BMI)
    obesity = case_when(
      bmi >= 30 ~ 1,
      bmi < 30 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Diabetes (general prevalence ~10%, adjusted by income/education)
    diabetes = rbinom(n, 1, prob = 0.1 - 0.03 * (income > 60000) - 0.02 * (educationalAttainment %in% c("AdvancedDegree", "Bachelors"))),
    
    # Heart disease (influenced by age, obesity, and diabetes)
    heartDisease = rbinom(n, 1, prob = 0.05 + 0.02 * diabetes + 0.03 * obesity + 0.01 * (age > 35)),
    
    # Multiple gestation (slightly more likely for older/high-income mothers)
    multipleGestation = rbinom(n, 1, prob = 0.03 + 0.01 * (age > 35) + 0.01 * (income > 60000)),
    
    # Placenta previa (random with slight increase for older mothers)
    placentaPrevia = rbinom(n, 1, prob = 0.01 + 0.01 * (age > 35)),
    
    # Preeclampsia (influenced by obesity, multiple gestation, and placenta previa)
    preeclampsia = rbinom(n, 1, prob = 0.05 + 0.03 * obesity + 0.02 * multipleGestation + 0.02 * placentaPrevia)
  )

# Generate distance to hospital
data <- data %>%
mutate(
  # Base random distance with regional adfjustments
  baseDistance = rnorm(n, mean = 8, sd = 5), # Mean ~8 miles, wide variation
  
  regionAdjustment = case_when(
    region == "Northeast" ~ -2,  # Closer to hospitals
    region == "Midwest" ~ 1,    # Slightly farther
    region == "South" ~ 2,      # Farther
    region == "West" ~ 0,       # Mixed
    TRUE ~ 0
  ),
  
  # Calculate final distance
  distanceToHospitalMiles = pmax(0, baseDistance + regionAdjustment) # Ensure no negative distances
) %>%
  mutate(
    distanceToHospitalMiles = round(distanceToHospitalMiles, 1) # Round to 1 decimal place
  )

# Preview the data
head(data)

data <- data %>%
  mutate(
    # Generate a base random provider quality score
    qualityProviderRaw = rnorm(n, mean = 50, sd = 10), # Random baseline score
    
    # Adjust for proximity to a hospital (closer = higher score)
    distanceAdjustment = case_when(
      distanceToHospitalMiles <= 5 ~ 5,
      distanceToHospitalMiles <= 10 ~ 2,
      distanceToHospitalMiles <= 20 ~ -1,
      TRUE ~ -5
    ),
    
    # Adjust for income (higher income = higher score)
    incomeAdjustment = case_when(
      income > 100000 ~ 5,
      income > 60000 ~ 2,
      income <= 30000 ~ -3,
      TRUE ~ 0
    ),
    
    # Final quality score
    qualityOfProviderScore = qualityProviderRaw + distanceAdjustment + incomeAdjustment
  ) %>%
  mutate(
    # Normalize quality score to 0–100
    qualityOfProviderScore = round((qualityOfProviderScore - min(qualityOfProviderScore)) / 
                                     (max(qualityOfProviderScore) - min(qualityOfProviderScore)) * 100, 1)
  )

head(data)

data <- data %>%
  # Personal capacity to attend doctor
  mutate(
    capacityToAttendDoctor = 1 - (0.3 * distanceToHospitalMiles / max(distanceToHospitalMiles)) - # Distance reduces capacity
      0.2 * (numberOfDependents / max(numberOfDependents)) + # Dependents reduce capacity
      case_when(
        jobType == "Professional" ~ 0.3,
        jobType == "Trade" ~ 0.1,
        jobType == "Unskilled" ~ -0.1,
        TRUE ~ 0
      ) +
      rnorm(n, mean = 0, sd = 0.1), # Add random noise
    capacityToAttendDoctor = pmax(0, pmin(1, capacityToAttendDoctor)), # Normalize to [0, 1]
    
    # Willingness to pay
    willingnessToPay = case_when(
      insurance == "Private" ~ 0.7,
      insurance == "StateProvided" ~ 0.5,
      insurance == "None" ~ 0.2,
      TRUE ~ 0
    ) +
      0.3 * (income / max(income)) + # Income increases willingness
      rnorm(n, mean = 0, sd = 0.1), # Add random noise
    willingnessToPay = pmax(0, pmin(1, willingnessToPay)) # Normalize to [0, 1]
  )

# Preview the updated dataset
head(data)


data <- data %>%
  mutate(
    # Had birth in hospital (~98%)
    birthInHospital = rbinom(n, 1, prob = 0.98 - 0.01 * distanceToHospitalMiles / max(distanceToHospitalMiles) +
                               0.01 * willingnessToPay +
                               0.01 * trustInHealthcareProvider / 100 +
                               rnorm(n, mean = 0, sd = 0.02)), # 20% random
    
    # Had documented provider visit within 14 days (~50%)
    providerVisit14Days = rbinom(n, 1, prob = 0.5 +
                                   0.2 * capacityToAttendDoctor +
                                   0.2 * willingnessToPay +
                                   0.2 * trustInHealthcareProvider / 100 +
                                   0.2 * qualityOfProviderScore / 100 +
                                   0.1 * (obesity + diabetes + heartDisease + multipleGestation + placentaPrevia + preeclampsia) +
                                   rnorm(n, mean = 0, sd = 0.1)), # 20% random
    
    # Had documented provider visit between 15 days and 6 months (~20%)
    providerVisit15DaysTo6Months = rbinom(n, 1, prob = 0.2 +
                                            0.15 * capacityToAttendDoctor +
                                            0.15 * willingnessToPay +
                                            0.15 * trustInHealthcareProvider / 100 +
                                            0.15 * qualityOfProviderScore / 100 +
                                            0.1 * (obesity + diabetes + heartDisease + multipleGestation + placentaPrevia + preeclampsia) +
                                            rnorm(n, mean = 0, sd = 0.05)) # 20% random
  )

# Ensure binary outputs are within [0, 1]
data <- data %>%
  mutate(
    birthInHospital = ifelse(birthInHospital > 0.98, 1, birthInHospital),
    providerVisit14Days = ifelse(providerVisit14Days > 0.5, 1, providerVisit14Days),
    providerVisit15DaysTo6Months = ifelse(providerVisit15DaysTo6Months > 0.2, 1, providerVisit15DaysTo6Months)
  )

# Preview
head(data)

## Now, let's only include the variables we could actually observe in a real-world scenario

data <- data %>%
  mutate(
    # Bin directly based on income with noise and cutoff at $200k
    selfReportIncome = cut(
      pmin(pmax(income + rnorm(n, mean = 0, sd = 5000), 0), 200000), # Add noise and cap
      breaks = seq(0, 200000, by = 5000),
      include.lowest = TRUE,
      right = FALSE,
      labels = paste0("$", seq(0, 195000, by = 5000), "–$", seq(5000, 200000, by = 5000))
    )
  )

# Preview the dataset
head(data %>% select(income, selfReportIncome))


analysisData <- data |> select(
  region,
  raceEthnicity,
  selfReportIncome,
  educationalAttainment,
  insurance,
  age,
  height,
  weight,
  obesity,
  diabetes,
  heartDisease,
)

saveRDS(data, "raw_sim_data.rds")
saveRDS(analysisData, "analysis_data.rds")



# Define the DAG
dag <- dagitty('dag {
  HQ -> PNC
  PC -> PNC
  WTP -> PNC
  T -> PNC
  H -> PNC
  G -> HQ
  D -> PC
  Inc -> PC
  Inc -> WTP
  Ins -> WTP
  RE -> T
  RE -> H
  Ed -> Inc
  RE -> Ed
  RE -> Inc
  Regn -> HQ
  Regn -> PC
  Relgn -> T
  Job -> Ins
  Ed -> Job
  
}')

labels <- c(
  HQ = "Hospital Quality",
  PNC = "Postnatal Care",
  WTP = "Willingness to Pay",
  T = "Trust",
  H = "Health",
  G = "Geography",
  D = "Distance",
  Inc = "Income",
  Ins = "Insurance",
  RE = "Race/Ethnicity",
  Ed = "Education",
  Regn = "Region",
  Relgn = "Religion",
  PC = "Personal Capacity",
  Job = "Job Type"
)

# Add labels to the DAG visualization


# Define the DAG
dag <- dagitty('dag {
  HQ -> PNC
  PC -> PNC
  WTP -> PNC
  T -> PNC
  H -> PNC
  G -> HQ
  D -> PC
  Inc -> PC
  Inc -> WTP
  Ins -> WTP
  RE -> T
  RE -> H
  Ed -> Inc
  RE -> Ed
  RE -> Inc
  Regn -> HQ
  Regn -> PC
  Relgn -> T
  Job -> Ins
  Ed -> Job
}')

# Define labels for the abbreviations
labels <- c(
  HQ = "Hospital Quality",
  PNC = "Postnatal Care",
  WTP = "Willingness to Pay",
  T = "Trust",
  H = "Health",
  G = "Geography",
  D = "Distance",
  Inc = "Income",
  Ins = "Insurance",
  RE = "Race/Ethnicity",
  Ed = "Education",
  Regn = "Region",
  Relgn = "Religion",
  PC = "Personal Capacity",
  Job = "Job Type"
)

# Customize the DAG with labels on the nodes
ggdag(dag) +
  geom_dag_edges(edge_color = "black") + # Show edges
  geom_dag_label(aes(label = labels[name]), color = "black", size = 3, fill = "lightgray") + # Place labels on nodes
  theme_void() + # Clean theme
  labs(title = "Causal DAG") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    panel.background = element_rect(fill = "white", color = NA)
  )