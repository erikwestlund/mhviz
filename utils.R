normalize_to_0_1 <- function(x) {
  if (!is.numeric(x)) stop("Input must be numeric.")
  
  if (max(x) == min(x)) {
    return(rep(0.5, length(x)))  # Assign all values to 0.5 if there's no range
  }
  
  (x - min(x)) / (max(x) - min(x))
}

normalize_rows_to_1 <- function(df) {
  as.data.frame(
    t(apply(df, 1, function(row) row / sum(row)))
  )
}

normalize_to_1 <- function(x) {
  (x - mean(x)) / sd(x)
}

gen_correlated <- function(x, target_r, noise_sd = 1) {
  # Check input
  if (!is.numeric(x)) stop("Input 'x' must be numeric.")
  if (target_r < -1 || target_r > 1) stop("Correlation 'target_r' must be between -1 and 1.")
  
  x <- scale(x)[, 1]
    noise <- rnorm(length(x), mean = 0, sd = noise_sd)
  
  target_r * x + sqrt(1 - target_r^2) * scale(noise)[, 1]
}

generate_cultural_orientation <- function(
    n, 
    parent_income, 
    parent_edu, 
    state_pec, 
    religion, 
    community_engagement
) {
  parent_income <- normalize_to_0_1(parent_income)
  state_pec <- normalize_to_0_1(state_pec)
  community_engagement <- normalize_to_0_1(community_engagement)
  
  parent_edu_weights <- c(
    less_than_hs = 0.2,
    hs = 0.4,
    some_college = 0.6,
    college = 0.8,
    post_grad = 1
  )
  
  parent_edu_value <- sapply(parent_edu, function(ed) parent_edu_weights[ed])
  
  # Religion boost (0.05 boost if religious)
  religion_boost <- ifelse(!is.na(religion), 0.05, 0)
  
  random_component <- runif(n, 0, 1)
  
  cultural_orientation <- 0.2 * parent_income +
    0.2 * parent_edu_value +
    0.2 * state_pec +
    0.2 * community_engagement +
    0.05 * religion_boost +
    0.35 * random_component
  
  # Ensure values are within [0, 1]
  cultural_orientation <- normalize_to_0_1(cultural_orientation)
  
  return(cultural_orientation)
}

generate_education <- function(
    n, 
    education_levels, 
    intelligence, 
    self_reliance, 
    motivation, 
    community_engagement, 
    parent_income = NULL,
    parent_education = NULL
) {
  population_proportions <- c(
    less_than_hs = 0.1,
    hs = 0.3,
    some_college = 0.3,
    college = 0.2,
    post_grad = 0.1
  )
  
  composite_weight <- 0.25 * scale(intelligence)[, 1] +
    0.15 * scale(self_reliance)[, 1] +
    0.2 * scale(motivation)[, 1] +
    0.1 * scale(community_engagement)[, 1]
  
  if (!is.null(parent_education)) {
    # Assign weights to each parental education level
    parent_ed_weights <- c(
      less_than_hs = -0.2,
      hs = -0.1,
      some_college = 0,
      college = 0.2,
      post_grad = 0.3
    )
    
    parent_ed_contribution <- sapply(parent_education, function(ed) {
      parent_ed_weights[ed]
    })
    
    composite_weight <- composite_weight + 0.2 * parent_ed_contribution
  }
  
  if (!is.null(parent_income)) {
    parent_income <- normalize_to_0_1(parent_income)
    composite_weight <- composite_weight + 0.3 * parent_income
  }
  
  composite_weight <- normalize_to_0_1(composite_weight)
  
  education_probs <- data.frame(
    less_than_hs = population_proportions["less_than_hs"] * (1 - composite_weight),
    hs = population_proportions["hs"] * (1 - composite_weight / 2),
    some_college = population_proportions["some_college"] * composite_weight,
    college = population_proportions["college"] * composite_weight ^ 2,
    post_grad = population_proportions["post_grad"] * composite_weight ^ 3
  )
  
  education_probs <- normalize_rows_to_1(education_probs)
  
  apply(education_probs, 1, function(p) {
    sample(education_levels, size = 1, prob = p)
  })
}

generate_religion <- function(n, rel_cats, race) {
  race_religion_probs <- list(
    white = c(christian = 0.7, muslim = 0.02, jewish = 0.1, buddhist = 0.03, hindu = 0.01, other = 0.14),
    black = c(christian = 0.8, muslim = 0.05, jewish = 0.01, buddhist = 0.01, hindu = 0.01, other = 0.12),
    hispanic = c(christian = 0.75, muslim = 0.02, jewish = 0.01, buddhist = 0.02, hindu = 0.01, other = 0.19),
    asian = c(christian = 0.4, muslim = 0.1, jewish = 0.01, buddhist = 0.3, hindu = 0.15, other = 0.04),
    aian = c(christian = 0.6, muslim = 0.01, jewish = 0.01, buddhist = 0.02, hindu = 0.01, other = 0.35),
    nhpi = c(christian = 0.5, muslim = 0.02, jewish = 0.01, buddhist = 0.4, hindu = 0.02, other = 0.05),
    other = c(christian = 0.5, muslim = 0.1, jewish = 0.01, buddhist = 0.1, hindu = 0.1, other = 0.19)
  )
  
  if (length(race) != n) {
    race <- sample(race, n, replace = TRUE)
  }
  
  if (!all(race %in% names(race_religion_probs))) {
    stop("Invalid race category detected.")
  }
  
  sapply(race, function(r) {
    probs <- race_religion_probs[[r]]
    sample(rel_cats, size = 1, prob = probs)
  })
}

generate_incomes <- function(n, median_income = 60000, sd = 25000, min_income = 0, max_income = 1000000) {
  # Generate log-normal incomes based on median and standard deviation
  incomes <- rlnorm(n, meanlog = log(median_income), sdlog = log(1 + sd / median_income))
  
  # Trim incomes to the specified range
  incomes <- pmin(pmax(incomes, min_income), max_income)
  
  return(round(incomes))  # Return rounded incomes
}

generate_mother_ages <- function(n) {
  # Approximate U.S. mother age distribution (source: CDC, Census)
  mother_age_distribution <- data.frame(
    age_group = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44"),
    lower_bound = c(15, 20, 25, 30, 35, 40),
    upper_bound = c(19, 24, 29, 34, 39, 44),
    proportion = c(0.08, 0.22, 0.30, 0.25, 0.12, 0.03)  # Approximate proportions for mothers
  )
  
  sampled_groups <- sample(
    mother_age_distribution$age_group,
    size = n,
    replace = TRUE,
    prob = mother_age_distribution$proportion
  )
  
  sampled_ages <- unlist(lapply(sampled_groups, function(group) {
    row <- mother_age_distribution[mother_age_distribution$age_group == group, ]
    sample(row$lower_bound:row$upper_bound, size = 1)
  }))
  
  return(sampled_ages)
}

generate_job_type <- function(n, job_type_cats, education_levels, edu, parent_income) {
  proportions <- c(0.05, 0.50, 0.20, 0.25)
  if (length(proportions) != length(job_type_cats)) stop("Proportions must match job type categories")
  
  education_scores <- seq(0, 0.2 * (length(education_levels) - 1), length.out = length(education_levels))
  
  edu_scores <- sapply(edu, function(e) {
    score <- education_scores[match(e, education_levels)]
    if (is.na(score)) stop(paste("Invalid education level:", e))
    return(score)
  })
  
  parent_income_scores <- parent_income / pmax(200000, parent_income)
  random_scores <- runif(n)
  combined_scores <- 0.5 * (edu_scores + parent_income_scores) + 0.5 * random_scores
  
  base_thresholds <- quantile(combined_scores, probs = cumsum(proportions), names = FALSE)
  noise <- runif(length(base_thresholds), min = -0.02, max = 0.02)
  thresholds <- sort(c(0, base_thresholds + noise, max(combined_scores) + 0.01))
  
  job_types <- sapply(combined_scores, function(score) {
    index <- findInterval(score, thresholds, rightmost.closed = TRUE)
    if (index > length(job_type_cats)) {
      index <- length(job_type_cats)  # Handle edge case
    }
    job_type_cats[index]
  })
  
  return(job_types)
}

simulate_condition <- function(n, predictor, target_prevalence, correlation_weight) {
  # Blend predictor with random noise to achieve target correlation
  random_component <- runif(n)
  blended_predictor <- (1 - correlation_weight) * random_component + 
    correlation_weight * predictor
  
  # Assign outcomes based on prevalence
  num_positive <- round(target_prevalence * n)
  outcomes <- rep(0, n)
  ranked_indices <- order(blended_predictor, decreasing = TRUE)
  outcomes[ranked_indices[1:num_positive]] <- 1
  
  outcomes
}

simulate_obesity <- function(n, income, education, state_pec, age, target_prevalence, correlation_weight = 0.15) {
  # Define weights for inputs
  weights <- c(-0.3, -0.2, -0.3, 0.4)  # Income, Education, State PEC = less likely; Age = more likely
  
  # Normalize inputs and calculate linear predictor
  income_scaled <- normalize_to_0_1(income)
  education_scaled <- as.numeric(as.factor(education)) / max(as.numeric(as.factor(education)))
  state_pec_scaled <- normalize_to_0_1(state_pec)
  age_scaled <- normalize_to_0_1(age)
  
  predictor <- weights[1] * income_scaled +
    weights[2] * education_scaled +
    weights[3] * state_pec_scaled +
    weights[4] * age_scaled
  
  # Use helper function to simulate condition
  simulate_condition(n, predictor, target_prevalence, correlation_weight)
}

simulate_multiple_gestation <- function(n, age, obesity, target_prevalence, correlation_weight = 0.15) {
  weights <- c(0.4, 0.6)  # AGE = higher, OBE = higher
  predictor <- weights[1] * normalize_to_0_1(age) + 
    weights[2] * normalize_to_0_1(obesity)
  simulate_condition(n, predictor, target_prevalence, correlation_weight)
}

simulate_diabetes <- function(n, age, obesity, income, target_prevalence, correlation_weight = 0.15) {
  weights <- c(0.4, 0.5, -0.2)  # AGE = higher, OBE = higher, INC = lower
  predictor <- weights[1] * normalize_to_0_1(age) + 
    weights[2] * normalize_to_0_1(obesity) + 
    weights[3] * normalize_to_0_1(income)
  simulate_condition(n, predictor, target_prevalence, correlation_weight)
}

simulate_heart_disease <- function(n, age, obesity, dm, target_prevalence, correlation_weight = 0.15) {
  weights <- c(0.3, 0.3, 0.4)  # AGE = higher, OBE = higher, DM = higher (bigger weight)
  predictor <- weights[1] * normalize_to_0_1(age) + 
    weights[2] * normalize_to_0_1(obesity) + 
    weights[3] * normalize_to_0_1(dm)
  simulate_condition(n, predictor, target_prevalence, correlation_weight)
}

simulate_placenta_previa <- function(n, age, mg, target_prevalence, correlation_weight = 0.15) {
  weights <- c(0.3, 0.7)  # AGE = higher, MG = higher (bigger weight)
  predictor <- weights[1] * normalize_to_0_1(age) + 
    weights[2] * normalize_to_0_1(mg)
  simulate_condition(n, predictor, target_prevalence, correlation_weight)
}

simulate_hypertension <- function(n, age, obesity, target_prevalence, correlation_weight = 0.15) {
  weights <- c(0.5, 0.5)  # AGE = higher, OBE = higher
  predictor <- weights[1] * normalize_to_0_1(age) + 
    weights[2] * normalize_to_0_1(obesity)
  simulate_condition(n, predictor, target_prevalence, correlation_weight)
}

simulate_gest_hypertension <- function(n, hypertension, mg, target_prevalence, correlation_weight = 0.15) {
  weights <- c(0.7, 0.3)  # HT = a lot higher, MG = higher
  predictor <- weights[1] * normalize_to_0_1(hypertension) + 
    weights[2] * normalize_to_0_1(mg)
  simulate_condition(n, predictor, target_prevalence, correlation_weight)
}

simulate_preeclampsia <- function(n, age, hypertension, gest_hypertension, mg, target_prevalence, correlation_weight = 0.15) {
  weights <- c(0.2, 0.4, 0.3, 0.1)  # AGE = higher, HT = higher (bigger weight), GHT = higher (bigger weight), MG = higher
  predictor <- weights[1] * normalize_to_0_1(age) + 
    weights[2] * normalize_to_0_1(hypertension) + 
    weights[3] * normalize_to_0_1(gest_hypertension) + 
    weights[4] * normalize_to_0_1(mg)
  simulate_condition(n, predictor, target_prevalence, correlation_weight)
}

simulate_dependents <- function(n, income, job_type, age) {
  job_type_dependents <- c(
    "unemployed" = 3,
    "unskilled" = 2.5,
    "trade" = 2,
    "professional" = 1.5
  )
  
  job_dependents <- sapply(job_type, function(j)
    job_type_dependents[match(j, names(job_type_dependents))])
  
  income_effect <- 1 / (1 + normalize_to_0_1(income))
  age_effect <- normalize_to_0_1(age)
  
  base_dependents <- job_dependents * income_effect + age_effect
  
  skewed_random <- rlnorm(n, meanlog = log(1), sdlog = 0.3)
  dependents <- base_dependents * skewed_random
  
  dependents <- round(pmin(dependents, 8))
  if (median(dependents) > 2)
    dependents <- pmax(dependents - 1, 0)
  
  dependents
}

simulate_insurance <- function(n, job_type_cats, job_type, state_pec, age) {
  job_type_probs <- data.frame(
    job_type = job_type_cats,
    no_insurance = c(0.7, 0.5, 0.4, 0.1),
    state_provided = c(0.7, 0.3, 0.3, 0.1),
    private = c(0.1, 0.2, 0.4, 0.7)
  )
  
  state_pec_scaled <- normalize_to_0_1(state_pec)
  age_scaled <- normalize_to_0_1(age)
  
  state_adj <- pmax(0, 0.3 * state_pec_scaled + 0.3 * age_scaled)
  private_adj <- pmax(0, 0.4 * state_pec_scaled + 0.2 * age_scaled)
  no_insurance_adj <- pmax(0, 1 - state_adj - private_adj)
  
  # Match job type with probabilities
  job_effect <- t(sapply(job_type, function(j) {
    probs <- job_type_probs[job_type_probs$job_type == j, -1]
    if (nrow(probs) == 0) stop(paste("Invalid job type:", j))
    as.numeric(probs)
  }))
  
  probabilities <- data.frame(
    no_insurance = no_insurance_adj * job_effect[, 1],
    state_provided = state_adj * job_effect[, 2],
    private = private_adj * job_effect[, 3]
  )
  
  probabilities <- normalize_rows_to_1(probabilities)
  
  apply(probabilities, 1, function(p) {
    sample(c("no_insurance", "state_provided", "private"), 1, prob = p)
  })
}

simulate_distance <- function(n, state_pec) {
  distances <- rexp(n, rate = 1 / 10) * (1 - abs(state_pec) * 0.1)
  
  distances / median(distances) * 10
}

generate_income <- function(n, job_type, edu, race, pec, age) {
  job_type_income <- c(
    "unemployed" = 10000,
    "unskilled" = 40000,
    "trade" = 80000,
    "professional" = 140000
  )
  
  edu_multiplier <- c(
    "less_than_hs" = 0.7,
    "hs" = 0.9,
    "some_college" = 1.0,
    "college" = 1.2,
    "post_grad" = 1.5
  )
  
  race_multiplier <- c(
    "white" = 1.0,
    "asian" = 1.3,
    "hispanic" = 0.75,
    "black" = 0.65,
    "aian" = 0.6,
    "nhpi" = 0.85,
    "other" = 0.8
  )
  
  age_multiplier <- 1 + (age - mean(age)) * 0.005
  pec_multiplier <- 1 + (pec - mean(pec)) * 0.02
  
  # Match categories explicitly to avoid NA
  base_income <- job_type_income[match(job_type, names(job_type_income))]
  edu_multiplier <- edu_multiplier[match(edu, names(edu_multiplier))]
  race_multiplier <- race_multiplier[match(race, names(race_multiplier))]
  
  # Ensure no NAs from mismatches
  if (any(is.na(base_income) | is.na(edu_multiplier) | is.na(race_multiplier))) {
    stop("Invalid category found in job_type, edu, or race")
  }
  
  # Calculate deterministic component of income
  income <- base_income *
    edu_multiplier *
    race_multiplier *
    age_multiplier *
    pec_multiplier
  
  # Add positively skewed noise
  positive_noise <- rlnorm(n, meanlog = 0, sdlog = 0.5) * 10000
  
  # Add negative noise to bring some incomes to zero
  negative_noise <- rnorm(n, mean = -20000, sd = 5000) * (runif(n) < 0.1)
  
  # Calculate final income
  income <- income + positive_noise + negative_noise
  
  # Ensure no negative incomes
  income <- pmax(income, 0)
  
  return(round(income, 2))
}

extract_formula_vars <- function(formula) {
  # Extract all terms (variables) from the formula
  terms <- all.vars(formula)
  
  # Separate dependent and independent variables
  list(
    y = terms[1],
    x = terms[-1],
    all = terms
  )
}

gen_dag <- function(formula, outcome, labels) {
  dagify(formula, outcome=outcome, labels=labels) |> tidy_dagitty()
}

gen_dag_plot <- function(dag, labels, title, subtitle = NA, legend_width=12, legend_unit="cm", height=8) {
  legend_text <- paste(paste0("**", names(labels), ":** "), labels, collapse = "<br>")
  
  dagPlot <- ggdag(dag,  layout="circle") +
    geom_dag_edges() +
    geom_dag_text(aes(label = name), size = 4) +
    theme_dag() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      legend.position = "bottom"
    ) +
    ggtitle(title, subtitle = subtitle) 
  
  annotationPlot <- ggplot() +
    geom_textbox(
      aes(x = 0.5, y = 1),
      label = legend_text,
      halign = 0,
      valign = 1,
      size = 5,
      box.size = NA,
      fill = NA,
      width = unit(legend_width, "cm"),
      family = "sans",
      color = "black",
      lineheight = 1.2
    ) +
    theme_void() +
    theme(plot.margin = margin(10, 10, 10, 20)) 
  
  combinedPlot <- plot_grid(
    dagPlot, annotationPlot,
    ncol = 1, # Two columns
    rel_heights = c(2, 1) # Adjust proportions for balance
  )
  combinedPlot
}

save_dag_plot <- function(dag, title, width, height) {
  ggsave(paste0("images/", title, ".png"), dag, width = width, height = height, dpi = 300)
}


summarize_design <- function(dagFormula, plot_width=10, plot_height = 8) {
  
  design <- extract_formula_vars(dagFormula)
  labels <- dag_labels[design$all]
  label <- labels[design$y]
  filename <- str_replace_all(
    str_to_lower(label), c(
      " " = "_",
      "/" = "_",
      "\\(" = "",
      "\\)" = ""
    )
  )
  dag <- gen_dag(dagFormula, design$y, labels)
  dag_plot <- gen_dag_plot(dag,
                         labels=labels,
                         title=paste0("Causal Model of ", label),
                         subtitle=paste0("Hypothesized factors causing ", str_to_lower(label) ))
  save_dag_plot(dag_plot, paste0(filename, "_dag_plot"), width =plot_width, height=plot_height)
  
  list(
    filename = filename,
    label = label,
    dag = dag,
    dag_plot = dag_plot,
    image_path = paste0("images/", filename, "_dag_plot.png")
  )
}