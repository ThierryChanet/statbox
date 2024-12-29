# lab_analysis.R
#
# This script performs statistical analysis on synthetic medical data, including:
# - Loading/generating synthetic patient data with comorbidities and survival times
# - Running survival analysis using Cox proportional hazards and Kaplan-Meier curves
# - Comparing traditional and Bayesian regression approaches
#
# The script assumes analytics_pipeline.R and setup.R have been sourced.
# Dependencies: survival, survminer, brms, dplyr, ggplot2, reticulate packages

##### Setup
path <- "~/Documents/code/statbox/"
# We assume "analytics_pipeline.R" has been sourced or is in the same project:
source(paste0(path,"analytics_pipeline.R"))

# Source setup script to configure environment and install required packages
source(paste0(path,"analytics_pipeline.R"))
  
##### Load or generate synthetic data
#' Load existing or generate new synthetic medical data
#' 
#' @param use_existing Logical indicating whether to use existing data file (TRUE) or generate new data (FALSE)
#' @param filename Name of the CSV file containing/to contain the synthetic data
#' @return Processed dataframe containing synthetic medical data
preprocess_synthetic_data <- function(synthetic_data) {
  # Convert to R dataframe and apply initial processing
  synthetic_df <- as.data.frame(synthetic_data)
  synthetic_df <- basic_pipeline(synthetic_df)    # Apply basic data processing
  
  return(synthetic_df)
}

get_synthetic_data <- function(
  use_existing = TRUE,
  filename = "synthetic_medical_data.csv") 
{
  # Construct full path to data file
  data_path <- file.path(path, "data", "generated_data", filename)
  
  if (use_existing && file.exists(data_path)) {
    # Load existing data from CSV
    synthetic_data <- read.csv(data_path)
  } else {
    # Import Python module and generate new data
    py_run_file("generate_synthetic_medical_data.py")
    
    # Set parameters for synthetic data generation
    params <- list(
      num_samples = as.numeric(Sys.getenv("NUM_SYNTHETIC_SAMPLES", 2000)),
      age_params = c(60, 10),            # mean age 60, sd 10 
      weight_params = c(70, 15),         # mean weight 70kg, sd 15
      length_of_stay_params = c(2.0, 2), # gamma params for hospital stay
      comorbidity_list = c("diabetes", "hypertension", "cancer"),
      comorbidity_prevalences = c(0.2, 0.3, 0.05),
      survival_shape = 2.0,              # gamma distribution shape for survival time
      survival_scale = 5.0,              # gamma distribution scale for survival time
      random_seed = as.numeric(Sys.getenv("RANDOM_SEED", 42))
    )
    
    # Generate synthetic data using Python function
    synthetic_data <- py$generate_synthetic_data(
      num_samples = params$num_samples,
      age_params = params$age_params,
      weight_params = params$weight_params, 
      length_of_stay_params = params$length_of_stay_params,
      comorbidity_list = params$comorbidity_list,
      comorbidity_prevalences = params$comorbidity_prevalences,
      survival_shape = params$survival_shape,
      survival_scale = params$survival_scale,
      random_seed = params$random_seed
    )
  }
  
  return(synthetic_df)
}

# Get the data (set use_existing=FALSE to force regeneration)
synthetic_df <- get_synthetic_data(use_existing = TRUE)

# Print the first few rows of the data for debugging
cat("Synthetic data frame:\n")
print(head(synthetic_df))


### Preprocessing
# Filtering the data
clean <- preprocess_synthetic_data(synthetic_df) 

# Print the first few rows of the preprocessed data for debugging
cat("Applying preprocessing:\n  ")
print(head(clean))

# Create study groups
clean_groups <- create_groups(clean)

# Print the first few rows of the preprocessed data for debugging
cat("Creating study groups:\n  ")
print(head(clean_groups))


### Analysis

#' Perform survival analysis using Cox model and Kaplan-Meier curves
#' 
#' @param data Processed dataframe containing survival data
#' @return List containing Cox model summary and Kaplan-Meier plot
run_survival_analysis <- function(data) {
 
  
  # Build survival object from time and event data
  s_obj <- Surv(time = data$survival_time, event = data$event_occurred)
  
  # Fit Cox proportional hazards model
  fit_cox <- coxph(s_obj ~ study_group + age + num_comorbidities, data = data)
  summary_cox <- summary(fit_cox)
  
  # Generate Kaplan-Meier survival curves
  fit_km <- survfit(s_obj ~ study_group, data = data)
  
  # Create survival plot with p-value
  ggsurv <- ggsurvplot(fit_km, data = data, pval = TRUE)
  
  # Return analysis results
  return(list(
    cox_summary = summary_cox,
    km_plot = ggsurv
  ))
}

#' Compare traditional and Bayesian regression approaches
#' 
#' @param data Processed dataframe containing medical data
#' @param prior_spec Optional list of prior specifications for Bayesian model
#' @return List containing summaries of both regression models
run_regression_comparison <- function(data, prior_spec = NULL) {
  # Fit traditional logistic regression
  standard_model <- glm(event_occurred ~ study_group + age + num_comorbidities,
                       data = data, family = binomial(link = "logit"))
  
  # Set up priors for Bayesian model if not provided
  if (is.null(prior_spec)) {
    prior_spec <- c(
      set_prior("normal(0, 2)", class = "b"),     # weakly informative prior for coefficients
      set_prior("student_t(3, 0, 10)", class = "Intercept")
    )
  }
  
  # Fit Bayesian logistic regression using brms
  bayes_model <- brm(
    formula = event_occurred ~ study_group + age + num_comorbidities,
    data = data,
    family = bernoulli(link = "logit"),
    prior = prior_spec,
    chains = 2, cores = 2, iter = 2000
  )
  
  # Return model summaries
  return(list(
    glm_summary = summary(standard_model),
    brms_summary = summary(bayes_model)
  ))
}

# Example usage:
# pipeline_result <- basic_pipeline(create_groups(synthetic_df))
# survival_res <- run_survival_analysis(pipeline_result)
# regression_res <- run_regression_comparison(pipeline_result)
# print(survival_res$cox_summary)   # coxph result
# print(regression_res$glm_summary) # logistic regression
# print(regression_res$brms_summary) # Bayesian logistic
