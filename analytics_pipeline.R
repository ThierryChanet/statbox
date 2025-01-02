# Load functions from tools folder
source("tools/analyze_data.R")

main_analysis <- function() {
  # Load data using function from tools/analyze_data.R
  data <- load_data()
  
  # Display help menu
  cat("\nWelcome to the Bayesian Analysis Pipeline")
  cat("\nEnter 'h' for detailed help/documentation")
  cat("\nEnter 'v' to see available variables\n")
  
  user_input <- readline(prompt = "Command (or press Enter to continue): ")
  
  if (user_input == "h") {
    cat("\nDetailed Documentation:")
    cat("\n----------------------")
    cat("\nThis analysis pipeline fits Bayesian models to medical data.")
    cat("\n\nModel Types:")
    cat("\n- linear: For continuous outcomes (e.g. length of stay)")
    cat("\n- logistic: For binary outcomes (e.g. event occurrence)")
    cat("\n- survival: For time-to-event data with censoring")
    cat("\n\nRequired Inputs:")
    cat("\n1. Response variable: The outcome you want to model")
    cat("\n2. Predictor variables: The features used to predict the outcome")
    cat("\n3. Model type: The type of analysis to perform")
    cat("\n\nPress Enter to continue...\n")
    readline()
  }
  
  if (user_input == "v" || user_input == "h") {
    cat("\nAvailable Variables:\n")
    print(names(data))
    cat("\nVariable Types:")
    cat("\nResponse variables: survival_time, event_occurred, length_of_stay")
    cat("\nPredictor variables: age, diabetes, hypertension, cancer\n")
  }
  
  # Get response variable
  cat("\nResponse Variable Selection")
  response_vars <- names(data)
  for (i in seq_along(response_vars)) {
    cat(sprintf("\n%d. %s", i, response_vars[i]))
  }
  selection <- as.numeric(readline(prompt = "\nSelect response variable number: "))
  
  if (is.na(selection) || selection < 1 || selection > length(response_vars)) {
    stop("Invalid selection")
  }
  response <- response_vars[selection]
  
  # Get predictor variables
  cat("\nPredictor Variable Selection")
  # Remove response variable from predictor options
  predictor_vars <- response_vars[response_vars != response]
  for (i in seq_along(predictor_vars)) {
    cat(sprintf("\n%d. %s", i, predictor_vars[i]))
  }
  cat("\nEnter numbers separated by spaces (e.g. '1 2')")
  
  pred_nums <- as.numeric(strsplit(readline(prompt = "\nSelect predictor numbers: "), " ")[[1]])
  
  if (any(is.na(pred_nums)) || any(pred_nums < 1) || any(pred_nums > length(predictor_vars))) {
    stop("Invalid selection")
  }
  predictors <- predictor_vars[pred_nums]
  
  # Get model type
  cat("\nModel Type Selection")
  model_types <- c("linear", "logistic", "survival")
  for (i in seq_along(model_types)) {
    cat(sprintf("\n%d. %s", i, model_types[i]))
  }
  cat("\nNote: Should match response variable type")
  model_selection <- as.numeric(readline(prompt = "\n\nSelect model type (1-3): "))
  
  if (is.na(model_selection) || model_selection < 1 || model_selection > length(model_types)) {
    stop("Invalid selection")
  }
  model_type <- model_types[model_selection]
  
  # Validate inputs
  if (!response %in% names(data)) {
    stop("Invalid response variable")
  }
  if (!all(predictors %in% names(data))) {
    stop("Invalid predictor variable(s)")
  }
  if (!model_type %in% c("linear", "logistic", "survival")) {
    stop("Invalid model type")
  }
  
  # Fit model using function from tools/analyze_data.R
  fit <- fit_bayesian_model(data, response, predictors, model_type)
  
  # Plot results using function from tools/analyze_data.R
  plot_model_results(fit, c("beta", "sigma"))
  
  return(list(
    data = data,
    fit = fit
  ))
}

# Execute main function when script is run directly
if (!interactive()) {
  main()
}
