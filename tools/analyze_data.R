library(rstan)
library(ggplot2)
library(bayesplot)
library(cowplot)

#' Load and validate input data
#' @param input_subfolder Subfolder within "data" directory containing input files
#' @return Selected data frame
load_data <- function(input_subfolder = "cleaned_data") {
  data_dir <- file.path("data", input_subfolder)
  
  # List available CSV files
  csv_files <- list.files(data_dir, pattern = "\\.csv$")
  
  if (length(csv_files) == 0) {
    stop("No CSV files found in ", data_dir)
  }
  
  # Print available files
  cat("\nAvailable data files:\n")
  for (i in seq_along(csv_files)) {
    cat(sprintf("%d. %s\n", i, csv_files[i]))
  }
  
  # Get user selection
  selection <- as.numeric(readline(prompt = "Select file number to analyze: "))
  
  if (is.na(selection) || selection < 1 || selection > length(csv_files)) {
    stop("Invalid selection")
  }
  
  selected_file <- csv_files[selection]
  input_path <- file.path(data_dir, selected_file)
  
  data <- read.csv(input_path)
  return(data)
}

#' Fit Bayesian model using Stan
#' @param data Input data frame
#' @param response Response variable name
#' @param predictors Vector of predictor variable names
#' @param model_type Type of model ("linear", "logistic", or "survival")
#' @return Stan model fit
fit_bayesian_model <- function(data, response, predictors, model_type = "linear") {
  
  # Prepare data for Stan
  X <- as.matrix(data[, predictors, drop = FALSE])
  y <- data[[response]]
  
  stan_data <- list(
    N = nrow(X),
    K = ncol(X),
    X = X,
    y = y
  )
  
  # Select Stan model based on type
  if (model_type == "linear") {
    model_code <- "
    data {
      int<lower=0> N;
      int<lower=0> K;
      matrix[N, K] X;
      vector[N] y;
    }
    parameters {
      vector[K] beta;
      real<lower=0> sigma;
    }
    model {
      beta ~ normal(0, 10);
      sigma ~ cauchy(0, 5);
      y ~ normal(X * beta, sigma);
    }
    "
  } else if (model_type == "logistic") {
    model_code <- "
    data {
      int<lower=0> N;
      int<lower=0> K;
      matrix[N, K] X;
      int<lower=0,upper=1> y[N];
    }
    parameters {
      vector[K] beta;
    }
    model {
      beta ~ normal(0, 10);
      y ~ bernoulli_logit(X * beta);
    }
    "
  } else if (model_type == "survival") {
    model_code <- "
    data {
      int<lower=0> N;
      int<lower=0> K;
      matrix[N, K] X;
      vector[N] y;
    }
    parameters {
      vector[K] beta;
      real<lower=0> alpha;
    }
    model {
      beta ~ normal(0, 10);
      alpha ~ cauchy(0, 5);
      y ~ weibull(alpha, exp(X * beta));
    }
    "
  }
  
  # Compile and fit model
  fit <- stan(model_code = model_code, data = stan_data, 
             iter = 2000, chains = 4)
  
  return(fit)
}

#' Plot model diagnostics and results
#' @param fit Stan model fit
#' @param params Parameters to plot
plot_model_results <- function(fit, params = NULL) {
  # Trace plots
  mcmc_trace(fit, pars = params)
  
  # Posterior distributions
  mcmc_areas(fit, pars = params)
  
  # Parameter summary
  print(summary(fit)$summary)
}

#' Generate diagnostic plots for model parameters
#' @param fit Stan model fit object
#' @param params Vector of parameter names to plot. If NULL, plots all parameters.
#' @param save_path Optional path to save plots. If NULL, displays plots interactively.
#' @return List of generated plots
generate_diagnostic_plots <- function(fit, params = NULL, save_path = NULL) {
  # If no parameters specified, get all parameters from fit
  if (is.null(params)) {
    params <- names(fit)
  }
  
  # Create combined plots for each parameter
  plots <- list()
  for (param in params) {
    # Create trace plot
    trace <- mcmc_trace(fit, pars = param) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        plot.margin = margin(5, 5, 5, 5),
        legend.position = "none"
      )
    
    # Create density plot rotated 90 degrees
    density <- mcmc_dens_overlay(fit, pars = param) +
      theme_minimal() +
      coord_flip() +
      theme(
        axis.title = element_blank(),
        plot.margin = margin(5, 5, 5, 5)
      )
    
    # Combine plots side by side with wider trace plot
    combined_plot <- plot_grid(
      trace, density,
      ncol = 2,
      align = "h",
      rel_widths = c(1.2, 0.4)  # Increased width for trace plot
    ) +
      ggtitle(paste("Diagnostic Plots for", param)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    plots[[param]] <- combined_plot
    
    # Save plot if path provided
    if (!is.null(save_path)) {
      current_date <- format(Sys.Date(), "%Y%m%d")
      
      # Get list of existing files and find next number for this run
      if (!exists("current_run_num")) {
        existing_files <- list.files(save_path, pattern = paste0(current_date, "_\\d+"))
        if (length(existing_files) == 0) {
          current_run_num <- 1
        } else {
          nums <- as.numeric(gsub(paste0(".*", current_date, "_(\\d+).*"), "\\1", existing_files))
          current_run_num <- max(nums) + 1
        }
      }
      
      ggsave(
        file.path(save_path, sprintf("diagnostic_%s_%03d_%s.pdf", current_date, current_run_num, param)),
        combined_plot
      )
    }
  }
  
  return(plots)
}

