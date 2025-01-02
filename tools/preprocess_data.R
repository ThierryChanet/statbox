# analytics_pipeline.R

#' Round numeric columns in a dataframe
#' @param data Dataframe containing numeric columns to round
#' @param decimals Number of decimal places (default = 2)
#' @return Dataframe with rounded numeric columns
round_numeric_cols <- function(data, decimals = 2) {
  numeric_cols <- sapply(data, is.numeric)
  data[numeric_cols] <- round(data[numeric_cols], decimals)
  return(data)
}

#' Convert binary 0/1 columns to logical TRUE/FALSE
#' @param data Dataframe containing binary columns
#' @return Dataframe with binary columns converted to logical
convert_binary_to_logical <- function(data) {
  # Find columns that only contain 0s and 1s
  binary_cols <- sapply(data, function(x) {
    all(x %in% c(0, 1, NA))
  })
  
  # Convert identified binary columns to logical
  data[binary_cols] <- lapply(data[binary_cols], as.logical)
  return(data)
}

# basic_pipeline():
#   Applies a chain of transformations (could incorporate comorbidity summary, etc.)
basic_pipeline <- function(data) {
  # Ask whether to convert binary columns
  convert_binary <- tolower(readline(prompt = "Convert binary columns to logical? (y/n): "))
  if (convert_binary == "y") {
    data <- convert_binary_to_logical(data)
  }
  
  return(data)
}

#' Main function to preprocess data files
#' 
#' @param input_subfolder Subfolder within "data" directory containing input files
#' @param output_subfolder Subfolder within "data" directory for preprocessed output
#' @return Path to preprocessed output file
main <- function(input_subfolder = "generated_data", output_subfolder = "preprocessed_data") {
  # Get user selection for input subfolder
  data_subfolders <- list.dirs("data", full.names = FALSE, recursive = FALSE)
  
  if (length(data_subfolders) == 0) {
    stop("No subfolders found in data directory")
  }
  
  cat("\nAvailable data subfolders for input files:\n")
  for (i in seq_along(data_subfolders)) {
    cat(sprintf("%d. %s\n", i, data_subfolders[i]))
  }
  
  # Get user selection
  subfolder_selection <- as.numeric(readline(prompt = "Select input subfolder number: "))
  
  if (is.na(subfolder_selection) || subfolder_selection < 1 || subfolder_selection > length(data_subfolders)) {
    stop("Invalid subfolder selection")
  }
  
  input_subfolder <- data_subfolders[subfolder_selection]
  # Get full path to data folders
  data_dir <- file.path("data", input_subfolder)
  output_dir <- file.path("data", output_subfolder)
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
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
  selection <- as.numeric(readline(prompt = "Select file number to process: "))
  
  if (is.na(selection) || selection < 1 || selection > length(csv_files)) {
    stop("Invalid selection")
  }
  
  selected_file <- csv_files[selection]
  
  # Read and process data
  input_path <- file.path(data_dir, selected_file)
  data <- read.csv(input_path)
  
  # Apply preprocessing pipeline
  clean <- basic_pipeline(data)
  
  # Ask whether to round numeric columns
  round_numeric <- tolower(readline(prompt = "Round numeric columns? (y/n): "))
  if (round_numeric == "y") {
    # Get decimal places from user (default to 2 if no input)
    decimals <- as.numeric(readline(prompt = "Enter number of decimal places (default: 2): "))
    if (is.na(decimals)) decimals <- 2
    
    # Round numeric columns
    clean <- round_numeric_cols(clean, decimals)
  }
  
  # Generate output filename with "clean_" prefix
  output_filename <- paste0("clean_", selected_file)
  output_path <- file.path(output_dir, output_filename)
  # Ask if user wants to modify output filename
  modify_filename <- tolower(readline(prompt = sprintf("Modify output filename '%s'? (y/n): ", output_filename)))
  if (modify_filename == "y") {
    new_filename <- readline(prompt = "Enter new filename (without .csv extension): ")
    if (nchar(new_filename) > 0) {
      output_filename <- paste0(new_filename, ".csv")
    }
  }  
  # Save preprocessed data
  write.csv(clean, output_path, row.names = FALSE)
  
  cat(sprintf("\nPreprocessed data saved to: %s\n", output_path))
  return(output_path)
}


# Example usage:
# processed_file <- main("input_data", "preprocessed_data")
