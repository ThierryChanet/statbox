import os
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

def load_data(file_path):
    """Load data from a CSV file."""
    return pd.read_csv(file_path)

def describe_data(df):
    """Print basic statistics and descriptions of the dataframe."""
    print("\nColumn Descriptions:")
    print(df.describe(include='all'))

def plot_distributions(df):
    """Plot distribution of numerical columns."""
    num_cols = df.select_dtypes(include=['number']).columns
    df[num_cols].hist(bins=15, figsize=(15, 6), layout=(2, -1))
    plt.suptitle('Distribution of Numerical Columns')
    plt.show()

def plot_correlation_matrix(df):
    """Plot a heatmap of the correlation matrix for selected columns."""
    # Get numeric and boolean columns as candidates
    candidate_cols = df.select_dtypes(include=['number', 'bool']).columns
    
    if len(candidate_cols) == 0:
        print("No numeric or boolean columns found for correlation analysis")
        return
        
    # Show available columns and get user input
    print("\nAvailable columns for correlation analysis:")
    for i, col in enumerate(candidate_cols, 1):
        print(f"{i}. {col}")
    
    print("\nPress Enter to keep all columns, or enter numbers of columns to exclude (separated by spaces):")
    user_input = input()
    
    if user_input.strip():
        # Convert input to list of indices and remove selected columns
        try:
            exclude_idx = [int(x)-1 for x in user_input.split()]
            selected_cols = [col for i, col in enumerate(candidate_cols) if i not in exclude_idx]
        except ValueError:
            print("Invalid input. Using all columns.")
            selected_cols = candidate_cols
    else:
        selected_cols = candidate_cols
    
    if len(selected_cols) < 2:
        print("Need at least 2 columns for correlation analysis")
        return
        
    plt.figure(figsize=(10, 8))
    corr = df[selected_cols].corr()
    sns.heatmap(corr, annot=True, fmt=".2f", cmap='coolwarm', cbar=True)
    plt.title('Correlation Matrix')
    plt.xticks(rotation=45)
    plt.yticks(rotation=0)
    plt.tight_layout()
    plt.show()

def main():
    # Define the path to the data folder
    def get_data_path(subfolder="preprocessed_data"):
        """Get the full path to the data folder."""
        return os.path.join("data", subfolder)
    
    data_folder = get_data_path()
    
    # List all CSV files in the data folder
    files = [f for f in os.listdir(data_folder) if f.endswith('.csv')]
    
    if not files:
        print("No CSV files found in the data folder.")
        return
    
    print("\nAvailable CSV files:")
    for i, file in enumerate(files, 1):
        print(f"{i}. {file}")
    
    # Get user input for file selection
    while True:
        try:
            selection = int(input("\nSelect a file number to analyze: "))
            if 1 <= selection <= len(files):
                break
            print(f"Please enter a number between 1 and {len(files)}")
        except ValueError:
            print("Please enter a valid number")
    
    # Load the selected CSV file
    file_path = os.path.join(data_folder, files[selection-1])
    df = load_data(file_path)
    
    # Describe the data
    describe_data(df)
    
    # Plot distributions
    plot_distributions(df)
    
    # Plot correlation matrix for comorbidities
    plot_correlation_matrix(df)

if __name__ == "__main__":
    main()