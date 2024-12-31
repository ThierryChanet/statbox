"""
generate_synthetic_medical_data.py
----------------------------------
Generates synthetic medical data with user-controlled distributions,
including demographics, comorbidities, and simulated survival times.
"""

import numpy as np
import pandas as pd
from scipy.stats import norm, gamma, binom, uniform
import os
import datetime

def generate_correlated_variables(n, mean1, std1, mean2, std2, correlation):
    """
    Generate two correlated normally distributed variables.
    
    Args:
        n: Number of samples
        mean1, std1: Parameters for first variable
        mean2, std2: Parameters for second variable  
        correlation: Desired correlation coefficient (-1 to 1)
    """
    # Generate uncorrelated variables
    x1 = np.random.normal(0, 1, n)
    x2 = np.random.normal(0, 1, n)
    
    # Create correlation
    x2 = correlation * x1 + np.sqrt(1 - correlation**2) * x2
    
    # Transform to desired distributions
    var1 = x1 * std1 + mean1
    var2 = x2 * std2 + mean2
    
    return var1, var2

def generate_synthetic_data(
    num_samples=1000,
    age_params=(60, 10),            # Normal: mean=60, std=10
    weight_params=(70, 15),         # Normal: mean=70, std=15
    length_of_stay_params=(2.0, 2), # Gamma: alpha=2.0, beta=2 (scale=2)
    comorbidity_list=None,         # e.g., ["diabetes", "hypertension", "cancer"]
    comorbidity_prevalences=None,  # e.g., [0.2, 0.3, 0.05]
    survival_shape=2.0,            # Survival time gamma shape
    survival_scale=5.0,            # Survival time gamma scale
    correlated_vars=None,          # Dict with correlation parameters
    random_seed=42
):
    """
    Generates synthetic data based on various distributions commonly seen in medical research.
    For distribution references, see resources such as:
    [PubMed](https://pubmed.ncbi.nlm.nih.gov/).
    
    Args:
        ...
        correlated_vars: Optional dict specifying correlation between two variables:
            {
                'var1': {'name': str, 'mean': float, 'std': float},
                'var2': {'name': str, 'mean': float, 'std': float},
                'correlation': float (-1 to 1)
            }
    """
    np.random.seed(random_seed)
    
    # Initialize data dictionary
    data_dict = {}
    
    # Handle correlated variables if specified
    if correlated_vars:
        var1, var2 = generate_correlated_variables(
            num_samples,
            correlated_vars['var1']['mean'],
            correlated_vars['var1']['std'],
            correlated_vars['var2']['mean'],
            correlated_vars['var2']['std'],
            correlated_vars['correlation']
        )
        data_dict[correlated_vars['var1']['name']] = var1
        data_dict[correlated_vars['var2']['name']] = var2
    else:
        # Generate independent variables as before
        data_dict["age"] = norm.rvs(loc=age_params[0], scale=age_params[1], size=num_samples).astype(int)
        data_dict["weight"] = norm.rvs(loc=weight_params[0], scale=weight_params[1], size=num_samples)
    
    # Hospital-related variable: length of stay
    data_dict["length_of_stay"] = gamma.rvs(a=length_of_stay_params[0],
                                           scale=length_of_stay_params[1],
                                           size=num_samples)
    
    # Comorbidities
    if comorbidity_list is None:
        comorbidity_list = ["diabetes", "hypertension", "cancer"]
    if comorbidity_prevalences is None:
        comorbidity_prevalences = [0.2, 0.3, 0.05]  # Adjust as desired

    # Build comorbidity data
    for comorbidity, prevalence in zip(comorbidity_list, comorbidity_prevalences):
        data_dict[comorbidity] = binom.rvs(n=1, p=prevalence, size=num_samples)
    
    # Survival time and event occurrence
    data_dict["survival_time"] = gamma.rvs(a=survival_shape, scale=survival_scale, size=num_samples)
    data_dict["event_occurred"] = binom.rvs(n=1, p=0.3, size=num_samples)
    
    return pd.DataFrame(data_dict)

def export_synthetic_data(df, filename=None):
    """
    Exports the synthetic medical data to a CSV file in the data/generated_data directory.
    The filename includes the current date if no filename is provided.
    
    Args:
        df: pandas DataFrame containing the synthetic medical data
        filename: name of the CSV file to create (default: None, will use date-based name)
    """
    # Create data/generated_data directory if it doesn't exist
    output_dir = "data/generated_data"
    os.makedirs(output_dir, exist_ok=True)
    
    # Generate default filename with date if none provided
    if filename is None:
        today = datetime.date.today().strftime("%Y%m%d")
        filename = f"synthetic_medical_data_{today}.csv"
    else:
        # Add .csv extension if not present
        if not filename.endswith('.csv'):
            filename = f"{filename}.csv"
    
    # Construct full output path
    output_path = os.path.join(output_dir, filename)
    
    # Export to CSV
    df.to_csv(output_path, index=False)
    print(f"Data exported to {output_path}")


if __name__ == "__main__":
    # Get parameters via user input
    samples = int(input("Enter number of samples to generate (default: 2000): ") or "2000")
    filename = input("Enter output filename (press Enter for date-based name): ") or None
    survival_shape = float(input("Enter shape parameter for survival time distribution (default: 2.0): ") or "2.0")
    survival_scale = float(input("Enter scale parameter for survival time distribution (default: 5.0): ") or "5.0")
    
    # Ask about correlated variables
    correlate = input("Generate correlated variables? (y/n, default: n): ").lower() == 'y'
    
    if correlate:
        corr_params = {
            'var1': {
                'name': input("Enter name for first variable: "),
                'mean': float(input("Enter mean for first variable: ")),
                'std': float(input("Enter standard deviation for first variable: "))
            },
            'var2': {
                'name': input("Enter name for second variable: "),
                'mean': float(input("Enter mean for second variable: ")),
                'std': float(input("Enter standard deviation for second variable: "))
            },
            'correlation': float(input("Enter desired correlation coefficient (-1 to 1): "))
        }
    else:
        corr_params = None
    
    # Generate data with specified parameters
    df = generate_synthetic_data(
        num_samples=samples,
        survival_shape=survival_shape, 
        survival_scale=survival_scale,
        correlated_vars=corr_params
    )
    print(df.head(10))
    export_synthetic_data(df, filename=filename)