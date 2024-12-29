"""
generate_synthetic_medical_data.py
----------------------------------
Generates synthetic medical data with user-controlled distributions,
including demographics, comorbidities, and simulated survival times.
"""

import numpy as np
import pandas as pd
from scipy.stats import norm, gamma, binom, uniform

def generate_synthetic_data(
    num_samples=1000,
    age_params=(60, 10),            # Normal: mean=60, std=10
    weight_params=(70, 15),         # Normal: mean=70, std=15
    length_of_stay_params=(2.0, 2), # Gamma: alpha=2.0, beta=2 (scale=2)
    comorbidity_list=None,         # e.g., ["diabetes", "hypertension", "cancer"]
    comorbidity_prevalences=None,  # e.g., [0.2, 0.3, 0.05]
    survival_shape=2.0,            # Survival time gamma shape
    survival_scale=5.0,            # Survival time gamma scale
    random_seed=42
):
    """
    Generates synthetic data based on various distributions commonly seen in medical research.
    For distribution references, see resources such as:
    [PubMed](https://pubmed.ncbi.nlm.nih.gov/).
    """
    np.random.seed(random_seed)
    
    # 1) Demographics
    ages = norm.rvs(loc=age_params[0], scale=age_params[1], size=num_samples).astype(int)
    weights = norm.rvs(loc=weight_params[0], scale=weight_params[1], size=num_samples)
    
    # 2) Hospital-related variable: length of stay
    length_of_stay = gamma.rvs(a=length_of_stay_params[0],
                               scale=length_of_stay_params[1],
                               size=num_samples)
    
    # 3) Comorbidities
    if comorbidity_list is None:
        comorbidity_list = ["diabetes", "hypertension", "cancer"]
    if comorbidity_prevalences is None:
        comorbidity_prevalences = [0.2, 0.3, 0.05]  # Adjust as desired

    # Build a dictionary that indicates presence (1) or absence (0).
    comorbidity_data = {}
    for comorbidity, prevalence in zip(comorbidity_list, comorbidity_prevalences):
        comorbidity_data[comorbidity] = binom.rvs(n=1, p=prevalence, size=num_samples)
    
    # 4) Survival time (simple random scenario):
    survival_times = gamma.rvs(a=survival_shape, scale=survival_scale, size=num_samples)
    
    # 5) Event occurrence (censored vs. event); let's assume uniform cutoff
    #    For example, ~30% events. Adjust logic as needed.
    event_occurred = binom.rvs(n=1, p=0.3, size=num_samples)
    
    # Assemble DataFrame
    data = pd.DataFrame({
        "age": ages,
        "weight": weights,
        "length_of_stay": length_of_stay,
        "survival_time": survival_times,
        "event_occurred": event_occurred
    })
    
    # Insert comorbidities
    for c_name, c_values in comorbidity_data.items():
        data[c_name] = c_values
    
    return data

if __name__ == "__main__":
    # Example usage
    df = generate_synthetic_data(num_samples=2000)
    print(df.head(10))