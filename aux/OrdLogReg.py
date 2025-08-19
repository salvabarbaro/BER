import pandas as pd
import pyreadstat
import numpy as np
import statsmodels.api as sm
from statsmodels.miscmodels.ordinal_model import OrderedModel
import matplotlib.pyplot as plt
import seaborn as sns

# Load the data from Stata file
data, meta = pyreadstat.read_dta("/home/sbarbaro/Documents/Research/Yardstick/BER/ReplicationFiles/Main_data_set_replication.dta")

# Filter the data and create the 'timetrend' variable
df1 = data[data['wave'].isin([2, 3])].copy()
df1['timetrend'] = (pd.to_datetime(df1['date']) - pd.to_datetime(df1['date']).min()).dt.days

# Rename 'index' to 'index_var' to avoid conflict with pandas index
df1 = df1.rename(columns={'index': 'index_var'})

# Convert 'index_var' to an ordered categorical variable
df1['index_var'] = pd.Categorical(df1['index_var'], ordered=True)

# Define the model formulas
mod1 = 'index_var ~ lninc + lnincfed'
mod2 = 'index_var ~ lninc + lnincfed + timetrend'
mod3 = 'index_var ~ lninc + lnincfed + east + berlin + timetrend'
mod4 = 'index_var ~ lninc + lnincfed + att_t_fed + FKM21 + econ_strength + east + berlin + timetrend'
mod5 = 'index_var ~ lninc + lnincfed + lnvac + lnvacfed + prevac + att_t_fed + FKM21 + econ_strength + east + berlin + timetrend'

# List of model formulas
models = [mod1, mod2, mod3, mod4, mod5]

# Define a function to run the ordinal regression using OrderedModel
def clm_fun(formula, data):
    model = OrderedModel.from_formula(formula, data, distr='logit')
    result = model.fit(method='bfgs', disp=False)
    return result

# Run all the models
res_main = [clm_fun(mod, df1) for mod in models]

# Display the summary of the first model as an example
print(res_main[0].summary())

def print_exponentiated_results(result):
    print("\nExponentiated Coefficients (Odds Ratios):\n")
    print(np.exp(result.params))  # Exponentiate the coefficients
    print("\nStandard Errors:\n")
    print(result.bse)  # Standard errors remain the same
    print("\nP-values:\n")
    print(result.pvalues)  # P-values remain the same

# Display the exponentiated results for the first model as an example
print_exponentiated_results(res_main[0])
print_exponentiated_results(res_main[1])
print_exponentiated_results(res_main[2])
print_exponentiated_results(res_main[3])
print_exponentiated_results(res_main[4])
## plot
# Prepare a function to extract exponentiated coefficients and confidence intervals
def extract_model_info(result):
    coef = np.exp(result.params)  # Exponentiated coefficients (Odds Ratios)
    conf = np.exp(result.conf_int())  # Exponentiated confidence intervals
    conf.columns = ['2.5%', '97.5%']  # Rename the columns
    return coef, conf

# Create a DataFrame to store the results for plotting
plot_data = pd.DataFrame()

# Loop over models to extract results and combine them
for i, result in enumerate(res_main):
    coef, conf = extract_model_info(result)
    model_data = pd.DataFrame({
        'Odds Ratio': coef,
        'Lower CI': conf['2.5%'],
        'Upper CI': conf['97.5%'],
        'Variable': coef.index,
        'Model': f'Model {i+1}'
    })
    plot_data = pd.concat([plot_data, model_data])

# Plot the results
plt.figure(figsize=(10, 6))
sns.pointplot(x='Odds Ratio', y='Variable', hue='Model', data=plot_data,
              join=False, dodge=True, markers='o', capsize=.1, errwidth=1.5)

# Add vertical line for OR=1 (no effect)
plt.axvline(x=1, color='grey', linestyle='--')

# Customize plot
plt.title('Odds Ratios and Confidence Intervals for Ordinal Regression Models')
plt.xlabel('Odds Ratio')
plt.ylabel('Variable')
plt.tight_layout()
plt.show()

####################################################
# Prepare a function to extract exponentiated coefficients and confidence intervals
def extract_model_info(result):
    coef = np.exp(result.params)  # Exponentiated coefficients (Odds Ratios)
    conf = np.exp(result.conf_int())  # Exponentiated confidence intervals
    conf.columns = ['2.5%', '97.5%']  # Rename the columns
    return coef, conf

# Create a DataFrame to store the results for plotting
plot_data = pd.DataFrame()

# Loop over models to extract results and combine them, but only for lninc and lnvac
for i, result in enumerate(res_main):
    coef, conf = extract_model_info(result)
    # Filter for 'lninc' and 'lnvac' only (if they exist in the model)
    variables_of_interest = coef.index.intersection(['lninc', 'lnvac'])
    # Calculate y positions so that Model 1 is on top
    max_models = len(models) * 2  # max y position based on number of models and variables
    model_data = pd.DataFrame({
        'Odds Ratio': coef[variables_of_interest],
        'Lower CI': conf.loc[variables_of_interest, '2.5%'],
        'Upper CI': conf.loc[variables_of_interest, '97.5%'],
        'Variable': variables_of_interest,
        'Model': f'Model {i+1}',
        'y_pos': max_models - (i * 2 + np.arange(len(variables_of_interest)))  # Reverse the stacking order
    })
    plot_data = pd.concat([plot_data, model_data])

# Customize plot style and background
plt.style.use('seaborn-darkgrid')
plt.figure(figsize=(10, 8))

# Set the gray background
plt.gca().set_facecolor('lightgray')
plt.gcf().set_facecolor('lightgray')

# Iterate over each variable and plot with error bars, stacked vertically (red color)
for i, row in plot_data.iterrows():
    plt.errorbar(row['Odds Ratio'], row['y_pos'], xerr=[[row['Odds Ratio'] - row['Lower CI']], [row['Upper CI'] - row['Odds Ratio']]],
                 fmt='o', capsize=4, markersize=8, color='red', label=row['Model'] if row['y_pos'] % 2 == 0 else "")

# Add vertical line for OR=1 (no effect)
plt.axvline(x=1, color='grey', linestyle='--')

# Customize plot
plt.title('Stacked Odds Ratios and Confidence Intervals (State-level Incidence and Vaccination Rates)')
plt.xlabel('Odds Ratio')

# Replace variable names with custom labels on the y-axis
plot_data['Variable'] = plot_data['Variable'].replace({
    'lninc': 'State-level incidence rate',
    'lnvac': 'State-level vaccination rate'
})

plt.yticks(plot_data['y_pos'], plot_data['Variable'] + " (" + plot_data['Model'] + ")")
plt.tight_layout()
plt.show()


