import pandas as pd
import numpy as np
import statsmodels.api as sm
from statsmodels.miscmodels.ordinal_model import OrderedModel

# --- Load your data ---
df = pd.read_stata("Main_data_set_replication.dta")

# --- Subset the data (wave 2 or 3 only) ---
df = df.query("wave in [2, 3]").copy()

# --- Define the formula-like model variables ---
y = df["index"].astype("category")  # dependent variable must be ordinal

X = df[["incdev", "vacdev", "prevac", "att_t_fed",
        "FKM21", "econ_strength", "weeknr", "date"]].copy()

# Convert categorical predictor (like i.weeknr in Stata)
X = pd.get_dummies(X, columns=["weeknr"], drop_first=True)

# --- Fit the Ordered Logit model ---
model = OrderedModel(
    endog=y,
    exog=X.drop(columns=["date"]),   # date used only for clustering
    distr="logit"
)

res = model.fit(method='bfgs', disp=False)

# --- Cluster-robust standard errors (cluster by date) ---
cov = res.get_robustcov_results(cov_type='cluster',
                                groups=X["date"])

# --- Display odds ratios and cluster-robust CIs ---
params = np.exp(cov.params)  # odds ratios
conf_int = np.exp(cov.conf_int())  # 95% CI in OR scale

results_table = pd.DataFrame({
    "Odds Ratio": params,
    "CI lower": conf_int[0],
    "CI upper": conf_int[1],
    "p-value": cov.pvalues
})

print(results_table.loc[["incdev", "vacdev", "prevac",
                         "att_t_fed", "FKM21", "econ_strength"]])
