# slim_ologit.py
import numpy as np
import pandas as pd
from pandas.api.types import CategoricalDtype
from statsmodels.miscmodels.ordinal_model import OrderedModel

# --- Load & subset ---
df = pd.read_stata("Main_data_set_replication.dta")
df = df[df["wave"].isin([2, 3])].copy()

# Dep var as ordered categorical
if not isinstance(df["index"].dtype, CategoricalDtype) or not df["index"].dtype.ordered:
    cats = np.sort(df["index"].dropna().unique())
    df["index"] = pd.Categorical(df["index"], categories=cats, ordered=True)
else:
    df["index"] = df["index"].cat.remove_unused_categories()

# 'date' as linear trend if datetime
if np.issubdtype(df["date"].dtype, np.datetime64):
    df["date_num"] = (df["date"] - df["date"].min()).dt.days
    date_var = "date_num"
else:
    date_var = "date"

# week fixed effects (i.weeknr) — make dummies as float to avoid object dtype issues
wk = pd.get_dummies(df["weeknr"], prefix="week", drop_first=True, dtype=float)
df = pd.concat([df, wk], axis=1)

def fit_and_print(title, y, X, cluster):
    d = df[[y, cluster] + X].dropna().copy()

    # exog must be purely numeric float
    exog = d[X].astype(float)

    # --- DROP constant (zero-variance) regressors ---
    const_cols = [c for c in exog.columns if np.nanstd(exog[c].to_numpy()) == 0.0]
    if const_cols:
        print(f"[{title}] Dropping constant column(s): {const_cols}")
        exog = exog.drop(columns=const_cols)

    # fit
    mod = OrderedModel(d[y], exog, distr="logit")
    res = mod.fit(method="bfgs", disp=False,
                  cov_type="cluster", cov_kwds={"groups": d[cluster]})

    # slope terms only (exclude thresholds)
    slope = [nm for nm in res.params.index if nm in exog.columns]
    b, se = res.params[slope], res.bse[slope]
    z = 1.959963984540054
    lo, hi = b - z*se, b + z*se

    print(f"\n=== {title} ===  (N={len(d)}, clusters={d[cluster].nunique()})")
    for nm in slope:
        OR, L, H, p = np.exp(b[nm]), np.exp(lo[nm]), np.exp(hi[nm]), res.pvalues[nm]
        print(f"{nm:>18s}: {OR:6.2f}  [{L:6.2f}, {H:6.2f}]  p={p:.3g}")

# ----- Model specs (Stata parity) -----
fit_and_print("model1",
              y="index",
              X=["lninc", "lnincfed"],
              cluster="date")

fit_and_print("model2",
              y="index",
              X=["lninc", "lnincfed", date_var],
              cluster="date")

fit_and_print("model3",
              y="index",
              X=["lninc", "lnincfed"] + list(wk.columns),
              cluster="date")

fit_and_print("model4",
              y="index",
              X=["lninc", "lnincfed", "att_t_fed", "FKM21", "econ_strength"] + list(wk.columns),
              cluster="date")

fit_and_print("model5",
              y="index",
              X=["lninc", "lnincfed", "lnvac", "lnvacfed", "att_t_fed", "FKM21", "econ_strength"] + list(wk.columns),
              cluster="date")

#######   FIXED EFFECTS ORDERED MODELS
# 1) Dependent variable must be *ordered* categorical
#    (adjust levels/order if you need a specific ordering)
df["index"] = pd.Categorical(df["index"], ordered=True)

# 2) Fixed-effects group as categorical
df["statenumber"] = df["statenumber"].astype("category")

# 3) Convert date to a numeric regressor; keep original for clustering if needed
df["date"] = pd.to_datetime(df["date"], errors="coerce")
df["date_num"] = df["date"].astype("int64") / 1e9 / 86400

# 4) Build design matrix with state FE (dummies)
X = df[["lninc", "lnincfed", "date_num", "statenumber"]].copy()
X = pd.get_dummies(X, columns=["statenumber"], drop_first=True)

# 5) Ensure purely numeric, drop any zero-variance/constant columns
X = X.apply(pd.to_numeric, errors="coerce").astype(float)
nonconst = X.nunique(dropna=True) > 1
X = X.loc[:, nonconst]

# 6) Align X and y, drop rows with missing values consistently
y = df["index"]
keep = X.notna().all(axis=1) & y.notna()
X = X.loc[keep]
y = y.loc[keep]

# 7) Fit ordered logit
mod = OrderedModel(endog=y, exog=X, distr="logit")
res = mod.fit(method="bfgs", disp=False)

print(res.summary())

# Odds ratios
print("\nOdds ratios:\n", np.exp(res.params))
params = res.params
cov = res.cov_params()

# Compute standard errors
se = np.sqrt(np.diag(cov))

# Wald 95% CIs in log-odds scale
z = 1.96
lower = params - z * se
upper = params + z * se

# Exponentiate to get odds ratios and CIs
or_df = pd.DataFrame({
    "Odds Ratio": np.exp(params),
    "CI Lower": np.exp(lower),
    "CI Upper": np.exp(upper)
})

# Show numbers with 3 decimal places (adjust as needed)
pd.options.display.float_format = '{:.3f}'.format

print(or_df)


## FE2:
## lninc + lnincfed + lnvac + lnvacfed +  prevac + timetrend + factor(Land),
X = df[["lninc", "lnincfed", "lnvac", "lnvacfed", "prevac", "date_num", "statenumber"]].copy()
X = pd.get_dummies(X, columns=["statenumber"], drop_first=True)

# 5) Ensure purely numeric, drop any zero-variance/constant columns
X = X.apply(pd.to_numeric, errors="coerce").astype(float)
nonconst = X.nunique(dropna=True) > 1
X = X.loc[:, nonconst]

# 6) Align X and y, drop rows with missing values consistently
y = df["index"]
keep = X.notna().all(axis=1) & y.notna()
X = X.loc[keep]
y = y.loc[keep]

# 7) Fit ordered logit
mod = OrderedModel(endog=y, exog=X, distr="logit")
res = mod.fit(method="bfgs", disp=False)

print(res.summary())

# Odds ratios
print("\nOdds ratios:\n", np.exp(res.params))
params = res.params
cov = res.cov_params()

# Compute standard errors
se = np.sqrt(np.diag(cov))

# Wald 95% CIs in log-odds scale
z = 1.96
lower = params - z * se
upper = params + z * se

# Exponentiate to get odds ratios and CIs
or_df = pd.DataFrame({
    "Odds Ratio": np.exp(params),
    "CI Lower": np.exp(lower),
    "CI Upper": np.exp(upper)
})

# Show numbers with 3 decimal places (adjust as needed)
pd.options.display.float_format = '{:.3f}'.format

print(or_df)