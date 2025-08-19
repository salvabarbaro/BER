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
