## Figure 2
from pytrends.request import TrendReq
import pandas as pd
import time
from datetime import date
import matplotlib.pyplot as plt

# --- 2. Define parameters ----------------------------------------------------
keywords_all = ["MPK", "Ministerpräsidentenkonferenz", "corona mpk", "beschlüsse mpk", "ergebnisse ministerpräsidentenkonferenz"]  # example list of keywords
geo_region = "DE"                # Germany
time_range = "2020-03-01 2021-09-21"  # same as in your R snippet

# --- 3. Connect to Google Trends --------------------------------------------
# hl = interface language; tz = timezone offset in minutes
pytrends = TrendReq(hl="de-DE", tz=120)

# --- 4. Download interest over time -----------------------------------------
# pytrends allows multiple keywords (max 5 per request)
pytrends.build_payload(kw_list=keywords_all,
                       timeframe=time_range,
                       geo=geo_region)

data = pytrends.interest_over_time()
data["hits"] = data.iloc[:, 0:5].sum(axis=1)


mpk_dates = [
    date(2020, 3, 12), date(2020, 3, 16), date(2020, 3, 22),
    date(2020, 4, 1),  date(2020, 4, 15), date(2020, 4, 30),
    date(2020, 5, 6),  date(2020, 5, 26),
    date(2020, 6, 17),
    date(2020, 7, 16),
    date(2020, 8, 27),
    date(2020, 9, 29),
    # date(2020, 10, 7),  # Telefonschalte
    date(2020, 10, 14), date(2020, 10, 28),
    date(2020, 11, 16), date(2020, 11, 25),
    # date(2020, 12, 2),
    date(2020, 12, 13),
    date(2021, 1, 5),  date(2021, 1, 19),
    date(2021, 2, 1),  date(2021, 2, 10),
    date(2021, 3, 3),  date(2021, 3, 19), date(2021, 3, 22),
    date(2021, 4, 26),
    date(2021, 5, 27),
    date(2021, 8, 10)
]

plt.figure(figsize=(10, 5))
plt.plot(data.index, data["hits"], color="steelblue", linewidth=2)

# --- 2. Aesthetics --------------------------------------
plt.title("Google Trends — Combined Interest (Hits) in Germany, 2020–2021", fontsize=13)
plt.xlabel("Date")
plt.ylabel("Search interest (sum of keywords)")
plt.grid(True, linestyle="--", alpha=0.6)

# vertical lines for MPK dates ----------
for d in mpk_dates:
    plt.axvline(d, color="red", linestyle="--", linewidth=1, alpha=0.6)

plt.tight_layout()
plt.savefig("Fig02Python.pdf", dpi = 300)
plt.show()

