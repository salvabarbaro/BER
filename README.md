# BER

**Replication Data for the Paper:**\
*Autonomy and Accountability: Strategic Behaviour of German State Leaders During the COVID-19 Pandemic*\
*Public Choice (2025/2026) - In-Press status*\
*DOI: tba*

------------------------------------------------------------------------

## 📂 Description

This repository contains **R**, **Stata**, and **Python** scripts, along with embedded datasets, for replicating the analyses presented in the paper.\
The main replication files are the two **`.do`** files. However, the **R** and **Python** scripts also reproduce most of the empirical evaluations reported in the paper.

In addition to the index values derived by human coders, we provide **AI-generated index values** obtained via the **OpenAI API**. The connection to this API was established through a Python script, while the matching to the existing dataset was implemented in the R replication file.

------------------------------------------------------------------------

## 🧮 Overview of Replication Files

The following table summarises which analyses can be reproduced by which replication file.

| Assessment | MainStata.do | ReplicationR.R | ReplicationPython.py | TweetsStata.do | aiprompt2.py |
|------------|:----------:|:----------:|:----------:|:----------:|:----------:|
| Table 2 | x | x | x |  |  |
| AI-generated index values |  |  |  |  | x |
| Table 3 (1)–(3) | x | x |  |  |  |
| Table 3 (4)–(5) | x |  |  |  |  |
| Table 4 | x |  |  |  |  |
| Table 5 | x | x |  |  |  |
| Table C1 | x | x | x |  |  |

------------------------------------------------------------------------

## 📊 Figures

The following table indicates which script produces each figure.

| Figure | Script                  |
|:-------|:------------------------|
| Fig. 1 | Fig01.R                 |
| Fig. 2 | Fig02.py *(or Fig02.R)* |
| Fig. 3 | Fig03.R                 |
| Fig. 4 | Fig03.R                 |

------------------------------------------------------------------------

## 📁 Data Summary

1.  **`IDtable.csv`**\
    Lookup table linking AI-generated index values to the corresponding rows in the main dataset.

2.  **`Main_data_set_replication.dta`**\
    Main dataset. For convenience, both R and Python scripts import this file directly.

3.  **`Tweets_replication.dta`**\
    Matches the dates of statements to the corresponding Prime Minister Conferences.

4.  **`Statement23.csv`**\
    Contains statements from the second and third waves. This dataset is used by `aiprompt2.py`.

------------------------------------------------------------------------

## 🚀 Getting Started

### 🔧 Dependencies

-   Scripts were tested on **Debian Linux**.\
-   The Stata files are compatible with **Windows OS**.

### 📦 Installation

These are standalone scripts. Please check the header comments in each script for required library dependencies.

------------------------------------------------------------------------

## 🖥️ Executing the Programs

You can run the scripts: - Line-by-line in an editor such as **RStudio**, **Positron**, or **Stata**, or\
- As complete scripts via the terminal:

\`\`\`bash Rscript \<script_name.R\>
