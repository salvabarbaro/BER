# BER
**Replication Data for the Paper:**  
*Autonomy and Accountability: Strategic Behavior of German State Leaders During the COVID-19 Pandemic*

---

## 📂 Description

This repository contains R and Stata scripts, along with embedded datasets, for replicating the analyses presented in the paper. Below is a summary of the included files:

1. **`analysis_replication file.do`**  
   Stata replication file for the main analyses (Tables 1 to 4). This script was used to conduct the primary regressions.

2. **`MainAnalysisOrdinal.R`**  
   R script replicating the analyses from the Stata `.do` file, focusing on ordinal regression.

3. **`bootstrap.R`**  
   R script performing bootstrap analyses for the main results (Table 1).

4. **`bootstrapFE.R`**  
   R script for fixed effects regressions (Table 2 and 3) using bootstrapping.

5. **`Visualisations.R`**  
   R script generating visualizations (e.g., ggplot, plotreg, texreg) based on results from `MainAnalysisOrdinal.R`.

6. **`Tweets_replication file.do`**  
   Stata script analyzing data related to the prime ministers' conferences.

---

## 🚀 Getting Started

### 🔧 Dependencies

- The scripts were tested on Debian Linux.  
- The Stata file is compatible with Windows OS.  
- The bootstrap scripts use parallel computing. Please ensure your hardware meets the requirements. Refer to comments in the scripts for further details.

### 📦 Installation

These are standalone scripts. Check the header comments in each script for library dependencies.

---

## 🖥️ Executing the Program

You can run the scripts:
- Line-by-line in an editor like RStudio or Stata.
- As complete scripts using:
  ```bash
  Rscript <script_name.R>


## Common Issues

Non reported (yet)

## Authors

Salvatore Barbaro
Reyn van Ewijk
Julia M.Rode

## Version History
0.1 

## License
This project is licensed under the GNU General Public License.
