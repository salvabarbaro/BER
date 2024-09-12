# BER
Replication Data to the Paper entitled "Laboratory Experiments vs. Opportunistic Behavior: State Governments Strategies in Times of Pandemic."

## Description

This repository contains R and Stata files for replication purposes. Data sets are also embedded in this repository.

1. Main analysis_replication file.de [Stata replication file for the main analyses (Tables 1 to 4)] This file has actually been used for conducting the regressions.
2. MainAnalysisOrdinal.R [Similar to the *.do file (1), but written for R.]
3. bootstrap.R [The main analyses (Table 1) conducted by a boostrap approach]
4. bootstrapFE.R [Fixed effect regressions (Table 2 (3) by bootstrapping]
5. Visualisations.R [Some R code to visualize (ggplot, plotreg, texreg etc.) the results conducted by MainAanalysisOrdinal.R]
6. Tweets_replication file.do [Analysis for the analyses that centered around the prime ministers' conferences]



## Getting Started

### Dependencies

* The codes were all tested on a debian linux. The stata file also works well on a windows OS. However, the bootstrap replication file uses parallel computing. Be aware of the hardware requirements. Please refer to the comments in the bootstrap script for more details. 

### Installing

Stand alone script. The library requirements can be seen at the scripts' headings.

### Executing program

* Line-by-line in an Editor (like RStudio) as well as as entire scripts (R < *.R)

## Help

Any advise for common problems or issues:
None. Please get in touch with the maintainer in case of issues with the scripts.

## Authors

ex. Salvatore Barbaro  
ex. [@salvabarbaro](https://decision-making.economics.uni-mainz.de)

## Version History

* 0.1
    * Initial Release

## License

This project is licensed under the GNU License.

## Acknowledgments

